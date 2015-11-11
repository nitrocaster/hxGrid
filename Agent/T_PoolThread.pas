//==============================================================================
// hxGrid framework
// Copyright (C) 2007 by Roman Lut
// hax@deep-shadows.com
// http://www.deep-shadows.com/hax/
//==============================================================================

unit T_PoolThread;

interface
uses Windows, Classes, T_FileCache, I_GenericStream, T_AgentPeer, T_AgentWorkerThread;

//==============================================================================
//==============================================================================
type TPoolTaskItem = class
  public
   userIP      : string;
   sessionId   : DWORD;

   taskId      : DWORD;
   fileName    : string;
   procName    : string;
   inStream    : IGenericStream;

   fileCache   : TFileCache;
   peer        : TAgentPeer;
   workerThread: TAgentWorkerThread;
 end;

//==============================================================================
//==============================================================================
//one of the threads in pool, runs tasks
type TPoolThread = class(TThread)
   private

    parent : TObject; //TAgentTreadPool

   protected
    procedure Execute; override;

   public
    //guarded by parent.taskListAccessCS
    //can be NULL
    currentTask: TPoolTaskItem;

    constructor Create(parent: TObject);

    function GetTerminated(): boolean;
  end;

implementation
uses Sysutils, T_AgentThreadPool, I_GridUser, HTTPApp, T_Agent,
     T_GenericStream, hxgridcommon;

//==============================================================================
//==============================================================================
constructor TPoolThread.Create(parent: TObject);
begin
 self.parent := parent;
 currentTask:=nil;
 inherited Create(false);
end;

//==============================================================================
//==============================================================================
procedure TPoolThread.Execute;
var
 DLLhandle: THANDLE;
 proc: TTaskProc;
 outStream: IGenericStream;
 s:string;
 d: dword;

 procedure FreeCurrentTask();
 var
  workerThread: TAgentWorkerThread;
  d: DWORD;
 begin
  ((parent as TAgentThreadPool).taskListAccessCS.Enter());

  d:=currenttask.inStream._AddRef();
  d:=currenttask.inStream._Release();
  assert(d=1);

  currenttask.inStream:=nil;

  workerThread:=currentTask.workerThread;

  currenttask.Destroy();

  currenttask:=nil;

  ((parent as TAgentThreadPool).taskListAccessCS.Leave());

  workerThread.UpdateStatus();
 end;

begin
 SetThreadName('Pool thread');

 priority:=tpIdle;

 //Windows boosts thread priority when thread does not receive
 //CPU time for a long time
 //we want our agent to work as transparent as possible;
 //disable priority boost

 SetThreadPriorityBoost(handle, true);

 while (not Terminated) do
  begin
   //note: need a procedure, not a function here
   //task should ALWAYS persist somewhere - in tasks list,
   //or in currentTask variable of pool thread -
   //see threadPool.WaitForTasksCompletion()
   (parent as TAgentThreadPool).RetriveItem(currenttask, self);
   if (currenttask=nil) then break; //got terminate event

   d:=currenttask.inStream._addref();
   d:=currenttask.inStream._release();
   assert(d=1);

   //got task from pool tasks list
   DLLhandle:=currenttask.fileCache.GetDLLHandle(HTTPDecode(currentTask.fileName));

   if (DLLHandle=0) then
    begin
     s:='POOLTHREAD: unable to load dll '+currenttask.fileName+ ' for task '+inttostr(int64(currenttask.taskId));

     (((parent as TAgentThreadPool).parent) as TAgent).DebugWrite(s,[]);

     if currenttask.Peer.TaskFailed(currentTask.taskId, s) = false then
      begin
       currentTask.workerThread.SetPeerHasConnectionProblems();
      end;

     FreeCurrentTask();

     continue;
    end;

   @proc:=GetProcAddress(DLLhandle, pchar(currenttask.procName));

   if (@proc=nil) then
    begin
     s:='POOLTHREAD:  unable to run task - proc=nil. filename='+
        currenttask.fileName+
        ' proname='+
        currenttask.procName+
        ' taskId='+
        inttostr(int64(currenttask.taskId));

     (((parent as TAgentThreadPool).parent) as TAgent).DebugWrite(s,[]);
     
     if currenttask.Peer.TaskFailed(currentTask.taskId,s) = false then
      begin
       currentTask.workerThread.SetPeerHasConnectionProblems();
      end;

     FreeCurrentTask();

     continue;
    end;

   //should be created with refcount=1
   pointer(outStream):=pointer(IGenericStream(TGenericStream.Create()));

   (((parent as TAgentThreadPool).parent) as TAgent).DebugWrite('POOLTHREAD: running task %u', [currentTask.taskId]);

   if IsCompressedStream(currentTask.instream) then
    begin
     (((parent as TAgentThreadPool).parent) as TAgent).DebugWrite('POOLTHREAD: Decompressing data for task %u',[currentTask.taskId]);
     DecompressStream(currentTask.inStream);
    end;

   currentTask.inStream.Seek(0);

   try
    //Собственно, запускаем задачу!
    if proc((((parent as TAgentThreadPool).parent) as TAgent),currentTask.sessionId, currentTask.inStream, outStream) = false then
     begin
      raise EInOutError.Create('TaskProc returned false');
     end;

    (((parent as TAgentThreadPool).parent) as TAgent).DebugWrite('POOLTHREAD: task %u done', [currentTask.taskId]);

    if currentTask.Peer.TaskDone(currentTask.taskId,outStream) = false then
     begin
      currentTask.workerThread.SetPeerHasConnectionProblems();
     end;

   except
    on e: Exception do
     begin
      s:='POOLTHREAD: exception "'+E.Message+'" in task '+inttostr(int64(currentTask.taskId));
      (((parent as TAgentThreadPool).parent) as TAgent).DebugWrite(s,[]);
      currentTask.Peer.TaskFailed(currentTask.taskId,s);

      currentTask.workerThread.SetPeerHasConnectionProblems();

     end;
   end;

   d:=outstream._AddRef();
   d:=outstream._Release();
   assert(d=1);
   
   outStream:=nil;

   FreeCurrentTask();
  end;

 (((parent as TAgentThreadPool).parent) as TAgent).DebugWrite('PoolThread terminated...',[]);
end;

//==============================================================================
//==============================================================================
function TPoolThread.GetTerminated(): boolean;
begin
 result:=terminated;
end;


end.
