//==============================================================================
// hxGrid framework
// Copyright (C) 2007 by Roman Lut
// hax@deep-shadows.com
// http://www.deep-shadows.com/hax/
//==============================================================================

unit T_AgentThreadPool;

interface
uses Windows, Classes, SysUtils, SyncObjs, T_Semaphore, T_PoolThread,
     T_AgentPeer, T_FileCache, I_GenericStream, T_AgentWorkerThread;


//==============================================================================
//==============================================================================
type TAgentThreadPool = class
   public
    parent          : TObject; //TAgent

    //contatins number of free threads
    //used by AgentWorkerThread to assign tasks only when there is free threads in the pool
    freeThreadCountSemaphore  : TSemaphore;

    //used while waiting for all tasks became finished
    taskDoneEvent    : TEvent;

    terminateEvent   : TEvent;
    newTaskEvent     : TSemaphore;

    poolSize         : DWORD;
    pool             : array of TPoolThread;

    tasksList        : TList;  //^TPoolTaskItem
    taskListAccessCS : TCriticalSection;

    constructor Create(parent: TObject);  //TAgent
    destructor Destroy(); override;

    function GetTasksList(const userIP: string): string;

    procedure RetriveItem(var item: TPoolTaskItem; thread :TPoolThread);

    procedure AddTask(const userIP : string;
                                   sessionId : DWORD;
                                   peer : TAgentPeer;
                                   fileCache : TFileCache;
                                   workerThread : TAgentWorkerThread;
                                   taskId : DWORD;
                                   const fileName : string;
                                   const procName : string;
                                   var inStream : IGenericStream);

    procedure WaitForTasksCompletion(sessionId: DWORD);

    class procedure SetSuspend(suspendTime_: DWORD);
    class function GetSuspend(): DWORD;

   private

    procedure AssignThreadsToCPUs();

    procedure FreeTasks();
  end;

implementation
uses hxgridcommon, T_Agent;

var
 suspendTime      : DWORD;  //number of seconds left, then resume

//==============================================================================
//==============================================================================
constructor TAgentThreadPool.Create(parent: TObject);
var
 i: DWORD;
begin
 self.parent:=parent;

 poolSize:=(parent as TAgent).settings.poolSize;

 (parent as TAgent).DebugWrite('Created pool of %u threads.',[poolSize]);

 FreeThreadCountSemaphore:=TSemaphore.Create(poolSize,poolSize);

 taskDoneEvent:=TEvent.Create(nil,false,false,'');

 terminateEvent:=TEvent.Create(nil, true, false, '');
 newTaskEvent:=TSemaphore.Create(0,100000);

 tasksList:=TList.Create();
 taskListAccessCS:=TCriticalSection.Create();

 SetLength(pool, poolSize);

 for i:=0 to poolSize-1 do pool[i]:=TPoolThread.Create(self);

 AssignThreadsToCPUs();
end;

//==============================================================================
//==============================================================================
destructor TAgentThreadPool.Destroy();
var
 i: DWORD;
begin
 for i:=0 to poolSize-1 do
  begin
   (parent as TAgent).DebugWrite('THREADPOOL: terminating thread %u...',[i]);

   pool[i].priority:=tpNormal;
   pool[i].Terminate();
   TerminateEvent.SetEvent();
   pool[i].WaitFor();

   (parent as TAgent).DebugWrite('THREADPOOL: thread %d terminated',[i]);
  end;

 FreeTasks();

 tasksList.Destroy();
 taskListAccessCS.Destroy();

 FreeThreadCountSemaphore.Destroy;

 taskDoneEvent.Destroy();

 terminateEvent.Destroy();
 newTaskEvent.Destroy();
end;

//==============================================================================
//==============================================================================
function TAgentThreadPool.GetTasksList(const userIP: string): string;
var
 i:integer;
begin
 taskListAccessCS.Enter();

 result:='';

 for i:=0 to tasksList.Count-1 do
  if TPoolTaskItem(tasksList.items[i]).userIP=userIP then
   begin
    if result<>'' then result:=result+', ';
    result:=result+intToStr(int64(TPoolTaskItem(tasksList.items[i]).taskId))+'#';
   end;

 for i:=0 to poolSize-1 do
  if (pool[i].currentTask<>nil) and (pool[i].currentTask.userIP = userIP) then
   begin
    if result<>'' then result:=result+', ';
    result:=result+intToStr(int64(pool[i].currentTask.taskId))+'*';
   end;

 taskListAccessCS.Leave();
end;

//==============================================================================
//==============================================================================
//this is called by each thread from pool
procedure TAgentThreadPool.RetriveItem(var item: TPoolTaskItem; thread : TPoolThread);
var
 events: array [0..1] of THANDLE;
 d: dword;
begin

 //in suspended mode, sleep here when poolthread is trying to request tasks
 while (suspendTime>0) and ((parent as TAgent).destroying=false) do
        begin
         //return nil immediately, if poolthread is going to be destroyed
         if (thread.GetTerminated()=true) then
          begin
           item:=nil;
           exit;
          end;

         (parent as TAgent).DebugWrite('Suspended...',[]);
         Sleep(1000);
        end;

 freeThreadCountSemaphore.ReleaseSemaphore();  //signal - we have waiting thread

 taskDoneEvent.SetEvent(); //signal - used in WaitForTasksCompletion()

 events[0]:=terminateEvent.handle;
 events[1]:=newTaskEvent.GetHandle();
 d:=WaitForMultipleObjects(2, @events[0], false, INFINITE);

 if (d=0) then
  begin
   item:=nil;
   exit;
  end;

 //note: we've got 1 semaphore count

 taskListAccessCS.Enter();

 if tasksList.Count=0 then
  begin
   (parent as TAgent).DebugWrite('tasksList.Count=0!!!1',[]);
  end;

 assert(tasksList.Count>0);

 item:=tasksList[0];
 tasksList.Delete(0);

 taskListAccessCS.Leave();
end;

//==============================================================================
//==============================================================================
procedure TAgentThreadPool.AddTask(const userIP : string;
                                   sessionId : DWORD;
                                   peer : TAgentPeer;
                                   fileCache : TFileCache;
                                   workerThread : TAgentWorkerThread;
                                   taskId : DWORD;
                                   const fileName : string;
                                   const procName : string;
                                   var inStream : IGenericStream);
var
 item: TPoolTaskItem;
 d: dword;
begin
 item:=TPoolTaskItem.Create();
 item.userIP := userIP;
 item.sessionId := sessionId;
 item.peer := peer;
 item.fileCache := fileCache;
 item.workerThread := workerThread;
 item.taskId := taskId;
 item.fileName := fileName;
 item.procName := procName;
 pointer(item.inStream) := pointer(inStream);  //no addref, transfer ownership

 d:=item.inStream._addref();
 d:=item.inStream._release();

 assert(d=1);  

 taskListAccessCS.Enter();

 tasksList.Add(item);

 taskListAccessCS.Leave();

 newTaskEvent.ReleaseSemaphore();  //signal - we have 1 new task
end;

//==============================================================================
//==============================================================================
procedure TAgentThreadPool.WaitForTasksCompletion(sessionId: DWORD);
var
 i: integer;
 b:boolean;
begin

 while (true) do
  begin
   b:=true;

   taskListAccessCS.Enter();

   //normally, workerthread are not adding tasks to threadpool
   //tasks list, if there is no free threads to run it.
   //but here is a very little chance that
   //pool thread was not quick enought to retrive task from list
   for i:=0 to tasksList.Count-1 do
    if TPoolTaskItem(tasksList.Items[i]^).sessionId=sessionId then
     begin
      b:=false;
      break;
     end;

   if (b=true) then
    for i:=0 to poolSize-1 do
     begin
      if (pool[i].currentTask<>nil) and
         (pool[i].currentTask.sessionId=sessionId) then
       begin
        b:=false;
        break;
       end;
     end;

   taskListAccessCS.Leave();

   if (b) then break;

   //todo: what if two workerthreads are waiting for taskDone event?
   //what if other thread has reset event before this thread
   //start waiting?
   //temp: wait for 1 sec, not infinite
   taskDoneEvent.WaitFor(1000);
  end;
end;

//==============================================================================
//==============================================================================
procedure TAgentThreadPool.AssignThreadsToCPUs();
var
 affinity : DWORD;
 count: DWORD;
 pa,sa: DWORD;
begin
 if (parent as TAgent).settings.explicitCPU=false then exit;

 GetProcessAffinityMask(GetCurrentProcess(), pa, sa);

(parent as TAgent).DebugWrite('GetProcessAffinityMask = %u.',[pa]);

 count:=poolsize-1;
 affinity:=$80000000;
 while true do
  begin
   if (pa and affinity)<>0 then
    begin
     SetThreadAffinityMask(pool[count].handle,affinity);
     (parent as TAgent).DebugWrite('Thread %u affinity = %u.',[count,affinity]);

     if (count=0) then break;

     dec(count);
    end;
   affinity:=affinity shr 1;
   if affinity=0 then affinity:=$80000000;
  end;
end;

//==============================================================================
//==============================================================================
class procedure TAgentThreadPool.SetSuspend(suspendTime_: DWORD);
begin
 suspendTime:=suspendTime_;
end;

//==============================================================================
//==============================================================================
class function TAgentThreadPool.GetSuspend(): DWORD;
begin
 result:=suspendTime;
end;

//==============================================================================
//==============================================================================
procedure TAgentThreadPool.FreeTasks();
var
 i: integer;
 d: dword;
begin
 for i:=0 to tasksList.Count-1 do
  begin
   d:=TPoolTaskItem(tasksList.Items[i]).inStream._AddRef();
   d:=TPoolTaskItem(tasksList.Items[i]).inStream._Release();
   assert(d=1);
   TPoolTaskItem(tasksList.Items[i]).inStream:=nil;
   TPoolTaskItem(tasksList.Items[i]).Destroy();
   tasksList.Items[i]:=nil;
  end;
 tasksList.Clear;
end;


begin
 suspendTime:=0;
end.
