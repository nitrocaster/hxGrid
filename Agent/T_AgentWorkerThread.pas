//==============================================================================
// hxGrid framework
// Copyright (C) 2007 by Roman Lut
// hax@deep-shadows.com
// http://www.deep-shadows.com/hax/
//==============================================================================

unit T_AgentWorkerThread;

interface
uses Windows, Classes, SyncObjs, hxgridcommon, T_AgentPeer, T_FileCache,
 I_GridUser, I_GenericStream, T_AgentDataCache, T_Semaphore, safefpu;

//==========================================
// class TAgentWorkerThread
//==========================================
//было - thread, который исполняет задачи
//стало - thread,который добавляет задачи в threadpool
type TAgentWorkerThread = class(TThread)
  private
   //доступ к очереди задач
   cs            : TCriticalSection;

   //появилась новая задача
   newTaskEvent    : TSemaphore;

   //should terminate
   terminateEvent  : TEvent;

   //очередь задач
   list          : TList;

   userIP        : string;

   peer          : TAgentPeer;
   fileCache     : TFileCache;
   dataCache     : TAgentDataCache;
   threadPool    : TObject; //TAgentThreadPool;

   parent        : TObject; //TAgent

   bPeerHasConnectionProblems : boolean;

   function GetTasksList(): string;

  protected
   procedure Execute; override;

  public

   //gTerminated flag is used in IAgent->TestConnection() to return S_FALSE to running task,
   //if worker thread is destroyed 
   gTerminated : boolean;
   bInitialized : boolean;

   constructor Create(parent: TObject; const userIp: string);
   destructor Destroy(); override;

   procedure AddTask(taskId: DWORD; const fileName: String; const procName: String; var stream: IGenericStream);

   procedure PurgeTaskList();

   function GetData(dataDesc: pchar; var stream: IGenericStream): HRESULT;
   procedure FreeCachedData(dataDesc: pchar);

   procedure UpdateStatus();

   procedure SetPeerHasConnectionProblems();
   function GetPeerHasConnectionProblems() : boolean;

   procedure GetSessionCacheDirectory(path: pchar);

 end;

implementation
uses T_Agent, SysUtils, Unit1, HTTPApp, T_GenericStream, T_AgentThreadPool;

//========================================
// TListItem
//========================================
type TListItem = record
      taskId    : DWORD;
      fileName  : string;
      procName  : string;
      inStream  : IGenericStream;
     end;

type PListItem = ^TListItem;

//==============================================================================
//==============================================================================
constructor TAgentWorkerThread.Create(parent: TObject; const userIp: string);
begin
 self.UserIP:=userIP;
 self.parent:=parent;
 gTerminated:=false;
 bInitialized:=false;

 bPeerHasConnectionProblems:=false;

 terminateEvent:=TEvent.Create(nil,true,false,'');
 cs:=TCriticalSection.Create();

 list:=TList.Create();

 newTaskEvent:=TSemaphore.Create(0,10000);

 inherited Create(false);
end;

//==============================================================================
//==============================================================================
destructor TAgentWorkerThread.Destroy();
begin
 gTerminated:=true;

 priority:=tpNormal;
 Terminate();
 terminateEvent.SetEvent();

 //WorkerThread will not terminate until all tasks in threadpool are finished
 WaitFor();

 PurgeTaskList();
 
 cs.Destroy();
 terminateEvent.Destroy();
 newTaskEvent.Destroy();
 list.Destroy();
end;

//==============================================================================
//==============================================================================
//вызывается из AgentLobbyThread
procedure TAgentWorkerThread.AddTask(taskId: DWORD; const fileName: String; const procName: String; var stream: IGenericStream);
var
 item : ^TListItem;
 d: dword;
begin
 while (bInitialized=false) do Sleep(1);

 d:=stream._addref();
 d:=stream._Release();

 assert(d=1);

 new(item);
 item.taskId:=taskId;
 item.fileName:=fileName;
 item.procName:=procName;
 pointer(item.inStream):=pointer(stream);
 pointer(stream):=nil;

 cs.Acquire();
 list.add(item);
 cs.Leave();

 newTaskEvent.ReleaseSemaphore();

 UpdateStatus();
end;

//==============================================================================
//==============================================================================
procedure TAgentWorkerThread.Execute;
var
 item : ^TListItem;
 b: boolean;
 count: DWORD;
 sessionId: DWORD;
 events1: array [0..1] of THANDLE;
 events2: array [0..1] of THANDLE;
 d: DWORD;
begin
 SetThreadName('AgentWorkerThread');

 priority:=tpLowest;

 sessionId:=DWORD(self);

 peer:=TAgentPeer.Create(parent,userIP);
 fileCache:=TFileCache.Create(peer,parent,sessionId);
 dataCache:=TAgentDataCache.Create(peer,parent);

 threadPool:=TAgentThreadPool.Create(parent);

 //do not allow AddTask() untill properly initialized
 bInitialized:=true;

 events1[0]:=terminateEvent.handle;
 events1[1]:=newTaskEvent.GetHandle();

 events2[0]:=terminateEvent.handle;
 events2[1]:=(threadPool as TAgentThreadPool).freeThreadCountSemaphore.GetHandle();


 UpdateStatus();

 while (not Terminated) do
  begin
   //wait for terminate, or new task
   d:=WaitForMultipleObjects(2, @events1[0], false, INFINITE);
   if (d=0) then break; //got terminate event

   //note: we have acquired 1 semaphore count - newTask

   UpdateStatus();

   //Waiting for semaphore:
   //The function modifies the state of some types of synchronization objects.
   //Modification occurs only for the object or objects whose signaled state
   //caused the function to return. For example, the count of a semaphore
   //object is decreased by one. When fWaitAll is FALSE, and multiple objects
   //are in the signaled state, the function chooses one of the objects to
   //satisfy the wait; the states of the objects not selected are uneffected.

   //wait for terminate, or free thread in thread pool
   d:=WaitForMultipleObjects(2, @events2[0], false, INFINITE);
   if (d=0) then break; //got terminate event
                               
   //note: we have acquired 1 semaphore count

   cs.Acquire();

   count:=list.count;
   assert(list.count>0);

   item:=list[0];
   list.Delete(0);

   d:=item.inStream._AddRef();
   d:=item.inStream._Release();

   if (d<>1) then
    begin
     messagebeep(0);       //todo !!! wht is it??!!!!
    end;

   assert(d=1);

   (threadPool as TAgentThreadPool).AddTask(userIP, sessionId,
      peer, fileCache, self,
      item.taskId, item.fileName, item.procName, item.inStream);

   //ownership has been transferred to TAgentThreadPool 
   pointer(item.inStream):=nil;   

   //should release cs only after adding item to threadpool list -
   //item should ALWAYS live somewhere - see GetTasksList()
   cs.Leave();

   Dispose(item);


   //taskcompletion should call ReleaseSemaphore() and UpdateStatus()
  end;

 (parent as TAgent).DebugWrite('WORKERTHREAD: Waiting for threadpool to terminate...',[]);

 //need to wait until all tasks of this session became finished in threadPool
 (threadPool as TAgentThreadPool).WaitForTasksCompletion(sessionId);

 (parent as TAgent).DebugWrite('WORKERTHREAD: Waiting for threadpool to finish tasks - done',[]);

 threadPool.Free;

 (parent as TAgent).DebugWrite('WORKERTHREAD: Threadpool released.',[]);

 fileCache.EndSession();

 (parent as TAgent).DebugWrite('WORKERTHREAD: Filecache.Endsession() done.',[]);

 fileCache.Destroy();

 (parent as TAgent).DebugWrite('WORKERTHREAD: Filecache released.',[]);

 dataCache.Destroy();

 (parent as TAgent).DebugWrite('WORKERTHREAD: Datacache released.',[]);

 peer.Destroy();

 (parent as TAgent).DebugWrite('WORKERTHREAD: Peer released.',[]);

 (parent as TAgent).DebugWrite('WORKERTHREAD: Terminated.',[]);
end;

//==============================================================================
//==============================================================================
//отменить все задачи (очистить список)
procedure TAgentWorkerThread.PurgeTaskList();
begin
 cs.Acquire();

 while (list.Count>0) do
  begin
   TListItem(list[0]^).inStream:=nil;
   dispose(PListItem(list[0]));
   list.delete(0);
  end;
  
 cs.Leave();
end;

//==============================================================================
//==============================================================================
function TAgentWorkerThread.GetTasksList(): string;
var
 i: integer;
begin
 cs.Acquire();

 result:=(threadPool as TAgentThreadPool).GetTasksList(userIP);

 for i:=0 to list.Count-1 do
  begin
   if (length(result)>0) then result:=result+',';
   result:=result+intToStr(int64(TListItem(list[i]^).taskid));
  end;

 cs.Leave();
end;

//==============================================================================
//==============================================================================
function TAgentWorkerThread.GetData(dataDesc: pchar; var stream: IGenericStream): HRESULT;
begin
 result:=dataCache.GetData(dataDesc, stream);
end;

//==============================================================================
//==============================================================================
procedure TAgentWorkerThread.FreeCachedData(dataDesc: pchar);
begin
 dataCache.PurgeCache(dataDesc);
end;

//==============================================================================
//==============================================================================
procedure TAgentWorkerThread.UpdateStatus();
begin
 (parent as TAgent).UpdateStatus_SetTasks(userIP, GetTasksList());
end;

//==============================================================================
//==============================================================================
procedure TAgentWorkerThread.SetPeerHasConnectionProblems();
begin
 bPeerHasConnectionProblems:=true;
end;

//==============================================================================
//==============================================================================
function TAgentWorkerThread.GetPeerHasConnectionProblems() : boolean;
begin
 result:=bPeerHasConnectionProblems;
end;

//==============================================================================
//==============================================================================
procedure TAgentWorkerThread.GetSessionCacheDirectory(path: pchar);
begin
 FileCache.GetSessionCacheDirectory(path);
end;



end.
