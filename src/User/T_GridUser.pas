//==============================================================================
// hxGrid framework
// Copyright (C) 2007 by Roman Lut
// hax@deep-shadows.com
// http://www.deep-shadows.com/hax/
//==============================================================================

{$I-}
unit T_GridUser;

interface
uses Windows, Classes, SyncObjs, T_GridUserLobby, hxgridcommon,
     forms, hxScktComp, I_GenericStream, I_GridUser,
     IdUDPClient, IdBaseComponent, IdComponent,
     IdUDPBase, IdUDPServer, IdStack, IdSocketHandle, safefpu;

//========================================
// TGridTask
//========================================
type TGridTask = record
      id               : DWORD;
      moduleName       : string;
      taskProcName     : String;
      inStream         : IGenericStream;
      finalizeProc     : TFinalizeProc;

      agentIp          : string;  //'' или IP адрес агента, на котором в текущий момент выполняется задача

      swappedOut       : boolean;  //task instream has been swapped to disk

      lockedInMemory   : DWORD; //increased in RetriveTask() -  locked untill is sent sucessfully to agent
     end;

type PGridTask = ^TGridTask;


//==========================================
// class TGridUser
//==========================================
//пользователь грида
//thread занимается получением списка агентов с координатора,
//и созданием T_AgentBoss thread
type TGridUser = class (TThread, IGridUser)
  private
   _RefCount      : integer;


   taskCompleteEvent  : TEvent; //закончили выполенение задачи, задачу вернули в список, появился новый агент
   TerminateEvent     : TEvent;

   //доступ к taskList
   tasksListCS    : TCriticalSection;

   tasksList      : TList;

   nextTaskId     : DWORD;

   //доступ к agentsList
   agentsListCS       : TCriticalSection;
   agentsList         : TList;  //TAgentBoss

   //соединение с координатором
   CoordinatorClient  : TClientSocket;

   AllowDublicatesTick : DWORD;  //do not allow dublicates until this time

   IdUDPServer1: TIdUDPServer;

   procedure DisconnectAgents();
   procedure RemoveDeadAgents();

   procedure LoadSettings();

   function TrySwapOut(): boolean;
   function GetSwapFileName(taskId: DWORD): string;

   procedure DumpTasks();
   procedure RemoveSwapFile(taskId: DWORD);
   procedure DumpHeapStatus();

   procedure Broadcast();

   procedure IdUDPServer1UDPRead(Sender: TObject; AData: TStream;
      ABinding: TIdSocketHandle);

   procedure RemoveTaskObject(var task:PGridTask);

  protected
   procedure Execute; override;

  public

   taskAddEvent     : TEvent;

   lobby            : TGridUserLobby;

   getDataCallback  : TGetDataProc;

   settings         : TGridUserSettings;
   settingsCS       : TCriticalSection;


   //number of agents wihtout tasks at all
   starvingAgentsCount : integer;

   constructor Create();                                 
   destructor Destroy(); override;

   procedure DebugWrite(const Format: string; const Args: array of const);

   function GetTotalTasksSize():DWORD;
   function GetTasksCount():DWORD;

   procedure SaveSettings();

   //====== begin COM interface ==========
   function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
   function _AddRef: Integer; stdcall;
   function _Release: Integer; stdcall;

   function GetVersion(var version: DWORD): HRESULT; stdcall;

   //выполнить задачу в гриде
   //возвращает уникальный id
   //moduleName - модуль,в котором находится процедура
   //taskProcName - имя процедуры
   //inStream - stream, который нужно передать на вход taskProc
   //finalizeProc - локальная процедура, которая вызывается после выполнения task
   //
   //очередность выполнения задач не гарантирована
   //thread-safe method
   //taskProc is executed in the grid
   //finalizeProc is executed locally, but in different threads - should be thread-safe
   //
   //stream переадется в эксклюзивное пользование
   //больше его использовать нелья
   //
   //если blocking = true, метод не возвратит управление, пока
   //задача не будет добавлена в список (может бы не добавлена из-за ограничений
   //на количество задач или общей памяти задач)
   //если false - может вернуть S_FALSE, если добавить сейчас нельзя
   //в этом случае можно вызывать WaitForCompletionEvent()
   function RunTask(moduleName: pchar;
                    taskProcName: pchar;
                    inStream : IGenericStream;
                    finalizeProc : TFinalizeProc;
                    var taskId: DWORD;
                    blocking: boolean) : HRESULT; stdcall;

   //подождать выполнения всех запущенных задач
   function WaitForCompletion(): HRESULT; stdcall;

   function IsComplete(var complete: boolean): HRESULT; stdcall;

   procedure BindGetDataCallback(callback: TGetDataProc); stdcall;

   procedure GetSettings(var settings: TGridUserSettings); stdcall;

   procedure SetSettings(var settings: TGridUserSettings); stdcall;

   //дождаться выполнения одной из задач
   //(для задачи выполнился finalize и она удалена из очереди)
   //если задач в очереди 0 - возвращает управление сразу
   //нельзя надеяться, что при успехе очередь уменьшилась на единицу,
   //это только hint, т.к. event мог остаться raised с прошлой задачи
   //вернет S_FALSE если не дождались в течение timeout
   function WaitForCompletionEvent(timeout: DWORD) : HRESULT; stdcall;

   function CompressStream(stream: IGenericStream): HRESULT; stdcall;

   function CancelTasks(): HRESULT; stdcall;

   procedure GetConnectionStatus(var status: TGridUserConnectionStatus); stdcall;

   //====== end COM interface ==========

   //========= internal usage ==========
   procedure FinalizeTask(taskId: DWORD; var outStream: IGenericStream);

   function RetriveTask(const agentIp: string; allowDublicates: boolean) : PGridTask;
   procedure ReturnTaskToList(taskId: DWORD);

   procedure NotifyTaskDone(const agentIp : string; CPUUsed, CPUAvail: single; freeMemory: DWORD);
   procedure NotifyTaskFailed(const agentIp : string; taskid: DWORD);

 end;

implementation
uses SysUtils, T_AgentBoss, IniFiles, shfolder;


//==============================================================================
//==============================================================================
constructor TGridUser.Create();
begin
 SetThreadName('I_GridUser owner thread');

 _RefCount:=1;

 starvingAgentsCount:=0;

 settingsCS:=TCriticalSection.Create();

 LoadSettings();

 getDataCallback:=nil;

 AllowDublicatesTick:=GetTickCount();

 nextTaskId:=GetTickCount();

 taskAddEvent := TEvent.Create(nil, false, false, '');
 taskCompleteEvent:=TEvent.Create(nil, false, false, '');
 terminateEvent:=TEvent.Create(nil, false, false, '');

 tasksListCS := TCriticalSection.Create();
 agentsListCS := TCriticalSection.Create();

 tasksList := TList.Create();
 agentsList := TList.Create();

 lobby:=TGridUserLobby.Create(self);

 CoordinatorClient:=TClientSocket.Create(nil);
 CoordinatorClient.ClientType:=ctBlocking;
 CoordinatorClient.Port:=settings.coordinator_port;

 try
  IdUDPServer1:=TIdUDPServer.Create(nil);
  IdUDPServer1.defaultPort:=settings.user_broadcast_port;
  IdUDPServer1.OnUDPRead:=IdUDPServer1UDPRead;
  IdUDPServer1.ThreadedEvent:=true; //have to use second thread, since there is no main form of applicaton
  IdUDPServer1.Active:=true;
 except
  DebugWrite('USER: Unable to create UDP server socket on port %u',[settings.user_broadcast_port]);
 end;

 inherited Create(false);
end;

//==============================================================================
//==============================================================================
destructor TGridUser.Destroy();
begin
 DebugWrite('GRIDUSER: Destroy: waiting for tasks to complete...',[]);
 WaitForCompletion();

 DebugWrite('GRIDUSER: Terminating griduser thread...',[]);

 Terminate();
 terminateEvent.SetEvent();
 WaitFor();

 DebugWrite('Disconnecting from coordinator...',[]);
 CoordinatorClient.Close();
 CoordinatorClient.Free();

 //необходимо отсоединиться от агентов,
 //чтобы те в свою очередь отсоединились от userlobby
 //иначе не получится уничтожить userlobby
 //т.к. indyserver этого не позволяет -
 //генерит EIDClosedSocked exception
 //а она происходит из-за того, что commandhandler в thread ожидает команду (recvbuffer)
 DebugWrite('Disconnecting agents...',[]);
 DisconnectAgents();

 IdUDPServer1.Destroy();

 DebugWrite('GRIDUSER: Destroying lobby...',[]);
 lobby.Destroy();

 taskAddEvent.Destroy();
 taskCompleteEvent.Destroy();
 terminateEvent.Destroy();

 tasksListCS.Destroy();
 agentsListCS.Destroy();

 tasksList.Destroy();
 agentsList.Destroy();

 settingsCS.Destroy();

 DebugWrite('GRIDUSER: Destroyed.',[]);
end;

//==============================================================================
//==============================================================================
procedure TGridUser.DebugWrite(const Format: string; const Args: array of const);
begin
 if settings.enableDebugOutput=false then exit;

 try
  if (length(args)=0) then OutputDebugString(pchar(format+#13))
                      else OutputDebugString(pchar(sysutils.Format(format, args)+#13));
 except
  MessageBeep(0);
  OutputDebugString(pchar('DEBUGWRITE: Unable to format string'+#13));
 end;
end;

//==============================================================================
//==============================================================================
function TGridUser.GetSwapFileName(taskId: DWORD): string;
var
 strPath : array [0..MAX_PATH] of char;
begin
 SHGetFolderPath( 0, CSIDL_LOCAL_APPDATA, 0, 0 {SHGFP_TYPE_CURRENT}, strPath );
 result := IncludeTrailingPathDelimiter(strPath) + 'hxgrid\TEMP\'+inttostr(int64(taskId))+'.tmp';
end;

//==============================================================================
//==============================================================================
function TGridUser.TrySwapOut(): boolean;
var
 i: integer;
 f: file;
 fileName:string;
begin
 tasksListCS.Acquire();

 result:=false;

 for i:=0 to tasksList.count-1 do
  if (PGridTask(tasksList[i]).swappedOut=false) and
     (PGridTask(tasksList[i]).agentIp<>'') and
     (PGridTask(tasksList[i]).lockedInMemory=0) then
      begin
       //swap out this task
       //todo: handle errors

       DebugWrite('Swapping out task %u',[PGridTask(tasksList[i]).id]);

       fileName:=GetSwapFileName(PGridTask(tasksList[i]).id);
       EnsureFolderExists(fileName);
       assign(f,fileName);
       rewrite(f,1);
       blockwrite(f,PGridTask(tasksList[i]).inStream.GetBasePointer()^,PGridTask(tasksList[i]).inStream.GetLength());
       CloseFile(f);
       PGridTask(tasksList[i]).inStream.Clear();
       PGridTask(tasksList[i]).swappedOut:=true;

       result:=true;
       break;
      end;

 tasksListCS.Leave();
end;

//==============================================================================
//==============================================================================
function TGridUser.RunTask(moduleName: pchar;
                 taskProcName: pchar;
                 inStream : IGenericStream;
                 finalizeProc : TFinalizeProc;
                 var taskId: DWORD;
                 blocking : boolean): HRESULT;
var
 task: PGridTask;
 count: DWORD;
 blockedDueToMemoryUsage: boolean;
 blockedDueToTaskCount: boolean;
 swapOk: boolean;
 wasBlocked: boolean;
 d: dword;
 p: pointer;
begin

// DumpHeapStatus();

// DumpTasks();

 AllowDublicatesTick:=GetTickCount()+ADD_TASK_TIMEOUT;

// d:=gettickcount();
 if inStream.GetLength()>settings.compressThreshold then CompressStream(inStream);
// debugWrite('Stream compression took : %ums',[gettickcount()-d]);

 blockedDueToTaskCount := GetTasksCount()>settings.maxQueqedTasks;
 blockedDueToMemoryUsage := (GetTotalTasksSize()+inStream.GetLength()>settings.userMaxMemoryUsage) and (GetTasksCount()>0);;

 wasBlocked:=false;

 while(GetTasksCount()>0) and (blockedDueToTaskCount or blockedDueToMemoryUsage) do
  begin
   wasBlocked:=true;

   blockedDueToTaskCount := GetTasksCount()>settings.maxQueqedTasks;
   blockedDueToMemoryUsage := (GetTotalTasksSize()+inStream.GetLength()>settings.userMaxMemoryUsage) and (GetTasksCount()>0);

   if (blockedDueToTaskCount=false) and
      (blockedDueToMemoryUsage=true) and
      settings.allowSwapping  then
    begin                                         
     //try to swap out sent tasks
     repeat
      swapOk:=TrySwapOut();
      blockedDueToMemoryUsage := (GetTotalTasksSize()+inStream.GetLength()>settings.userMaxMemoryUsage) and (GetTasksCount()>0);
     until (swapOk=false) or (blockedDueToMemoryUsage=false);
    end;

   if (blockedDueToMemoryUsage or blockedDueToTaskCount)=false then break;
    
   if (blockedDueToMemoryUsage or blockedDueToTaskCount) and (blocking=false) then
    begin
     //can't swap out - exit
     result:=S_FALSE;
     exit;
    end;

   if blockedDueToMemoryUsage then
    begin
     DebugWrite('RunTask is blocked due to user memory usage limit (%uMb)...',[round(settings.userMaxMemoryUsage/(1024*1024))]);
    end
     else
   if blockedDueToTaskCount then
    begin
     DebugWrite('RunTask is blocked due to tasks queqe limit (%u)...',[settings.maxQueqedTasks]);
    end;

   //DumpTasks();
   WaitForCompletionEvent(5000);  //try to swapout in 5000 - maybe some agentBosses have finished sending tasks

  end;

 if wasBlocked then DebugWrite('RunTask unblocked.',[]);

 inStream.Compact();

 //добавить задачу в список
 new(task);
 task.moduleName := moduleName;
 task.taskProcName := taskProcName;
 pointer(task.inStream) := pointer(inStream);  //passed to be used exclusivelly
 task.finalizeProc := finalizeProc;

 task.agentIP:='';
 task.swappedOut:=false;
 task.lockedInMemory:=0;

 tasksListCS.Acquire();
 task.id := nextTaskId;
 taskId:=task.id;
 inc(nextTaskId);
 tasksList.add(task);
 count:=tasksList.Count;
 tasksListCS.Leave();

 AllowDublicatesTick:=GetTickCount()+ADD_TASK_TIMEOUT;

 DebugWrite('Task %u added to queque',[taskId]);

 taskAddEvent.SetEvent();

 result:=S_OK;
end;

//==============================================================================
//==============================================================================
function TGridUser.WaitForCompletion(): HRESULT; stdcall;
var
 b:boolean;
 label l1;
begin
 AllowDublicatesTick:=GetTickCount();

 l1:

 tasksListCS.Acquire();
 b:=(tasksList.count<>0);
 tasksListCS.Leave();

 if b then
  begin
   taskCompleteEvent.WaitFor(INFINITE);
   application.ProcessMessages;
   goto l1;
  end;

 result:=S_OK;
end;

//==============================================================================
//==============================================================================
//этот thread занимается исключительно подсоединением агентов
procedure TGridUser.Execute();
var
 s: string;
 sl :TStringList;
 i,j: integer;
 aboss: TAgentBoss;
 label l1;
begin
  SetThreadName('TGridUser task manager thread');

 sl:=TStringList.Create();

 while (not Terminated) do
  begin
   RemoveDeadAgents();

   try
    if (CoordinatorClient.Socket.Connected = false) then
     begin
      settingsCS.Enter();
      s:=settings.coordinator_ip;
      settingsCS.Leave();

      if s='' then
       begin
        BroadCast();
        terminateEvent.WaitFor(3000);
        continue;
       end
        else
       begin
        CoordinatorClient.Host:=s;

        DebugWrite('USER: Trying to connect to coordinator: %s',[s]);
        
        CoordinatorClient.Open();

        DebugWrite('USER: Connected to coordinator: %s',[s]);
        
       end;
     end;


    if SocketWriteString(CoordinatorClient.Socket,
                         'GET_AGENTS '+inttostr(CURRENT_AGENT_VERSION),
                         MAXINT,
                         settings.enableDebugOutput)=false then
     begin
      raise EInOutError.Create('');
     end;

    if SocketReadString(CoordinatorClient.socket, s, MAXINT) = false then
     begin
      raise EInOutError.Create('');
     end;

    if s<>'none' then
     begin
      sl.commatext:=s;

      // ------ remove already aquired agents from list --------

      agentsListCS.Enter();

      l1:

      for i:=0 to sl.count-1 do
       begin
        for j:=0 to agentsList.Count-1 do
         if TAgentBoss(agentsList[j]).agentIp=sl[i] then
          begin
           sl.delete(i);
           goto l1;
          end;
       end;


      agentsListCS.Leave();

      //---------------------------------------------------------

      //создать TAgentBoss для всех остальных агентов

      for i:=0 to sl.count-1 do
       begin
        aboss:=TAgentBoss.Create(self,sl[i]);

        agentsListCS.Enter();

        AgentsList.Add(aboss);

        agentsListCS.Leave();

        TaskCompleteEvent.SetEvent();
       end;
     end;

   except
    try
     CoordinatorClient.Close();
    except
     //ignore
    end;

    settingsCS.Enter();
    if settings.allowDiscardCoordinatorIp and (settings.coordinator_ip<>'')then 
     begin
      DebugWrite('USER: Discarding invalid coordinator IP: %s',[settings.coordinator_ip]);
      settings.coordinator_ip:='';
     end; 
    settingsCS.Leave();
    BroadCast();
   end;

   terminateEvent.WaitFor(3000);
  end;

 sl.Destroy();
end;

//==============================================================================
//==============================================================================
//вызвать finalize для выполненной задачи
//работает в UserLobbySocketThread
procedure TGridUser.FinalizeTask(taskId: DWORD; var outStream: IGenericStream);
var
 task: PGridTask;
 i: integer;
 d: dword;
 label l1;
begin
 tasksListCS.Acquire();

 for i:=0 to tasksList.Count-1 do
  begin
   if (PGridTask(tasksList.items[i]).id=taskId) then
    begin
     task:=PGridTask(tasksList.items[i]);
     tasksList.Delete(i);

     tasksListCS.Leave();

     DebugWrite('GRIDUSER: Running Finalize for taskId=%u',[taskId]);

     outStream.Seek(0);

     d:=outStream._addref();
     d:=outStream._release();

     try
      task.FinalizeProc(outstream);
     except
      DebugWrite('GRIDUSER: Exception in Finalize for taskId=%u',[taskId]);
     end;

     outstream:=nil;

     RemoveTaskObject(task);

     DebugWrite('GRIDUSER: TaskId=%u finalized',[taskId]);

     goto l1;
    end;
  end;

 tasksListCS.Leave();

 DebugWrite('GRIDUSER: Warning: unable to finalize taskId=%u',[taskId]);

// assert(false);

 l1:

 taskCompleteEvent.SetEvent();
end;

//==============================================================================
//==============================================================================
procedure TGridUser.DisconnectAgents();
var
 i: integer;
begin
 //wait untill all agentboss threads close

 agentsListCS.Enter();

 for i:=0 to agentsList.count-1 do
  begin
   TAgentBoss(agentsList[i]).EndSession();
  end;

 agentsListCS.Leave();

 RemoveDeadAgents();
end;

//==============================================================================
//==============================================================================
//пометить задачу, что она послана на агент agentIp
function TGridUser.RetriveTask(const agentIp: string; allowDublicates: boolean):PGridTask;
var
 i: integer;
 retryCount: integer;
 filename: string;
 f: file;
begin
 tasksListCS.Acquire();

 result:=nil;

 for i:=0 to tasksList.count-1 do
  if PGridTask(tasksList[i]).agentIp='' then
   begin
    PGridTask(tasksList[i]).agentIp:=agentIp;
    inc(PGridTask(tasksList[i]).lockedInMemory);
    result:=tasksList[i];
    break;
  end;

 if (result=nil) and
     (tasksList.count>0) and
     allowDublicates and
     (AllowDublicatesTick<=GetTickCount()) and
     settings.SendDublicateTasks then
  begin
   for retryCount:=1 to tasksList.count*2 do
    begin
     i:=random(tasksList.count);
     if PGridTask(tasksList[i]).agentIp<>agentIp then
      begin
       //check if it is swapped out !!!
       //todo: on network fault, ehrn resending tasks to agents,
       //we can swap in too many tasks and overload maxUserMemoryUsage
       //should check to tasks size and return nil if no memory to swap-in

       if (PGridTask(tasksList[i]).swappedOut=true) then
        begin
         //load task from disk

         DebugWrite('Swapping in task %u',[PGridTask(tasksList[i]).id]);

         filename:=GetSwapFilename(PGridTask(tasksList[i]).Id);
         assignFile(f,fileName);
         reset(f,1);
         PGridTask(tasksList[i]).inStream.SetLength(fileSize(f));
         blockread(f,PGridTask(tasksList[i]).inStream.GetBasePointer()^,PGridTask(tasksList[i]).inStream.GetLength());
         closefile(f);

         RemoveSwapFile(PGridTask(tasksList[i]).Id);

         PGridTask(tasksList[i]).swappedOut:=false;
        end;

       inc(PGridTask(tasksList[i]).lockedInMemory);

       result:=tasksList[i];
       DebugWrite('GRIDUSER: Sending dublicate task taskId=%u to free agent %s (already running at %s)',[result.id, agentIp, result.agentIp]);
       break;
      end;
    end;
  end;

 tasksListCS.Leave();
end;

//==============================================================================
//==============================================================================
//агент не смог отослать задачу агенту. Снять пометку "задача выполняется".
procedure TGridUser.ReturnTaskToList(taskId: DWORD);
var
 i: integer;
begin
 tasksListCS.Acquire();

 for i:=0 to tasksList.count-1 do
  if PGridTask(tasksList[i]).Id=taskId then
   begin
    PGridTask(tasksList[i]).agentIp:='';
    dec(PGridTask(tasksList[i]).lockedInMemory);
    break;
  end;

 tasksListCS.Leave();

 taskAddEvent.SetEvent();
end;

//==============================================================================
//==============================================================================
//сообщить AgentBoss, что на его агенте завершилось выполнение задачи
procedure TGridUser.NotifyTaskDone(const agentIp : string; CPUUsed, CPUAvail: single; freeMemory: DWORD);
var
 i: integer;
begin
 AgentsListCS.Acquire();

 for i:=0 to agentsList.count-1 do
  if (TAgentBoss(agentsList.items[i]).agentIP=agentIp) then
  begin
   TAgentBoss(agentsList.items[i]).TaskCompletedNotify(CPUUsed, CPUAvail, freeMemory);
   break;
  end;

 AgentsListCS.Leave();
end;

//==============================================================================
//==============================================================================
//убрать агентов, от которых давно не было отклика
//и выставить задачам, которые на них выполняюся статус "не выполняются"
procedure TGridUser.RemoveDeadAgents();
var
 i,j: integer;
 dead: TAgentBoss;
 label l1;
begin
 l1:

 agentsListCS.Enter();

 for i:=0 to agentsList.count-1 do
  begin
   if TAgentBoss(agentsList[i]).dead then
    begin
     dead:=TAgentBoss(agentsList[i]);
     agentsList.Delete(i);
     agentsListCS.Leave();

     TasksListCS.Enter();

     for j:=0 to tasksList.count-1 do
      if PGridTask(tasksList.Items[j]).agentIp = dead.agentIp then
       begin
        PGridTask(tasksList.Items[j]).agentIp:='';
       end;

     TasksListCS.Leave();

     dead.EndSession;
     dead.Destroy();

     goto l1;
    end;
  end;

 agentsListCS.Leave();
end;

//==============================================================================
//==============================================================================
function TGridUser.IsComplete(var complete: boolean): HRESULT; stdcall;
begin
 tasksListCS.Acquire();
 complete:=(tasksList.count=0);
 tasksListCS.Leave();

 result:=S_OK;

// complete:=not complete;
end;

//==============================================================================
//==============================================================================
function TGridUser.GetVersion(var version: DWORD): HRESULT; stdcall;
begin
 version:=IGRIDUSER_VERSION;
 result:=S_OK;
end;

//========================================================
//function TGridUser.QueryInterface(): HResult;
//========================================================
function TGridUser.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
 result:=E_NOINTERFACE;
end;

//========================================================
//function TGridUser._AddRef: Integer;
//========================================================
function TGridUser._AddRef: Integer;
begin
 result:=InterlockedIncrement(_RefCount);
end;

//========================================================
//function TGridUser._Release: Integer;
//========================================================
function TGridUser._Release: Integer;
begin
 result:=InterlockedDecrement(_RefCount);
 if result=0 then Free();
end;

//========================================================
//========================================================
procedure TGridUser.BindGetDataCallback(callback: TGetDataProc);
begin
 self.getDataCallback := callback;
end;

//========================================================
//========================================================
procedure TGridUser.GetSettings(var settings: TGridUserSettings); stdcall;
begin
 //todo: CriticalSection
 settings:=self.settings;
end;

//========================================================
//========================================================
procedure TGridUser.SetSettings(var settings: TGridUserSettings); stdcall;
begin
 //todo: CriticalSection
 self.settings := settings;
end;

//==============================================================================
//==============================================================================
//вернуть количество памяти (примерно), которое занимает очередь задач
function TGridUser.GetTotalTasksSize():DWORD;
var
 i: integer;
begin
 tasksListCS.Acquire();

 result:=0;

 for i:=0 to tasksList.count-1 do
  result:=result +  PGridTask(tasksList[i]).inStream.GetLength();

 tasksListCS.Leave();
end;

//==============================================================================
//==============================================================================
function TGridUser.GetTasksCount():DWORD;
begin
 tasksListCS.Acquire();
 result:=tasksList.count;
 tasksListCS.Leave();
end;

//==============================================================================
//==============================================================================
function TGridUser.WaitForCompletionEvent(timeout: DWORD) : HRESULT; stdcall;
var
 b: boolean;
begin
 result:=S_OK;

 tasksListCS.Acquire();
 b:=(tasksList.count<>0);
 tasksListCS.Leave();

 if b then
  begin
   if taskCompleteEvent.WaitFor(timeout)<>wrSignaled then result:=S_FALSE;
  end;
end;

//==============================================================================
//==============================================================================
procedure TGridUser.LoadSettings();
var
 Ini: TIniFile;
 ms: TMEMORYSTATUS;
begin
 GlobalMemoryStatus(ms);
 settings.userMaxMemoryUsage := ms.dwTotalPhys div 8;
 if (settings.userMaxMemoryUsage<100*1024*1024) then settings.userMaxMemoryUsage := 100*1024*1024;

 Ini := TIniFile.Create(ExtractFilePath( Application.ExeName) + 'hxgrid.INI');

 try
  settings.maxSendAheadTasks := Ini.ReadInteger('user', 'maxSendAheadTasks', 1 );
  settings.maxQueqedTasks := Ini.ReadInteger('user', 'maxQueqedTasks', 500 );
  settings.userMaxMemoryUsage := Ini.ReadInteger('user', 'userMaxMemoryUsage', settings.userMaxMemoryUsage );
  settings.agentMinFreeMemory := Ini.ReadInteger('user', 'agentMinFreeMemory', 50*1024*1024 );
  settings.agentMinFreeMemoryFactor := Ini.ReadFloat('user', 'agentMinFreeMemoryFactor', 1.5);
  settings.sendDublicateTasks := Ini.ReadBool('user', 'sendDublicateTasks', true );
  settings.compressThreshold := Ini.ReadInteger('user', 'compressThreshold', 65535 );

  strcopy(settings.coordinator_ip, pchar(Ini.ReadString('coordinator', 'coordinator_ip', '' )));
  settings.coordinator_port := Ini.ReadInteger('coordinator', 'bind_port', COORDINATOR_PORT);
  settings.user_data_port := Ini.ReadInteger('user', 'data_port', USER_DATA_PORT);
  settings.agent_lobby_port := Ini.ReadInteger('agent', 'bind_port', AGENT_LOBBY_PORT);      

  settings.allowSwapping := Ini.ReadBool('user', 'allowSwapping', true );

  settings.coordinator_broadcast_port := Ini.ReadInteger('coordinator', 'broadcast_port', COORDINATOR_BROADCAST_PORT);
  settings.user_broadcast_port := Ini.ReadInteger('user', 'broadcast_port', USER_BROADCAST_PORT);

  settings.enableDebugOutput := Ini.ReadBool('user', 'enableDebugOutput', false );

  settings.failSuspendTimeout := Ini.ReadInteger('user', 'failed_agent_suspend_timeout', 10000);

  settings.allowDiscardCoordinatorIp := Ini.ReadBool('coordinator', 'allowDiscardCoordinatorIp', true);
  
 finally
  Ini.Free;
 end;
end;

//==============================================================================
//==============================================================================
procedure TGridUser.DumpTasks();
var
 i: integer;
 mem: DWORD;
begin
 tasksListCS.Acquire();

 DebugWrite('-----------------------------------',[]);

 mem:=0;

 for i:=0 to tasksList.count-1 do
  begin
   DebugWrite('Id=%u,  SentToIP=%s,  swappedOut=%u,  lockedInMemory=%u,  size=%u\n',
     [PGridTask(tasksList[i]).id,
     PGridTask(tasksList[i]).agentIp,
     integer(PGridTask(tasksList[i]).swappedOut),
     integer(PGridTask(tasksList[i]).lockedInMemory),
     PGridTask(tasksList[i]).inStream.GetLength()]);

   mem:=mem+PGridTask(tasksList[i]).inStream.GetLength();
  end;

 DebugWrite('-----------------------------------',[]);
 DebugWrite('Total size of tasks: %uMb',[mem div (1024*1024)]);

 tasksListCS.Leave();
end;

//==============================================================================
//==============================================================================
procedure TGridUser.RemoveSwapFile(taskId: DWORD);
var
 f: file;
begin
 assignFile(f,GetSwapFileName(taskId));
 ioresult;
 erase(f);
 ioresult;
end;

//==============================================================================
//==============================================================================
procedure TGridUser.DumpHeapStatus();
var
 ts: THeapStatus;
begin
 DebugWrite('Allocated memory size: %u\n',[AllocMemSize]);

 ts:=GetHeapStatus();

 DebugWrite('TotalAddrSpace: %u\n',[ts.TotalAddrSpace]);
 DebugWrite('TotalUncommitted: %u\n',[ts.TotalUncommitted]);
 DebugWrite('TotalCommitted: %u\n',[ts.TotalCommitted]);
 DebugWrite('TotalAllocated: %u\n',[ts.TotalAllocated]);
 DebugWrite('TotalFree: %u\n',[ts.TotalFree]);
 DebugWrite('FreeSmall: %u\n',[ts.FreeSmall]);
 DebugWrite('FreeBig: %u\n',[ts.FreeBig]);
 DebugWrite('Unused: %u\n',[ts.Unused]);
 DebugWrite('Overhead: %u\n',[ts.Overhead]);
 DebugWrite('HeapErrorCode: %u\n',[ts.HeapErrorCode]);
end;

//==============================================================================
//==============================================================================
procedure TGridUser.NotifyTaskFailed(const agentIp : string; taskid: DWORD);
var
 i: integer;
begin

 //return task to list
 tasksListCS.Acquire();

 for i:=0 to tasksList.Count-1 do
  begin
   if (PGridTask(tasksList.items[i]).id=taskId) then
    begin
     if PGridTask(tasksList.items[i]).agentIp=agentIP then PGridTask(tasksList.items[i]).agentIP:='';
     break;
    end;
  end;

 tasksListCS.Leave();

 AgentsListCS.Acquire();

 for i:=0 to agentsList.count-1 do
  if (TAgentBoss(agentsList.items[i]).agentIP=agentIp) then
  begin
   TAgentBoss(agentsList.items[i]).TaskFailedNotify(taskId);
   break;
  end;

 AgentsListCS.Leave();

 taskAddEvent.SetEvent();

end;

//==============================================================================
//==============================================================================
function TGridUser.CompressStream(stream: IGenericStream): HRESULT; stdcall;
begin
 hxgridcommon.CompressStream(stream);
 result:=S_OK;
end;

//==============================================================================
//==============================================================================
procedure TGridUser.SaveSettings();
var
 Ini: TIniFile;
begin
 settingsCS.Enter();

 Ini := TIniFile.Create( ExtractFilePath( Application.ExeName ) + 'hxgrid.INI' );
  try
   Ini.WriteString('coordinator', 'coordinator_ip', settings.coordinator_ip);
  finally
   Ini.Free;
  end;

 settingsCS.Leave();
end;

//=============================================================================
//=============================================================================
//broadcast message to coordinator - request broadcast echo to AGENT_BROADCAST_PORT
procedure TGridUser.Broadcast();
var
 IdUDPClient1 :TIdUDPClient;
 b: byte;
begin
 DebugWrite('USER: Broadcasting to find coordinator address...',[]);

 IdUDPClient1:=TIdUDPClient.Create(nil);
 IdUDPClient1.Host := '255.255.255.255';
 IdUDPClient1.Port := settings.coordinator_broadcast_port;
 IdUDPClient1.Active := true;
 IdUDPClient1.BroadcastEnabled := True;

 b:=ID_USER_BROADCAST;
 IdUDPClient1.SendBuffer(b,1);

 IdUDPClient1.Destroy;
end;

//=============================================================================
//=============================================================================
procedure TGridUser.IdUDPServer1UDPRead(Sender: TObject; AData: TStream;
  ABinding: TIdSocketHandle);
var
  b: byte;
begin
 AData.Read(b, 1);

 DebugWrite('USER: Got broadcast from coordinator %s: %u bytes',[ABinding.PeerIP, AData.Size]);

 settingsCS.Enter();

  if (settings.coordinator_ip='') then
   begin
    strcopy(settings.coordinator_ip,pchar(ABinding.PeerIp));
   end;

 settingsCS.Leave();

 SaveSettings();

 terminateEvent.SetEvent();
end;

//=============================================================================
//=============================================================================
procedure TGridUser.RemoveTaskObject(var task: PGridTask);
begin
 //now sending dublicate task to some agent?
 while (task.lockedInMemory<>0) do
  begin
   Sleep(1000);  //todo: event
  end;

 if (task.swappedOut=true) then
  begin
   RemoveSwapFile(task.Id);
  end;

 task.inStream:=nil; //release()
 Dispose(task);
end;

//=============================================================================
//=============================================================================
function TGridUser.CancelTasks(): HRESULT; stdcall;
var
 task: PGridTask;
label l1;
begin
 l1:

 tasksListCS.Acquire();

 if (tasksList.count>0) then
  begin
   task:=PGridTask(tasksList.items[0]);
   tasksList.Delete(0);
  end
   else
  begin
   task:=nil;
  end;

 tasksListCS.Leave();

 if (task<>nil) then
  begin
   RemoveTaskObject(task);
   goto l1;
  end;

 result:=S_OK;
end;

//=============================================================================
//=============================================================================
procedure TGridUser.GetConnectionStatus(var status: TGridUserConnectionStatus); stdcall;
begin
 status.bConnectedToCoordinator := CoordinatorClient.Socket.Connected;

 AgentsListCS.Enter();

 status.connectedAgentsCount := AgentsList.Count;

 AgentsListCS.Leave();
end;


begin
end.



