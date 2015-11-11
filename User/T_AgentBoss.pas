//==============================================================================
// hxGrid framework
// Copyright (C) 2007 by Roman Lut
// hax@deep-shadows.com
// http://www.deep-shadows.com/hax/
//==============================================================================

unit T_AgentBoss;

interface
uses Windows, SysUtils, hxScktComp, Classes, SyncObjs;

//==========================================
// class TAgentBoss
//==========================================
//thread занимается отсылкой заданий агенту
//для каждого агента создается boss
type TAgentBoss = class (TThread)
  private
   parent            :   TObject;  //TGridUser

   ClientSocket      : TClientSocket;

   //used to signal thread to exit;
   exitEvent         : TEvent;

   //завершена задача на этом агенте
   taskCompleteEvent : TEvent;

   //cpu used by grid on agent Mhz!
   CPUUsed           : single;
   //cpu available on agent Mhz!
   CPUAvail           : single;
   //free physical memory on agent
   //MAXDWORD if not known yet, in MB
   freeMemory        : DWORD;

   threadPoolSize    : integer;

   //agent failed task recently
   agentFailed       : boolean;

   procedure SendKeepAlive();

  protected
   procedure Execute; override;

  public

   agentIp           : string;
   dead              : boolean;

   //количество задач, посланных на выполнение на агент
   runningTasksCount  : integer;

   constructor Create(parent: TObject; const agentIP: String);
   destructor Destroy(); override;

   procedure TaskCompletedNotify(CPUUsed, CPUAvail: single; freeMemory: DWORD);
   procedure TaskFailedNotify(taskId: DWORD);

   procedure EndSession();
 end;

implementation
uses T_GridUser, hxgridcommon, HTTPApp;

 const
  SENDTASK_TIMEOUT = 30000;

//==============================================================================
//==============================================================================
constructor TAgentBoss.Create(parent: TObject; const agentIP: String);
begin
 self.parent := parent;
 self.agentIp := agentIp;

 runningTasksCount:=0;
 CPUUsed:=0;
 CPUAvail:=0;
 freeMemory:=MAXDWORD;

 exitEvent:=TEvent.Create(nil, false, false, '');
 taskCompleteEvent:=TEvent.Create(nil, false, false, '');

 dead:=false;

 ClientSocket:=TClientSocket.Create(nil);
 ClientSocket.Host:=agentIP;
 ClientSocket.Port:=(parent as TGridUser).settings.agent_lobby_port;
 ClientSocket.ClientType:=ctBlocking;

 inherited Create(false);
end;

//==============================================================================
//==============================================================================
destructor TAgentBoss.Destroy();
begin
 exitEvent.SetEvent();
 Terminate();
 WaitFor();

 exitEvent.Destroy();
 taskCompleteEvent.Destroy();

 ClientSocket.Destroy();
 ClientSocket:=nil;

 Inherited;
end;

//==============================================================================
//==============================================================================
procedure TAgentBoss.Execute;
var
 user: TGridUser;
 task: PGridTask;
 events1: array [0..1] of THANDLE;
 events2: array [0..1] of THANDLE;
 events3: array [0..2] of THANDLE;
 taskId: DWORD;
 delay: DWORD;
 blockedDueToFreeMemory: boolean;
 blockedDueToSentTasksCount: boolean;

 //agent is really starving. false if actually has not enought memory to run tasks
 imstarving: boolean;

 //agent has icremented starvingAgentsCount var. Should decrement as soon as get task to run
 starvingCountIncremented: boolean;
 s:string;
 code: integer;

 begin

  SetThreadName('TAgentBoss thread');

 //connect to agent

 user:=(parent as TGridUser);

 user.DebugWrite('AGENTBOSS: Starting communication with agent %s',[agentIP]);

 try
  ClientSocket.Open();
 except
  user.DebugWrite('AGENTBOSS: Unable to connect to agent %s',[agentIP]);
  dead:=true;
  exit;
 end;

 events1[0]:=user.taskAddEvent.handle;
 events1[1]:=exitEvent.handle;

 events2[0]:=taskCompleteEvent.handle;
 events2[1]:=exitEvent.handle;

 events3[0]:=taskCompleteEvent.handle;
 events3[1]:=user.taskAddEvent.handle;
 events3[2]:=exitEvent.handle;

 imstarving:=true;
 starvingCountIncremented:=false;
 agentFailed:=false;

 if SocketWriteString(ClientSocket.Socket,'GET_POOLSIZE',SENDTASK_TIMEOUT,
       (parent as TGridUser).settings.enableDebugOutput)=false then
  begin
   Terminate();
  end
   else
  if SocketReadString(ClientSocket.Socket,s,SENDTASK_TIMEOUT)=false then
   begin
    Terminate();
   end
    else
   begin
    val(s,threadPoolSize,code);
    if (threadPoolSize<1) or (threadPoolSize>32) then threadPoolSize:=1;
   end;

 while (not terminated) and ClientSocket.Socket.Connected do
  begin
   try

    if agentFailed then
     begin
      exitEvent.WaitFor((parent as TGridUser).settings.FailSuspendTimeout);
      agentFailed:=false;
     end;
     
    blockedDueToFreeMemory := (freeMemory*1024*1024<(parent as TGridUser).settings.agentMinFreeMemory);
    blockedDueToSentTasksCount := (RunningTasksCount>=threadPoolSize+(parent as TGridUser).settings.maxSendAheadTasks);

    if (freeMemory=MAXDWORD) or blockedDueToFreeMemory or blockedDueToSentTasksCount then
     begin
      //can't send
      //send probe message

      if (blockedDueToFreeMemory) and (freeMemory<>MAXDWORD) and (blockedDueToSentTasksCount=false) then
       begin
        user.DebugWrite('AGENTBOSS: Agent %s has not enought memory, required %uMb, has %uMb',
             [agentIP, round((parent as TGridUser).settings.agentMinFreeMemory/(1024*1024)), freeMemory]);
       end;

      if (blockedDueToFreeMemory=false) and (blockedDueToSentTasksCount=true) then
       begin
        user.DebugWrite('AGENTBOSS: Can''t send to agent %s - already running %u tasks',
             [agentIP, runningTasksCount]);
       end;

      //do not sleep, run task as soon as freeMemory is known
      if (freeMemory=MAXDWORD) then
       begin
        delay:=0
       end
        else
       begin
        delay:=5000;

        //set starving to false until at least one task is run on agent
        imstarving:=false;
       end;

      SendKeepAlive();

      WaitForMultipleObjects(2, @events2[0], false, delay);

      continue;
     end;

    if (runningTasksCount>0) and
       ((parent as TGridUser).starvingAgentsCount<>0) then
        begin
         user.DebugWrite('AGENTBOSS: Agent %s is skiping for starving agents...',[agentIP]);

         SendKeepAlive();

         WaitForMultipleObjects(3, @events3[0], false, 5000);
         continue;
        end;

    task := user.RetriveTask(agentIp, (RunningTasksCount=0));

    if task=nil then
     begin

      SendKeepAlive();

      if (runningTasksCount=0) and
          imstarving and
          (starvingCountIncremented=false) then
           begin
            InterlockedIncrement((parent as TGridUser).starvingAgentsCount);
            starvingCountIncremented:=true;
           end;

      WaitForMultipleObjects(2, @events1[0], false, 5000);

      continue;
     end;

    if (starvingCountIncremented) then
     begin
      InterlockedDecrement((parent as TGridUser).starvingAgentsCount);
      starvingCountIncremented:=false;
     end;

    if (GetUncompressedStreamSize(task.inStream)*(parent as TGridUser).settings.agentMinFreeMemoryFactor>freeMemory*1024*1024) then
     begin
      //not enought free memory on agent
      user.ReturnTaskToList(task.Id);

      user.DebugWrite('AGENTBOSS: Agent %s has not enought memory, required %uMb, has %uMb, task %u returned to list',
           [agentIP,
            round(GetUncompressedStreamSize(task.inStream)*(parent as TGridUser).settings.agentMinFreeMemoryFactor/(1024*1024)),
            freeMemory,
            task.id]);

      SendKeepAlive();

      WaitForMultipleObjects(2, @events2[0], false, 5000);

      imstarving:=false;

      continue;
     end;

    imstarving:=true;

    user.DebugWrite('AGENTBOSS: Sending task %u to agent %s',[task.id, agentIP]);

    taskid:=task.id; //we are loosing ownersip of task object after sending

    if SocketWriteStringAndStream(ClientSocket.Socket,
                    'RUN_TASK '+inttostr(int64(task.id))+' '+HTTPEncode(task.moduleName)+' '+task.taskProcName,
                    task.inStream,
                    SENDTASK_TIMEOUT,
                    (parent as TGridUser).settings.enableDebugOutput)=false then
     begin
      user.ReturnTaskToList(Task.id);
      raise EInOutError.Create('');
     end;

    user.DebugWrite('AGENTBOSS: Task %u sent to agent %s',[taskid, agentIP]);

    InterlockedDecrement(integer(task.lockedInMemory));

    InterlockedIncrement(runningTasksCount);

   except
    user.DebugWrite('AGENTBOSS: Unable to communicate with agent %s. Disconnecting...',[agentIP]);
    Terminate;
    user.DebugWrite('AGENTBOSS: Agent %s terminated.',[agentIP]);
   end;
  end;

  if (ClientSocket.Socket.Connected) then
   begin
    try
     SocketWriteString(ClientSocket.Socket,'DISCONNECT',5000,true);

    except
     //ignore
    end;
   end;

 if (starvingCountIncremented) then
  begin
   InterlockedDecrement((parent as TGridUser).starvingAgentsCount);
  end;

 ClientSocket.Close();

 user.DebugWrite('AGENTBOSS: Agent %s marked as dead.',[agentIP]);

 dead:=true;
end;


//==============================================================================
//==============================================================================
//агент прислал на user lobby сообщение о завершении выполения задачи
procedure TAgentBoss.TaskCompletedNotify(CPUUsed, CPUAvail: single; freeMemory: DWORD);
begin
 InterlockedDecrement(runningTasksCount);

 self.CPUUsed:=CPUUsed;
 self.CPUAvail:=CPUAvail;
 self.freeMemory:=freeMemory;

 taskCompleteEvent.SetEvent();
end;

//==============================================================================
//==============================================================================
procedure TAgentBoss.EndSession();
begin
//todo: send command to disconnect
//removeme
// ClientSocket.Close();
ClientSocket.active:=false;
 ExitEvent.SetEvent();
 WaitFor();
end;

//==============================================================================
//==============================================================================
procedure TAgentBoss.SendKeepAlive();
var
 s: string;
 command: TStringList;
begin
 if SocketWriteString(ClientSocket.Socket,
                      'KEEP_ALIVE',
                      SENDTASK_TIMEOUT,
                      (parent as TGridUser).settings.enableDebugOutput)=false then
  begin
   (parent as TGridUser).DebugWrite('AGENTBOSS: Agent %s - unable to send KEEP_ALIVE, disconnecting.',[agentIP]);
   raise EInOutError.Create('');
  end;

 if SocketReadString(ClientSocket.Socket,s,SENDTASK_TIMEOUT)=false then
  begin
   (parent as TGridUser).DebugWrite('AGENTBOSS: Agent %s - unable to receive KEEP_ALIVE responce, disconnecting.',[agentIP]);
   raise EInOutError.Create('');
  end;

 command:=TStringList.Create();
 command.CommaText:=s;

 if command.count<>3 then
  begin
   (parent as TGridUser).DebugWrite('AGENTBOSS: Agent %s - bad responce to KEEP_ALIVE, disconnecting.',[agentIP]);
   raise EInOutError.Create('');
  end;

 CPUUsed:=strToFloat(command[0]);
 CPUAvail:=strToFloat(command[1]);
 freeMemory:=strToInt64(command[2]);

 command.Free();
end;

//==============================================================================
//==============================================================================
procedure TAgentBoss.TaskFailedNotify(taskId: DWORD);
begin
 interlockedDecrement(runningtasksCount);
 taskCompleteEvent.SetEvent();
 AgentFailed:=true;
end;



end.
