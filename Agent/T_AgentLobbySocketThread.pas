//==============================================================================
// hxGrid framework
// Copyright (C) 2007 by Roman Lut
// hax@deep-shadows.com
// http://www.deep-shadows.com/hax/
//==============================================================================

unit T_AgentLobbySocketThread;

interface
uses Windows, hxScktComp, SyncObjs, Classes, hxgridcommon, T_AgentWorkerThread, safefpu;

//==========================================
// class TAgentLobbySocketThread
//==========================================
//поток для user
//создает свои peer, fileCache и workerThread
type TAgentLobbySocketThread = class(TServerClientThread)
  protected

   parent: TObject; //TAgent

   workerThread  : TAgentWorkerThread;

   procedure ClientExecute(); override;

  public

  constructor Create(parent: TObject; ASocket: TServerClientWinSocket);
 end;

implementation
uses T_Agent, SysUtils, unit1, I_GenericStream;

//==============================================================================
//==============================================================================
constructor TAgentLobbySocketThread.Create(parent: TObject; ASocket: TServerClientWinSocket);
begin
 self.parent:=parent;
 inherited Create(false, ASocket);
end;

//==============================================================================
//==============================================================================
procedure TAgentLobbySocketThread.ClientExecute();
var
 d            : DWORD;
 s            : string;
 command      : TStringList;
 agent        : TAgent;
 stream       : IGenericStream;
 userIP       : string;
 sessionId    : DWORD;
begin
 SetThreadName('AgentLobbySocketThread');

 agent:=parent as TAgent;

 //refuse connection during destruction
 if (agent.destroying) then
  begin
   try
    ClientSocket.Close();
   except
   //ignore
   end; 
   exit;
  end;

 //сохранить userIP, т.к. socket может его выставить в ''
 userIP:=ClientSocket.RemoteAddress;
 agent.UpdateStatus_Add(userIP);

 //если peer не сможет соединиться с user,
 //то worketThread никак не будет от этом сигнализировать.
 //предполагается, что user определит, что от агента
 //не идут данные, и отключит его

 workerThread:=TAgentWorkerThread.Create(agent,ClientSocket.RemoteAddress);

 sessionId:=DWORD(workerThread);

 command:=TStringList.Create();

 while (not Terminated) and ClientSocket.Connected do
  begin
   try
    //timeout = 20000 disconnect if no KEEP_ALIVE messages for a long time (connection lost)
    if SocketReadString(ClientSocket,s,20000)=false then
     begin
      (parent as TAgent).DebugWrite('AGENTLOBBYTHREAD: No KEEP_ALIVE for a long time, disconnecting.',[]);

      raise EInOutError.Create('');
     end;

    if WorkerThread.GetPeerHasConnectionProblems() then
     begin
      (parent as TAgent).DebugWrite('AGENTLOBBYTHREAD: Peer has connection problems, disconnecting.',[]);

      raise Exception.Create('');
     end;

    command.commaText:=s;

    if (command.Count=0) then
     begin
      (parent as TAgent).DebugWrite('AGENTLOBBYTHREAD: command.itemsCount=0!!! s="%s"',[s]);
     end
      else
    if (command[0]='RUN_TASK') then
     begin
      (parent as TAgent).DebugWrite('AGENTLOBBYTHREAD: Got RUN_TASK command, reading data...',[]);

      if (command.count<>4) then
       begin
        raise Exception.Create('');
       end;

      if SocketReadStream(ClientSocket, stream, 20000)=false then
       begin
        (parent as TAgent).DebugWrite('AGENTLOBBYTHREAD: Unable to receive data for task %s',[command[1]]);
        raise EInOutError.Create('');
       end;

      (parent as TAgent).DebugWrite('AGENTLOBBYTHREAD: Data for task %s received',[command[1]]);

      d:=stream._AddRef();
      d:=stream._Release();
      assert(d=1);

      //taskId fileName procName
      WorkerThread.AddTask(StrToInt64(command[1]), command[2], command[3], stream);
      pointer(stream):=nil;  //transfer ownership to WorkerThread
     end
      else
    if (command[0]='KEEP_ALIVE') then
     begin
      (parent as TAgent).DebugWrite('AGENTLOBBYTHREAD: KEEP_ALIVE received ',[]);

      SocketWriteString(ClientSocket,
                        (parent as TAgent).statusThread.GetCPUAndMemoryStatus(),
                        20000,
                        (parent as TAgent).settings.enableDebugOutput);
     end
      else
    if (command[0]='DISCONNECT') then
     begin
      (parent as TAgent).DebugWrite('AGENTLOBBYTHREAD: Received DISCONNECT command.',[]);
      Terminate();
     end
      else
    if (command[0]='GET_POOLSIZE') then
     begin
      SocketWriteString(ClientSocket,
                        inttostr((parent as TAgent).settings.poolSize),
                        20000,
                        (parent as TAgent).settings.enableDebugOutput);
     end;

   except
    on E:Exception do
     begin
      (parent as TAgent).DebugWrite('AGENTLOBBYTHREAD: exception %s, closing socket',[e.message]);
      Terminate();
     end;
   end;
  end;

 try
  if ClientSocket.Connected then ClientSocket.Close;
 except
  //ignore
 end;

 command.Destroy();

 (parent as TAgent).DebugWrite('AGENTLOBBYTHREAD: Thread closing, Disconnecting...',[]);

 workerThread.Destroy();

 agent.UpdateStatus_Remove(userIP);

 (parent as TAgent).DebugWrite('AGENTLOBBYTHREAD: Exit',[]);
end;

end.

