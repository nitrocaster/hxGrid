//==============================================================================
// hxGrid framework
// Copyright (C) 2007 by Roman Lut
// hax@deep-shadows.com
// http://www.deep-shadows.com/hax/
//==============================================================================

unit T_AgentPeer;

//todo:
//handle user disconnect event

interface
uses Classes,Windows, hxScktComp, hxgridcommon, I_GenericStream, StrUtils,
 SyncObjs, safefpu;

//=====================================
//class TAgentPeer
//=====================================
//класс, который общается с пользователем
//получение дополнительных данных с пользователя
//рапорт о выполнении работы
type TAgentPeer = class
  public

   constructor Create(parent: TObject; const userIP:String);
   destructor Destroy(); override;

   function RequestFile(const fileName: String): TMemoryStream;

   procedure RequestData(const dataDesc: String; var stream: IGenericStream);

   function TaskDone(taskId: DWORD; outStream: IGenericStream): boolean;
   function TaskFailed(taskId: DWORD; const desc: string): boolean;

   function GetUserIP(): String;

  private

   parent: TObject; //tagent

   ClientSocket: TClientSocket;

   cs: TCriticalSection;

 end;

implementation
uses T_Agent, unit1, SysUtils, HTTPApp;

//==============================================================================
//==============================================================================
constructor TAgentPeer.Create(parent: TObject;const userIP:String);
begin
 self.parent:=parent;

 cs:=TCriticalSection.Create();

 ClientSocket:=TClientSocket.Create(nil);
 ClientSocket.ClientType:=ctBlocking;

 ClientSocket.Host:=userIP;
 ClientSocket.Port:=(parent as TAgent).settings.user_data_port;

 try
  (parent as TAgent).DebugWrite('AGENTPEER: Connecting to user data port, IP=%s',[userIP]);
  ClientSocket.Open();
  (parent as TAgent).DebugWrite('AGENTPEER: Connected to user data port sucessfully, IP=%s',[userIP]);
 except
  (parent as TAgent).DebugWrite('AGENTPEER: Unable to connect to user data port, IP=%s',[userIP]);
 end;

end;

//==============================================================================
//==============================================================================
destructor TAgentPeer.Destroy();
begin
 ClientSocket.Close();
 ClientSocket.Destroy();
 cs.Destroy();
end;

//==============================================================================
//==============================================================================
function TAgentPeer.RequestFile(const fileName: String): TMemoryStream;
var
 stream: TMemoryStream;
 s: string;
 label l1;
begin
 cs.Enter();
 try
  s:='GET_FILE '+HTTPEncode(fileName);

  if SocketWriteString(clientSocket.socket,s,MAXINT, (parent as TAgent).settings.enableDebugOutput)=false then
   begin
    l1:
    (parent as TAgent).DebugWrite('AGENTPEER: GET_FILE %s failed, file not found on user, IP=%s',[fileName, GetUserIP()]);
    result:=nil;
    cs.Leave();
    exit;
   end;

  if SocketReadString(ClientSocket.socket, s, MAXINT)=false then goto l1;

  if s<>'Ok' then goto l1;

  if SocketReadStream(ClientSocket.socket, stream,MAXINT)=false then goto l1;

  result:=stream;
 except
  (parent as TAgent).DebugWrite('AGENTPEER: GET_FILE %s failed, IP=%s',[fileName, GetUserIP()]);
  result:=nil;
 end;
 cs.Leave();
end;

//==============================================================================
//==============================================================================
//вызывается AgentWorkerThread после выполнения задачи
function TAgentPeer.TaskDone(taskId: DWORD; outStream: IGenericStream) : boolean;
var
 s: string;
begin
 cs.Enter();

 (parent as TAgent).DebugWrite('AGENTPEER: TASK_DONE %u for IP=%s',[taskId, GetUserIP()]);

 //todo: how many tasks are running simultaneously ??
 //(how many workerThreads created)?
 //divide by this value
 s:='TASK_DONE '+IntToStr(int64(taskId))+' '+(parent as TAgent).statusThread.GetCPUAndMemoryStatus();

 result:=SocketWriteStringAndStream(ClientSocket.socket, s, outStream, MAXINT, (parent as TAgent).settings.enableDebugOutput);

 cs.Leave();
end;

//==============================================================================
//==============================================================================
//вызывается AgentWorkerThread после выполнения задачи
function TAgentPeer.TaskFailed(taskId: DWORD; const desc: string): boolean;
var
 s: string;
begin
 cs.Enter();

 (parent as TAgent).DebugWrite('AGENTPEER: TASK_FAILED %u for IP=%s - %s',[taskId, GetUserIP(), desc]);

 s:='TASK_FAILED '+IntToStr(int64(taskId))+' '+ HTTPEncode(desc);

 result:=SocketWriteString(ClientSocket.socket, s, MAXINT, (parent as TAgent).settings.enableDebugOutput);

 cs.Leave();
end;

//==============================================================================
//function TAgentPeer.GetUserIP(): String;
//==============================================================================
function TAgentPeer.GetUserIP(): String;
begin
 result:=ClientSocket.host;
end;

//==============================================================================
//==============================================================================
procedure TAgentPeer.RequestData(const dataDesc: String; var stream: IGenericStream);
var
 s: string;
 d: DWORD;
 label l1;
begin
 cs.Enter();

 try
  s:='GET_DATA '+HTTPEncode(dataDesc);

  if SocketWriteString(clientSocket.socket,s,MAXINT, (parent as TAgent).settings.enableDebugOutput)=false then
   begin
    l1:
    (parent as TAgent).DebugWrite('AGENTPEER: GET_DATA failed! User IP=%s',[GetUserIP()]);
    stream:=nil;
    cs.Leave();
    exit;
   end;

  if SocketReadString(ClientSocket.socket, s, MAXINT)=false then goto l1;

  if s<>'Ok' then goto l1;

  if SocketReadStream(ClientSocket.socket, stream,MAXINT)=false then goto l1;

  d:=stream._AddRef();
  d:=stream._Release();
  assert(d=1);

 except
  (parent as TAgent).DebugWrite('AGENTPEER: GET_DATA failed, user IP=%s',[GetUserIP()]);
  stream:=nil;
 end;
 cs.Leave();
end;


end.
