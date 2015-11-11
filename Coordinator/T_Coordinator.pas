//==============================================================================
// hxGrid framework
// Copyright (C) 2007 by Roman Lut
// hax@deep-shadows.com
// http://www.deep-shadows.com/hax/
//==============================================================================

unit T_Coordinator;

interface
uses Windows, hxScktComp, SyncObjs, Classes, hxgridcommon,
  IdSocketHandle, IdBaseComponent, IdComponent, IdUDPClient,
  IdUDPBase, IdUDPServer;

//========================================
// TCoordinatorSettings
//========================================
type TCoordinatorSettings = record
   bind_ip                     : string;  //'' to bind to any port
   bind_port                   : WORD;    //default is COORDINATOR_PORT
   agent_broadcast_port        : DWORD;
   coordinator_broadcast_port  : DWORD;
   user_broadcast_port         : DWORD;
   enableDebugOutput           : boolean;
  end;

//========================================
// TAgentStatus
//========================================
type TAgentStatus = record
       version        : DWORD;  //minor<<8 + major
       IP             : string; //agent IP or address
       port           : DWORD;  //agent PORT
       name           : string; //agent name
       CPUSpeed       : single; //MHhz
       CPUCount       : DWORD;
       CPUAvail       : DWORD;  //%
       CPUUsed        : DWORD;  //%
       physRAMAvail   : DWORD;  //KB
       status         : string; //status, reported from agent
       lastUpdate     : DWORD;  //GetTickCount()
       threadPoolSize : DWORD;
      end;

 PAgentStatus = ^TAgentStatus;

//==========================================
// class TCoordinator
//==========================================
//Класс координатора
type TCoordinator = class(TObject)
  private
   serverSocket: hxScktComp.TServerSocket;

   listAccessCS: TCriticalSection;

   //holds list of all agents
   agentsList: TList;

   settings : TCoordinatorSettings;

   IdUDPServer1: TIdUDPServer;

   procedure OnGetThread(Sender: TObject; ClientSocket: TServerClientWinSocket; var SocketThread: TServerClientThread);
   procedure OnListen(Sender: TObject; Socket: TCustomWinSocket);

   function GetAgentIndex(const IP: string; port: DWORD): integer;
   procedure RemoveTimedOutAgents();

   procedure LoadSettings();

   procedure IdUDPServer1UDPRead(Sender: TObject; AData: TStream;
      ABinding: TIdSocketHandle);

   procedure Broadcast(const peerIP: string; peerPort :DWORD);

  public

   constructor Create();
   destructor Destroy(); override;

   procedure UpdateStatus(socket: TCustomWinSocket; command : TStringList);
   procedure GetAgents(socket: TCustomWinSocket; command : TStringList);

   function GetAgentsList(version: DWORD): TList;

   procedure DebugWrite(const Format: string; const Args: array of const);

 end;

implementation
uses T_CoordinatorSocketThread, HTTPApp, SysUtils, WinSock, IniFiles, forms;

//==============================================================================
//==============================================================================
constructor TCoordinator.Create();
begin
 LoadSettings();

 agentsList := TList.Create();
 listAccessCS := TCriticalSection.Create();

 ServerSocket:=TServerSocket.Create(nil);
 ServerSocket.ServerType:=stThreadBlocking;

 if settings.bind_ip='' then ServerSocket.port:=COORDINATOR_PORT
                        else ServerSocket.port:=0;

 try
  IdUDPServer1:=TIdUDPServer.Create(nil);
  IdUDPServer1.defaultPort:=settings.coordinator_broadcast_port;
  IdUDPServer1.OnUDPRead:=IdUDPServer1UDPRead;
  IdUDPServer1.active:=true;
 except
  DebugWrite('COORDINATOR: Unable to create UDP server socket on port %u',[settings.coordinator_broadcast_port]);
 end;

 ServerSocket.OnGetThread:=OnGetThread;
 ServerSocket.OnListen:=OnListen;
 ServerSocket.Open();

 DebugWrite('Coordinator is listening on ip: "%s",',[serversocket.socket.localaddress]);
end;

//==============================================================================
//==============================================================================
destructor TCoordinator.Destroy();
begin
 ServerSocket.Active:=false;
 ServerSocket.Destroy();

 IdUDPServer1.Destroy();

 agentsList.Destroy();
 listAccessCS.Destroy();
end;

var
 inListen: boolean = false;

//==============================================================================
//==============================================================================
procedure TCoordinator.OnListen(Sender: TObject; Socket: TCustomWinSocket);
begin
 if settings.bind_ip='' then exit;

 //это извратный способ забиндить serversocket на определенный Ip
 //http://www.delphikingdom.com/asp/answer.asp?IDAnswer=20380
 if not inListen then
 try
  inListen := True;
  Socket.Listen('',settings.bind_ip,'',settings.bind_port,1);
 finally
  inListen := false;
 end;
end;

//==============================================================================
//==============================================================================
procedure TCoordinator.OnGetThread(Sender: TObject;
                                   ClientSocket: TServerClientWinSocket;
                                   var SocketThread: TServerClientThread);
begin
 SocketThread:=TCoordinatorSocketThread.Create(self, ClientSocket);
end;

//============================================================================
//============================================================================
procedure TCoordinator.UpdateStatus(socket: TCustomWinSocket; command : TStringList);
var
 agentStatus : PAgentStatus;
 code: integer;
 i: integer;
begin
 //expecting:
 //0 - agent version       [int]
 //1 - COMPUTER_NAME       [string]
 //2 - CPU SPEED           [float, MHhz]
 //3 - cpu available       [%, DWORD]
 //4 - CPU used by agent   [%, DWORD]
 //5 - physram available   [MB, DWORD]
 //6 - status              [string] - "Unassigned" ,"Working for HAX (192.168.0.2)" -- HTTPEncoded
 //7 - CPU count           [integer]
 //8 - thread pool size    [integer]

 //agent send CPU_AVAIL 0 when working for someone

 ///temp
 if (command.Count=9) then command.add('1');

 if command.Count<>10 then
  begin
   exit;
  end;

 new(agentStatus);

 agentStatus.IP:=Socket.RemoteAddress;
 agentStatus.port:=Socket.RemotePort;

 val(command[1], agentStatus.version,code);

 agentStatus.name:=HTTPDecode(command[2]);

 val(command[3], agentStatus.CPUSpeed,code);

 val(command[4], agentStatus.CPUAvail,code);

 val(command[5], agentStatus.CPUUsed,code);

 val(command[6], agentStatus.physRAMAvail,code);

 agentStatus.status:=HTTPDecode(command[7]);

 val(command[8], agentStatus.CPUCount,code);

 val(command[9], agentStatus.threadPoolSize,code);

 agentStatus.lastUpdate:=GetTickCount();

 listAccessCS.Acquire();

 i:=GetAgentIndex(agentStatus.IP, agentStatus.port);

 if (i=-1) then
  begin
   agentsList.Add(pointer(agentStatus));
  end
   else
  begin
   Dispose(PAgentStatus(agentsList[i]));
   agentsList[i]:=pointer(agentStatus);
  end;

 listAccessCS.Leave();
end;

//============================================================================
//============================================================================
//returns index of agent in list, or -1
//should be called when agentlist is locked
function TCoordinator.GetAgentIndex(const IP: string; port: DWORD): integer;
var
 i: integer;
begin
 for i:=0 to agentsList.Count-1 do
  if (PAgentStatus(agentsList[i]).IP=IP) and (PAgentStatus(agentsList[i]).port=port) then
   begin
    result:=i;
    exit;
   end;
 result:=-1;
end;


//==============================================================================
//==============================================================================
procedure TCoordinator.RemoveTimedOutAgents();
var
 i: integer;
 tick: DWORD;
begin
 //remove agent from list, if id does not send UPDATE_STATUS for a long time

 tick:=GetTickCount();

 listAccessCS.Acquire();

 i:=0;
 while (i<agentsList.Count) do
  begin
   if (tick - PAgentStatus(agentsList[i]).lastUpdate > AGENT_STATUS_TIMEOUT) then
    begin
     Dispose(PAgentStatus(agentsList[i]));
     agentsList.Delete(i);
     continue;
    end;
   inc(i);
  end;

 listAccessCS.Leave();
end;

//==============================================================================
//==============================================================================
procedure TCoordinator.GetAgents(socket: TCustomWinSocket; command : TStringList);
var
 s: string;
 list : TList;
 i: integer;
 version : DWORD;
 code: integer;
begin
 if command.Count<>2 then
  begin
   exit;
  end;

 val(command[1], version,code);

 RemoveTimedOutAgents();

 list:=GetAgentsList(version);

 if list.count=0 then
  begin
   s:='none';
  end
   else
  begin

   for i:=0 to list.count-1 do
    if i=0 then s:=PAgentStatus(list[i]).ip
           else s:=s+','+PAgentStatus(list[i]).ip;
  end;

 list.destroy;

 SocketWriteString(socket,s,10000, settings.enableDebugOutput);
end;

//==============================================================================
//==============================================================================
function TCoordinator.GetAgentsList(version: DWORD): TList;
var
 item: PAgentStatus;
 i: integer;
begin
 RemoveTimedOutAgents();

 listAccessCS.Acquire();

 result:=TList.Create();

 for i:=0 to agentsList.Count-1 do
  begin
   if (version<>0) and (TAgentStatus(agentsList[i]^).version<>version) then continue;
   new(item);
   item^:=TAgentStatus(agentsList[i]^);
   result.add(item);
  end;

 listAccessCS.Leave();
end;

//==============================================================================
//procedure TCoordinator.DebugWrite();
//==============================================================================
procedure TCoordinator.DebugWrite(const Format: string; const Args: array of const);
begin
 if settings.enableDebugOutput=false then exit;
 OutputDebugString(pchar(sysutils.Format(format, args)+#13));
end;


//==============================================================================
//==============================================================================
procedure TCoordinator.LoadSettings();
var
 Ini: TIniFile;
begin
 Ini := TIniFile.Create( ExtractFilePath( Application.ExeName) + 'hxgrid.INI' );
  try
   settings.bind_ip := Ini.ReadString('coordinator', 'bind_ip', '' );
   settings.bind_port := Ini.ReadInteger('coordinator', 'bind_port', COORDINATOR_PORT );
   settings.coordinator_broadcast_port := Ini.ReadInteger('coordinator', 'broadcast_port', COORDINATOR_BROADCAST_PORT );
   settings.agent_broadcast_port := Ini.ReadInteger('agent', 'broadcast_port', AGENT_BROADCAST_PORT );
   settings.user_broadcast_port := Ini.ReadInteger('user', 'broadcast_port', USER_BROADCAST_PORT );
   settings.enableDebugOutput := Ini.ReadBool('coordinator', 'enableDebugOutput', false );
  finally
   Ini.Free;
  end;
end;

//=============================================================================
//=============================================================================
procedure TCoordinator.IdUDPServer1UDPRead(Sender: TObject; AData: TStream;
  ABinding: TIdSocketHandle);
var
 b: byte;
begin
 AData.Read(b, 1);

 if (b=ID_AGENT_BROADCAST) then
  begin
   DebugWrite('COORDINATOR: Got broadcast from agent %s',[ABinding.PeerIP]);
   Broadcast(ABinding.PeerIP, settings.agent_broadcast_port);
  end
  else

 if (b=ID_USER_BROADCAST) then
  begin
   DebugWrite('COORDINATOR: Got broadcast from user %s',[ABinding.PeerIP]);
   Broadcast(ABinding.PeerIP, settings.user_broadcast_port);
  end
   else

  begin
   DebugWrite('COORDINATOR: Got unknown broadcast from %s, data size = %u',[ABinding.PeerIP, AData.size]);
  end;
end;

//=============================================================================
//=============================================================================
procedure TCoordinator.Broadcast(const peerIP: string; peerPort :DWORD);
var
 IdUDPClient1 :TIdUDPClient;
 b: byte;
begin
 IdUDPClient1:=TIdUDPClient.Create(nil);
 IdUDPClient1.Host := '255.255.255.255';
 IdUDPClient1.Port := peerPort;
 IdUDPClient1.Active := true;
 IdUDPClient1.BroadcastEnabled := True;

 b:=ID_COORDINATOR_BROADCAST;
 IdUDPClient1.SendBuffer(b,1);

 IdUDPClient1.Destroy;
end;

end.
