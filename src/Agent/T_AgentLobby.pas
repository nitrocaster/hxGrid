//==============================================================================
// hxGrid framework
// Copyright (C) 2007 by Roman Lut
// hax@deep-shadows.com
// http://www.deep-shadows.com/hax/
//==============================================================================

unit T_AgentLobby;

interface
uses Windows, Classes,hxScktComp;


//==========================================
// class TAgentLobby
//==========================================
//сервер, принимающий задания от user
//для каждого user открывается свой thread
type TAgentLobby = class
  private

   parent: TObject; //TAgent

   serverSocket: hxScktComp.TServerSocket;

   procedure OnGetThread(Sender: TObject; ClientSocket: TServerClientWinSocket; var SocketThread: TServerClientThread);

   procedure OnListen(Sender: TObject; Socket: TCustomWinSocket);

  public
   constructor Create(parentAgent: TObject);
   destructor Destroy(); override;

 end;

implementation
uses Unit1, T_Agent, hxgridcommon, SysUtils, T_AgentLobbySocketThread;

//==============================================================================
//constructor TAgentLobby.Create();
//==============================================================================
constructor TAgentLobby.Create(parentAgent: TObject);
begin
 self.parent:=parentAgent;

 ServerSocket:=TServerSocket.Create(nil);
 ServerSocket.ServerType:=stThreadBlocking;
 if ((parent as TAgent).settings.bind_ip='') then ServerSocket.port:=(parent as TAgent).settings.bind_port
                                             else ServerSocket.port:=0;
 ServerSocket.OnGetThread:=OnGetThread;
 ServerSocket.OnListen:=OnListen;
 ServerSocket.ThreadCacheSize:=0;
 ServerSocket.Open();
end;

//==============================================================================
//destructor TAgentLobby.Destroy();
//==============================================================================
destructor TAgentLobby.Destroy();
begin
 ServerSocket.active:=false;
 ServerSocket.Free;
end;

//==============================================================================
//==============================================================================
procedure TAgentLobby.OnGetThread(Sender: TObject;
                                   ClientSocket: TServerClientWinSocket;
                                   var SocketThread: TServerClientThread);
begin
 SocketThread:=TAgentLobbySocketThread.Create(parent, ClientSocket);
end;

var
 inListen: boolean = false;

//==============================================================================
//==============================================================================
procedure TAgentLobby.OnListen(Sender: TObject; Socket: TCustomWinSocket);
begin
 if (parent as TAgent).settings.bind_ip='' then exit;

 //это извратный способ забиндить serversocket на определенный Ip
 //http://www.delphikingdom.com/asp/answer.asp?IDAnswer=20380
 if not inListen then
 try
  inListen := True;
  Socket.Listen('',(parent as TAgent).settings.bind_ip,'',(parent as TAgent).settings.bind_port,1);
 finally
  inListen := false;
 end;
end;


end.
