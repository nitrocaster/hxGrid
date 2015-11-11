//==============================================================================
// hxGrid framework
// Copyright (C) 2007 by Roman Lut
// hax@deep-shadows.com
// http://www.deep-shadows.com/hax/
//==============================================================================

unit T_GridUserLobby;

interface
uses Windows, Classes, SysUtils, hxScktComp;

//==========================================
// class TGridUserLobby
//==========================================
//принимает запросы от агентов на получение файла,
//репорты о завершении работы
//больше ничем кроме этого не занимается
//для каждого agent выполняется свой thread (TUserLobbySocketThread)
type TGridUserLobby = class
  private
   parent: TObject;  //TGridUser

   serverSocket: hxScktComp.TServerSocket;

   procedure OnGetThread(Sender: TObject; ClientSocket: TServerClientWinSocket; var SocketThread: TServerClientThread);

  public

   constructor Create(parentUser: TObject);
   destructor Destroy(); override;


 end;

implementation
uses T_GridUser, hxgridcommon, HTTPApp, T_UserLobbySocketThread;

//==============================================================================
//==============================================================================
constructor TGridUserLobby.Create(parentUser: TObject);
begin
 self.parent:=parentUser;

 ServerSocket:=TServerSocket.Create(nil);
 ServerSocket.ServerType:=stThreadBlocking;
 ServerSocket.port:=(parent as TGridUser).settings.user_data_port;
 ServerSocket.OnGetThread:=OnGetThread;
 ServerSocket.Open();
end;

//==============================================================================
//==============================================================================
destructor TGridUserLobby.Destroy();
begin
 (parent as TGridUser).DebugWrite('TEST: ServerSocket.Active:=false',[]);
 ServerSocket.Active:=false;
 (parent as TGridUser).DebugWrite('TEST: ServerSocket.destroy()',[]);
 ServerSocket.Destroy();
 (parent as TGridUser).DebugWrite('TEST: ServerSocket.destroy() done',[]);
end;

//==============================================================================
//==============================================================================
procedure TGridUserLobby.OnGetThread(Sender: TObject;
                                   ClientSocket: TServerClientWinSocket;
                                   var SocketThread: TServerClientThread);
begin
 SocketThread:=TUserLobbySocketThread.Create(parent, ClientSocket);
end;


end.
