//==============================================================================
// hxGrid framework
// Copyright (C) 2007 by Roman Lut
// hax@deep-shadows.com
// http://www.deep-shadows.com/hax/
//==============================================================================

unit T_CoordinatorSocketThread;

interface
uses Windows, hxScktComp, SyncObjs, Classes, hxgridcommon;

//==========================================
// class TCoordinatorSocketThread
//==========================================
//поток для каждого клиента координатора
//мы переопределяем thread для того, чтобы не пользоваться
//OnClientRead/OnClientWrite, которые посылаются как сообщения окну
//Вместо этого будем все делать в этом thread
type TCoordinatorSocketThread = class(TServerClientThread)
  protected

   parent: TObject;

   procedure ClientExecute(); override;

  public

  constructor Create(parent: TObject; ASocket: TServerClientWinSocket);
 end;

implementation
uses T_Coordinator;

//==============================================================================
//==============================================================================
constructor TCoordinatorSocketThread.Create(parent: TObject; ASocket: TServerClientWinSocket);
begin
 self.parent:=parent;
 inherited Create(false, ASocket);
end;

//==============================================================================
//==============================================================================
procedure TCoordinatorSocketThread.ClientExecute();
var
 s            : string;
 command      : TStringList;
 coordinator  : TCoordinator;
begin
 command:=TStringList.Create();

 coordinator:=parent as TCoordinator;

 while (not Terminated) and ClientSocket.Connected do
  begin
   if SocketReadString(ClientSocket,s,20000) then
    begin
     command.commaText:=s;

     if (command[0]='UPDATE_STATUS') then
      begin
       Coordinator.UpdateStatus(ClientSocket, command)
      end
       else
     if (command[0]='GET_AGENTS') then
      begin
       Coordinator.GetAgents(ClientSocket, command);
      end;

    end
     else
    begin
     //timeout or no update for a long time
     ClientSocket.Close;
    end;
  end;

 command.Destroy();
end;

end.
