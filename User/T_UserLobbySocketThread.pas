//==============================================================================
// hxGrid framework
// Copyright (C) 2007 by Roman Lut
// hax@deep-shadows.com
// http://www.deep-shadows.com/hax/
//==============================================================================

unit T_UserLobbySocketThread;

interface
uses Windows, hxScktComp, SyncObjs, Classes, hxgridcommon, forms;

//==========================================
// class TUserLobbySocketThread
//==========================================
type TUserLobbySocketThread = class(TServerClientThread)
  protected

   parent: TObject; //TGridUser

   procedure ClientExecute(); override;

   procedure GetFile(socket: TServerClientWinSocket; filename: string);
   procedure GetData(socket: TServerClientWinSocket; dataDesc: string);
   procedure TaskDone(socket: TServerClientWinSocket; const taskIdstr, CPUUsedStr, CPUAvailStr, freeMemoryStr: string);
   procedure TaskFailed(socket: TServerClientWinSocket; const taskIdstr, errorstr: string);

   procedure SuspendIfNetworkOverloaded(size: DWORD);
   procedure NotifySendEnd(size: DWORD);

  public

   constructor Create(parent: TObject; ASocket: TServerClientWinSocket);

 end;

implementation
uses T_GridUser, SysUtils, HTTPApp, I_GenericStream, T_GenericStream;

var
 //at the start of the session, network stack can be overloaded due to massive
 //requests from agents (like a DOS attack :)
 //we are tracking size of buffers, sent at current moment, is global variable

 //there a drawback in this algorithm: agent with slow connection can slow down
 //oversal performance by suspending communication with all other agents
 //we just assume we have enought memory and resonably fast connections to
 //make this case impossible.
 nowSending: DWORD;

 //maximum size of buffers, default is physicalMemory/5, but not less then 64MB
// allowedToSend: DWORD;

 cs: TCriticalSection;

 netevent: TEvent;

 ms: TMEMORYSTATUS;

//==============================================================================
//==============================================================================
constructor TUserLobbySocketThread.Create(parent: TObject; ASocket: TServerClientWinSocket);
begin
 self.parent:=parent;
 inherited Create(false, ASocket);
end;

//==============================================================================
//==============================================================================
procedure TUserLobbySocketThread.ClientExecute();
var
 d            : DWORD;
 s            : string;
 command      : TStringList;
 user         : TGridUser;
 stream       : TMemoryStream;
begin
 SetThreadName('TUserLobbySocketThread');

 user:=parent as TGridUser;

 command:=TStringList.Create();


 //todo: если вторая строна плохо закрыла socket (убили процесс)
 //-- остаются "висящие" соединения
 //посылать probes с агента
 while (not Terminated) and ClientSocket.Connected do
  begin
   try
    if SocketReadString(ClientSocket,s,MAXINT)=false then
     begin
      Terminate();
      continue;
     end;

//    (parent as TGridUser).debugwrite('got string=%s',[s]);

    command.commaText:=s;

    if (command[0]='GET_FILE') then
     begin
      if (command.count<>2) then
       begin
        raise Exception.Create('');
       end;

      GetFile(ClientSocket, HTTPDecode(command[1]));
     end
      else

    if (command[0]='GET_DATA') then
     begin
      if (command.count<>2) then
       begin
        raise Exception.Create('');
       end;

      GetData(ClientSocket, HTTPDecode(command[1]));
     end
      else

    if (command[0]='TASK_DONE') then
     begin
      //expecting: task_id CPUUsage(MHz!) CPUAvail(MHz!) freememory(bytes)
      if (command.count<>5) then
       begin
        raise Exception.Create('');
       end;

      TaskDone(ClientSocket, command[1], command[2], command[3], command[4]);
     end
      else

    if (command[0]='TASK_FAILED') then
     begin
      if (command.count<>3) then
       begin
        raise Exception.Create('Number of parameters for TASK_FAILED incorrect');
       end;
      TaskFailed(ClientSocket, command[1], command[2]);
     end
        else

    if (command[0]='PROBE') then
     begin
      //todo: Update lastProbeTick
      //user.Lobby.TaskFailed(ClientSocket, command[1], command[2]);
     end;

   except
    ClientSocket.Close;
   end;
  end;

 command.Destroy();
end;

//==============================================================================
//==============================================================================
//syntax: GET_FILE filename
//reply: code = 200 - read stream
//       code = 0 - file not found
//выполняется в userlobbysocketthread
//review: может все-таки использовать critical section, чтобы одновременно не открывать файлы ?
procedure TUserLobbySocketThread.GetFile(socket: TServerClientWinSocket; filename: string);
var
 stream: TFileStream;
 memStream: TMemoryStream;
begin
 //check free memory
 SuspendIfNetworkOverloaded(0);

 memStream:=TMemoryStream.Create();

 try
  stream:=TFileStream.Create(fileName, fmOpenRead or fmShareDenyWrite);

  memStream.CopyFrom(stream,0);

  Stream.Destroy();

 except
  try
   stream:=TFileStream.Create(ExtractFilePath( Application.ExeName)+fileName, fmOpenRead or fmShareDenyWrite);

   memStream.CopyFrom(stream,0);

   Stream.Destroy();
  except
   if SocketWriteString(socket, 'File Not Found', MAXINT, (parent as TGridUser).settings.enableDebugOutput) = false then
    begin
     memstream.Destroy();
     raise Exception.Create('');
    end;
  end;
 end;

 if SocketWriteString(socket, 'Ok', MAXINT, (parent as TGridUser).settings.enableDebugOutput) = false then
  begin
   memstream.Destroy();
   raise EInOutError.Create('');
  end;


 SuspendIfNetworkOverloaded(memStream.Size);

 if SocketWriteStream(socket, memStream, MAXINT, (parent as TGridUser).settings.enableDebugOutput) = false then
  begin
   NotifySendEnd(memStream.Size);
   memstream.Destroy();
   raise EInOutError.Create('');
  end;

 NotifySendEnd(memStream.Size);

 memstream.Destroy();
end;

//==============================================================================
//==============================================================================
procedure TUserLobbySocketThread.GetData(socket: TServerClientWinSocket; dataDesc: string);
var
 stream: IGenericStream;
 d: DWORD;
begin
 if (@(parent as TGridUser).GetDataCallback=nil) then
  begin
   if SocketWriteString(socket, 'GetData() callback not specified', MAXINT, (parent as TGridUser).settings.enableDebugOutput) = false then
    begin
     raise Exception.Create('');
    end;
  end;

 //check free memory
 SuspendIfNetworkOverloaded(0);

 (parent as TGridUser).GetDataCallback(pchar(dataDesc), stream);
 if (stream=nil) then
  begin
   if SocketWriteString(socket, 'GetData() callback returned NULL', MAXINT, (parent as TGridUser).settings.enableDebugOutput) = false then
    begin
     raise Exception.Create('');
    end;
  end;

 if SocketWriteString(socket, 'Ok', MAXINT, (parent as TGridUser).settings.enableDebugOutput) = false then
  begin
   raise Exception.Create('');
  end;

 SuspendIfNetworkOverloaded(stream.GetLength());

 (parent as TGridUser).DebugWrite('Sending userdata... "%s"',[dataDesc]);

 //todo: what if stream = nil ????
 if SocketWriteStream(socket, stream, MAXINT, (parent as TGridUser).settings.enableDebugOutput) = false then
  begin
   NotifySendEnd(stream.GetLength());
   Stream:=nil;
   raise Exception.Create('');
  end;

  NotifySendEnd(stream.GetLength());

 (parent as TGridUser).DebugWrite('Done sending userdata...',[]);

 d:=stream._AddRef();
 d:=stream._Release();
 assert(d=1);

 stream:=nil;
end;

//==============================================================================
//==============================================================================
//агент выполнил задачу и послал результаты
procedure TUserLobbySocketThread.TaskDone(socket: TServerClientWinSocket;
           const taskIdstr, CPUUsedStr, CPUAvailStr, freeMemoryStr: string);
var
 taskId: DWORD;
 outStream : IGenericStream;
 CPUUsed : single;
 CPUAvail : single;
 d: DWORD;
 freeMemory: DWORD;
begin
 //will throw out of proc
 taskId:=StrToInt64(taskIdStr);

 CPUUsed:=strToFloat(CPUUsedStr);  //Mhz!
 CPUAvail:=strToFloat(CPUAvailStr); //Mhz!
 freeMemory:=strToInt64(freeMemoryStr);

 (parent as TGridUser).DebugWrite('GRIDUSERLOBBY: TASK_DONE %u from %s',
                    [taskId, Socket.RemoteAddress]);

 if SocketReadStream(socket,outStream,MAXINT) = false then
  begin
   outStream:=nil; //release
   raise EInOutError.Create('');
  end;

 outStream._AddRef();
 d:=outStream._Release();

 //review: move up
 (parent as TGridUser).NotifyTaskDone(Socket.RemoteAddress, CPUUsed, CPUAvail, freeMemory);

 outStream._AddRef();
 d:=outStream._Release();

 (parent as TGridUser).FinalizeTask(taskId, outStream);
end;

//==============================================================================
//==============================================================================
//задача не выполнилась на агенте из-за ошибки
procedure TUserLobbySocketThread.TaskFailed(socket: TServerClientWinSocket; const taskIdstr, errorstr: string);
var
 taskId: DWORD;
begin
 taskId:=StrToInt64(taskIdStr);

 (parent as TGridUser).DebugWrite('GRIDUSERLOBBY: TASK_FAILED %u from %s - %s',
                    [taskId, Socket.RemoteAddress, HTTPDecode(errorstr)]);

 (parent as TGridUser).NotifyTaskFailed(socket.RemoteAddress, taskId);
end;

//==============================================================================
//==============================================================================
//suspend execution if network overloaded
//size - we are going to send size bytes
procedure TUserLobbySocketThread.SuspendIfNetworkOverloaded(size: DWORD);
label l1;
begin
 l1:

 cs.Enter();

 GlobalMemoryStatus(ms);

 if (nowSending>0) and
    ((size*2>ms.dwAvailPhys) or (ms.dwAvailPhys<ms.dwTotalPhys div 5)) then
  begin
   cs.Leave();

   (parent as TGridUser).DebugWrite('GRIDUSERLOBBY: Suspending - network overload...',[]);

   netevent.WaitFor(3000);
   goto l1;
  end;

 nowSending := nowSending+size;

 cs.Leave();
end;

//==============================================================================
//==============================================================================
procedure TUserLobbySocketThread.NotifySendEnd(size: DWORD);
begin
 cs.Enter();

 nowSending := nowSending-size;

 cs.Leave();

 netEvent.SetEvent();
end;

begin
 nowSending:=0;
 cs:=TCriticalSection.Create();
 netevent:=TEvent.Create(nil, false, false, '');

// GlobalMemoryStatus(ms);
// allowedToSend := ms.dwTotalPhys div 5;
// if (allowedToSend<64*1024*1024) then allowedToSend:=64*1024*1024;
end.

finalization
 cs.Destroy();
 netevent.Destroy();
end;


