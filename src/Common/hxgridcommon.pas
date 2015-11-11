//==============================================================================
// hxGrid framework
// Copyright (C) 2007 by Roman Lut
// hax@deep-shadows.com
// http://www.deep-shadows.com/hax/
//==============================================================================

{$I-}
unit hxgridcommon;

interface
uses Windows, Classes, hxScktComp, I_GenericStream;

const CURRENT_AGENT_VERSION = $109;

//port for sending status update
const COORDINATOR_PORT : DWORD = 4100;

//remove agent from list of agents on coordinator
//if it does not send UPDATE_STATUS more then N period
const AGENT_STATUS_TIMEOUT = 10000;

//порт agent lobby
const AGENT_LOBBY_PORT : DWORD = 4102;

//порт на клиенте, с которого можно получать файлы
const USER_DATA_PORT  : DWORD = 4103;

const AGENT_BROADCAST_PORT        : DWORD = 4104; //agent's serverssocket port
const COORDINATOR_BROADCAST_PORT  : DWORD = 4105; //coordinators's serverssocket port
const USER_BROADCAST_PORT         : DWORD = 4106; //user's serverssocket port

 //agent sends updated status to coordinator, period in ms
const AGENT_UPDATE_STATUS_PERIOD  =  1000;

//agent to reconnect to coordinator timeout if connection failed
const AGENT_COORDINATOR_RECONNECT_TIMEOUT = 5000;

//allow to send dublicate task if task RunTask is not called more then timeout
//also allows send if RunTask is blocked - no free memory on user or quere is full
const ADD_TASK_TIMEOUT = 30000;

const COMPRESSED_STREAM_SIGNATURE1 : DWORD = $94903495;
const COMPRESSED_STREAM_SIGNATURE2 : DWORD = $A40943FF;

//max data cache size on agent
const AGENT_MAX_DATA_CACHE_SIZE : DWORD = 100000;

//send /receive with this size of block
const NET_BLOCK_SIZE = 32*1024;

const ID_AGENT_BROADCAST             = 1;
const ID_USER_BROADCAST              = 2;
const ID_COORDINATOR_BROADCAST       = 3;

//if there ir less free CPU time (%), then suspend
const MIN_FREE_CPU  = 10;

//==================================
//==================================
//state of TAgentStatusThread
type TAgentState1 =
(
 AS1_BROADCAST_TO_COORDINATOR,
 AS1_CONNECT_TO_COORDINATOR,
 AS1_CONNECT_TO_COORDINATOR_PAUSE,
 AS1_UPDATE_STATUS,
 AS1_UPDATE_STATUS_PAUSE
);

type TThreadNameInfo = record
    FType: LongWord;     // must be 0x1000
    FName: PChar;        // pointer to name (in user address space)
    FThreadID: LongWord; // thread ID (-1 indicates caller thread)
    FFlags: LongWord;    // reserved for future use, must be zero
  end;

function SocketReadString(socket: TCustomWinSocket; var s: string; timeout: DWORD): boolean;
function SocketReadStream(socket: TCustomWinSocket; var memStream: TMemoryStream; timeout: DWORD): boolean; overload;
function SocketReadStream(socket: TCustomWinSocket; var memStream: IGenericStream; timeout: DWORD): boolean; overload;
function SocketWriteString(socket: TCustomWinSocket; s: String; timeOut: DWORD; enableDebugOutput: boolean): boolean;
function SocketWriteStream(socket: TCustomWinSocket; memStream: TMemoryStream; timeout: DWORD; enableDebugOutput: boolean): boolean; overload;
function SocketWriteStream(socket: TCustomWinSocket; memStream: IGenericStream; timeout: DWORD; enableDebugOutput: boolean): boolean; overload;
function SocketWriteStringAndStream(socket: TCustomWinSocket; s: String; memStream: IGenericStream; timeOut: DWORD; enableDebugOutput: boolean): boolean; overload;
//function SocketWriteStringAndStream(socket: TCustomWinSocket; s: String; memStream: TMemoryStream; timeOut: DWORD): boolean; overload;

procedure EnsureFolderExists(fname: string);

procedure CompressStream(stream: IGenericStream);
procedure DeCompressStream(stream: IGenericStream);
function IsCompressedStream(stream: IGenericStream): boolean;
function GetUncompressedStreamSize(stream: IGenericStream): DWORD;

function StreamReadString(stream : IGenericStream) : string;
procedure StreamWriteString(stream : IGenericStream; const s: string);

procedure SetThreadName(const name: string);

function GetCPUCount(): DWORD;

function IsWindowsXP(): boolean;

implementation
uses SysUtils, T_GenericStream, Registry, hxzlib;

//==============================================================================
//==============================================================================
procedure StreamReadBytes(stream: TWinSocketStream; p:pointer; count :DWORD);
var
 d: DWORD;
 d1: dword;
begin
 while count>0 do
  begin
   d1:=count;
   if d1>NET_BLOCK_SIZE then d1:=NET_BLOCK_SIZE;

   d:=stream.Read(p^, d1);

   if (d=0) then
    begin
     raise EInOutError.Create('');
    end;

   inc(DWORD(p),d);
   dec(count,d);
  end;
end;

//==============================================================================
//==============================================================================
function SocketReadString(socket: TCustomWinSocket; var s: string; timeout: DWORD): boolean;
var
 stream: TWinSocketStream;
 d: dword;
 label l1;
begin
 result:=false;
 if socket.Connected=false then exit;

 Stream := TWinSocketStream.Create(socket, timeOut);

 try
  StreamReadBytes(stream, @d, 4);
  SetLength(s,d);
  StreamReadBytes(stream, @s[1], d);
  result:=true;
 except
  result:=false;
 end;

 Stream.Free;
end;

//==============================================================================
//==============================================================================
function SocketWriteString(socket: TCustomWinSocket; s: String; timeOut: DWORD; enableDebugOutput: boolean): boolean;
var
 stream: TWinSocketStream;
 d: DWORD;
begin
 result:=false;
 if socket.Connected=false then exit;

 Stream := TWinSocketStream.Create(socket, timeOut);

 try
  d:=length(s);
  Stream.WriteBuffer(d, 4);
  Stream.WriteBuffer(s[1], d);
  result:=true;
 except
  on E: Exception do
   begin
    if enableDebugOutput then OutputDebugString(pchar('SocketWriteString: '+E.Message));
    result:=false;
   end;
 end;

 Stream.Free;
end;

//==============================================================================
//==============================================================================
function SocketReadStream(socket: TCustomWinSocket; var memStream: TMemoryStream; timeout: DWORD): boolean;
var
 s:string;
begin
 result:=false;
 if socket.Connected=false then exit;

 if SocketReadString(socket,s,timeout)=false then exit;

 memStream := TMemoryStream.Create();
 memStream.Write(s[1],length(s));
 result:=true;
end;

//==============================================================================
//==============================================================================
function SocketReadStream(socket: TCustomWinSocket; var memStream: IGenericStream; timeout: DWORD): boolean;
var
 s:string;
begin
 result:=false;
 if socket.Connected=false then exit;

 if SocketReadString(socket,s,timeout)=false then exit;

 //should return with refcount=1
 pointer(memStream) := pointer(IGenericStream(TGenericStream.Create()));
 memStream.Write(s[1],length(s));
 result:=true;
end;


//==============================================================================
//==============================================================================
function SocketWriteStream(socket: TCustomWinSocket; memStream: TMemoryStream; timeout: DWORD; enableDebugOutput: boolean): boolean;
var
 stream: TWinSocketStream;
 d,d1,index: DWORD;
begin
 result:=false;
 if socket.Connected=false then exit;

 Stream := TWinSocketStream.Create(socket, timeOut);

 try
  d:=memStream.Size;
  Stream.WriteBuffer(d, 4);

  index:=0;
  while (d>0) do
   begin
    d1:=d;
    if d1>NET_BLOCK_SIZE then d1:=NET_BLOCK_SIZE;

    Stream.WriteBuffer(TByteArray(memStream.memory^)[index],d1);

    dec(d,d1);
    inc(index,d1);
   end;

  result:=true;
 except
  on E: Exception do
   begin
    if enableDebugOutput then OutputDebugString(pchar('SocketWriteStringAndStream: '+E.Message));
    result:=false;
   end;
 end;

 Stream.Free;
end;

//==============================================================================
//==============================================================================
function SocketWriteStream(socket: TCustomWinSocket; memStream: IGenericStream; timeout: DWORD; enableDebugOutput: boolean): boolean;
var
 stream: TWinSocketStream;
 d,d1,index: DWORD;
begin
 result:=false;
 if socket.Connected=false then exit;

 Stream := TWinSocketStream.Create(socket, timeOut);

 try
  d:=memStream.GetLength();
  Stream.WriteBuffer(d, 4);

  index:=0;
  while (d>0) do
   begin
    d1:=d;
    if d1>NET_BLOCK_SIZE then d1:=NET_BLOCK_SIZE;

    Stream.WriteBuffer(TByteArray(memStream.GetBasePointer()^)[index],d1);

    dec(d,d1);
    inc(index,d1);
   end;

  result:=true;
 except
  on E: Exception do
   begin
    if enableDebugOutput then OutputDebugString(pchar('SocketWriteStringAndStream: '+E.Message));
    result:=false;
   end;
 end;

 Stream.Free;
end;

//==============================================================================
//==============================================================================
function SocketWriteStringAndStream(socket: TCustomWinSocket;
                                    s: String;
                                    memStream: IGenericStream;
                                    timeOut: DWORD;
                                    enableDebugOutput: boolean): boolean;
var
 stream: TWinSocketStream;
 d,d1,index: DWORD;
begin
 result:=false;
 if socket.Connected=false then exit;

 Stream := TWinSocketStream.Create(socket, timeOut);

 try
  d:=length(s);
  Stream.WriteBuffer(d, 4);
  Stream.WriteBuffer(s[1], d);

  d:=memStream.GetLength();
  Stream.WriteBuffer(d, 4);

  index:=0;
  while (d>0) do
   begin
    d1:=d;
    if d1>NET_BLOCK_SIZE then d1:=NET_BLOCK_SIZE;

    Stream.WriteBuffer(TByteArray(memStream.GetBasePointer()^)[index],d1);

    dec(d,d1);
    inc(index,d1);
   end;


  result:=true;
 except
  on E: Exception do
   begin
    if enableDebugOutput then OutputDebugString(pchar('SocketWriteStringAndStream: '+E.Message));
    result:=false;
   end;
 end;

 Stream.Free;
end;

(*
//==============================================================================
//==============================================================================
function SocketWriteStringAndStream(socket: TCustomWinSocket;
                                    s: String;
                                    memStream: TMemoryStream;
                                    timeOut: DWORD): boolean;
var
 stream: TWinSocketStream;
 d: DWORD;
begin
 result:=false;
 if socket.Connected=false then exit;

 Stream := TWinSocketStream.Create(socket, timeOut);

 try
  d:=length(s);
  Stream.WriteBuffer(d, 4);
  Stream.WriteBuffer(s[1], d);

  d:=memStream.Size;
  SetLength(s,d);
  memStream.Seek(0,soFromBeginning);
  memStream.Read(s[1],length(s));

  Stream.WriteBuffer(d, 4);
  Stream.WriteBuffer(s[1], d);

  result:=true;
 except
  result:=false;
 end;

 Stream.Free;
end;
*)
(*
//==============================================================================
//==============================================================================
procedure CompressStream(stream: IGenericStream);
var
 s1: TMemoryStream;
 cstream: TCompressionStream;
 d,d1: DWORD;
begin
 s1:=TMemoryStream.Create();
 cstream:=TCompressionStream.Create(clFastest, s1);

 cstream.WriteBuffer(stream.GetBasePointer()^, stream.GetLength());

 cstream.Free;

 d:=stream.GetLength();

 stream.FastClear();

 stream.Seek(0);
 d1:=COMPRESSED_STREAM_SIGNATURE1;
 stream.Write(d1,4);
 d1:=COMPRESSED_STREAM_SIGNATURE2;
 stream.Write(d1,4);
 stream.Write(d,4);
 stream.Write(s1.memory^, s1.Size);

// stream.Compact();
 stream.Seek(0);

 s1.Free;
end;

//==============================================================================
//==============================================================================
procedure DeCompressStream(stream: IGenericStream);
var
 s1: TMemoryStream;
 cstream: TDeCompressionStream;
 d: DWORD;
begin
 s1:=TMemoryStream.Create();
 s1.WriteBuffer(stream.GetBasePointer()^, stream.GetLength());

 s1.Seek(0, soFromBeginning);
 s1.ReadBuffer(d,4);
 assert(d = COMPRESSED_STREAM_SIGNATURE1);
 s1.ReadBuffer(d,4);
 assert(d = COMPRESSED_STREAM_SIGNATURE2);

 s1.ReadBuffer(d,4);

 cstream:=TDeCompressionStream.Create(s1);

 stream.SetLength(d);

 cstream.ReadBuffer(stream.GetBasePointer()^, d);

 stream.Seek(0);

 cstream.Free;
 s1.Free;
end;
*)

//==============================================================================
//==============================================================================
procedure CompressStream(stream: IGenericStream);
var
 buffer: pointer;
 size: DWORD;
 uncompressedSize: DWORD;
 d1: dword;
 res: DWORD;
begin
 if isCompressedStream(stream) then exit;

 uncompressedSize := stream.GetLength();

 size:=zlib_compressBound(uncompressedSize);

 getmem(buffer,size+1);

 d1:=size;
 pbytearray(buffer)[d1]:=$aa;

 res:=zlib_compress(buffer, size, stream.GetBasePointer(), uncompressedSize, 1);
 assert(res=ZLIB_OK);
 assert(pbytearray(buffer)[d1]=$aa);
 //now 'size' holds compressed size

 stream.FastClear();

 stream.Seek(0);
 d1:=COMPRESSED_STREAM_SIGNATURE1;
 stream.Write(d1,4);
 d1:=COMPRESSED_STREAM_SIGNATURE2;
 stream.Write(d1,4);
 stream.Write(uncompressedSize,4);
 stream.Write(buffer^, size);

 stream.Compact();

 stream.Seek(0);

 freemem(buffer);
end;

//==============================================================================
//==============================================================================
procedure DeCompressStream(stream: IGenericStream);
var
 uncompressedSize: DWORD;
 compressedSize: DWORD;
 buffer: pointer;
 d: dword;
 res: dword;
begin
 stream.Seek(0);
 stream.Read(d,4);
 assert(d = COMPRESSED_STREAM_SIGNATURE1);
 stream.Read(d,4);
 assert(d = COMPRESSED_STREAM_SIGNATURE2);

 stream.Read(uncompressedSize,4);

 compressedSize := stream.GetLength()-stream.GetPos();

 getmem(buffer,compressedSize);

 stream.Read(buffer^, compressedSize);

 stream.SetLength(uncompressedSize);

 res:=zlib_uncompress(stream.GetBasePointer(), uncompressedSize, buffer, compressedSize);
 assert(res=ZLIB_OK);

 stream.Seek(0);

 freemem(buffer);
end;


//==============================================================================
//==============================================================================
function IsCompressedStream(stream: IGenericStream): boolean;
type TDWORDARRAY = array [0..1] of DWORD;
type PDWORDARRAY = ^TDWORDARRAY;
begin
 result:=(stream.GetLength()>=12) and
         (PDWORDARRAY(stream.GetBasePointer())[0]=COMPRESSED_STREAM_SIGNATURE1) and
         (PDWORDARRAY(stream.GetBasePointer())[1]=COMPRESSED_STREAM_SIGNATURE2);
end;

//==============================================================================
//==============================================================================
function GetUncompressedStreamSize(stream: IGenericStream): DWORD;
type TDWORDARRAY = array [0..3] of DWORD;
type PDWORDARRAY = ^TDWORDARRAY;
begin
  if (IsCompressedStream(stream)) then result := PDWORDARRAY(stream.GetBasePointer())[2]
                                  else result := stream.GetLength();
end;

(*
//====================================================
//====================================================
//fname = 'c:\game\minitex\list.dat'
procedure EnsureFolderExists(fname: string);
var
 i: integer;
 s,s1,s2: String;
 pushdir: string;
 label l1,l2;
begin
 pushdir:=GetCurrentDir();
 ioresult;

 i:=1;
 s:='';
 l2:
 s2:='';
 while (i<=length(fname)) and (fname[i]<>'\') do
  begin
   s:=s+fname[i];
   s2:=s2+fname[i];
   inc(i);
  end;

 if i=length(fname)+1 then goto l1;

 if s[length(s)]=':' then chdir(s+'\') else chdir(s);
 if ioresult<>0 then
  begin
   if (length(s1)>0) and (s1[length(s1)]=':') then chdir(s1+'\') else chdir(s1);
   mkdir(s2);
   if (length(s)>0) and  (s[length(s)]=':') then chdir(s+'\') else chdir(s);
   if ioresult<>0 then goto l1;
  end;

 s1:=s;

 while (i<=length(fname)) and (fname[i]='\') do
  begin
   s:=s+fname[i];
   inc(i);
  end;

 goto l2;

 l1:
 ioresult;
 chdir(pushdir);
end;
*)

//====================================================
//====================================================
//fname = 'c:\game\minitex\list.dat'
procedure EnsureFolderExists(fname: string);
var
 i: integer;
 s,s1,s2: String;
 label l1,l2;
begin
 ioresult;

 i:=1;
 s:='';
 l2:
 s2:='';
 while (i<=length(fname)) and (fname[i]<>'\') do
  begin
   s:=s+fname[i];
   s2:=s2+fname[i];
   inc(i);
  end;

 if i=length(fname)+1 then goto l1;

// if s[length(s)]=':' then chdir(s+'\') else chdir(s);
 if DirectoryExists(includetrailingpathdelimiter(s))=false then
  begin
   CreateDir(includetrailingpathdelimiter(s));
   if ioresult<>0 then goto l1;
  end;

 s1:=s;

 while (i<=length(fname)) and (fname[i]='\') do
  begin
   s:=s+fname[i];
   inc(i);
  end;

 goto l2;

 l1:
 ioresult;
end;

//====================================================
//====================================================
function StreamReadString(stream : IGenericStream) : string;
var
 d: DWORD;
begin
 stream.Read(d,sizeof(DWORD));
 setLength(result,d);
 stream.read(result[1],d);
end;

//====================================================
//====================================================
procedure StreamWriteString(stream : IGenericStream; const s: string);
var
 d: DWORD;
 p: pointer;
begin
 d:=length(s);
 stream.Write(d,sizeof(DWORD));
 p:=@s[1];
 stream.write(p^,d);
end;

//==============================================================================
//==============================================================================
function GetCPUCount(): DWORD;
var
 Registry: TRegistry;
 i: integer;
begin
 Registry := TRegistry.Create(KEY_READ);

 result:=1;
 for i:=1 to 10 do
  begin
   Registry.RootKey:= HKEY_LOCAL_MACHINE;
   if Registry.OpenKey('HARDWARE\DESCRIPTION\System\CentralProcessor\'+inttostr(i-1), False) = false then break;
   Registry.CloseKey();
   result:=i;
   if result<1 then result:=1;
  end;

 Registry.Free;
end;

//==============================================================================
//==============================================================================
procedure SetThreadName(const name: string);
var
  ThreadNameInfo: TThreadNameInfo;
begin
  ThreadNameInfo.FType := $1000;
  ThreadNameInfo.FName := pchar(name);
  ThreadNameInfo.FThreadID := $FFFFFFFF;
  ThreadNameInfo.FFlags := 0;

  try
    RaiseException( $406D1388, 0, sizeof(ThreadNameInfo) div sizeof(LongWord), @ThreadNameInfo );
  except
  end;
end;

//=================================================
//=================================================
function IsWindowsXP(): boolean;
var
 osVer   : OSVERSIONINFO;
begin
 ZeroMemory(@osVer, Sizeof(osVer));
 osVer.dwOSVersionInfoSize := Sizeof(osVer);
 if not GetVersionEx(osVer) then result:=true
  else result:=(osVer.dwMajorVersion=5) and (osVer.dwMinorVersion=1);
end;


end.
