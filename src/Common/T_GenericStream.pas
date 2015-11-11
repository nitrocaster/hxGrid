//==============================================================================
// hxGrid framework
// Copyright (C) 2007 by Roman Lut
// hax@deep-shadows.com
// http://www.deep-shadows.com/hax/
//==============================================================================

unit T_GenericStream;

interface
uses Windows, Classes, I_GenericStream, SysUtils;

//===========================================================
// TGenericStream
//===========================================================
type TGenericStream = class (TObject, IGenericStream)
  public

   //====== begin COM interface ==========
   function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
   function _AddRef: Integer; stdcall;
   function _Release: Integer; stdcall;

   function GetVersion(): DWORD;

   function GetBasePointer(): PByteArray; stdcall;

   function GetCurPointer(): PByteArray; stdcall;

   function isReadOnly(): boolean; stdcall;

   function GetLength(): DWORD; stdcall;

   procedure Write(var data; count: DWORD); stdcall;

   function  Read(var data; count: DWORD): DWORD; stdcall;

   procedure Seek(pos: DWORD); stdcall;

   function GetPos(): DWORD; stdcall;

   procedure Clear(); stdcall;

   procedure FastClear(); stdcall;

   procedure GrowToPos(DestSize : integer = -1); stdcall;

   procedure Skip(count: DWORD); stdcall;

   procedure SetLength(newLength: DWORD); stdcall;

   procedure Compact(); stdcall;

   //====== end COM interface ==========

   constructor Create();
   destructor Destroy(); override;

   protected
    _RefCount : integer;
    memStream: TMemoryStream;

end;


implementation

//==============================================================================
//==============================================================================
constructor TGenericStream.Create();
begin
 _RefCount:=1;
 memStream:=TMemoryStream.Create();
end;

//==============================================================================
//==============================================================================
function TGenericStream.QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
begin
 result:=0;
end;

//==============================================================================
//==============================================================================
function TGenericStream._AddRef: Integer; stdcall;
begin
 result:=InterlockedIncrement(_RefCount);
end;

//==============================================================================
//==============================================================================
function TGenericStream._Release: Integer; stdcall;
begin
 result:=InterlockedDecrement(_RefCount);
 if result=0 then free();
end;

//==============================================================================
//==============================================================================
destructor TGenericStream.Destroy();
begin
 memStream.Free();
end;

//==============================================================================
//==============================================================================
function TGenericStream.GetVersion(): DWORD;
begin
 result:=IGENERICSTREAM_VERSION;
end;

//==============================================================================
//==============================================================================
function TGenericStream.GetBasePointer(): PByteArray; stdcall;
begin
 result:=PBYTEARRAY(memStream.memory);
end;

//==============================================================================
//==============================================================================
function TGenericStream.GetCurPointer(): PByteArray; stdcall;
begin
 result:=PBYTEARRAY(memStream.memory);
 inc(DWORD(result),memStream.Position);
end;

//==============================================================================
//==============================================================================
function TGenericStream.isReadOnly(): boolean; stdcall;
begin
 result:=false;
end;

//==============================================================================
//==============================================================================
function TGenericStream.GetLength(): DWORD; stdcall;
begin
 result:=memStream.size;
end;

//==============================================================================
//==============================================================================
procedure TGenericStream.Write(var data; count: DWORD); stdcall;
begin
 memStream.Write(data,count);
end;

//==============================================================================
//==============================================================================
function  TGenericStream.Read(var data; count: DWORD): DWORD; stdcall;
begin
 result:=memStream.Read(data,count);
end;

//==============================================================================
//==============================================================================
procedure TGenericStream.Seek(pos: DWORD); stdcall;
begin
 memStream.Position:=pos;
end;

//==============================================================================
//==============================================================================
function TGenericStream.GetPos(): DWORD; stdcall;
begin
 result:=memStream.Position;
end;

//==============================================================================
//==============================================================================
procedure TGenericStream.Clear(); stdcall;
begin
 memStream.Clear();
end;

//==============================================================================
//==============================================================================
procedure TGenericStream.FastClear(); stdcall;
begin
 memStream.Clear();
end;

//==============================================================================
//==============================================================================
procedure TGenericStream.GrowToPos(DestSize : integer = -1); stdcall;
begin
 if (DestSize=-1) then memStream.SetSize(memStream.position)
                  else memStream.SetSize(DestSize);
end;

//==============================================================================
//==============================================================================
procedure TGenericStream.Skip(count: DWORD); stdcall;
begin
 assert(false);
end;

//==============================================================================
//==============================================================================
procedure TGenericStream.SetLength(newLength: DWORD); stdcall;
begin
 memStream.SetSize(newLength);
end;

//==============================================================================
//==============================================================================
procedure TGenericStream.Compact(); stdcall;
var
 pushpos: DWORD;
 memStream1 : TMemoryStream;
begin
 pushPos:=memStream.Position;
 memstream1:=TMemoryStream.Create();
 memStream1.LoadFromStream(memStream);
 memStream.Destroy();
 memStream:=memStream1;
 memStream.Seek(pushPos, soFromBeginning);
end;


end.
