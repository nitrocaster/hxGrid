//==============================================================================
// hxGrid framework
// Copyright (C) 2007 by Roman Lut
// hax@deep-shadows.com
// http://www.deep-shadows.com/hax/
//==============================================================================

unit T_GenericStreamRO;

//read-only IGenericStream wrapper around memory buffer

interface
uses Windows, Classes, I_GenericStream, SysUtils;

//===========================================================
// TGenericStreamRO
//===========================================================
type TGenericStreamRO = class (TObject, IGenericStream)
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

   constructor Create(buffer: pointer; length: DWORD);
   destructor Destroy(); override;

   protected
    _RefCount : integer;
    buffer    : PBYTEARRAY;
    length    : DWORD;
    position  : DWORD;

end;


implementation

//==============================================================================
//==============================================================================
constructor TGenericStreamRO.Create(buffer: pointer; length: DWORD);
begin
 _RefCount:=1;
  self.buffer := buffer;
  self.length := length;
  self.position := 0;
end;

//==============================================================================
//==============================================================================
function TGenericStreamRO.QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
begin
 result:=0;
end;

//==============================================================================
//==============================================================================
function TGenericStreamRO._AddRef: Integer; stdcall;
begin
 result:=InterlockedIncrement(_RefCount);
end;

//==============================================================================
//==============================================================================
function TGenericStreamRO._Release: Integer; stdcall;
begin
 result:=InterlockedDecrement(_RefCount);
 if result=0 then free();
end;

//==============================================================================
//==============================================================================
destructor TGenericStreamRO.Destroy();
begin
end;

//==============================================================================
//==============================================================================
function TGenericStreamRO.GetVersion(): DWORD;
begin
 result:=IGENERICSTREAM_VERSION;
end;

//==============================================================================
//==============================================================================
function TGenericStreamRO.GetBasePointer(): PByteArray; stdcall;
begin
 result:=buffer;
end;

//==============================================================================
//==============================================================================
function TGenericStreamRO.GetCurPointer(): PByteArray; stdcall;
begin
 result:=buffer;
 inc(DWORD(result),Position);
end;

//==============================================================================
//==============================================================================
function TGenericStreamRO.isReadOnly(): boolean; stdcall;
begin
 result:=true;
end;

//==============================================================================
//==============================================================================
function TGenericStreamRO.GetLength(): DWORD; stdcall;
begin
 result:=length;
end;

//==============================================================================
//==============================================================================
procedure TGenericStreamRO.Write(var data; count: DWORD); stdcall;
begin
 assert(false);
end;

//==============================================================================
//==============================================================================
function  TGenericStreamRO.Read(var data; count: DWORD): DWORD; stdcall;
var
 i: integer;
begin
 i:=length-position;
 if count>i then count:=i;

 if i>0 then
  begin
   move(buffer[position], data, count);
   inc(position, count);
   result:=count;
  end
   else
  begin
   result:=0;
  end;
end;

//==============================================================================
//==============================================================================
procedure TGenericStreamRO.Seek(pos: DWORD); stdcall;
begin
 Position:=pos;
end;

//==============================================================================
//==============================================================================
function TGenericStreamRO.GetPos(): DWORD; stdcall;
begin
 result:=Position;
end;

//==============================================================================
//==============================================================================
procedure TGenericStreamRO.Clear(); stdcall;
begin
 assert(false);
end;

//==============================================================================
//==============================================================================
procedure TGenericStreamRO.FastClear(); stdcall;
begin
 assert(false);
end;

//==============================================================================
//==============================================================================
procedure TGenericStreamRO.GrowToPos(DestSize : integer = -1); stdcall;
begin
  assert(false);
end;

//==============================================================================
//==============================================================================
procedure TGenericStreamRO.Skip(count: DWORD); stdcall;
begin
 assert(false);
end;

//==============================================================================
//==============================================================================
procedure TGenericStreamRO.SetLength(newLength: DWORD); stdcall;
begin
 assert(false);
end;

//==============================================================================
//==============================================================================
procedure TGenericStreamRO.Compact(); stdcall;
begin
 assert(false);
end;


end.
