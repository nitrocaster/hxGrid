program PICalculator;

{$APPTYPE CONSOLE}

uses
  Windows,
  SysUtils,
  I_Agent in '..\..\..\Interface\I_Agent.pas',
  I_GridUser in '..\..\..\Interface\I_GridUser.pas',
  hxgridcommon in '..\..\..\Common\hxgridcommon.pas',
  hxzlib in '..\..\..\Common\hxzlib.pas',
  T_GenericStream in '..\..\..\Common\T_GenericStream.pas',
  I_GenericStream in '..\..\..\Interface\I_GenericStream.pas',
  hxScktComp in '..\..\..\Common\hxScktComp.pas';

var
  pival : string;
  digits : DWORD;
  i : integer;
  d : DWORD;
  tick: DWORD;
  stream: IGenericStream;
  user: IGridUser;

//===============================================================
procedure Finalize(outStream: IGenericStream); cdecl;
var
 n: DWORD;
 d: DWORD;
begin
 assert(outstream.GetLength()=4+9);
 assert(outstream.GetPos()=0);

 outstream.read(n,4);

 outstream.read(pival[n],9);

 d:=outstream._AddRef();
 d:=outstream._Release();
end;


label l1;

begin
 if paramcount<>1 then
  begin
   l1:
   Writeln('Usage: picalculator [number_of_digits]');
   exit;
  end;

 val(paramStr(1), digits, i);

 if (digits<1) or (i<>0) then goto l1;

 digits:=(digits-1) div 9;
 inc(digits);

 Writeln('Calculating '+inttostr(digits*9+1)+' PI digits...');

 tick:=GetTickCount();

 setLength(pival, digits*9);

 IGridUser_Create(user);
 d:=user._AddRef();
 d:=user._Release();

 for i:=0 to digits-1 do
  begin
   stream:=TGenericStream.Create();
   d:=1+i*9;
   stream.write(d,4);
   user.RunTask('picalculator_task.dll','RunTask',stream,Finalize,d,true);
   pointer(stream):=nil;
  end;

 user.WaitForCompletion();

 Writeln('Done in '+inttostr(GetTickCount()-tick)+' ms');
 Writeln('');
 Writeln('PI=3.'+pival);
 Writeln('');
 Writeln('Releasing GridUser...');


 d:=user._AddRef();
 d:=user._Release();
 user:=nil;

 Writeln('Press any key...');

 readln;
end.
