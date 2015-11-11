program AutoUpgrade;

{$APPTYPE CONSOLE}

uses
  Windows, SysUtils, I_GridUser, I_GenericStream, T_GenericStream;

var
  i : integer;
  stream: IGenericStream;
  user: IGridUser;
  d: DWORD;

type
 TCreateGridUserObject = function(version: DWORD): pointer; cdecl;

var
 DLLHandle : HINST;
 pProc: TCreateGridUserObject;
  
begin
 Writeln('Updating agents..., 5 minutes cycle');

 IGridUser_Create(user);

 for i:=0 to 10 do
  begin
   stream:=TGenericStream.Create();
   user.RunTask('autoupgrade_task.dll,DoAutoUpgrade.exec,hxgrid_agent_setup.exe','RunTask',stream,nil,d,true);
   pointer(stream):=nil;
  end;

 d:=GetTickCount();

 while (GetTickCount()-d<5*60000) do
  begin
   user.WaitForCompletionEvent(1000);
  end; 

 Writeln('Done.');
end.
