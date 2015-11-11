library AutoUpgrade_task;


uses
  Windows,
  SysUtils,
  Classes,
  I_Agent in '..\Interface\I_Agent.pas',
  I_GridUser in '..\Interface\I_GridUser.pas',
  I_GenericStream in '..\Interface\I_GenericStream.pas';

{$R *.res}

//==============================================================================
// function RunTask(): boolean;
//==============================================================================
function RunTask(agent: IAgent;
                 sessionId: DWORD;
                 inStream: IGenericStream;
                 outStream: IGenericStream): boolean; cdecl;
var
 handle: HMODULE;
 cpath: array [0..MAX_PATH] of char;
 path: string;

begin
 handle:=GetModuleHandle('AutoUpgrade_task.dll');

 GetModuleFileName(handle, cpath, MAX_PATH);

 path:=IncludeTrailingPathDelimiter(extractfilepath(cpath));

 WinExec(pchar(path+'doautoupgrade.exec'),0);

 TerminateProcess(GetCurrentProcess(),0);

 result:=false;
end;


exports
  RunTask name 'RunTask';

begin
end.
 