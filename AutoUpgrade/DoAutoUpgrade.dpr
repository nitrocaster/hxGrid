program DoAutoUpgrade;

{$APPTYPE CONSOLE}

uses
  windows, SysUtils;

{$E exe}

var
 startupinfo : tstartupinfo;
 processinfo : tprocessinformation;
 i: integer;

 handle: HMODULE;
 cpath: array [0..MAX_PATH] of char;
 path: string;

begin
 handle:=GetModuleHandle('doautoupgrade.exec');

 GetModuleFileName(handle, cpath, MAX_PATH);

 path:=IncludeTrailingPathDelimiter(extractfilepath(cpath));

 Sleep(5000);

 with startupinfo do
   begin
    cb:=sizeof(TStartUpInfo);
    lpReserved:=nil;
    lpDeskTop:=nil;
    lpTitle:=nil;
    dwFlags:=0;
    cbReserved2:=0;
    lpReserved2:=nil;
   end;

 if not createprocess(nil,pchar(path+'hxgrid_agent_setup.exe /VERYSILENT  /SUPPRESSMSGBOXES'),nil,nil,true,
   CREATE_DEFAULT_ERROR_MODE or CREATE_NEW_CONSOLE or CREATE_NEW_PROCESS_GROUP,
      nil,nil,startupinfo,processinfo) then
        begin
         exit;
        end;

 repeat
  GetExitCodeProcess(processinfo.hprocess,dword(i));
  sleep(100);
 until i<>STILL_ACTIVE;

 Sleep(60000);

  if not createprocess(nil,'"C:\Program Files\hxGrid\agent.exe"',nil,nil,true,
   CREATE_DEFAULT_ERROR_MODE or CREATE_NEW_CONSOLE or CREATE_NEW_PROCESS_GROUP,
      nil,nil,startupinfo,processinfo) then
        begin
         exit;
        end;
end.
 