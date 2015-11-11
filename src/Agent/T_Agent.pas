//==============================================================================
// hxGrid framework
// Copyright (C) 2007 by Roman Lut
// hax@deep-shadows.com
// http://www.deep-shadows.com/hax/
//==============================================================================

unit T_Agent;

interface
uses Windows, Classes, T_AgentLobby, T_AgentStatusThread, T_AgentPeer,
     T_FileCache, SyncObjs, T_AgentWorkerThread, hxgridcommon, I_agent,
     I_GenericStream, T_AgentDataCache, safefpu;

//=========================================
// Agent threads:
// main thread - обслуживание окна приложения
// worker thread - здесь выполняются задания
// lobby thread - открыта TCP сервером, принимает задачи
// status thread - посылает status на координатор
//=========================================


//==========================================
// class TAgent
//==========================================
//собственно агент
type TAgent = class(TOBject,IAgent)
  private
   owner         : TComponent;

   usersListCS   : TCriticalSection;

   //list of users
   usersList       : TStringList;
   usersTasks      : TStringList;

   globalCS        : TRTLCriticalSection;

   procedure LoadSettings();

  public

   lobby           : TAgentLobby;
   statusThread    : TAgentStatusThread;

   settings        : TAgentSettings;

   //guard access to settings.coordinator_ip
   settingsCS      : TCriticalSection;

   //set to true when form is closed, we starting to destroying TAgent
   //checked in TestConnection()
   destroying       : boolean;

   //'application data\local\user\' - with trailing delimiter
   baseCachePath    : string;

   //============== begin COM interface =========
   function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
   function _AddRef: Integer; stdcall;
   function _Release: Integer; stdcall;

   function GetVersion(var version: DWORD): HRESULT; stdcall;

   function GetData(sessionId: DWORD; dataDesc: pchar; var stream:IGenericStream): HRESULT; stdcall;

   function FreeCachedData(sessionId: DWORD; dataDesc: pchar): HRESULT; stdcall;

   function TestConnection(sessionId :DWORD): HRESULT; stdcall;

   function GetSessionCacheDirectory(sessionId :DWORD; path: pchar): HRESULT; stdcall;

   function GetGlobalCriticalSection(var cs: PRTLCriticalSection): HRESULT; stdcall;

  //============== end COM interface =========

   constructor Create(owner: TComponent);
   destructor Destroy(); override;

   procedure SaveSettings();

   //=== internal use ===
   function GetStatus(): string;

   procedure UpdateStatus_Add(const userIP: string);
   procedure UpdateStatus_Remove(const userIP: string);
   procedure UpdateStatus_SetTasks(const userIP: string; const tasksList: string);

   procedure DebugWrite(const Format: string; const Args: array of const);
 end;

implementation
uses unit1,Sysutils, Inifiles, forms, T_AgentThreadPool, SHFolder;

//==============================================================================
//constructor TAgent.Create();
//==============================================================================
constructor TAgent.Create(owner: TComponent);
var
 strpath: array [0..MAX_PATH] of char;
begin
 destroying:=false;

 InitializeCriticalSection(globalCS);

 self.owner:=owner;

 SHGetFolderPath( 0, CSIDL_LOCAL_APPDATA, 0, 0 {SHGFP_TYPE_CURRENT}, strPath );

 baseCachePath:=IncludeTrailingPathDelimiter(strPath) + 'hxgrid\TEMP\';

 EnsureFolderExists(baseCachePath+'dummy.txt');

 settingsCS := TCriticalSection.Create();

 LoadSettings();

 usersListCS:=TCriticalSection.Create();
 usersList:=TStringList.Create();
 usersTasks:=TStringList.Create();

 lobby:=TAgentLobby.Create(self);
 statusThread:=TAgentStatusThread.Create(self);
end;

//==============================================================================
//destructor TAgent.Destroy();
//==============================================================================
destructor TAgent.Destroy();
begin
 destroying:=true;

 statusThread.Destroy();
 lobby.Destroy();

 usersList.Destroy();
 usersTasks.Destroy();
 usersListCS.Destroy();

 settingsCS.Destroy();

 DeleteCriticalSection(globalCS);
end;

//==============================================================================
//procedure TAgent.DebugWrite();
//==============================================================================
procedure TAgent.DebugWrite(const Format: string; const Args: array of const);
var 
 locale: TFormatSettings;
begin
 if settings.enableDebugOutput=false then exit;

 try
  GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT, locale);

  if (length(args)=0) then OutputDebugString(pchar(format+#13))
                      else OutputDebugString(pchar(sysutils.Format(format, args, locale)+#13));

 except
  OutputDebugString('DEBUGWRITE ERROR: unable to format string');
  MessageBeep(0);
 end;

end;


//==============================================================================
//==============================================================================
//вернуть текущий статус
//вызывается из agent status thread и main thread (апдейт окна)
function TAgent.GetStatus(): string;
var
 i: integer;
begin
 usersListCS.Enter();

 if (usersList.count=0) then
  begin
   result:='Unassigned';
  end
   else
  begin
   result:='Working for '+usersList[0]+'[' + usersTasks[0] + ']';

   for i:=1 to usersList.Count-1 do
       result:=result+', '+usersList[i]+'[' + usersTasks[i] +']';
  end;

 if TAgentThreadPool.GetSuspend()>0 then result:=result+' [suspended, '+inttostr(TAgentThreadPool.GetSuspend())+']';

 if (statusThread.GetState()=AS1_CONNECT_TO_COORDINATOR) or
    (statusThread.GetState()=AS1_CONNECT_TO_COORDINATOR_PAUSE) then
     result:=result + ', connecting to coordinator...';

 if (statusThread.GetState()=AS1_BROADCAST_TO_COORDINATOR) then
     result:=result + ', searching for coordinator...';

 usersListCS.Leave();
end;

//==============================================================================
//==============================================================================
procedure TAgent.UpdateStatus_Add(const userIP: string);
begin
 usersListCS.Enter();

 usersList.Add(userIp);
 usersTasks.Add('');

 usersListCS.Leave();
end;

//==============================================================================
//==============================================================================
procedure TAgent.UpdateStatus_Remove(const userIP: string);
var
 index: integer;
begin
 usersListCS.Enter();

 for index:=usersList.count-1 downto 0 do
  begin
   if usersList[index]=userIp then
    begin
     usersList.Delete(index);
     usersTasks.Delete(index);
     break;
    end;
  end;

 usersListCS.Leave();
end;

//==============================================================================
//==============================================================================
procedure TAgent.UpdateStatus_SetTasks(const userIP: string; const tasksList: string);
var
 index: integer;
begin
 usersListCS.Enter();

 index:=usersList.IndexOf(userIp);
 usersTasks[index]:=tasksList;

 usersListCS.Leave();
end;

//==============================================================================
//==============================================================================
procedure TAgent.LoadSettings();
var
 Ini: TIniFile;
 i: integer;
begin
 Ini := TIniFile.Create( ExtractFilePath( Application.ExeName ) + 'hxgrid.INI' );
  try
   strcopy(settings.bind_ip, pchar(Ini.ReadString('agent', 'bind_ip', '' )));
   settings.bind_port := Ini.ReadInteger('agent', 'bind_port', AGENT_LOBBY_PORT );

   strcopy(settings.coordinator_ip, pchar(Ini.ReadString('coordinator', 'coordinator_ip', '' )));
   settings.coordinator_port := Ini.ReadInteger('coordinator', 'bind_port', COORDINATOR_PORT );

   settings.user_data_port  := Ini.ReadInteger('user', 'data_port', USER_DATA_PORT );

   settings.useHT := Ini.ReadBool('agent', 'useHT', true );

   settings.explicitCPU := Ini.ReadBool('agent', 'explicitCPU', true );

   settings.freeCPUCount := Ini.ReadInteger('agent', 'freeCPUCount', 0 );

   settings.suspend_timeout := Ini.Readinteger('agent', 'suspend_timeout', 30 );

   settings.agent_broadcast_port  := Ini.ReadInteger('agent', 'broadcast_port', AGENT_BROADCAST_PORT );
   settings.coordinator_broadcast_port  := Ini.ReadInteger('coordinator', 'broadcast_port', COORDINATOR_BROADCAST_PORT );

   settings.enableDebugOutput := Ini.ReadBool('agent', 'enableDebugOutput', false );

   settings.maxDataCacheSize  := Ini.ReadInteger('agent', 'maxDataCacheSize', AGENT_MAX_DATA_CACHE_SIZE );

   settings.allowDiscardCoordinatorIp := Ini.ReadBool('coordinator', 'allowDiscardCoordinatorIp', true);
  finally
   Ini.Free;
  end;

 i:=GetCPUCount();
 if settings.freeCPUCount>i then
  begin
   settings.poolSize:=1;
  end
   else
  begin
   settings.poolSize:=i-settings.freeCPUCount;
  end;

end;

//==============================================================================
//==============================================================================
procedure TAgent.SaveSettings();
var
 Ini: TIniFile;
begin
 settingsCS.Enter();

 Ini := TIniFile.Create( ExtractFilePath( Application.ExeName ) + 'hxgrid.INI' );
  try
   //todo: full save
   Ini.WriteString('coordinator', 'coordinator_ip', settings.coordinator_ip);
  finally
   Ini.Free;
  end;

 settingsCS.Leave();
end;


//==============================================================================
//==============================================================================
function TAgent.GetVersion(var version: DWORD): HRESULT; stdcall;
begin
 version:=IAGENT_VERSION;
 result:=S_OK;
end;

//========================================================
//function TAgent.QueryInterface(): HResult;
//========================================================
function TAgent.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
 result:=E_NOINTERFACE;
end;

//========================================================
//function TAgent._AddRef: Integer;
//========================================================
function TAgent._AddRef: Integer;
begin
 result:=1;
end;

//========================================================
//function TAgent._Release: Integer;
//========================================================
function TAgent._Release: Integer;
begin
 result:=1;
end;

//========================================================
//========================================================
function TAgent.GetData(sessionId: DWORD; dataDesc: pchar; var stream: IGenericStream): HRESULT;
begin
 pointer(stream):=nil; //do not try to release garbage ptr 
 result:=TAgentWorkerThread(sessionId).GetData(dataDesc, stream);
 if stream<>nil then stream.Seek(0);
end;

//========================================================
//========================================================
function TAgent.FreeCachedData(sessionId: DWORD; dataDesc: pchar): HRESULT;
begin
 TAgentWorkerThread(sessionId).FreeCachedData(dataDesc);
 result:=S_OK;
end;

//========================================================
//========================================================
//sessionId is actually a pointer to TAgentWorkerThread
function TAgent.TestConnection(sessionId :DWORD): HRESULT; stdcall;
begin
 while (TAgentThreadPool.GetSuspend()>0) and
       (destroying=false) and
       (TAgentWorkerThread(sessionId).gTerminated=false) do
        begin
         DebugWrite('Suspended...',[]);
         Sleep(1000);
        end;

 if (TAgentWorkerThread(sessionId).gTerminated=false) then result:=S_OK else result:=S_FALSE;
end;

//========================================================
//========================================================
function TAgent.GetSessionCacheDirectory(sessionId :DWORD; path: pchar): HRESULT; stdcall;
begin
 TAgentWorkerThread(sessionId).GetSessionCacheDirectory(path);
 result:=S_OK;
end;

//========================================================
//========================================================
function TAgent.GetGlobalCriticalSection(var cs: PRTLCriticalSection): HRESULT; stdcall;
begin
 cs:=@globalcs;
 result:=S_OK;
end;


end.
