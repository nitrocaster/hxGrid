program agent;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  T_AgentStatusThread in 'T_AgentStatusThread.pas',
  T_AgentLobby in 'T_AgentLobby.pas',
  T_Agent in 'T_Agent.pas',
  T_FileCache in 'T_FileCache.pas',
  hxgridcommon in '..\Common\hxgridcommon.pas',
  T_AgentPeer in 'T_AgentPeer.pas',
  T_AgentWorkerThread in 'T_AgentWorkerThread.pas',
  T_AgentLobbySocketThread in 'T_AgentLobbySocketThread.pas',
  T_GenericStream in '..\Common\T_GenericStream.pas',
  I_GridUser in '..\Interface\I_GridUser.pas',
  windows,
  safefpu,
  hxzlib in '..\Common\hxzlib.pas',
  I_Agent in '..\Interface\I_Agent.pas',
  T_AgentDataCache in 'T_AgentDataCache.pas',
  I_GenericStream in '..\Interface\I_GenericStream.pas',
  T_Semaphore in '..\Common\T_Semaphore.pas',
  T_AgentThreadPool in 'T_AgentThreadPool.pas',
  T_PoolThread in 'T_PoolThread.pas',
  crc32 in '..\Common\crc32.pas',
  T_AgentGenericStreamWrapper in 'T_AgentGenericStreamWrapper.pas',
  T_GenericStreamRO in '..\Common\T_GenericStreamRO.pas',
  WbemScripting_TLB in '..\Common\WbemScripting_TLB.pas';

//  memcheck;

{$R *.res}

begin
//  memchk();

  IsMultiThread:=true;
  
  SetThreadName('Main thread');

  CreateMutex(nil,false,'hxgridagent');
  if GetLastError()=ERROR_ALREADY_EXISTS then exit;

  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
