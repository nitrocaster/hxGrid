//==============================================================================
// hxGrid framework
// Copyright (C) 2007 by Roman Lut
// hax@deep-shadows.com
// http://www.deep-shadows.com/hax/
//==============================================================================

library hxGridUserDLL;

uses
  SysUtils,
  Classes,
  T_UserLobbySocketThread in 'T_UserLobbySocketThread.pas',
  T_GridUser in 'T_GridUser.pas',
  T_GridUserLobby in 'T_GridUserLobby.pas',
  hxgridcommon in '..\Common\hxgridcommon.pas',
  T_GenericStream in '..\Common\T_GenericStream.pas',
  I_GridUser in '..\Interface\I_GridUser.pas',
  T_AgentBoss in 'T_AgentBoss.pas',
  windows,
  SafeFPU in '..\..\..\VE_SDK3a\SHARED\safefpu.pas',
  hxzlib in '..\Common\hxzlib.pas',
  I_Agent in '..\Interface\I_Agent.pas',
  I_GenericStream in '..\Interface\I_GenericStream.pas',
  hxScktComp in '..\Common\hxScktComp.pas';

//  memcheck;

{$R *.res}

//==========================================================================
//==========================================================================
function CreateGridUserObject(version: DWORD): pointer; cdecl;
begin
 result:=nil;

 if (version = IGRIDUSER_VERSION) then result := pointer(IGridUser(TGridUser.Create()));
end;

//==========================================================================
//==========================================================================
function CreateGenericStream(): pointer; cdecl;
begin
 result:=pointer(IGenericStream(TGenericStream.Create()));
end;

var
p: pointer;

exports
 CreateGridUserObject index 1,
 CreateGenericStream index 2;

begin
// memchk();
// getmem(p,100);
 decimalseparator:='.';
end.
