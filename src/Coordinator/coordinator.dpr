//==============================================================================
// hxGrid framework
// Copyright (C) 2007 by Roman Lut
// hax@deep-shadows.com
// http://www.deep-shadows.com/hax/
//==============================================================================

program coordinator;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  hxgridcommon in '..\Common\hxgridcommon.pas',
  T_Coordinator in 'T_Coordinator.pas',
  T_CoordinatorSocketThread in 'T_CoordinatorSocketThread.pas',
  T_GenericStream in '..\Common\T_GenericStream.pas',
  SafeFPU in '..\..\..\VE_SDK3a\SHARED\safefpu.pas',
  hxzlib in '..\Common\hxzlib.pas',
  windows,
  I_GenericStream in '..\Interface\I_GenericStream.pas';

{$R *.res}

begin
  CreateMutex(nil,false,'hxgridcoordinator');
  if GetLastError()=ERROR_ALREADY_EXISTS then exit;

  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
