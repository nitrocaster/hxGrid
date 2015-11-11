program GridGMP;

uses
  Forms,
  GMPPort in '..\..\..\..\GMPDelphiPort\GMPPort.pas',
  T_GenericStream in '..\..\..\Common\T_GenericStream.pas',
  I_GridUser in '..\..\..\Interface\I_GridUser.pas',
  I_Agent in '..\..\..\Interface\I_Agent.pas',
  hxzlib in '..\..\..\Common\hxzlib.pas',
  Unit1 in 'Unit1.pas' {Form1},
  I_GenericStream in '..\..\..\Interface\I_GenericStream.pas',
  hxgridcommon in '..\..\..\Common\hxgridcommon.pas';
//  memcheck;

{$R *.res}

begin
// memchk();
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
