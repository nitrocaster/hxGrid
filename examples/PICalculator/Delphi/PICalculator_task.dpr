library PICalculator_task;

uses
  Windows,
  SysUtils,
  Classes,
  I_Agent in '..\..\..\Interface\I_Agent.pas',
  I_GridUser in '..\..\..\Interface\I_GridUser.pas',
  PICalculator in 'PICalculator.pas',
  T_GenericStream in '..\..\..\Common\T_GenericStream.pas',
  hxgridcommon in '..\..\..\Common\hxgridcommon.pas',
  hxzlib in '..\..\..\Common\hxzlib.pas',
  I_GenericStream in '..\..\..\Interface\I_GenericStream.pas',
  hxScktComp in '..\..\..\Common\hxScktComp.pas';

{$R *.res}


//==============================================================================
// function RunTask(): boolean;
//==============================================================================
function RunTask(agent: IAgent;
                 sessionId: DWORD;
                 inStream: IGenericStream;
                 outStream: IGenericStream): boolean; cdecl;
var
 n: DWORD;
 s :string;
begin
 instream.read(n,4);
 s:=CalculatePiDigits(n);
 outStream.Write(n,4);
 outStream.Write(s[1],9);

 result:=true;
end;


exports
  RunTask name 'RunTask';


begin
end.
