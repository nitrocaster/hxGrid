//==============================================================================
// hxGrid framework
// Copyright (C) 2007 by Roman Lut
// hax@deep-shadows.com
// http://www.deep-shadows.com/hax/
//==============================================================================

unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, JvComponentBase, JvTrayIcon, Grids, ExtCtrls, StdCtrls,
  T_Coordinator;

type
  TForm1 = class(TForm)
    StringGrid1: TStringGrid;
    JvTrayIcon1: TJvTrayIcon;
    Timer2: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private

   coordinator : TCoordinator;

    { Private declarations }
   procedure UpdateList();


  public
    { Public declarations }

  end;

var
  Form1: TForm1;

implementation
uses HTTPApp, hxgridcommon;

{$R *.dfm}

//==============================================================================
//==============================================================================
procedure TForm1.FormCreate(Sender: TObject);
begin
 coordinator:=TCoordinator.Create();

 StringGrid1.ColWidths[2]:=80;
 StringGrid1.ColWidths[3]:=80;
 StringGrid1.ColWidths[4]:=80;
 StringGrid1.ColWidths[5]:=80;

 StringGrid1.ColWidths[6]:=StringGrid1.Width*3;

 Timer2.Enabled:=true;
end;

//==============================================================================
//==============================================================================
procedure TForm1.FormDestroy(Sender: TObject);
begin
 Timer2.Enabled:=false;

 coordinator.Destroy();
end;


//==============================================================================
//==============================================================================
procedure TForm1.Timer2Timer(Sender: TObject);
begin
 if Visible then UpdateList();
end;

//==============================================================================
//==============================================================================
procedure TForm1.FormShow(Sender: TObject);
begin
 UpdateList();
end;

//==============================================================================
// procedure TForm1.UpdateList();
//==============================================================================
procedure TForm1.UpdateList();
var
 i: integer;
 agentsList: TList;
 mem: DWORD;
 cpu1, cpu2, cpu3: single;
begin
 agentsList:=Coordinator.GetAgentsList(0);

 StringGrid1.RowCount:=agentsList.Count+3;
 StringGrid1.FixedRows:=1;

 StringGrid1.Cells[0,0]:='ADDRESS';
 StringGrid1.Cells[1,0]:='NAME';
 StringGrid1.Cells[2,0]:='CPU POWER';
 StringGrid1.Cells[3,0]:='CPU AVAIL';
 StringGrid1.Cells[4,0]:='USED BY GRID';
 StringGrid1.Cells[5,0]:='RAM AVAIL';
 StringGrid1.Cells[6,0]:='STATUS';

 mem:=0;
 cpu1:=0;
 cpu2:=0;
 cpu3:=0;

 for i:=0 to agentsList.Count-1 do
  with PAgentStatus(agentsList[i])^ do
   begin
    StringGrid1.Cells[0,i+1]:=IP+':'+inttostr(port);
    StringGrid1.Cells[1,i+1]:=name;
    if (CPUCount>1) then StringGrid1.Cells[2,i+1]:=inttostr(CPUCount)+'x'+formatfloat('0.000MHz',CPUSpeed)+' ('+inttostr(threadPoolSize)+')'
                    else StringGrid1.Cells[2,i+1]:=formatfloat('0.000MHz',CPUSpeed)+' ('+inttostr(threadPoolSize)+')';
    StringGrid1.Cells[3,i+1]:=inttostr(CPUAvail)+'%';
    StringGrid1.Cells[4,i+1]:=inttostr(CPUUsed)+'%';
    StringGrid1.Cells[5,i+1]:=inttostr(int64(physRAMAvail))+' MB';
    StringGrid1.Cells[6,i+1]:=status +
                              ' (v'+
                              inttostr(version shr 8)+'.'+
                              inttostr(version and $f0 shr 4)+
                              inttostr(version and $f)+
                              ')';

    mem:=mem+physRAMAvail;
    cpu1:=cpu1+CPUSpeed*CPUCount;
    cpu2:=cpu2+CPUSpeed*CPUCount*CPUAvail/100;
    cpu3:=cpu3+CPUSpeed*CPUCount*CPUUsed/100;
   end;

 for i:=0 to agentsList.Count-1 do
  Dispose(PAgentStatus(agentsList[i]));

 StringGrid1.Cells[0,agentsList.Count+1]:='';
 StringGrid1.Cells[1,agentsList.Count+1]:='';
 StringGrid1.Cells[2,agentsList.Count+1]:='';
 StringGrid1.Cells[3,agentsList.Count+1]:='';
 StringGrid1.Cells[4,agentsList.Count+1]:='';
 StringGrid1.Cells[5,agentsList.Count+1]:='';
 StringGrid1.Cells[6,agentsList.Count+1]:='';

 StringGrid1.Cells[0,agentsList.Count+2]:='TOTAL';
 StringGrid1.Cells[1,agentsList.Count+2]:='';
 StringGrid1.Cells[2,agentsList.Count+2]:=formatfloat('0.000MHz',cpu1);
 StringGrid1.Cells[3,agentsList.Count+2]:=formatfloat('0.000MHz',cpu2);
 StringGrid1.Cells[4,agentsList.Count+2]:=formatfloat('0.000MHz',cpu3);
 StringGrid1.Cells[5,agentsList.Count+2]:=inttostr(mem)+' MB';
 StringGrid1.Cells[6,agentsList.Count+2]:='';

 agentsList.Destroy();
end;

procedure TForm1.FormResize(Sender: TObject);
begin
 UpdateList();
end;

begin
 decimalSeparator:='.';
end.
