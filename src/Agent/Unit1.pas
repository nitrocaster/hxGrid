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
  Dialogs, JvComponentBase, JvTrayIcon, T_Agent, StdCtrls, IdBaseComponent,
  IdComponent, IdTCPConnection, IdTCPClient, JvgXMLSerializer, SyncObjs,
  ExtCtrls, Menus, hxgridcommon, safefpu, OleServer, WbemScripting_TLB,
  ActiveX;

type
  TForm1 = class(TForm)
    JvTrayIcon1: TJvTrayIcon;
    Timer1: TTimer;
    GroupBox1: TGroupBox;
    Label2: TLabel;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    PopupMenu1: TPopupMenu;
    Exit1: TMenuItem;
    N1: TMenuItem;
    Status1: TMenuItem;
    Label6: TLabel;
    Label7: TLabel;
    SWbemLocator1: TSWbemLocator;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure Status1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
   shouldExit:boolean;

   Service:             ISWbemServices;

   procedure WMQueryEndSession(var Message: TWMQueryEndSession); message WM_QUERYENDSESSION;

  public
    { Public declarations }
   agent: TAgent;

   lpUserTime: int64;
   lpTimer100ns: int64;
   mainThreadId: DWORD;

   procedure UpdateCPUUsageWMI();
  end;

var
 Form1: TForm1;

implementation

{$R *.dfm}

//==============================================================================
//==============================================================================
procedure TForm1.FormCreate(Sender: TObject);
begin
 agent:=nil;
 mainThreadId:=GetCurrentThreadId();
 agent:=TAgent.Create(self);
 shouldExit:=false;
 caption:= 'hxgrid agent v.'+inttohex(CURRENT_AGENT_VERSION,3);
end;

//==============================================================================
//==============================================================================
procedure TForm1.FormDestroy(Sender: TObject);
begin
 agent.Destroy();
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
 Label4.Caption:=inttostr(agent.statusThread.CPUAvail)+'%';
 Label5.Caption:=inttostr(agent.statusThread.CPUUsageByAgent)+'%';
 Label3.caption:='Status: '+agent.GetStatus();
 Label7.caption:=inttostr(agent.settings.poolSize);

 jvtrayicon1.Hint:='Status: '+agent.GetStatus()+#13+'CPU Used By Agent: '+inttostr(agent.statusThread.CPUUsageByAgent)+'%';
end;

procedure TForm1.Exit1Click(Sender: TObject);
begin
 ShouldExit:=true;
 Close;
end;

procedure TForm1.Status1Click(Sender: TObject);
begin
 JvTrayIcon1.ShowApplication();
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
 if shouldExit=false then
  begin
   JvTrayIcon1.HideApplication();
   CanClose:=false;
  end
   else
  begin
   CanClose:=true;
  end;
end;


procedure TForm1.WMQueryEndSession(var Message: TWMQueryEndSession);
begin
 shouldExit:=true;
 inherited;
end;

//=================================================================
//=================================================================
procedure TForm1.UpdateCPUUsageWMI();
var
  ObjectSet:           ISWbemObjectSet;
  SObject:             ISWbemObject;
  PropSet:             ISWbemPropertySet;
  SProp:               ISWbemProperty;

  PropEnum, Enum:      IEnumVariant;
  TempObj:             OleVariant;
  Value:               Cardinal;
  StrValue :            string;
  s:string;
  b1,b2,b3: boolean;
begin
 lpUserTime:=0;

 if service=nil then
  begin
   Service:= SWbemLocator1.ConnectServer('.', 'ROOT\CIMV2', '', '', '', '', 0, nil);
  end;

 SObject:= Service.Get('Win32_PerfRawData_PerfOS_Processor', wbemFlagUseAmendedQualifiers, nil);

 ObjectSet:= SObject.Instances_(0, nil);

 Enum:= (ObjectSet._NewEnum) as IEnumVariant;

 TempObj:=null;
 while Enum.Next(1, TempObj, Value) = S_OK do
  begin

   SObject:= IUnknown(TempObj) as SWBemObject;

   PropSet := SObject.Properties_;

   PropEnum := (PropSet._NewEnum) as IEnumVariant;

   TempObj:=null;

   b1:=false;
   b2:=false;
   b3:=false;

   while (PropEnum.Next(1, TempObj, Value) = S_OK) do
    begin
     SProp:= IUnknown(TempObj) as SWBemProperty;

     if (SProp.Name='PercentProcessorTime') and
        (SProp.Get_Value <> null) then
         begin
          lpUserTime:=SProp.Get_Value;
          b1:=true;
          if b1 and b2 and b3 then exit;
         end;

     if (SProp.Name='Timestamp_Sys100NS') and
        (SProp.Get_Value <> null) then
         begin
          lpTimer100NS:=SProp.Get_Value;

          //REVIEW: Timestamp_Sys100NS does not chage by some reason!!!
          //use GetTickCount()
          lpTimer100NS:=int64(GetTickCount())*10000;

          b2:=true;
          if b1 and b2 and b3 then exit;
         end;

     if (SProp.Name='Name') and
        (SProp.Get_Value <> null) then
         begin
          if (SProp.Get_Value = '_Total') then b3:=true;
          if b1 and b2 and b3 then exit;
         end;

     TempObj:=null;
    end;
   TempObj:=null;
  end;

end;


begin
 decimalSeparator:='.';
end.

(*
todo:
убивать workerthread если windows shutdown, когда задача больше 3 сек
получение данных по запросу гет дата, калбек гет дата
*)

