unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, SyncObjs,
  ExtCtrls, gmpport, I_GridUser, I_GenericStream, ComCtrls;

const  
   WM_PROCESSSTREAM             = WM_USER+1000;

type
 TState = (ST_IDLE, ST_WORKING, ST_CANCELING);

type
  TForm1 = class(TForm)
    Button2: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Image1: TImage;
    Edit4: TEdit;
    Image2: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    ProgressBar1: TProgressBar;
    Timer1: TTimer;
    Label6: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Image2MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Timer1Timer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }

    cs    : TCriticalSection;
    list  : TList;
    state : TState;

    procedure Render();
    procedure ProcessStream(var outstream: IGenericStream);
    procedure OnProcessStream(var Message: TMessage); message WM_PROCESSSTREAM;
    procedure UpdateConnectionStatus(var user:IGridUser);
    
    
  public
    { Public declarations }

   class procedure DebugWrite(const Format: string; const Args: array of const);
  end;

var
 Form1 : TForm1;

implementation
uses T_GenericStream, hxgridcommon;

{$R *.dfm}


//==============================================================================
//==============================================================================
procedure TForm1.FormCreate(Sender: TObject);
begin
 cs:=TCriticalSection.Create();
 list:=TList.Create();
 DoubleBuffered:=true;
// image2.Picture.Bitmap:=TBitmap.Create();
 image2.Picture.Bitmap.width:=202;
 image2.Picture.Bitmap.height:=202;
 image2.Picture.Bitmap.pixelformat:=pf24Bit;

 state:=ST_IDLE;
end;


//==============================================================================
//procedure TForm1.DebugWrite();
//==============================================================================
class procedure TForm1.DebugWrite(const Format: string; const Args: array of const);
begin
 try
  if (length(args)=0) then OutputDebugString(pchar(format+#13))
                      else OutputDebugString(pchar(sysutils.Format(format, args)+#13));
 except
  MessageBeep(0);
  OutputDebugString(pchar('DEBUGWRITE: Unable to format string'+#13));
 end;
end;

//==============================================================================
//==============================================================================
procedure Finalize(outStream: IGenericStream); cdecl;
begin
 form1.DebugWrite('TestFinalize: outstream.size=%d',[outstream.GetLength()]);
 form1.DebugWrite('TestFinalize: outstream.position=%d',[outstream.GetPos()]);

 //VCL objects can not be accessed from second thread
 //add stream to queue and process it in main thread

 form1.cs.Enter();

 outStream._Addref(); //cast reference to keep stream alive
 form1.list.add(pointer(outStream));

 form1.cs.Leave();

 OutputDebugString(pchar('Finalize has been executed OK'));

 PostMessage(form1.handle, WM_PROCESSSTREAM, 0, 0);
end;


//==============================================================================
//==============================================================================
procedure TForm1.ProcessStream(var outstream: IGenericStream);
var
 n: DWORD;
 s,z: integer;
 zaehler:byte;
 tag: BYTE;
begin
 outstream.read(tag,1);

 while(tag=1) do
  begin
   outstream.read(s,4);

   for z:=1 to 200 do
    begin
     outstream.read(zaehler,1);
     form1.image2.Picture.Bitmap.canvas.Pixels[s,z]:=form1.image1.picture.bitmap.canvas.pixels[zaehler,0];
    end;

   outstream.read(tag,1);
  end;

end;


//==============================================================================
//==============================================================================
procedure TForm1.OnProcessStream(var Message: TMessage);
var
 stream : IGenericStream;
 b: boolean;
 d: DWORD;
begin
 b:=true;

 while (b) do
  begin
   cs.Enter();

   if list.count>0 then
    begin
     pointer(stream):=list.items[0];
     list.delete(0);
    end
     else
    begin
     b:=false;
    end;

   cs.Leave();

   if (Stream<>nil) then
    begin
     ProcessStream(stream);

     d:=stream._addref();
     d:=stream._Release();

//     assert(d=1);
     
     stream:=nil; //release
     progressbar1.Position:=progressbar1.Position+1;
    end;

  end;

end;

//==============================================================================
//==============================================================================
procedure TForm1.Button2Click(Sender: TObject);
begin
 if (state=ST_IDLE) then
  begin
   Render();
  end
   else
 if (state=ST_WORKING) then
  begin
   state:=ST_CANCELING;
   button2.enabled:=false;
  end;  
end;

//==============================================================================
//==============================================================================
procedure TForm1.UpdateConnectionStatus(var user:IGridUser);
var
 status: TGridUserConnectionStatus;
begin
 user.GetConnectionStatus(status); 

 if (status.bConnectedToCoordinator) then
  label6.Caption:='Connected to coordiantor, agents count = '+inttostr(status.connectedAgentsCount)
                                     else 
  label6.Caption:='Searching for coordinator...';
end; 

//==============================================================================
//==============================================================================
procedure TForm1.Render();
const
 MIN_LINES_TO_SEND = 5;
var
 stream: IGenericStream;
 d: dword;
 user: IGridUser;
 i: integer;
 amin,bmin,kante,ds: mpf_t;
 a,b,x,y,xx,yy : mpf_t;
 s,z,zaehler :integer;
 tmp1, tmp2, tmp3: mpf_t;
 maxIterations: DWORD;
 count: DWORD;
 tag: BYTE;
 settings: TGridUserSettings;
 bl: boolean;
begin

 state:=ST_WORKING;
 label5.visible:=true;
 Button2.caption:='Cancel';
 ProgressBar1.Visible:=true;
 ProgressBar1.position:=0;

 IGridUser_Create(user);
 d:=user._AddRef();
 d:=user._Release();
 assert(d=1);

 MPF_set_default_prec(256);

 tmp1:=mpf_create();
 tmp2:=mpf_create();
 tmp3:=mpf_create();

 kante:=mpf_create();
 amin:=mpf_create();
 bmin:=mpf_create();

 ds:=mpf_create();

 a:=mpf_create();
 b:=mpf_create();
 x:=mpf_create();
 y:=mpf_create();
 xx:=mpf_create();
 yy:=mpf_create();

 image2.Picture.Bitmap.canvas.brush.color:=clWhite;
 image2.Picture.Bitmap.canvas.pen.color:=clBlack;
 image2.Picture.Bitmap.canvas.rectangle(0,0,202,202);

 maxIterations:=strToInt(Edit4.text);

 mpf_set_str(tmp1,pchar(Edit3.text));
 mpf_ui_div(kante,1,tmp1);


 mpf_set_str(tmp1,pchar(Edit1.text));
 mpf_div_ui(tmp2,kante,2);
 mpf_sub(amin, tmp1, tmp2);

 mpf_set_str(tmp1,pchar(Edit2.text));
 mpf_div_ui(tmp2,kante,2);
 mpf_sub(bmin, tmp1, tmp2);

 mpf_div_ui(ds,kante,200);

 mpf_set(a,amin);

 pointer(stream):=pointer(IGenericStream(TGenericStream.Create()));
 //stream now points to IGenericStream with refcount 1:
 //- TGenericStream.Create() returns TGenericStream with refcount 1
 //- IGenericStream(TGenericStream.Create()) casts TGenericStream to IGenericStream
 // - pointers(stream) := pointer (...) assigns iface ptr without addref
 //This is equivalent to:
 // stream:=TGenericStream.Create(); // addref, refocunt=2
 // stream._Release(); 

 count:=0;

 for s:=1 to 200 do
  begin
   mpf_set(b,bmin);

   tag:=1;
   stream.Write(tag,1);

   stream.write(s,4);
   stream.write(maxiterations,4);
   StreamWriteString(stream,mpf_get_string(a));
   StreamWriteString(stream,mpf_get_string(bmin));
   StreamWriteString(stream,mpf_get_string(ds));

   inc (count);

   if (count=MIN_LINES_TO_SEND) or (s=200) then
    begin
     tag:=0;
     stream.Write(tag,1);

     d:=stream._addref();
     d:=stream._release(); 
     assert(d=1);

     while (user.RunTask('GridGMP_task.dll,GMPPort.dll','RunTask',stream,Finalize,d,false)<>S_OK) do
      begin
       Application.ProcessMessages();
       user.WaitForCompletionEvent(100);

       if state=ST_CANCELING then 
        begin
         break;
        end; 

       UpdateConnectionStatus(user); 
      end;

     pointer(stream):=nil; //stream отдан в эксклюзивное пользование, не пытаться освобождать

     if state=ST_CANCELING then break; 

     count:=0;
     if (s<200) then pointer(stream):=pointer(IGenericStream(TGenericStream.Create()));
    end;

   mpf_add(tmp1,a,ds);
   mpf_set(a,tmp1);

  end;

 assert(stream=nil); 

 mpf_delete(tmp1);
 mpf_delete(tmp2);
 mpf_delete(tmp3);

 mpf_delete(kante);
 mpf_delete(amin);
 mpf_delete(bmin);

 mpf_delete(ds);

 mpf_delete(a);
 mpf_delete(b);
 mpf_delete(x);
 mpf_delete(y);
 mpf_delete(xx);
 mpf_delete(yy);

//===========================

 bl:=false;
 while (bl=false) do
  begin
   Application.ProcessMessages();
   Sleep(100);

   if (state=ST_CANCELING) then
    begin
     user.CancelTasks();
     break;
    end;

   UpdateConnectionStatus(user); 
   
   user.IsComplete(bl);
  end;

 DebugWrite('UNIT1: All completed',[]);

 image2.Invalidate;
 user:=nil; //Release()

 label5.visible:=false;

 ProgressBar1.Visible:=false;

 Button2.caption:='Render';
 Button2.enabled:=true;
 state:=ST_IDLE;
 label6.Caption:='';
end;

//==============================================================================
//==============================================================================
procedure TForm1.Image2MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
 tmp1,tmp2,tmp3,amin,bmin,kante,ds: mpf_t;

begin
 tmp1:=mpf_create();
 tmp2:=mpf_create();
 tmp3:=mpf_create();
 kante:=mpf_create();
 amin:=mpf_create();
 bmin:=mpf_create();

// kante:=1/strtofloat(Edit3.text);
 mpf_set_str(tmp1,pchar(Edit3.text));
 mpf_ui_div(kante,1,tmp1);

// amin:=strtofloat(Edit1.text)-kante/2;
 mpf_set_str(tmp1,pchar(Edit1.text));
 mpf_div_ui(tmp2,kante,2);
 mpf_sub(amin, tmp1, tmp2);

// bmin:=strtofloat(Edit2.text)-kante/2;
 mpf_set_str(tmp1,pchar(Edit2.text));
 mpf_div_ui(tmp2,kante,2);
 mpf_sub(bmin, tmp1, tmp2);

// Edit1.text:=floattostr(strtofloat(Edit1.text)+kante/200*(x-1-100));
 mpf_set_str(tmp1,pchar(floattostr(1/200*(x-1-100))));
 mpf_mul(tmp2, tmp1, kante);
 mpf_set_str(tmp1,pchar(Edit1.text));
 mpf_add(tmp3, tmp1, tmp2);
 Edit1.Text:=mpf_get_string(tmp3);


// Edit2.text:=floattostr(strtofloat(Edit2.text)+kante/200*(y-1-100));
 mpf_set_str(tmp1,pchar(floattostr(1/200*(y-1-100))));
 mpf_mul(tmp2, tmp1, kante);
 mpf_set_str(tmp1,pchar(Edit2.text));
 mpf_add(tmp3, tmp1, tmp2);
 Edit2.Text:=mpf_get_string(tmp3);

// Edit3.text:=floattostr(strtofloat(Edit3.text)*2);
 mpf_set_str(tmp1,pchar(Edit3.text));
 mpf_mul_ui(tmp2,tmp1,2);
 Edit3.Text:=mpf_get_string(tmp2);

 Render();

 mpf_delete(tmp1);
 mpf_delete(tmp2);
 mpf_delete(tmp3);
 mpf_delete(kante);
 mpf_delete(amin);
 mpf_delete(bmin);
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
 CanClose:= state = ST_IDLE;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
 //uncomment below for automatic testing

{
 timer1.interval:=1000+random(3000);

 if button2.enabled then Button2Click(self);
}
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
 list.destroy;
 cs.Destroy();
end;


begin
 decimalseparator:='.';
end.
