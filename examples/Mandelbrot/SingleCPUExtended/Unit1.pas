unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,StdCtrls, ExtCtrls;

  const
   WM_DRAW                = WM_USER+1000;

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
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Image2MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }

   procedure Render();
   procedure OnDraw(var Message: TMessage); message WM_DRAW;

  public
    { Public declarations }

  end;


var
 Form1: TForm1;

implementation

{$R *.dfm}

//==============================================================================
//==============================================================================
procedure TForm1.FormCreate(Sender: TObject);
begin
 image2.Picture.Bitmap:=TBitmap.Create();
 image2.Picture.Bitmap.width:=202;
 image2.Picture.Bitmap.height:=202;
 image2.Picture.Bitmap.pixelformat:=pf24Bit;

 postmessage(form1.handle,WM_DRAW,0,0);
end;

//==============================================================================
//==============================================================================
procedure TForm1.Button2Click(Sender: TObject);
begin
 Render();
end;


//==============================================================================
//==============================================================================
procedure TForm1.Render();
var
 amin,bmin,kante,ds: extended;
 a,b,x,y,xx,yy : extended;
 s,z,zaehler :integer;
 maxIterations: DWORD;

begin
 enabled:=false;

 image2.Picture.Bitmap.canvas.brush.color:=clWhite;
 image2.Picture.Bitmap.canvas.pen.color:=clBlack;
 image2.Picture.Bitmap.canvas.rectangle(0,0,202,202);

 maxIterations:=strToInt(Edit4.text);

 kante:=1/strtofloat(Edit3.text);

 amin:=strtofloat(Edit1.text)-kante/2;

 bmin:=strtofloat(Edit2.text)-kante/2;

 ds:=kante/200;

 a:=amin;

 for s:=1 to 200 do
  begin
   b:=bmin;

   for z:=1 to 200 do
    begin
     x:=0;
     y:=0;

     zaehler:=0;
     while (zaehler<3255) and (x*x+y*y<4) do
      begin
       inc(zaehler);
       xx:=x*x-y*y+a;
       y:=2*x*y+b;
       x:=xx;
      end;

     image2.Picture.Bitmap.canvas.Pixels[s,z]:=image1.picture.bitmap.canvas.pixels[zaehler mod 256,0];
     b:=b+ds;
    end;
   a:=a+ds;
   image2.Invalidate;
   Application.ProcessMessages();
  end;

 enabled:=true;
end;


//==============================================================================
//==============================================================================
procedure TForm1.Image2MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
 amin,bmin,kante,ds: extended;

begin
 kante:=1/strtofloat(Edit3.text);

 amin:=strtofloat(Edit1.text)-kante/2;

 bmin:=strtofloat(Edit2.text)-kante/2;

 Edit1.text:=floattostr(strtofloat(Edit1.text)+kante/200*(x-1-100));

 Edit2.text:=floattostr(strtofloat(Edit2.text)+kante/200*(y-1-100));

 Edit3.text:=floattostr(strtofloat(Edit3.text)*2);

 Render();
end;

//==============================================================================
//==============================================================================
procedure TForm1.OnDraw(var Message: TMessage);
begin
 Render();
end;

begin
 decimalseparator:='.';
end.
