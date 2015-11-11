unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,gmpport, StdCtrls, ExtCtrls;

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
 amin,bmin,kante,ds: mpf_t;
 a,b,x,y,xx,yy : mpf_t;
 s,z,zaehler :integer;
 tmp1, tmp2, tmp3: mpf_t;
 maxIterations: DWORD;

 function rtest(x,y, tmp1, tmp2, tmp3: mpf_t): boolean;
 begin
  mpf_mul(tmp1,x,x);
  mpf_mul(tmp2,y,y);
  mpf_add(tmp3,tmp1,tmp2);
  result:=mpf_cmp_d(tmp3,4)<0;
 end;

begin
 enabled:=false;

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

//    kante:=1/strtofloat(Edit3.text);
 mpf_set_str(tmp1,pchar(Edit3.text));
 mpf_ui_div(kante,1,tmp1);


//    amin:=strtofloat(Edit1.text)-kante/2;
 mpf_set_str(tmp1,pchar(Edit1.text));
 mpf_div_ui(tmp2,kante,2);
 mpf_sub(amin, tmp1, tmp2);

//    bmin:=strtofloat(Edit2.text)-kante/2;
 mpf_set_str(tmp1,pchar(Edit2.text));
 mpf_div_ui(tmp2,kante,2);
 mpf_sub(bmin, tmp1, tmp2);

//    ds:=kante/200;
 mpf_div_ui(ds,kante,200);

//    a:=amin;
 mpf_set(a,amin);

 for s:=1 to 200 do
  begin
//      b:=bmin;
   mpf_set(b,bmin);

   for z:=1 to 200 do
    begin
//        x:=0;
     mpf_set_d(x,0);

//        y:=0;
     mpf_set_d(y,0);

     zaehler:=0;
//        while (zaehler<3255) and (x*x+y*y<4) do
     while (zaehler<maxIterations) and (rtest(x,y,tmp1,tmp2,tmp3)) do
      begin
       inc(zaehler);
//          xx:=x*x-y*y+a;
       mpf_mul(tmp1,x,x);
       mpf_mul(tmp2,y,y);
       mpf_sub(tmp3,tmp1,tmp2);
       mpf_add(xx,tmp3,a);

//          y:=2*x*y+b;
       mpf_mul_ui(tmp1,x,2);
       mpf_mul(tmp2,tmp1,y);
       mpf_add(y,tmp2,b);

//          x:=xx;
       mpf_set(x,xx);
      end;

     image2.Picture.Bitmap.canvas.Pixels[s,z]:=image1.picture.bitmap.canvas.pixels[zaehler mod 256,0];
//        b:=b+ds;
     mpf_add(tmp1,b,ds);
     mpf_set(b,tmp1);
    end;
//      a:=a+ds;
   mpf_add(tmp1,a,ds);
   mpf_set(a,tmp1);

   image2.Invalidate;
   Application.ProcessMessages();
  end;

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

 enabled:=true;
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

//==============================================================================
//==============================================================================
procedure TForm1.OnDraw(var Message: TMessage);
begin
 Render();
end;

begin
 decimalseparator:='.';
end.
