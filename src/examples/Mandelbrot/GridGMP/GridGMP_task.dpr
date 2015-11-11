library GridGMP_task;

uses
  Windows,
  SysUtils,
  Classes,
  GMPPort in '..\..\..\..\GMPDelphiPort\GMPPort.pas',
  T_GenericStream in '..\..\..\Common\T_GenericStream.pas',
  I_GenericStream in '..\..\..\Common\I_GenericStream.pas',
  I_Agent in '..\..\..\Interface\I_Agent.pas',
  hxgridcommon in '..\..\..\Common\hxgridcommon.pas',
  hxzlib in '..\..\..\Common\hxzlib.pas';

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
 a_s,bmin_s,ds_s: string;
 amin,bmin,ds: mpf_t;
 a,b,x,y,xx,yy : mpf_t;
 s,z,zaehler :integer;
 tmp1, tmp2, tmp3: mpf_t;

 maxIterations: DWORD;

 tag: byte;
 tag2: byte;

 label l1;

 function rtest(x,y, tmp1, tmp2, tmp3: mpf_t): boolean;
 begin
  mpf_mul(tmp1,x,x);
  mpf_mul(tmp2,y,y);
  mpf_add(tmp3,tmp1,tmp2);
  result:=mpf_cmp_d(tmp3,4)<0;
 end;


begin
 result:=true;

 instream.read(tag,1);

 while (tag=1) do
  begin
   instream.read(s,4);
   instream.read(maxiterations,4);

   a_s:=StreamReadString(instream);
   bmin_s:=StreamReadString(instream);
   ds_s:=StreamReadString(instream);

   tmp1:=mpf_create();
   tmp2:=mpf_create();
   tmp3:=mpf_create();

   amin:=mpf_create();
   bmin:=mpf_create();

   ds:=mpf_create();

   a:=mpf_create();
   b:=mpf_create();
   x:=mpf_create();
   y:=mpf_create();
   xx:=mpf_create();
   yy:=mpf_create();

   mpf_set_str(a,pchar(a_s));
   mpf_set_str(bmin,pchar(bmin_s));
   mpf_set_str(ds,pchar(ds_s));


   mpf_set(b,bmin);

   tag2:=1;
   outstream.write(tag2,1);

   outStream.Write(s,4);

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

         if (agent.TestConnection(sessionId)<>S_OK) then
          begin
           result:=false;
           goto l1;
          end;
        end;

       zaehler:=zaehler mod 256;
       outStream.Write(zaehler,1);

  //  image2.Picture.Bitmap.canvas.Pixels[s,z]:=image1.picture.bitmap.canvas.pixels[,0];
  //        b:=b+ds;
       mpf_add(tmp1,b,ds);
       mpf_set(b,tmp1);
      end;

   l1:

   mpf_delete(tmp1);
   mpf_delete(tmp2);
   mpf_delete(tmp3);

   mpf_delete(amin);
   mpf_delete(bmin);

   mpf_delete(ds);

   mpf_delete(a);
   mpf_delete(b);
   mpf_delete(x);
   mpf_delete(y);
   mpf_delete(xx);
   mpf_delete(yy);

   instream.read(tag,1);
  end;

 tag2:=0;
 outstream.write(tag2,1);
end;



exports
  RunTask name 'RunTask';


begin
end.
