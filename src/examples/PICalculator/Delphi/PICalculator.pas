unit PICalculator;

interface

function CalculatePiDigits(n:integer): string;

implementation
uses windows, sysutils;

function mul_mod(a, b, m: integer): integer;
begin
 result:=(DWORD(a) * DWORD(b)) mod m;
end;

function inv_mod(x,y: integer): integer;
var
 q,u,v,a,c,t : integer;
begin

 u:=x;
 v:=y;
 c:=1;
 a:=0;

 repeat
  q:=v div u;

  t:=c;
  c:=a-q*c;
  a:=t;

  t:=u;
  u:=v-q*u;
  v:=t;
 until (u=0);

 a:=a mod y;

 if (a<0) then a:=y+a;

 result:=a;
end;

function pow_mod(a,b,m: integer) : integer;
var
 r, aa : integer;
begin

 r:=1;
 aa:=a;

 while (true) do
  begin
   if ((b and 1) <> 0) then
    begin
     r := mul_mod(r, aa, m);
    end;

   b := b shr 1;

   if (b = 0) then break;

   aa := mul_mod(aa, aa, m);
  end;

 result:=r;
end;

///* return true if n is prime */
function is_prime(n: integer): boolean;
var
 i,r: integer;
begin
 if ((n mod 2) = 0) then
  begin
   result:=false;
   exit;
  end;

 r := round(int(Sqrt(n)));

 i:=3;
 while(i <= r) do
  begin
   if ((n mod i) = 0) then
    begin
     result:=false;
     exit;
    end;
   i := i+2;
  end;

 result:=true;
end;

///* return the prime number immediatly after n */
function next_prime(n : integer): integer;
begin
 repeat
  inc(n);
 until (is_prime(n)=true);

 result:=n;
end;

//start from digit n, return 9 digits
function CalculatePiDigits(n:integer): string;
var
 av, vmax, num, den, s, t :integer;
 nb: integer;
 sum: double;
 a: integer;
 i: integer;
 k,v,kq,kq2: integer;
begin
 nb := round(int( ((n + 20) * Ln(10) / Ln(2)) ));

 sum := 0;

 a:=3;

 while (a <= (2 * NB)) do
  begin
   vmax := round(int( (Ln(2 * NB) / Ln(a)) ));

   av := 1;

   for i:= 0 to vmax-1 do
    begin
     av := av * a;
    end;

   s := 0;
   num := 1;
   den := 1;
   v := 0;
   kq := 1;
   kq2 := 1;

   for k := 1 to NB do
    begin
     t := k;

     if (kq >= a) then
      begin
       repeat
        t := t div a;
        dec(v);
       until ((t mod a) <> 0);

       kq := 0;
      end;

     inc(kq);
     num := mul_mod(num, t, av);

     t := 2 * k - 1;

     if (kq2 >= a) then
      begin
       if (kq2 = a) then
        begin
         repeat
          t := t div a;
          inc(v);
         until ((t mod a) <> 0);
        end;
       kq2 := kq2-a;
      end;

     den := mul_mod(den, t, av);
     kq2 := kq2 + 2;

     if (v > 0) then
      begin
       t := inv_mod(den, av);
       t := mul_mod(t, num, av);
       t := mul_mod(t, k, av);

       for i:=v to vmax-1 do
        begin
         t := mul_mod(t, a, av);
        end;

       s := s + t;

       if (s >= av) then
        begin
         s := s-av;
        end;
      end;

    end;

   t := pow_mod(10, n - 1, av);
   s := mul_mod(s, t, av);
   sum := frac((sum + s/av));

   a := next_prime(a);
  end;

  Result := inttostr(round(int( (sum * 1e9))));

  while (length(result)<9) do result:='0'+result;
end;

begin
end.
