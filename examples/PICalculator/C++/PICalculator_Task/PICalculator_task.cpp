
#include "windows.h"
#include "math.h"
#include <string>
#include "IAgent.h"
#include "IGenericStream.h"

//============================================================
//============================================================
int mul_mod(int a, int b, int m)
{
 return (int)(((__int64)a) * (__int64)(b)) % m;
}

//============================================================
//============================================================
int inv_mod(int x, int y)
{                                      
 int q,u,v,a,c,t;

 u=x;
 v=y;
 c=1;
 a=0;

 do
 {
  q=v/u;

  t=c;
  c=a-q*c;
  a=t;

  t=u;
  u=v-q*u;
  v=t;
 } 
 while (u!=0);

 a=a % y;

 if (a<0) a=y+a;

 return a;
}

//============================================================
//============================================================
int pow_mod(int a, int b, int m)
{
int r, aa;

 r=1;
 aa=a;

 while (true)
  {
   if ((b & 1) != 0) 
    {
     r = mul_mod(r, aa, m);
    }

   b = b >> 1;

   if (b == 0) break;

   aa = mul_mod(aa, aa, m);
  }

 return r;
}

//============================================================
//============================================================
///* return true if n is prime */
bool is_prime(int n)
{
 int r;

 if ((n % 2) == 0) 
  {
   return false;
  }

 r = (int)sqrt((float)n);

 for (int i = 3; i <= r; i += 2)
  {
   if ((n % i) == 0) 
    {
     return false;
    }
  }

 return true;
}

//============================================================
//============================================================
///* return the prime number immediatly after n */
int next_prime(int n)
{
 do
 { 
  n++;
 } while (is_prime(n)==false);

 return n;
}

//============================================================
//============================================================
double frac(double d)
{
 if (d>0)
  {
   return d-int(d);
  } 
   else
  {
   return d+floor(d);
  } 
}

//============================================================
//============================================================
//start from digit n, return 9 digits
void CalculatePiDigits(int n, char* outs)
{
 int av, vmax, num, den, s, t;
 int nb;
 double sum;
 int a;
 int i;
 int k,v,kq,kq2;

 nb = (int) ((n + 20) * log(10.0f) / log(2.0f)) ;

 sum = 0;

 a=3;
 
 for (a = 3; a <= (2 * nb); a = next_prime(a)) 
  {
   vmax = (int) ((log((float)(2 * nb)) / log((float)a)));

   av = 1;

   for (i= 0; i<vmax; i++)
    {
     av = av * a;
    }

   s = 0;
   num = 1;
   den = 1;
   v = 0;
   kq = 1;
   kq2 = 1;

   for (k=1; k<=nb; k++)
    {
     t = k;

     if (kq >= a) 
      {
       do
       {
        t = t/a;
        v--;
       } 
       while ((t % a) == 0);

       kq = 0;
      }

     kq++;
     num = mul_mod(num, t, av);

     t = 2 * k - 1;

     if (kq2 >= a)
      {
       if (kq2 == a) 
        {
         do
         {
          t = t/a;
          v++;
         } 
         while ((t % a) == 0);
        }
       kq2 = kq2-a;
      }

     den = mul_mod(den, t, av);
     kq2 = kq2 + 2;

     if (v > 0) 
      {
       t = inv_mod(den, av);
       t = mul_mod(t, num, av);
       t = mul_mod(t, k, av);

       for (i=v; i<vmax; i++)
        {
         t = mul_mod(t, a, av);
        }

       s = s + t;

       if (s >= av)
        {
         s = s-av;
        }
      }

    }

   t = pow_mod(10, n - 1, av);
   s = mul_mod(s, t, av);
   sum = frac(sum + (double) s / (double) av);
  }

  sprintf(outs, "%09d",(int) (sum * 1e9));
}


//==============================================================================
// function RunTask(): boolean;
//==============================================================================
extern "C" bool __cdecl RunTask(IAgent* agent,
                 DWORD sessionId,
                 IGenericStream* inStream,
                 IGenericStream* outStream)
{

 char s[100];
 DWORD n;

/*
 for (DWORD i=0;i<10000; i++)
  {
   n = 1+i*9;
   CalculatePiDigits(n,s);
  } 
*/
 inStream->Read(&n,4);

 CalculatePiDigits(n,s);
 outStream->Write(&n,4);
 outStream->Write(&s[0],9);

 return true;
}
