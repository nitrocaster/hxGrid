
#include "windows.h"
#include "assert.h"
#include "stdio.h"
#include "conio.h"
#include "IGenericStream.h"
#include "hxgridinterface.h"


using namespace VITALENGINE;

char* pival;

//===============================================================
//===============================================================
void __cdecl Finalize(IGenericStream* outStream)
{
 assert(outStream->GetLength()==4+9);
 assert(outStream->GetPos()==0);

 DWORD n;
 outStream->Read(&n,4);

 outStream->Read(&pival[n-1],9);
}

//===============================================================
//===============================================================
void Writeln(const char *szFmt, ...)
{
 char sz[4096], szMsg[4096];
 va_list va;
 va_start(va, szFmt);
 vsprintf(szMsg, szFmt, va);
 va_end(va);

 sprintf(sz, "%s\n", szMsg);

 sz[sizeof(sz)-1] = '\0';

 printf(sz);
} 

//===============================================================
//===============================================================
int __cdecl main (int argc, char **argv)
{
 if (argc!=2)
 {
  printf("Usage: picalculator [number_of_digits]");
  return 0;
 }

 DWORD digits;
 digits = atoi(argv[1]);              

 if (digits<1) digits=1;

 digits = (digits-1)/9;
 digits++;

 Writeln("Calculating %d PI digits...", digits*9+1);

 DWORD tick = GetTickCount();

 pival = new char[digits*9+1];
 pival[digits*9]=0;

 IGridUser* user = CreateGridUserObject(IGridUser::VERSION);

 for (DWORD i=0; i<digits; i++)
 {
  IGenericStream* stream  = CreateGenericStream();
  DWORD d=1+i*9;
  stream->Write(&d,4);
  user->RunTask("picalculator_task.dll","RunTask",stream,Finalize,&d,true);
 }

 user->WaitForCompletion();

 Writeln("Done in %d ms",GetTickCount()-tick);
 Writeln("");
 Writeln("PI=3.%s",pival);
 Writeln("");
 Writeln("Releasing GridUser...");
 
 delete[] pival;

 user->Release();

 Writeln("Press any key...");

 _getch();
}