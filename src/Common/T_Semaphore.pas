//==============================================================================
// hxGrid framework
// Copyright (C) 2007 by Roman Lut
// hax@deep-shadows.com
// http://www.deep-shadows.com/hax/
//==============================================================================

unit T_Semaphore;

interface
uses Windows;

//============================================================
// class TSemaphore
//============================================================
type TSemaphore = class
  public

   constructor Create(initialValue, maxValue: DWORD);
   destructor Destroy(); override;

   function GetHandle(): THANDLE;

   //true if ok, false if timeout
   function Wait(timeout: DWORD = INFINITE): boolean;

   //returns previous count
   function ReleaseSemaphore(increment:DWORD = 1): DWORD;

  private

   hSemaphore: THANDLE;
 end;

implementation

//==============================================================================
//==============================================================================
constructor TSemaphore.Create(initialValue, maxValue: DWORD);
begin
 hSemaphore := CreateSemaphore(
    nil,   // no security attributes
    initialValue,   // initial count
    maxValue,   // maximum count
    nil);  // unnamed semaphore
end;

//==============================================================================
//==============================================================================
destructor TSemaphore.Destroy();
begin
 CloseHandle(hSemaphore);
end;

//==============================================================================
//==============================================================================
function TSemaphore.GetHandle(): THANDLE;
begin
 result:=hSemaphore;
end;

//==============================================================================
//==============================================================================
function TSemaphore.Wait(timeout: DWORD): boolean;
begin
 result:=WaitForSingleObject( hSemaphore, timeout)<>WAIT_TIMEOUT;
end;

//==============================================================================
//==============================================================================
function TSemaphore.ReleaseSemaphore(increment:DWORD):DWORD;
var
 prev: DWORD;
begin
 windows.ReleaseSemaphore(hSemaphore, increment, @prev);
 result:=prev;
end;

end.
