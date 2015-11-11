#include "MultiAppSync.h"

//CMultiAppSync::
CMultiAppSync::CMultiAppSync() :
 Mutex( 0 )
{
}

CMultiAppSync::~CMultiAppSync()
{
 if ( Mutex )
 {
  ReleaseMutex( Mutex );
  Mutex = 0;
 }
}

bool CMultiAppSync::Do( const char* mutexName, TWaitMsgCallback waitMsgCallback )
{
 Mutex = CreateMutex( 0, TRUE, mutexName );
 if ( Mutex == 0 )
 {
  int errCode = GetLastError();
  return false;
 }
 else
 {
  if ( GetLastError() == ERROR_ALREADY_EXISTS ) 
  {
   waitMsgCallback();
   WaitForSingleObject( Mutex, INFINITE );
  }
  
  return true;
 }
}