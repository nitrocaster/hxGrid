#pragma once

#include <windows.h>
class CMultiAppSync
{
 HANDLE Mutex;
public:
 CMultiAppSync();
 ~CMultiAppSync();

 typedef void (*TWaitMsgCallback)();
 bool Do( const char* mutexName, TWaitMsgCallback waitMsgCallback );
};

