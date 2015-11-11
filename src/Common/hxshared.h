//===================================================
// hsshared.h
// common функции, не зависящие от Vital Engine
//===================================================

#ifndef SHARED_INCLUDED
#define SHARED_INCLUDED

#include <windows.h>

#define INCREMENT_DWORD(p,add) (*((DWORD*)((void**)&p)))+=(add);

#endif SHARED_INCLUDED
