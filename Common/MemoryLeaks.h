//=======================================================
// MemoryLeaks.h
// Copyright (C) by Roman Lut
//=======================================================
// Модуль для отслеживания утечек памяти.
//
// Использование: подключить в главный cpp-файл проэкта после всех "#inlude".
// С самого начала вызвать функцию EnableMemoryLeaksReport(type) (например, в WinMain())
// Выключать в Release() не нужно (#ifdef _DEBUG) - выключается автоматически.
//
// При завершении программы в Output окно распечатываются неосвобожденные блоки памяти.
// Каждый блок имеет свой Id, указанный в скобках.
// Чтобы во время следующего запуска остановится во время выделения этого блока, 
// нужно поставить breakpoint на процедуру EnableMemoryLeaksReport(), зайти в Evaluate window (Shift-F9),
// ввести _crtBreakAlloc, присвоить этой переменной Id блока, и продолжить 
// выполнение программы. Во время new сработает breakpoint.

// В режиме MEMORYLEAKS_PARANOIC при возникновении heap corruption 
// вылетит assert _на следуюущем_ new или delete.
// Можно самостоятельно проверять heap на corruption в любой момент функцией CheckHeap();

//Вобщем, EnableMemoryLeaksReport(MEMORYLEAKS_BASIC) нужно использовать ВСЕГДА,
//т.к. это не тормозит.
//EnableMemoryLeaksReport(MEMORYLEAKS_PARANOIC) включить один раз и посмотреть, не происходит ли 
//corruption. Тормозит по-страшному.

//Это проверяет только блоки в heap. Для проверки переполнения массивов в стеке нужно
//включить Buffer security check:YES в закладке Code generation (не тормозит, можно оставить в release).

//Поскольку большинство ошибок с heap corruption возникает при работе со сроками, рекомендуется
//использовать std::string или библиотеку strsafe.h из directx sdk, поскольку
//strncopy() НЕ ЗАВЕРШАЕТ СТРОКУ НУЛЕМ, если она длиннее указанного значения.

//_crtBreakAlloc

#ifndef MEMORYLEAKS_INCLUDED
#define MEMORYLEAKS_INCLUDED

#ifdef _DEBUG
#define MEMORY_LEAKS
#endif

//==========================
// TMEMORYLEAKREPORTTYPE
//==========================
typedef enum
{
 MEMORYLEAKS_BASIC          =  0,  //only dump unfreed blocks at exit
 MEMORYLEAKS_PARANOIC       =  1   //dump unfreed blocks at exit and check heap corruption on each allocation
} TMEMORYLEAKREPORTTYPE; 

#include <malloc.h>
#include "hxplatform.h"           

#ifdef MEMORY_LEAKS              
#define _CRTDBG_MAP_ALLOC
#include <crtdbg.h>
#include <assert.h>
#endif

#include <stdio.h>           

//=============================================
//void __inline EnableMemoryLeaksReport(t) 
//=============================================
void __inline EnableMemoryLeaksReport(TMEMORYLEAKREPORTTYPE t) 
{
#ifdef MEMORY_LEAKS            

 if (t==MEMORYLEAKS_PARANOIC)                
  {                                          
   _CrtSetDbgFlag(_CRTDBG_CHECK_ALWAYS_DF | _CRTDBG_CHECK_CRT_DF | _CRTDBG_ALLOC_MEM_DF | _CRTDBG_LEAK_CHECK_DF | _CRTDBG_DELAY_FREE_MEM_DF);  
  }                                          
   else                                      
  {                                          
   _CrtSetDbgFlag( _CRTDBG_ALLOC_MEM_DF | _CRTDBG_LEAK_CHECK_DF);  
  }                                          
#endif MEMORY_LEAKS            
}

//=====================================
//void CheckHeap()
//=====================================
void __inline CheckHeap()
{
#ifdef MEMORY_LEAKS            
 assert(_CrtCheckMemory());
#endif MEMORY_LEAKS            
}

//=====================================
//void GetHeapInfo()
//=====================================
void __inline GetHeapInfo(DWORD* usedSize, DWORD* freeSize, 
                          DWORD* usedBlocks, DWORD* freeBlocks)
{
 *usedSize = 0;
 *freeSize = 0;
 *usedBlocks=0;
 *freeBlocks=0;
 
 _HEAPINFO hinfo;
 int heapstatus;
 hinfo._pentry = NULL;

#ifndef _XBOX

 while( ( heapstatus = _heapwalk( &hinfo ) ) == _HEAPOK )
  { 
    if (hinfo._useflag == _USEDENTRY) 
     {
      (*usedSize)+=hinfo._size;
      (*usedBlocks)++;
     }
      else
     {
      (*freeSize)+=hinfo._size;
      (*freeBlocks)++;
     } 
  }

#endif _XBOX
}


//=====================================
//void DumpHeap()
//=====================================
void __inline DumpHeap()
{
 DWORD usedSize = 0;
 DWORD freeSize = 0;

#ifdef MEMORY_LEAKS            
 
 _HEAPINFO hinfo;
 int heapstatus;
 hinfo._pentry = NULL;
 char msg[1024];

 while( ( heapstatus = _heapwalk( &hinfo ) ) == _HEAPOK )
  { sprintf(msg, "%6s block at %x08x of size %d bytes\n",
       ( hinfo._useflag == _USEDENTRY ? "USED" : "FREE" ),
         (DWORD)hinfo._pentry, hinfo._size );
    OutputDebugString(msg);     
    
    if (hinfo._useflag == _USEDENTRY) 
     {
      usedSize+=hinfo._size;
     }
      else
     {
      freeSize+=hinfo._size;
     } 
  }
  
 switch( heapstatus )
 {
  case _HEAPEMPTY:
     OutputDebugString( "OK - empty heap\n" );
     break;
  case _HEAPEND:
     OutputDebugString( "OK - end of heap\n" );
     break;
  case _HEAPBADPTR:
     OutputDebugString( "ERROR - bad pointer to heap\n" );
     break;
  case _HEAPBADBEGIN:
     OutputDebugString( "ERROR - bad start of heap\n" );
     break;
  case _HEAPBADNODE:
     OutputDebugString( "ERROR - bad node in heap\n" );
     break;
 }
#endif 
}

#endif MEMORYLEAKS_INCLUDED
