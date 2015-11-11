
#include "TGenericStream.h"
#include "hxshared.h"

namespace VITALENGINE
{

//===========================================================
// TGenericStream::TMemoryStream()  
//===========================================================
TGenericStream::TGenericStream(DWORD initialSize_, DWORD increment_)
{
 readOnly=false;
 allocatedSize=initialSize_;
 initialSize=initialSize_;
 increment=increment_;
 basePointer=new BYTE[initialSize];
 curPointer=basePointer;
 length=0;
}

//===========================================================
// TGenericStream::TGenericStream()   2
//===========================================================
TGenericStream::TGenericStream(void* pointer, DWORD length, bool readOnly)
{
 basePointer=(BYTE*)pointer;
 curPointer=basePointer;
 this->length=length;
 allocatedSize=length;
 increment=0;
 this->readOnly=readOnly;
}


//===========================================================
// TGenericStream::~TGenericStream()
//===========================================================
TGenericStream::~TGenericStream()
{
 if (!readOnly) delete[] basePointer; 
}

//===========================================================
// void TGenericStream::Grow()
//===========================================================
void TGenericStream::Grow(DWORD destsize)
//grow to destsize+increment
{                                       
 BYTE* tmp;
 if (increment!=0)
  {
   tmp=new BYTE[destsize+increment];
   allocatedSize=destsize+increment;
  }
   else
  {
   tmp=new BYTE[destsize*2];
   allocatedSize=destsize*2;
  }  
  
 memcpy(tmp,basePointer,length);

 *(DWORD*)&curPointer=(DWORD)curPointer-(DWORD)basePointer+(DWORD)tmp;
 delete[] basePointer;
                                   
 basePointer=tmp;
}


//====================================================
// 
//====================================================
BYTE* __stdcall TGenericStream::GetBasePointer()
{
 return basePointer;
}

//====================================================
// 
//====================================================
BYTE* __stdcall TGenericStream::GetCurPointer()
{
 return curPointer;
}

//====================================================
// 
//====================================================
bool __stdcall TGenericStream::isReadOnly()
{
 return readOnly;
}

//====================================================
// 
//====================================================
DWORD __stdcall TGenericStream::GetLength()
{
 return length;
}

//====================================================
// 
//====================================================
void __stdcall TGenericStream::Write(const void* Data, DWORD count)
{
 assert(!readOnly);
 if (GetPos()+count>allocatedSize) Grow(GetPos()+count);
 memcpy(curPointer,Data,count);
 DWORD length1=GetPos()+count;
 if (length<length1) length=length1;
 curPointer+=count;
}

//====================================================
// 
//====================================================
DWORD __stdcall TGenericStream::Read(void* Data, DWORD count)
{
 int tmp=length-GetPos();
 if (tmp<0) return 0;  //read beyond the end

 if (count>(DWORD)tmp) count=tmp;
 memcpy(Data,curPointer,count);
 curPointer+=count;
 return count;
}

//====================================================
// 
//====================================================
void __stdcall TGenericStream::Seek(DWORD pos)
{
 *(DWORD*)&curPointer=(DWORD)basePointer+pos;
}

//====================================================
// 
//====================================================
DWORD __stdcall TGenericStream::GetPos()
{
 return (DWORD)curPointer-(DWORD)basePointer;
}

//====================================================
// 
//====================================================
void __stdcall TGenericStream::Clear()
{
 assert(!readOnly);
 delete[] basePointer;

 allocatedSize=0;
 basePointer=new BYTE[0];
 curPointer=basePointer;
 length=0;
}

//====================================================
// 
//====================================================
void __stdcall TGenericStream::FastClear()
{
 assert(!readOnly);
 curPointer=basePointer;
 length=0;
}
 
//====================================================
// 
//====================================================
void __stdcall TGenericStream::GrowToPos(int DestSize)
{
 if (DestSize==-1) DestSize=(int)GetPos();

 assert(!readOnly);
 if (allocatedSize<(DWORD)DestSize) 
  {
   Grow(DestSize);
  }
 length=DestSize;
 curPointer=basePointer;
 INCREMENT_DWORD(curPointer,length);
}

//====================================================
// 
//====================================================
void __stdcall TGenericStream::Skip(DWORD count)
{
 Seek(GetPos()+count);
}

//====================================================
// 
//====================================================
void __stdcall TGenericStream::SetLength(DWORD newLength)
{
 if (allocatedSize<(DWORD)newLength) 
  {
   Grow(newLength);
  }
 length=newLength;
}

//====================================================
// 
//====================================================
void __stdcall TGenericStream::Compact()
{
 if (allocatedSize==length) return;
 
 BYTE* tmp;
 tmp=new BYTE[length];
 allocatedSize=length;

 memcpy(tmp,basePointer,length);

 *(DWORD*)&curPointer=(DWORD)curPointer-(DWORD)basePointer+(DWORD)tmp;
 delete[] basePointer;
                                   
 basePointer=tmp;
}


} //namespace