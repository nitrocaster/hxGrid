#ifndef TGENERICSTREAM_INCLUDED
#define TGENERICSTREAM_INCLUDED

#include "IGenericStream.h"
#include "assert.h"

namespace VITALENGINE
{

//====================================
//class TGenericStream
//====================================
class TGenericStream : public TInterfaceObject<IGenericStream>
{
 private: 
 
  BYTE *basePointer;
  BYTE *curPointer;
  bool readOnly;
  DWORD length;

  DWORD allocatedSize;
  DWORD initialSize;
  DWORD increment;
   
  void Grow(DWORD destsize);
 
  TGenericStream &operator =(const TGenericStream &source) {assert(false); return *new TGenericStream(0,0);}  //should not be copied
  TGenericStream (const TGenericStream &source) {assert(false);}  //should not be passed to function as non-pointer

 public:
 //======== BEGIN COM INTERFACE =======
  IUNKNOWN_METHODS_IMPLEMENTATION_INSTANCE()

  virtual BYTE* __stdcall GetBasePointer();
  virtual BYTE* __stdcall GetCurPointer();
  virtual bool __stdcall isReadOnly();
  virtual DWORD __stdcall GetLength();
  virtual void __stdcall Write(const void* Data, DWORD count);
  virtual DWORD __stdcall Read(void* Data, DWORD count);
  virtual void __stdcall Seek(DWORD pos);
  virtual DWORD __stdcall GetPos();
  virtual void __stdcall Clear();
  virtual void __stdcall FastClear(); 
  virtual void __stdcall GrowToPos(int DestSize=-1);
  virtual void __stdcall Skip(DWORD count);
  virtual void __stdcall SetLength(DWORD newLength);
  virtual void __stdcall Compact();
  
  //======== END COM INTERFACE =======
  
  //if incement!=0 - grows stream by incement
  //if incement=0 - grows twice current size
  TGenericStream(DWORD initialSize_, DWORD increment_ = 0);
  
  //create from buffer
  //creates a Read-Only stream on user-allocated memory block
  //only Read() and Seek() can be used
  //if readOnly=true - stream owns buffer
  //if readOnly=false - stream is just a shell around buffer, caller should delete buffer
  //todo: readOnly streams are not handled well, add more asserts
  TGenericStream(void* pointer, DWORD length, bool readOnly); 

  ~TGenericStream();
};     

} //namespace

#endif TGENERICSTREAM_INCLUDED
