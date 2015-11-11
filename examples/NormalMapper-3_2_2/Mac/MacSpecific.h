//========================================================================================================================================================
//
//========================================================================================================================================================
// $File: //depot/3darg/Tools/NormalMapper/mac/MacSpecific.h $ $Revision: 1.1 $ $Author: hax22 $
//========================================================================================================================================================
// (C) 2002 ATI Research, Inc., All rights reserved.
//========================================================================================================================================================

#ifndef _MacSpecific_h
#define _MacSpecific_h

#include "Types.h"

#define _strcmpi strcmpi
#define _stricmp stricmp
#define _strnicmp strnicmp

#define Sleep(n)

#define GL_COMBINE_EXT       GL_COMBINE
#define GL_COMBINE_RGB_EXT   GL_COMBINE_RGB
#define GL_DOT3_RGBA_EXT     GL_DOT3_RGBA
#define GL_SOURCE0_RGB_EXT   GL_SOURCE0_RGB
#define GL_PRIMARY_COLOR_EXT GL_PRIMARY_COLOR
#define GL_OPERAND0_RGB_EXT  GL_OPERAND0_RGB
#define GL_SOURCE1_RGB_EXT   GL_SOURCE1_RGB
#define GL_OPERAND1_RGB_EXT  GL_OPERAND1_RGB

typedef unsigned int   UINT;
typedef unsigned char  BYTE;
typedef bool           BOOL;
typedef unsigned long  DWORD;
typedef unsigned short WORD;
typedef long           LONG;

typedef struct tagBITMAPCOREHEADER {
  DWORD   bcSize; 
  LONG    bcWidth; 
  LONG    bcHeight; 
  WORD    bcPlanes; 
  WORD    bcBitCount; 
} BITMAPCOREHEADER, *PBITMAPCOREHEADER; 


typedef struct tagBITMAPFILEHEADER { 
  WORD    bfType; 
  DWORD   bfSize; 
  WORD    bfReserved1; 
  WORD    bfReserved2; 
  DWORD   bfOffBits; 
} BITMAPFILEHEADER, *PBITMAPFILEHEADER; 

int strcmpi(const char *str1, const char *str2);
int stricmp(const char *str1, const char *str2);
int strnicmp(const char *str1, const char *str2, unsigned long length);

OSErr GetBaseFilePath(char* filePath, int pathSize, bool posixStyle);

uint32 AtiByteSwap_bool8    (void* arg);
uint32 AtiByteSwap_char8    (void* arg, uint32 num);
uint32 AtiByteSwap_int8     (void* arg, uint32 num);
uint32 AtiByteSwap_uint8    (void* arg, uint32 num);

uint32 AtiByteSwap_int16    (void* arg, uint32 num);
uint32 AtiByteSwap_uint16   (void* arg, uint32 num);
uint32 AtiByteSwap_int32    (void* arg, uint32 num);
uint32 AtiByteSwap_uint32   (void* arg, uint32 num);
uint32 AtiByteSwap_float32  (void* arg, uint32 num);
uint32 AtiByteSwap_float64  (void* arg, uint32 num);

uint32 AtiByteSwap_NmHeader           (void* arg, uint32 num);
uint32 AtiByteSwap_NmRawPoint         (void* arg, uint32 num);
uint32 AtiByteSwap_NmRawPointD        (void* arg, uint32 num);
uint32 AtiByteSwap_NmRawTexCoord      (void* arg, uint32 num);
uint32 AtiByteSwap_NmRawTriangle      (void* arg, uint32 num);
uint32 AtiByteSwap_NmRawTangentSpace  (void* arg, uint32 num);
uint32 AtiByteSwap_NmRawTangentSpaceD (void* arg, uint32 num);
uint32 AtiByteSwap_NmIndex            (void* arg, uint32 num);
uint32 AtiByteSwap_NmTangentPointD    (void* arg, uint32 num);

uint32 AtiByteSwap_TGAHeaderInfo (void* arg, uint32 num);

uint32 AtiByteSwap_AtiBitmap (void* arg, uint32 num);

#endif