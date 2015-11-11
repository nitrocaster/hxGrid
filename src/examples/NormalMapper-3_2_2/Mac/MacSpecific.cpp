//========================================================================================================================================================
//
//========================================================================================================================================================
// $File: //depot/3darg/Tools/NormalMapper/mac/MacSpecific.cpp $ $Revision: 1.1 $ $Author: hax22 $
//========================================================================================================================================================
// (C) 2002 ATI Research, Inc., All rights reserved.
//========================================================================================================================================================

#include <string.h>
#include <ctype.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

#include "Endian.h"
//#include <OpenGL/gl.h>

#include "MacSpecific.h"

#include "NmFileIO.h"
#include "TGAIO.h"
#include "ArgFileIO.h"

// DEFINES ====================================================================
//#define DISABLE_BYTE_SWAPPING 1

// INLINE FUNCTIONS ===========================================================
inline UInt16 AtiEndian16_Swap(UInt16 i16)
{
#ifdef DISABLE_BYTE_SWAPPING
   return i16;
#else
   return Endian16_Swap(i16);
#endif
}

inline UInt32 AtiEndian32_Swap(UInt32 i32)
{
#ifdef DISABLE_BYTE_SWAPPING
   return i32;
#else
   return Endian32_Swap(i32);
#endif
}

inline UInt64 AtiEndian64_Swap(UInt64 i64)
{
#ifdef DISABLE_BYTE_SWAPPING
   return i64;
#else
   return Endian64_Swap(i64);
#endif
}

inline float32 AtiEndianFloat32_Swap(float32 f32)
{
#ifndef DISABLE_BYTE_SWAPPING
   uint32* ip_f32 = (uint32*)(&f32);
   *ip_f32 = AtiEndian32_Swap(*ip_f32);
#endif   
   return f32;
}

inline float64 AtiEndianFloat64_Swap(float64 f64)
{
#ifndef DISABLE_BYTE_SWAPPING
   UInt64* ip_f64 = (UInt64*)(&f64);
   *ip_f64 = AtiEndian64_Swap(*ip_f64);
#endif
   return f64;
}

#define AtiByteSwap_enum(arg, argType)      ((arg) = (argType)AtiEndian32_Swap((UInt32)arg))

//=============================================================================
// no swapping needed
//=============================================================================
uint32 AtiByteSwap_bool8(void* arg, uint32 num)   { return 1; }
uint32 AtiByteSwap_char8(void* arg, uint32 num)   { return 1; }
uint32 AtiByteSwap_int8(void* arg, uint32 num)   { return 1; }
uint32 AtiByteSwap_uint8(void* arg, uint32 num)   { return 1; }

//=============================================================================
// basic swapping
//=============================================================================
uint32 AtiByteSwap_int16(void* arg, uint32 num)
{
#ifndef DISABLE_BYTE_SWAPPING
   int16* ptr = (int16*)arg;
   for (uint32 idx=0; idx<num; idx++)
   {
      ptr[idx] = (int16)AtiEndian16_Swap(ptr[idx]);
   }
#endif
   return 1;
}

//=============================================================================
uint32 AtiByteSwap_uint16(void* arg, uint32 num)
{
#ifndef DISABLE_BYTE_SWAPPING
   uint16* ptr = (uint16*)arg;
   for (uint32 idx=0; idx<num; idx++)
   {
      ptr[idx] = (uint16)AtiEndian16_Swap(ptr[idx]);
   }
#endif
   return 1;
}

//=============================================================================
uint32 AtiByteSwap_int32(void* arg, uint32 num)            
{
#ifndef DISABLE_BYTE_SWAPPING
   int32* ptr = (int32*)arg;
   for (uint32 idx=0; idx<num; idx++)
   {
      ptr[idx] = (int32)AtiEndian32_Swap(ptr[idx]);
   }
#endif
   return 1;
}

//=============================================================================
uint32 AtiByteSwap_uint32(void* arg, uint32 num)            
{
#ifndef DISABLE_BYTE_SWAPPING
   uint32* ptr = (uint32*)arg;
   for (uint32 idx=0; idx<num; idx++)
   {
      ptr[idx] = (uint32)AtiEndian32_Swap(ptr[idx]);
   }
#endif
   return 1;
}

//=============================================================================
uint32 AtiByteSwap_float32(void* arg, uint32 num)
{
#ifndef DISABLE_BYTE_SWAPPING
   float32* ptr = (float32*)arg;
   for (uint32 idx=0; idx<num; idx++)
   {
      ptr[idx] = AtiEndianFloat32_Swap(ptr[idx]);
   }
#endif
   return 1;
}

//=============================================================================
uint32 AtiByteSwap_float64(void* arg, uint32 num)
{
#ifndef DISABLE_BYTE_SWAPPING
   float64* ptr = (float64*)arg;
   for (uint32 idx=0; idx<num; idx++)
   {
      ptr[idx] = AtiEndianFloat64_Swap(ptr[idx]);
   }
#endif
   return 1;
}

//=============================================================================
// NM swap routines
//=============================================================================
uint32 AtiByteSwap_NmHeader (void* arg, uint32 num)
{
#ifndef DISABLE_BYTE_SWAPPING
   NmHeader* ptr = (NmHeader*)arg;
   for (uint32 idx=0; idx<num; idx++)
   {
      AtiByteSwap_int32(&ptr[idx].size, 1);
   }
#endif
   return 1;  
}

uint32 AtiByteSwap_NmRawPoint (void* arg, uint32 num)
{
#ifndef DISABLE_BYTE_SWAPPING
   NmRawPoint* ptr = (NmRawPoint*)arg;
   for (uint32 idx=0; idx<num; idx++)
   {
      AtiByteSwap_float32(&ptr[idx].x, 1);
      AtiByteSwap_float32(&ptr[idx].y, 1);
      AtiByteSwap_float32(&ptr[idx].z, 1);
   }
#endif
   return 1;  
}

uint32 AtiByteSwap_NmRawPointD (void* arg, uint32 num)
{
#ifndef DISABLE_BYTE_SWAPPING
   NmRawPointD* ptr = (NmRawPointD*)arg;
   for (uint32 idx=0; idx<num; idx++)
   {
      AtiByteSwap_float64(&ptr[idx].x, 1);
      AtiByteSwap_float64(&ptr[idx].y, 1);
      AtiByteSwap_float64(&ptr[idx].z, 1);
   }
#endif
   return 1;  
}

uint32 AtiByteSwap_NmRawTexCoord (void* arg, uint32 num)
{
#ifndef DISABLE_BYTE_SWAPPING
   NmRawTexCoord* ptr = (NmRawTexCoord*)arg;
   for (uint32 idx=0; idx<num; idx++)
   {
      AtiByteSwap_float32(&ptr[idx].u, 1);
      AtiByteSwap_float32(&ptr[idx].v, 1);
   }
#endif
   return 1;  
}

uint32 AtiByteSwap_NmRawTriangle (void* arg, uint32 num)
{
#ifndef DISABLE_BYTE_SWAPPING
   NmRawTriangle* ptr = (NmRawTriangle*)arg;
   for (uint32 idx=0; idx<num; idx++)
   {
      AtiByteSwap_NmRawPoint(&ptr[idx].vert[0], 3);
      AtiByteSwap_NmRawPoint(&ptr[idx].norm[0], 3);
      AtiByteSwap_NmRawTexCoord(&ptr[idx].texCoord[0], 3);
   }
#endif
   return 1;  
}

uint32 AtiByteSwap_NmRawTangentSpace (void* arg, uint32 num)
{
#ifndef DISABLE_BYTE_SWAPPING
   NmRawTangentSpace* ptr = (NmRawTangentSpace*)arg;
   for (uint32 idx=0; idx<num; idx++)
   {
      AtiByteSwap_NmRawPoint(&ptr[idx].tangent[0], 3);
      AtiByteSwap_NmRawPoint(&ptr[idx].binormal[0], 3);
   }
#endif
   return 1;  
}

uint32 AtiByteSwap_NmRawTangentSpaceD (void* arg, uint32 num)
{
#ifndef DISABLE_BYTE_SWAPPING
   NmRawTangentSpaceD* ptr = (NmRawTangentSpaceD*)arg;
   for (uint32 idx=0; idx<num; idx++)
   {
      AtiByteSwap_NmRawPointD(&ptr[idx].tangent[0], 3);
      AtiByteSwap_NmRawPointD(&ptr[idx].binormal[0], 3);
   }
#endif
   return 1;  
}

uint32 AtiByteSwap_NmIndex (void* arg, uint32 num)
{
#ifndef DISABLE_BYTE_SWAPPING
   NmIndex* ptr = (NmIndex*)arg;
   for (uint32 idx=0; idx<num; idx++)
   {
      AtiByteSwap_int32(&ptr[idx].idx[0], 3);
   }
#endif
   return 1;  
}

uint32 AtiByteSwap_NmTangentPointD (void* arg, uint32 num)
{
#ifndef DISABLE_BYTE_SWAPPING
   NmTangentPointD* ptr = (NmTangentPointD*)arg;
   for (uint32 idx=0; idx<num; idx++)
   {
      AtiByteSwap_float64(&ptr[idx].vertex[0], 3);
      AtiByteSwap_float64(&ptr[idx].normal[0], 3);
      AtiByteSwap_float64(&ptr[idx].uv[0], 2);
      AtiByteSwap_float64(&ptr[idx].tangent[0], 3);
      AtiByteSwap_float64(&ptr[idx].binormal[0], 3);
      AtiByteSwap_int32(&ptr[idx].count, 1);
   }
#endif
   return 1;
}

//=============================================================================
// TGA structure swapping
//=============================================================================
uint32 AtiByteSwap_TGAHeaderInfo (void* arg, uint32 num)
{
#ifndef DISABLE_BYTE_SWAPPING
   TGAHeaderInfo* ptr = (TGAHeaderInfo*)arg;
   for (uint32 idx=0; idx<num; idx++)
   {
      AtiByteSwap_uint16(&ptr[idx].cmorg, 1);
      AtiByteSwap_uint16(&ptr[idx].cmcnt, 1);
      AtiByteSwap_uint16(&ptr[idx].imxorg, 1);
      AtiByteSwap_uint16(&ptr[idx].imyorg, 1);
      AtiByteSwap_uint16(&ptr[idx].imwidth, 1);
      AtiByteSwap_uint16(&ptr[idx].imheight, 1);
   }
#endif
   return 1;  
}

//=============================================================================
// ARG structures
//=============================================================================
uint32 AtiByteSwap_AtiBitmap (void* arg, uint32 num)
{
#ifndef DISABLE_BYTE_SWAPPING
   AtiBitmap* ptr = (AtiBitmap*)arg;
   for (uint32 idx=0; idx<num; idx++)
   {
      AtiByteSwap_uint16(&ptr[idx].width, 1);
      AtiByteSwap_uint16(&ptr[idx].height, 1);
      AtiByteSwap_enum(ptr[idx].format, AtiBitmapFormatEnum);
   }
#endif
   return 1;  
}

//=============================================================================


void StrToLower(char *str) 
{
   int i, length;
   
   length = strlen(str);
   
   for(i = 0;i < length;i++) 
      str[i] = tolower(str[i]);
}

int strcmpi(const char *str1, const char *str2) 
{
   char *str1Copy, *str2Copy;
   int retVal;
   
   str1Copy = new char[strlen(str1) + 1];
   str2Copy = new char[strlen(str2) + 1];

   strcpy(str1Copy, str1);
   strcpy(str2Copy, str2);

   StrToLower(str1Copy);
   StrToLower(str2Copy);
   
   retVal = strcmp(str1Copy, str2Copy);
   
   delete str1Copy;
   delete str2Copy;
   
   return retVal;
}

int stricmp(const char *str1, const char *str2) 
{
   char *str1Copy, *str2Copy;
   int retVal;
   
   str1Copy = new char[strlen(str1) + 1];
   str2Copy = new char[strlen(str2) + 1];

   strcpy(str1Copy, str1);
   strcpy(str2Copy, str2);

   StrToLower(str1Copy);
   StrToLower(str2Copy);
   
   retVal = strcmp(str1Copy, str2Copy);
   
   delete str1Copy;
   delete str2Copy;
   
   return retVal;
}

int strnicmp(const char *str1, const char *str2, unsigned long length) 
{
   char *str1Copy, *str2Copy;
   int retVal;
   
   str1Copy = new char[strlen(str1) + 1];
   str2Copy = new char[strlen(str2) + 1];

   strcpy(str1Copy, str1);
   strcpy(str2Copy, str2);

   StrToLower(str1Copy);
   StrToLower(str2Copy);
   
   retVal = strncmp(str1Copy, str2Copy, length);
   
   delete str1Copy;
   delete str2Copy;
   
   return retVal;
}

/*
char* strdup(const char *strSource) {
   char *dup;
   
   dup = new char[strlen(strSource) + 1];
   strcpy(dup, strSource);
   return dup;
}
*/

void ReadBitmapFileHeader(FILE *f, BITMAPFILEHEADER *bfh) 
{
   WORD w;
   DWORD dw;

   fread(&w, sizeof(WORD), 1, f);
   bfh->bfType = AtiEndian16_Swap(w);
   
   fread(&dw, sizeof(DWORD), 1, f);
   bfh->bfSize = AtiEndian32_Swap(dw);
   
   fread(&w, sizeof(WORD), 1, f);
   bfh->bfReserved1 = AtiEndian16_Swap(w);
   
   fread(&w, sizeof(WORD), 1, f);
   bfh->bfReserved2 = AtiEndian16_Swap(w);
   
   fread(&dw, sizeof(DWORD), 1, f);
   bfh->bfOffBits = AtiEndian32_Swap(dw);
}


void ReadBitmapCoreHeader(FILE *f, BITMAPCOREHEADER *bch)
{
   WORD w;
   LONG l;
   DWORD dw;

   fread(&dw, sizeof(DWORD), 1, f);
   bch->bcSize = AtiEndian32_Swap(dw);
   
   fread(&l, sizeof(LONG), 1, f);
   bch->bcWidth = AtiEndian32_Swap(l);
   
   fread(&l, sizeof(LONG), 1, f);
   bch->bcHeight = AtiEndian32_Swap(l);
   
   fread(&w, sizeof(WORD), 1, f);
   bch->bcPlanes = AtiEndian16_Swap(w);
   
   fread(&w, sizeof(WORD), 1, f);
   bch->bcBitCount = AtiEndian16_Swap(w);
}

/*
bool8 AtiReadBitmap(AtiBitmap *bmp, char *fileName)
{
   uint8 *tmpPtr, *tmpPtr2, *tmpPixels;
   char buff[256];
   FILE *f;
   BITMAPFILEHEADER bfh;
   BITMAPCOREHEADER bch;
    int32 i, j, rowStart;
    uint8 temp;
    int numChannels;

   
   f = fopen(fileName, "rb");
   if (f == NULL) {
      sprintf(buff, "\nERROR! unable to open file %s for reading\n", fileName);
      SuErrorLog(buff);
      return FALSE;
   }
   
    ReadBitmapFileHeader(f, &bfh);
    ReadBitmapCoreHeader(f, &bch);
         
   bmp->width = bch.bcWidth;
   bmp->height = bch.bcHeight;
   bmp->bitDepth = bch.bcBitCount;
   numChannels = bmp->bitDepth/8;
   
   // Make sure we have 2nX2n image
   for (i=0; i<30; i++) {
      if (bmp->width == 1<<i)
           break;
   }
   
   if (i == 30) {
        sprintf(buff, "\nERROR! \"%s\" has dimensions %d x %d\n", fileName, bmp->width, bmp->height);
        SuErrorLog(buff);
        return FALSE;
   }
   
   for (i=0; i<30; i++) {
        if (bmp->height == 1<<i)
           break;
   }
   
   if (i == 30) {
      sprintf(buff, "\nERROR! \"%s\" has dimensions %d x %d\n", fileName, bmp->width, bmp->height);
        SuErrorLog(buff);
        return FALSE;
   }
   
   // Allocate memory for texels
   bmp->pixels = NULL;
   if (bmp->width == 1)
        bmp->pixels = (uint8 *) malloc (sizeof(uint8)*bmp->width*bmp->height*(bmp->bitDepth/8)*2);
   else
        bmp->pixels = (uint8 *) malloc (sizeof(uint8)*bmp->width*bmp->height*(bmp->bitDepth/8));

   if (bmp->pixels == NULL) {
        sprintf(buff, "\nERROR allocating memory for pixels from file \"%s\"\n", fileName);
        SuErrorLog(buff);
        return FALSE;
   }
   
   // Gets bitmap bits. Greyscale or BGR
   fread(bmp->pixels, sizeof(uint8), bmp->width * bmp->height * (bmp->bitDepth/8), f);
      
   // Remove padding if image is 1 pixel wide
   if (bmp->width == 1) {
        for (i=0; i<bmp->height; i++) {
           tmpPtr = &(bmp->pixels[i*3]);
           tmpPtr2 = &(bmp->pixels[i*4]);
   
           *(tmpPtr++) = *(tmpPtr2++);
           *(tmpPtr++) = *(tmpPtr2++);
           *(tmpPtr++) = *(tmpPtr2++);
       }
   }
   
   // Change BGR to RGB and flip vertically
   if(bmp->bitDepth == 24) {
       i = 0;
    
       while (i < bmp->height) {
          rowStart = i * bmp->width * numChannels;
   
          for(j=0;j < (bmp->width * numChannels);j+=numChannels) {
             temp = bmp->pixels[rowStart + j];
             bmp->pixels[rowStart + j] = bmp->pixels[rowStart + j + 2];
             bmp->pixels[rowStart + j + 2] = temp;
          }
          i++;
       }   
      }

   fclose(f);
   
   return TRUE;
}
*/

OSErr GetBaseFilePath(char* filePath, int pathSize, bool posixStyle)
{
    CFBundleRef myAppsBundle = CFBundleGetMainBundle();
    CFURLRef      myBundleURL = NULL;
    Boolean         ok;
    CFStringRef      cfString = NULL;
    UInt32         u32Encoding = 0;
    char            testString[64];
    //char            replaceString[64];
        
   if( myAppsBundle != NULL )
   {
      myBundleURL = CFBundleCopyExecutableURL( myAppsBundle );
      if( myBundleURL != NULL )
      {
         if (posixStyle)
         {
            cfString = CFURLCopyFileSystemPath( myBundleURL, kCFURLPOSIXPathStyle );
            strcpy(testString, "NMFView");
            //strcpy(replaceString, "/Resources/");
         }
         else
         {
            cfString = CFURLCopyFileSystemPath( myBundleURL, kCFURLHFSPathStyle );
            strcpy(testString, "NMFView");
            //strcpy(replaceString, ":Resources:");
         }
            
         if( cfString != NULL )
         {
            ok = CFStringGetCString( cfString, filePath, pathSize, u32Encoding );
            if( ok )
            {
                for (unsigned int i=0; i<strlen(filePath); i++)
                {
                    if (strncmp(&filePath[i], testString, 7) == 0)
                    {
                        filePath[i] = '\0';
                        break;
                    }
                }
               //strcat(filePath, replaceString);
               return( noErr );
            }
         }

         CFRelease( myBundleURL );
      }
   }

   return( fnfErr );
}
