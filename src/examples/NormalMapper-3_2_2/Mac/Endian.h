/*
     File:       CarbonCore/Endian.h
 
     Contains:   Endian swapping utilties
 
     Version:    CarbonCore-545~1
 
     Copyright:  © 1997-2003 by Apple Computer, Inc., all rights reserved
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://developer.apple.com/bugreporter/
 
*/
#ifndef __ENDIAN__
#define __ENDIAN__

#ifndef __MACTYPES__
#include <CarbonCore/MacTypes.h>
#endif

#define Endian16_Swap(value)                 \
        (((((UInt16)value)<<8) & 0xFF00)   | \
         ((((UInt16)value)>>8) & 0x00FF))

#define Endian32_Swap(value)                     \
        (((((UInt32)value)<<24) & 0xFF000000)  | \
         ((((UInt32)value)<< 8) & 0x00FF0000)  | \
         ((((UInt32)value)>> 8) & 0x0000FF00)  | \
         ((((UInt32)value)>>24) & 0x000000FF))


#define Endian64_Swap(value)                                \
       (((((UInt64)value)<<56) & 0xFF00000000000000ULL)  | \
        ((((UInt64)value)<<40) & 0x00FF000000000000ULL)  | \
        ((((UInt64)value)<<24) & 0x0000FF0000000000ULL)  | \
        ((((UInt64)value)<< 8) & 0x000000FF00000000ULL)  | \
        ((((UInt64)value)>> 8) & 0x00000000FF000000ULL)  | \
        ((((UInt64)value)>>24) & 0x0000000000FF0000ULL)  | \
        ((((UInt64)value)>>40) & 0x000000000000FF00ULL)  | \
        ((((UInt64)value)>>56) & 0x00000000000000FFULL))

#endif /* __ENDIAN__ */

