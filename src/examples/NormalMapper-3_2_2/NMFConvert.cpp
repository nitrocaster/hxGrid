//=============================================================================
// NMFConvert.cpp -- Converts from old NMF file format to the new format
//=============================================================================
// $File: //depot/3darg/Tools/NormalMapper/NMFConvert.cpp $ $Revision: 1.1 $ $Author: hax22 $
//=============================================================================
// (C) 2002 ATI Research, Inc., All rights reserved.
//=============================================================================

#ifndef ATI_MAC_OS
 #include <windows.h>
#endif
#include <stdio.h>

#include "NmFileIO.h"

//////////////////////////////////////////////////////////////////////////
// Entry point
//////////////////////////////////////////////////////////////////////////
int 
main (int argc, char **argv)
{
   printf ("NMFConvert v01.01.00\n");

   // Check that we have the right arguments
   if (argc != 3)
   {
      printf ("Usage: NMFConvert inputfile outputfile\n");
      exit (-1);
   }
   char* fileNameIn = argv[1];
   char* fileNameOut = argv[2];
   
   // Open input file
   FILE* fpIn = fopen (fileNameIn, "rb");
   if (fpIn == NULL)
   {
      printf ("ERROR: Unable to open %s\n", fileNameIn);
      exit (-1);
   }

   // Open output file
   FILE* fpOut = fopen (fileNameOut, "wb");
   if (fpOut == NULL)
   {
      fclose (fpIn);
      printf ("ERROR: Unable to open %s\n", fileNameOut);
      exit (-1);
   }

   // Convert
   if (!NmConvertFile (fpIn, fpOut))
   {
      fclose (fpIn);
      fclose (fpOut);
      printf ("ERROR: Unable to convert file!\n");
      exit (-1);
   }

   // close files
   fclose (fpIn);
   fclose (fpOut);
   printf ("Sucessfully wrote %s\n", fileNameOut);
   
   return 0;
}
