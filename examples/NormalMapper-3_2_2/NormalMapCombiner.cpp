//=============================================================================
// NormalMapCombiner.cpp -- Program that combines two normal maps.
//                          lots of options see main() for details.
//=============================================================================
// $File: //depot/3darg/Tools/NormalMapper/NormalMapCombiner.cpp $ $Revision: 1.1 $ $Author: hax22 $
//=============================================================================
// (C) 2002 ATI Research, Inc., All rights reserved.
//=============================================================================

#ifndef ATI_MAC_OS
 #include <windows.h>
#endif
#include <stdio.h>
#include <math.h>
#include <float.h>

#include "NmFileIO.h"
#include "TGAIO.h"
#include "ArgFileIO.h"

static char* versionString =  "NormalMapCombiner v00.00.01\n";

//#define USE_SMD_FILES

#define PACKINTOBYTE_MINUS1TO1(X)  ((BYTE)((X)*127.5+127.5))
#define UNPACKBYTE_MINUS1TO1(x)    ((((float)(x)-127.5)/127.5))
#define PACKINTOBYTE_0TO1(x)       ((BYTE)((x)*255))
#define UNPACKBYTE_0TO1(x)         (((float)(x)/255.0f))

#define PACKINTOSHORT_0TO1(x)      ((unsigned short)((x)*65535))
#define UNPACKSHORT_0TO1(x)        (((float)(x)/65535.0f))
#define PACKINTOSHORT_MINUS1TO1(X)  ((short)((X)*32767.5+32767.5))
#define UNPACKSHORT_MINUS1TO1(x)    ((((float)(x)-32767.5)/32767.5))
#define PACKINTOSHORT_SMINUS1TO1(x) ((short)((x)*32767.5))
#define UNPACKSHORT_SMINUS1TO1(x)   (((float)(x))/32767.5)

#define VEC_Subtract(a, b, c) ((c)[0] = (a)[0] - (b)[0], \
                               (c)[1] = (a)[1] - (b)[1], \
                               (c)[2] = (a)[2] - (b)[2])
#define VEC_Add(a, b, c) ((c)[0] = (a)[0] + (b)[0], \
                          (c)[1] = (a)[1] + (b)[1], \
                          (c)[2] = (a)[2] + (b)[2])
#define VEC_Cross(a, b, c) ((c)[0] = (a)[1] * (b)[2] - (a)[2] * (b)[1], \
                            (c)[1] = (a)[2] * (b)[0] - (a)[0] * (b)[2], \
                            (c)[2] = (a)[0] * (b)[1] - (a)[1] * (b)[0])
#define VEC_DotProduct(a, b) ((a)[0] * (b)[0] + \
                              (a)[1] * (b)[1] + \
                              (a)[2] * (b)[2])
#define INT_ROUND_TEXCOORD_U(X)  (int)(((X)*(float)(gWidth-1))+0.5f)
#define INT_ROUND_TEXCOORD_V(X)  (int)(((X)*(float)(gHeight-1))+0.5f)
#define INT_TEXCOORD_U(X)  (int)((X)*(float)(gWidth-1))
#define INT_TEXCOORD_V(X)  (int)((X)*(float)(gHeight-1))

// Value that's close enough to be called 0.0
#define EPSILON 1.0e-7

static const double PI = 3.1415926535897932384626433832795;

// Tangent space structure.
typedef struct
{
   double m[3][9];
} NmTangentMatrix;

// Experimental pixel format.      
typedef union
{
   struct { BYTE r, g, b, a; };
   struct { BYTE v[4]; };
} NmExpPixel;

// Local print routines.
void NmErrorLog (const char *szFmt, ...);
void NmErrorLog (char *szFmt);
#define NmPrint NmErrorLog

// Width and height of the resultant texture
static int gWidth;
static int gHeight;

// The output format
enum
{
   NORM_OUTPUT_8_8_8_TGA = 0,
   NORM_OUTPUT_8_8_8_8_TGA,
   NORM_OUTPUT_EXP_TGA,
   NORM_OUTPUT_16_16_ARG,
   NORM_OUTPUT_16_16_16_16_ARG,
   NORM_OUTPUT_10_10_10_2_ARG,
   NORM_OUTPUT_10_10_10_2_ARG_MS,
   NORM_OUTPUT_11_11_10_ARG_MS,
};
static int gOutput = NORM_OUTPUT_8_8_8_TGA;

// How to generate mip levels
enum
{
   MIP_NONE = 0,
   MIP_RECOMPUTE,
   MIP_BOX
};
static int gComputeMipLevels = MIP_NONE;

// How quiet we should be.
enum
{
   NM_VERBOSE = 0,
   NM_QUIET,
   NM_SILENT
};
static int gQuiet = NM_VERBOSE;

static bool gExpandTexels = false;  // Expand the border texels so we don't get
                                    // crud when bi/trilinear filtering
static bool gBoxFilter = false;     // Perform a post-box filter on normal map?
static int gDilateTexels = 0;       // How many texels to dilate.
static bool gBumpMap = false;       // Treat input file like a height field

//////////////////////////////////////////////////////////////////////////////
//    From:
//      Paper: http://www.acm.org/jgt/papers/MollerHughes99/
//      Code:  http://www.acm.org/jgt/papers/MollerHughes99/code.html
//////////////////////////////////////////////////////////////////////////////

#define CROSS(dest, v1, v2){                 \
          dest[0] = v1[1] * v2[2] - v1[2] * v2[1]; \
          dest[1] = v1[2] * v2[0] - v1[0] * v2[2]; \
          dest[2] = v1[0] * v2[1] - v1[1] * v2[0];}

#define DOT(v1, v2) (v1[0] * v2[0] + v1[1] * v2[1] + v1[2] * v2[2])

#define SUB(dest, v1, v2){       \
          dest[0] = v1[0] - v2[0]; \
          dest[1] = v1[1] - v2[1]; \
          dest[2] = v1[2] - v2[2];}

/*
 * A function for creating a rotation matrix that rotates a vector called
 * "from" into another vector called "to".
 * Input : from[3], to[3] which both must be *normalized* non-zero vectors
 * Output: mtx[3][3] -- a 3x3 matrix in colum-major form
 * Authors: Tomas Moller, John Hughes
 *          "Efficiently Building a Matrix to Rotate One Vector to Another"
 *          Journal of Graphics Tools, 4(4):1-4, 1999
 */
void 
fromToRotation (float from[3], float to[3], float mtx[3][3]) 
{
   float v[3];
   float e, h, f;
   
   CROSS(v, from, to);
   e = DOT(from, to);
   f = (e < 0)? -e:e;
   if (f > 1.0 - EPSILON)     /* "from" and "to"-vector almost parallel */
   {
      float u[3], v[3]; /* temporary storage vectors */
      float x[3];       /* vector most nearly orthogonal to "from" */
      float c1, c2, c3; /* coefficients for later use */
      int i, j;
      
      x[0] = (from[0] > 0.0)? from[0] : -from[0];
      x[1] = (from[1] > 0.0)? from[1] : -from[1];
      x[2] = (from[2] > 0.0)? from[2] : -from[2];
      
      if (x[0] < x[1])
      {
         if (x[0] < x[2])
         {
            x[0] = 1.0; x[1] = x[2] = 0.0;
         }
         else
         {
            x[2] = 1.0; x[0] = x[1] = 0.0;
         }
      }
      else
      {
         if (x[1] < x[2])
         {
            x[1] = 1.0; x[0] = x[2] = 0.0;
         }
         else
         {
            x[2] = 1.0; x[0] = x[1] = 0.0;
         }
      }
      
      u[0] = x[0] - from[0]; u[1] = x[1] - from[1]; u[2] = x[2] - from[2];
      v[0] = x[0] - to[0];   v[1] = x[1] - to[1];   v[2] = x[2] - to[2];
      
      c1 = 2.0f / DOT(u, u);
      c2 = 2.0f / DOT(v, v);
      c3 = c1 * c2  * DOT(u, v);
      
      for (i = 0; i < 3; i++) {
         for (j = 0; j < 3; j++) {
            mtx[i][j] =  - c1 * u[i] * u[j]
               - c2 * v[i] * v[j]
               + c3 * v[i] * u[j];
         }
         mtx[i][i] += 1.0f;
      }
   }
   else  /* the most common case, unless "from"="to", or "from"=-"to" */
   {
#if 0
      /* unoptimized version - a good compiler will optimize this. */
      /* h = (1.0 - e)/DOT(v, v); old code */
      h = 1.0/(1.0 + e);      /* optimization by Gottfried Chen */
      mtx[0][0] = e + h * v[0] * v[0];
      mtx[0][1] = h * v[0] * v[1] - v[2];
      mtx[0][2] = h * v[0] * v[2] + v[1];
      
      mtx[1][0] = h * v[0] * v[1] + v[2];
      mtx[1][1] = e + h * v[1] * v[1];
      mtx[1][2] = h * v[1] * v[2] - v[0];
      
      mtx[2][0] = h * v[0] * v[2] - v[1];
      mtx[2][1] = h * v[1] * v[2] + v[0];
      mtx[2][2] = e + h * v[2] * v[2];
#else
      /* ...otherwise use this hand optimized version (9 mults less) */
      float hvx, hvz, hvxy, hvxz, hvyz;
      /* h = (1.0 - e)/DOT(v, v); old code */
      h = 1.0f/(1.0f + e);      /* optimization by Gottfried Chen */
      hvx = h * v[0];
      hvz = h * v[2];
      hvxy = hvx * v[1];
      hvxz = hvx * v[2];
      hvyz = hvz * v[1];
      mtx[0][0] = e + hvx * v[0];
      mtx[0][1] = hvxy - v[2];
      mtx[0][2] = hvxz + v[1];
      
      mtx[1][0] = hvxy + v[2];
      mtx[1][1] = e + h * v[1] * v[1];
      mtx[1][2] = hvyz - v[0];
      
      mtx[2][0] = hvxz - v[1];
      mtx[2][1] = hvyz + v[0];
      mtx[2][2] = e + hvz * v[2];
#endif
   }
} // end fromToRotation

//////////////////////////////////////////////////////////////////////////////
// Normalize a vector
//////////////////////////////////////////////////////////////////////////////
static void
Normalize(double v[3])
{
   if (v == NULL)
   {
      NmPrint ("ERROR: NULL pointer passed to Normalize!\n");
      exit (-1);
   }
   double len = sqrt((v[0]*v[0])+(v[1]*v[1])+(v[2]*v[2]));
   if (len < EPSILON)
   {
      v[0] = 1.0f;
      v[1] = 0.0f;
      v[2] = 0.0f;
   }
   else
   {
      v[0] = v[0]/len;
      v[1] = v[1]/len;
      v[2] = v[2]/len;
   }
}

///////////////////////////////////////////////////////////////////////////////
// Simple spinner display for progress indication
///////////////////////////////////////////////////////////////////////////////
static int gSpinner = 0;
static void
ShowSpinner (char* header = NULL)
{
   if (gQuiet > NM_VERBOSE)
   {
      return;
   }
   char* lineStart = header;
   if (header == NULL)
   {
      static char empty[] = "";
      lineStart = empty;
   }
   switch (gSpinner)
   {
      case 0:
         printf ("\r%s\\", lineStart);
         gSpinner++;
         break;
      case 1:
         printf ("\r%s|", lineStart);
         gSpinner++;
         break;
      case 2:
         printf ("\r%s/", lineStart);
         gSpinner++;
         break;
      default:
      case 3:
         printf ("\r%s-", lineStart);
         gSpinner = 0;
         break;
   }
}

////////////////////////////////////////////////////////////////////
// Multiply a 3x3 matrix with a 3 space vector (assuming w = 1), ignoring
// the last row of the matrix
////////////////////////////////////////////////////////////////////
static void
ConvertFromTangentSpace (double* m, double *vec, double *result)
{
   if ((m == NULL) || (vec == NULL) || (result == NULL))
   {
      NmPrint ("ERROR: NULL pointer pased to ConvertFromTangentSpace\n");
      exit (-1); 
   }

   double tmp[3];
   tmp[0] = vec[0]*m[0] + vec[1]*m[1] + vec[2]*m[2];
   tmp[1] = vec[0]*m[3] + vec[1]*m[4] + vec[2]*m[5];
   tmp[2] = vec[0]*m[6] + vec[1]*m[7] + vec[2]*m[8];

   result[0] = tmp[0];
   result[1] = tmp[1];
   result[2] = tmp[2];
}

////////////////////////////////////////////////////////////////////
// Multiply a 3x3 matrix with a 3 space vector (assuming w = 1), ignoring
// the last row of the matrix
////////////////////////////////////////////////////////////////////
static void
ConvertToTangentSpace (double* m, double *vec, double *result)
{
   if ((m == NULL) || (vec == NULL) || (result == NULL))
   {
      NmPrint ("ERROR: NULL pointer pased to ConvertFromTangentSpace\n");
      exit (-1);
   }

   double tmp[3];
   tmp[0] = vec[0]*m[0] + vec[1]*m[3] + vec[2]*m[6];
   tmp[1] = vec[0]*m[1] + vec[1]*m[4] + vec[2]*m[7];
   tmp[2] = vec[0]*m[2] + vec[1]*m[5] + vec[2]*m[8];

   result[0] = tmp[0];
   result[1] = tmp[1];
   result[2] = tmp[2];
}

//////////////////////////////////////////////////////////////////////////
// Convert to experimental pixel format. Divide by the maximum of the
// absloute value of x, y, or 1-z and store the maximum in alpha.
//////////////////////////////////////////////////////////////////////////
static void
ConvertToExpPixel (double x, double y, double z, NmExpPixel* jp)
{
   if (jp == NULL)
   {
      NmPrint ("ERROR: NULL pointer passed to ConvertToExpPixel!\n");
      exit (-1);
   }


   // Compute one minus z
   double omz = 1.0 - z;
   double ax = fabs (x);
   double ay = fabs (y);
   double aomz = fabs (omz);

   // Find max.
   double max;
   if (ax > ay)
   {
      if (ax > aomz)
      {
         max = ax;
      }
      else
      {
         max = aomz;
      }
   }
   else
   {
      if (ay > aomz)
      {
         max = ay;
      }
      else
      {
         max = aomz;
      }
   }

   // Now compute values.
   jp->r = PACKINTOBYTE_MINUS1TO1(x/max);
   jp->g = PACKINTOBYTE_MINUS1TO1(y/max);
   jp->b = PACKINTOBYTE_MINUS1TO1(omz/max);
   jp->a = PACKINTOBYTE_0TO1(max);
}

//////////////////////////////////////////////////////////////////////////
// Fetch from the normal map given the uv.
//////////////////////////////////////////////////////////////////////////
static void
Fetch (float* map, int width, int height, double u, double v, double bumpN[3])
{
   if ((map == NULL) || (bumpN == NULL))
   {
      NmPrint ("ERROR: NULL pointer passed to Fetch\n");
      exit (-1);
   }
   if (width < 1) 
   {
      NmPrint ("ERROR: Invaild width %d! (Fetch)\n", width);
      exit (-1);
   }
   if (height < 1)
   {
      NmPrint ("ERROR: Invaild height %d! (Fetch)\n", height);
      exit (-1);
   }

   // Get coordinates in 0-1 range
   if ((u < 0.0) || (u > 1.0))
   {
      u -= floor(u);
   }
   if ((v < 0.0) || (v > 1.0))
   {
      v -= floor(v);
   }

   // Now figure out the texel information for u coordinate
   double up = (double)(width-1) * u;
   double umin = floor (up);
   double umax = ceil (up);
   double ufrac = up - umin;

   // Now figure out the texel information for v coordinate
   double vp = (double)(height-1) * v;
   double vmin = floor (vp);
   double vmax = ceil (vp);
   double vfrac = vp - vmin;

   // First term umin/vmin
   int idx = (int)(vmin)*width*3 + (int)(umin)*3;
   bumpN[0] = ((1.0-ufrac)*(1.0-vfrac)*(double)(map[idx]));
   bumpN[1] = ((1.0-ufrac)*(1.0-vfrac)*(double)(map[idx+1]));
   bumpN[2] = ((1.0-ufrac)*(1.0-vfrac)*(double)(map[idx+2]));

   // Second term umax/vmin
   idx = (int)(vmin)*width*3 + (int)(umax)*3;
   bumpN[0] += (ufrac*(1.0-vfrac)*(double)(map[idx]));
   bumpN[1] += (ufrac*(1.0-vfrac)*(double)(map[idx+1]));
   bumpN[2] += (ufrac*(1.0-vfrac)*(double)(map[idx+2]));

   // Third term umin/vmax
   idx = (int)(vmax)*width*3 + (int)(umin)*3;
   bumpN[0] += ((1.0-ufrac)*vfrac*(double)(map[idx]));
   bumpN[1] += ((1.0-ufrac)*vfrac*(double)(map[idx+1]));
   bumpN[2] += ((1.0-ufrac)*vfrac*(double)(map[idx+2]));

   // Fourth term umax/vmax
   idx = (int)(vmax)*width*3 + (int)(umax)*3;
   bumpN[0] += (ufrac*vfrac*(double)(map[idx]));
   bumpN[1] += (ufrac*vfrac*(double)(map[idx+1]));
   bumpN[2] += (ufrac*vfrac*(double)(map[idx+2]));
}

//////////////////////////////////////////////////////////////////////////
// Get a pixel from the image.
//////////////////////////////////////////////////////////////////////////
static inline void 
ReadPixel (BYTE* image, int width, int off, pixel* pix, int x, int y)
{
#ifdef _DEBUG
   if ((image == NULL) || (pix == NULL))
   {
      NmPrint ("ERROR: NULL pointer passed to Readpixel!\n");
      exit (-1);
   }
#endif
   int idx = y*width*off + x*off;
   if (off > 0)
   {
      pix->red = image[idx];
   }
   if (off > 1)
   {
      pix->blue = image[idx + 1];
   }
   if (off > 2)
   {
      pix->green = image[idx + 2];
   }
}

//////////////////////////////////////////////////////////////////////////
// Read a normal map from disk and convert it into a floating point image
//////////////////////////////////////////////////////////////////////////
static void
ReadNormalMap (char* name, int* width, int* height, int* numComponents,
               float** normals)
{
   // Check parameters
   if ((name == NULL) || (width == NULL) ||(height == NULL) || 
       (numComponents == NULL) || (normals == NULL))
   {
      NmPrint ("ERROR: NULL pointer passed to ReadNormalMap!\n");
      exit (-1);
   }
   if (strlen (name) < 1)
   {
      NmPrint ("ERROR: No file name passed to ReadNormalMap!\n");
      exit (-1);
   }

   // First read in the normals.
   FILE* fp = fopen (name, "rb");
   if (fp == NULL)
   {
      NmPrint ("ERROR: Unable to open %s\n", name);
      exit (-1);
   }
   BYTE* image;
   int bpp;
   if (!TGAReadImage (fp, width, height, &bpp, &image))
   {
      NmPrint ("ERROR: Unable to read %s\n", name);
      exit (-1);
   }
   fclose (fp);

   // Figure out if this image makes for a normal map.
   int off = 0;
   switch (bpp)
   {
      case 8:
      case 16:
         NmPrint ("ERROR: Normal map should be at 32 or 24bits per pixel!\n");
         exit (-1);
      case 24:
         off = 3;
         break;
      case 32:
         off = 4;
         break;
      default:
         NmPrint ("ERROR: Unhandled bit depth for bump map!\n");
         exit (-1);
   }

   // Allocate the new normal array.
   (*normals) = new float [(*width)*(*height)*3];
   if ((*normals) == NULL)
   {
      NmPrint ("ERROR: Unable to allocate normal map memory!");
      exit (-1);
   }
   
   // Convert into a floating point normals
   pixel pix;
   for (int y = 0; y < (*height); y++)
   {
      for (int x = 0; x < (*width); x++)
      {
         // Read texel
         ReadPixel (image, (*width), off, &pix,  x, y);

         // Convert to a normal, special case 0.0 normal.
         double nn[3];
         if (pix.red == 127 && (pix.green == 127) && (pix.blue == 127))
         {
            nn[0] = 0.0;
            nn[1] = 0.0;
            nn[2] = 0.0;
         }
         else
         {
            nn[0] = (double)(UNPACKBYTE_MINUS1TO1(pix.green));
            nn[1] = (double)(UNPACKBYTE_MINUS1TO1(pix.blue));
            nn[2] = (double)(UNPACKBYTE_MINUS1TO1(pix.red));
            Normalize (nn);
         }

         // Stuff it into texture
         int idx = y*(*width)*3 + x*3;
         (*normals)[idx] = (float)(nn[0]);
         (*normals)[idx+1] = (float)(nn[1]);
         (*normals)[idx+2] = (float)(nn[2]);
      }
   }
} // ReadNormalMap

//////////////////////////////////////////////////////////////////////////
// Reads a height field file from disk and converts it into a normal for
// use in perturbing the normals generated from the high res model
//////////////////////////////////////////////////////////////////////////
static void
GetBumpMapFromHeightMap (char* bumpName, int* bumpWidth,  int* bumpHeight,
                         float** bumpMap, float scale)
{
   if ((bumpWidth == NULL) ||(bumpHeight == NULL) || (bumpMap == NULL))
   {
      NmPrint ("ERROR: NULL pointer passed to GetBumpMapFromHeightMap!\n");
      exit (-1);
   }

   // No height field
   if (bumpName == NULL)
   {
      (*bumpWidth) = 0;
      (*bumpHeight) = 0;
      (*bumpMap) = NULL;
      return;
   }

   // First read in the heightmap.
   FILE* fp = fopen (bumpName, "rb");
   if (fp == NULL)
   {
      NmPrint ("ERROR: Unable to open %s\n", bumpName);
      exit (-1);
   }
   BYTE* image;
   int bpp;
   if (!TGAReadImage (fp, bumpWidth, bumpHeight, &bpp, &image))
   {
      NmPrint ("ERROR: Unable to read %s\n", bumpName);
      exit (-1);
   }
   fclose (fp);

   // Allocate normal image.
   (*bumpMap) = new float [(*bumpWidth)*(*bumpHeight)*3];
   if ((*bumpMap) == NULL)
   {
      NmPrint ("ERROR: Unable to allocate normal map memory!");
      exit (-1);
   }
   
   // Get offset
   int off = 0;
   switch (bpp)
   {
      case 8:
         off = 1;
         break;
      case 16:
         off = 2;
         break;
      case 24:
         off = 3;
         break;
      case 32:
         off = 4;
         break;
      default:
         NmPrint ("ERROR: Unhandled bit depth for bump map!\n");
         exit (-1);
   }

   // Sobel the image to get normals.
   float dX, dY, nX, nY, nZ, oolen;
   pixel pix;
   for (int y = 0; y < (*bumpHeight); y++)
   {
      for (int x = 0; x < (*bumpWidth); x++)
      {
         // Do Y Sobel filter
         ReadPixel (image, (*bumpWidth), off,
                    &pix, (x-1+(*bumpWidth)) % (*bumpWidth), (y+1) % (*bumpHeight));
         dY  = ((((float) pix.red) / 255.0f)*scale) * -1.0f;
            
         ReadPixel(image, (*bumpWidth), off,
                   &pix,   x   % (*bumpWidth), (y+1) % (*bumpHeight));
         dY += ((((float) pix.red) / 255.0f)*scale) * -2.0f;
            
         ReadPixel(image, (*bumpWidth), off,
                   &pix, (x+1) % (*bumpWidth), (y+1) % (*bumpHeight));
         dY += ((((float) pix.red) / 255.0f)*scale) * -1.0f;
            
         ReadPixel(image, (*bumpWidth), off,
                   &pix, (x-1+(*bumpWidth)) % (*bumpWidth), (y-1+(*bumpHeight)) % (*bumpHeight));
         dY += ((((float) pix.red) / 255.0f)*scale) *  1.0f;
            
         ReadPixel(image, (*bumpWidth), off,
                   &pix,   x   % (*bumpWidth), (y-1+(*bumpHeight)) % (*bumpHeight));
         dY += ((((float) pix.red) / 255.0f)*scale) *  2.0f;
            
         ReadPixel(image, (*bumpWidth), off,
                   &pix, (x+1) % (*bumpWidth), (y-1+(*bumpHeight)) % (*bumpHeight));
         dY += ((((float) pix.red) / 255.0f)*scale) *  1.0f;
            
         // Do X Sobel filter
         ReadPixel(image, (*bumpWidth), off,
                   &pix, (x-1+(*bumpWidth)) % (*bumpWidth), (y-1+(*bumpHeight)) % (*bumpHeight));
         dX  = ((((float) pix.red) / 255.0f)*scale) * -1.0f;
            
         ReadPixel(image, (*bumpWidth), off,
                   &pix, (x-1+(*bumpWidth)) % (*bumpWidth),   y   % (*bumpHeight));
         dX += ((((float) pix.red) / 255.0f)*scale) * -2.0f;
            
         ReadPixel(image, (*bumpWidth), off,
                   &pix, (x-1+(*bumpWidth)) % (*bumpWidth), (y+1) % (*bumpHeight));
         dX += ((((float) pix.red) / 255.0f)*scale) * -1.0f;
            
         ReadPixel(image, (*bumpWidth), off,
                   &pix, (x+1) % (*bumpWidth), (y-1+(*bumpHeight)) % (*bumpHeight));
         dX += ((((float) pix.red) / 255.0f)*scale) *  1.0f;
            
         ReadPixel(image, (*bumpWidth), off,
                   &pix, (x+1) % (*bumpWidth),   y   % (*bumpHeight));
         dX += ((((float) pix.red) / 255.0f)*scale) *  2.0f;
            
         ReadPixel(image, (*bumpWidth), off,
                   &pix, (x+1) % (*bumpWidth), (y+1) % (*bumpHeight));
         dX += ((((float) pix.red) / 255.0f)*scale) *  1.0f;
            
            
         // Cross Product of components of gradient reduces to
         nX = -dX;
         nY = -dY;
         nZ = 1;
            
         // Normalize
         oolen = 1.0f/((float) sqrt(nX*nX + nY*nY + nZ*nZ));
         nX *= oolen;
         nY *= oolen;
         nZ *= oolen;

         int idx = y*(*bumpWidth)*3 + x*3;
         (*bumpMap)[idx] = nX;
         (*bumpMap)[idx+1] = nY;
         (*bumpMap)[idx+2] = nZ;
      }
   }
} // GetBumpMapFromHeightMap

//////////////////////////////////////////////////////////////////////////
// Figure out the name for the output file, we possibly need to tack on
// a number designating the mip level.
//////////////////////////////////////////////////////////////////////////
static void
GetOutputFilename (char* name, char* original, int mipCount)
{
   // Check arguments
   if ((name == NULL) || (original == NULL))
   {
      NmPrint ("ERROR: NULL pointer passed to GetOutputFilename\n");
      exit (-1);
   }

   // Copy over name
   strcpy (name, original);

   // If we are computing the mipmaps tack on an appropriate number
   if (gComputeMipLevels > 0)
   {
      char* dot = strrchr (name, '.');
      char ext[24];
      strcpy (ext, dot);
      if (dot != NULL)
      {
         sprintf (dot, "%02d%s", mipCount, ext);
      }
      else
      {
         sprintf (ext, "%02d.tga", mipCount);
         strcat (name, ext);
      }
   }
}

//////////////////////////////////////////////////////////////////////////
// Normalize the given image.
//////////////////////////////////////////////////////////////////////////
static void
ComputeNextMipLevel (int* lastWidth, int* lastHeight)
{
#ifdef _DEBUG
   if ( (lastWidth == NULL) || (lastHeight == NULL) )
   {
      NmPrint ("ERROR: Bad parameter to ComputeNextMipLevel!\n");
      exit (-1);
   }
#endif
   if (gComputeMipLevels > 0)
   {
      (*lastWidth) = gWidth;
      (*lastHeight) = gHeight;
      gWidth = gWidth / 2;
      gHeight = gHeight / 2;
      if (gWidth < 1)
      {
         if (gHeight > 1)
         {
            gWidth = 1;
         }
      }
      else if (gHeight < 1)
      {
         gHeight = 1;
      }
   }
   else
   {
      gWidth = 0;
      gHeight = 0;
   }
}

//////////////////////////////////////////////////////////////////////////
// Normalize the given image.
//////////////////////////////////////////////////////////////////////////
static void
NormalizeImage (float* img, int numComponents)
{
#ifdef _DEBUG
   if ((img == NULL) || (numComponents < 1))
   {
      NmPrint ("ERROR: Bad parameter to NormalizeImage\n");
      exit (-1);
   }
#endif
   NmPrint ("\rRenormalize\n");
   for (int y = 0; y < gHeight; y++)
   {
      ShowSpinner ();
      for (int x = 0; x < gWidth; x++)
      {
         // See if this pixel is empty
         int idx = y*gWidth*numComponents + x*numComponents;
         if ((fabs(img[idx]) >= EPSILON) ||
             (fabs(img[idx + 1]) >= EPSILON) || 
             (fabs(img[idx + 2]) >= EPSILON))
         {
            double iNorm[3];
            iNorm[0] = img[idx + 0];
            iNorm[1] = img[idx + 1];
            iNorm[2] = img[idx + 2];
            Normalize (iNorm);
            img[idx + 0] = (float)iNorm[0];
            img[idx + 1] = (float)iNorm[1];
            img[idx + 2] = (float)iNorm[2];
         }
      }
   }
   if (gQuiet == NM_VERBOSE)
   {
      printf ("\r");
   }
} // NormalizeImage

//////////////////////////////////////////////////////////////////////////
// Dilate image, by expanding neighbors to filled pixels. This fixes
// issues seen when blending. Otherwise the blend would be performed
// against a zero vector. This routine swaps the buffers around, in the 
// end img will contain the output and scratch will contain the last iteration
// of the dilation.
//////////////////////////////////////////////////////////////////////////
static void
DilateImage (float** img, float** scratch, int numComponents)
{
   if ((img == NULL) || (scratch == NULL))
   {
      NmPrint ("ERROR: NULL pointer passed to DilateImage\n");
      exit (-1);
   }
   NmPrint ("\rFilling borders\n");
   for (int f = 0; f < gDilateTexels; f++)
   {
      // Loop over the old image and create a new one.
      ShowSpinner ();
      for (int y = 0; y < gHeight; y++)
      {
         for (int x = 0; x < gWidth; x++)
         {
            // See if this pixel is empty
            int idx = y*gWidth*numComponents + x*numComponents;
            if ((fabs((*img)[idx]) <= EPSILON) &&
                (fabs((*img)[idx + 1]) <= EPSILON) && 
                (fabs((*img)[idx + 2]) <= EPSILON))
            {
               // Accumulate the near filled pixels
               double nn[5] = {0.0, 0.0, 0.0, 0.0, 0.0};
               int ncount = 0;
               int yv;
               int xv;
               int nidx;
                        
               // -Y
               yv = (y - 1)%gHeight;
               if (yv < 0) 
               {
                  yv = gHeight -1;
               }
               xv = (x)%gWidth;
               if (xv < 0) 
               {
                  xv = gWidth -1;
               }
               nidx = yv*gWidth*numComponents + xv*numComponents;
               if ((fabs((*img)[nidx]) > EPSILON) ||
                   (fabs((*img)[nidx + 1]) > EPSILON) ||
                   (fabs((*img)[nidx + 2]) > EPSILON))
               {
                  nn[0] += (*img)[nidx];
                  nn[1] += (*img)[nidx+1];
                  nn[2] += (*img)[nidx+2];
                  ncount++;
               }
                        
               // +Y
               yv = (y + 1)%gHeight;
               if (yv < 0) 
               {
                  yv = gHeight -1;
               }
               xv = (x)%gWidth;
               if (xv < 0) 
               {
                  xv = gWidth -1;
               }
               nidx = yv*gWidth*numComponents + xv*numComponents;
               if ((fabs((*img)[nidx]) > EPSILON) ||
                   (fabs((*img)[nidx + 1]) > EPSILON) ||
                   (fabs((*img)[nidx + 2]) > EPSILON))
               {
                  nn[0] += (*img)[nidx];
                  nn[1] += (*img)[nidx+1];
                  nn[2] += (*img)[nidx+2];
                  ncount++;
               }
               
               // -X
               yv = (y)%gHeight;
               if (yv < 0) 
               {
                  yv = gHeight -1;
               }
               xv = (x-1)%gWidth;
               if (xv < 0) 
               {
                  xv = gWidth -1;
               }
               nidx = yv*gWidth*numComponents + xv*numComponents;
               if ((fabs((*img)[nidx]) > EPSILON) ||
                   (fabs((*img)[nidx + 1]) > EPSILON) ||
                   (fabs((*img)[nidx + 2]) > EPSILON))
               {
                  nn[0] += (*img)[nidx];
                  nn[1] += (*img)[nidx+1];
                  nn[2] += (*img)[nidx+2];
                  ncount++;
               }
                        
               // -X
               yv = (y)%gHeight;
               if (yv < 0) 
               {
                  yv = gHeight -1;
               }
               xv = (x+1)%gWidth;
               if (xv < 0) 
               {
                  xv = gWidth -1;
               }
               nidx = yv*gWidth*numComponents + xv*numComponents;
               if ((fabs((*img)[nidx]) > EPSILON) ||
                   (fabs((*img)[nidx + 1]) > EPSILON) ||
                   (fabs((*img)[nidx + 2]) > EPSILON))
               {
                  nn[0] += (*img)[nidx];
                  nn[1] += (*img)[nidx+1];
                  nn[2] += (*img)[nidx+2];
                  ncount++;
               }
                        
               // If we found some neighbors that were filled, fill
               // this one with the average, otherwise copy
               if (ncount > 0)
               {
                  Normalize (nn);
                  (*scratch)[idx] = (float)nn[0];
                  (*scratch)[idx + 1] = (float)nn[1];
                  (*scratch)[idx + 2] = (float)nn[2];
               }
               else
               {
                  (*scratch)[idx] = (*img)[idx];
                  (*scratch)[idx + 1] = (*img)[idx  + 1];
                  (*scratch)[idx + 2] = (*img)[idx + 2];
               }
            } // end if this pixel is empty
            else
            {
               (*scratch)[idx] = (*img)[idx];
               (*scratch)[idx + 1] = (*img)[idx + 1];
               (*scratch)[idx + 2] = (*img)[idx + 2];
            }
         } // end for x
      } // end for y
      
      // Swap images
      float* i1 = (*scratch);
      (*scratch) = (*img);
      (*img) = i1;
   } // end for f
   if (gQuiet == NM_VERBOSE)
   {
      printf ("\r");
   }
} // DilateImage

//////////////////////////////////////////////////////////////////////////
// Box filter image. Last width determines if this routine reduces the
// image or just blurs it.
//////////////////////////////////////////////////////////////////////////
static void
BoxFilter (float* result, float* source, int lastWidth, int numComponents)
{
   // Check arguments.
   if ((result == NULL) || (source == NULL))
   {
      NmPrint ("ERROR: NULL pointer passed to BoxFilter!\n");
      exit (-1);
   }
   if (lastWidth < 1)
   {
      NmPrint ("ERROR: Bad width %d passed to BoxFilter!\n", lastWidth);
      exit (-1);
   }

   // Figure out width and height
   int height = gHeight;
   int width = gWidth;
   int mult = 2;
   if (lastWidth == gWidth)
   {
      mult = 1;
      height--;
      width--;
   }
   
   // Now do the filter.
   NmPrint ("Box filtering\n");
   for (int y = 0; y < height; y++)
   {
      ShowSpinner ();
      for (int x = 0; x < width; x++)
      {
         float norm[5] = {0.0f, 0.0f, 0.0f, 0.0f, 0.0f};
         int oldIdx = (y*mult)*lastWidth*numComponents + (x*mult)*numComponents;
         norm[0] += source[oldIdx];
         norm[1] += source[oldIdx+1];
         norm[2] += source[oldIdx+2];
         oldIdx = ((y*mult)+1)*lastWidth*numComponents + (x*mult)*numComponents;
         norm[0] += source[oldIdx];
         norm[1] += source[oldIdx+1];
         norm[2] += source[oldIdx+2];
         oldIdx = ((y*mult)+1)*lastWidth*numComponents + ((x*mult)+1)*numComponents;
         norm[0] += source[oldIdx];
         norm[1] += source[oldIdx+1];
         norm[2] += source[oldIdx+2];
         oldIdx = (y*mult)*lastWidth*numComponents + ((x*mult)+1)*numComponents;
         norm[0] += source[oldIdx];
         norm[1] += source[oldIdx+1];
         norm[2] += source[oldIdx+2];
         int idx = y*gWidth*numComponents + x*numComponents;
         double len = sqrt((norm[0]*norm[0])+(norm[1]*norm[1])+(norm[2]*norm[2]));
         if (len < EPSILON)
         {
            result[idx] = 0.0;
            result[idx + 1] = 0.0;
            result[idx + 2] = 0.0;
         }
         else
         {
            result[idx] = (float)(norm[0]/len);
            result[idx + 1] = (float)(norm[1]/len);
            result[idx + 2] = (float)(norm[2]/len);
         }
      }
   }
   if (gQuiet == NM_VERBOSE)
   {
      printf ("\r");
   }
}

//////////////////////////////////////////////////////////////////////////
// Write the output image in the format specified
//////////////////////////////////////////////////////////////////////////
static void
WriteOutputImage (char* fName, float* img, int numComponents)
{
   if ((fName == NULL) || (img == NULL))
   {
      NmPrint ("ERROR: NULL pointer passed to WriteOutputImage\n");
      exit (-1);
   }
   switch (gOutput)
   {
      case NORM_OUTPUT_8_8_8_TGA:
         {
            // Convert image
            BYTE* outImg = new BYTE[gWidth*gHeight*3];
            NmPrint ("\rConverting to 8x8x8 Targa\n");
            for (int y = 0; y < gHeight; y++)
            {
               ShowSpinner ();
               for (int x = 0; x < gWidth; x++)
               {
                  int idx = y*gWidth*numComponents + x*numComponents;
                  int oIdx = y*gWidth*3 + x*3;
                  outImg[oIdx + 0] = PACKINTOBYTE_MINUS1TO1(img[idx]);
                  outImg[oIdx + 1] = PACKINTOBYTE_MINUS1TO1(img[idx+1]);
                  outImg[oIdx + 2] = PACKINTOBYTE_MINUS1TO1(img[idx+2]);
               }
            }
            if (gQuiet == NM_VERBOSE)
            {
               printf ("\r");
            }
               
            // Write out image
            FILE* fp = fopen (fName, "wb");
            if (fp == NULL)
            {
               NmPrint ("ERROR: Unable to open %s\n", fName);
               exit (-1);
            }
            if (TGAWriteImage (fp, gWidth, gHeight, 24, outImg) != true)
            {
               NmPrint ("ERROR: Unable to write %s\n", fName);
               fclose (fp);
               exit (-1);
            }
            fclose (fp);
            NmPrint ("Wrote %s\n", fName);
            delete [] outImg;
         }
         break;

      case NORM_OUTPUT_8_8_8_8_TGA:
         {
            // Convert image
            BYTE* outImg = new BYTE[gWidth*gHeight*4];
            NmPrint ("\rConverting to 8x8x8x8 Targa\n");
            for (int y = 0; y < gHeight; y++)
            {
               ShowSpinner ();
               for (int x = 0; x < gWidth; x++)
               {
                  int idx = y*gWidth*numComponents + x*numComponents;
                  int oIdx = y*gWidth*4 + x*4;
                  outImg[oIdx + 0] = PACKINTOBYTE_MINUS1TO1(img[idx]);
                  outImg[oIdx + 1] = PACKINTOBYTE_MINUS1TO1(img[idx+1]);
                  outImg[oIdx + 2] = PACKINTOBYTE_MINUS1TO1(img[idx+2]);
               }
            }
            if (gQuiet == NM_VERBOSE)
            {
               printf ("\r");
            }
               
            // Write out image
            FILE* fp = fopen (fName, "wb");
            if (fp == NULL)
            {
               NmPrint ("ERROR: Unable to open %s\n", fName);
               exit (-1);
            }
            if (TGAWriteImage (fp, gWidth, gHeight, 32, outImg) != true)
            {
               NmPrint ("ERROR: Unable to write %s\n", fName);
               fclose (fp);
               exit (-1);
            }
            fclose (fp);
            NmPrint ("Wrote %s\n", fName);
            delete [] outImg;
         }
         break;
            
      case NORM_OUTPUT_EXP_TGA:
         {
            // Convert image
            BYTE* outImg = new BYTE[gWidth*gHeight*4];
            NmExpPixel jp;
            NmPrint ("\rConverting to experimental format\n");
            for (int y = 0; y < gHeight; y++)
            {
               ShowSpinner ();
               for (int x = 0; x < gWidth; x++)
               {
                  int fIdx = y*gWidth*numComponents + x*numComponents;
                  ConvertToExpPixel (img[fIdx], img[fIdx+1], img[fIdx+2], &jp);
                  int idx = y*gWidth*4 + x*4;
                  outImg[idx + 0] = jp.r;
                  outImg[idx + 1] = jp.g;
                  outImg[idx + 2] = jp.b;
                  outImg[idx + 3] = jp.a;
               }
            }
            if (gQuiet == NM_VERBOSE)
            {
               printf ("\r");
            }
               
            // Write out image
            FILE* fp = fopen (fName, "wb");
            if (fp == NULL)
            {
               NmPrint ("ERROR: Unable to open %s\n", fName);
               exit (-1);
            }
            if (TGAWriteImage (fp, gWidth, gHeight, 32, outImg) != true)
            {
               NmPrint ("ERROR: Unable to write %s\n", fName);
               fclose (fp);
               exit (-1);
            }
            fclose (fp);
            NmPrint ("Wrote %s\n", fName);
            delete [] outImg;
         }
         break;
            
      case NORM_OUTPUT_16_16_ARG:
         {
            // Convert
            short* outImg = new short[gWidth*gHeight*2];
            NmPrint ("\rConverting to 16x16 signed format\n");
            for (int y = 0; y < gHeight; y++)
            {
               ShowSpinner ();
               for (int x = 0; x < gWidth; x++)
               {
                  int fIdx = y*gWidth*numComponents + x*numComponents;
                  int idx = y*gWidth*2 + x*2;
                  outImg[idx + 0] = PACKINTOSHORT_SMINUS1TO1(img[fIdx]);
                  outImg[idx + 1] = PACKINTOSHORT_SMINUS1TO1(img[fIdx+1]);
               }
            }
            if (gQuiet == NM_VERBOSE)
            {
               printf ("\r");
            }
               
            // Now write the image.
            AtiBitmap atiOut;
            atiOut.width = gWidth;
            atiOut.height = gHeight;
            atiOut.bitDepth = 32;
            atiOut.format = ATI_BITMAP_FORMAT_S1616;
            atiOut.pixels = (unsigned char*)(outImg);
            if (!AtiWrite3DARGImageFile (&atiOut, fName))
            {
               NmPrint ("ERROR: Unable to write %s\n", fName);
               exit (-1);
            }
            delete [] outImg;
            NmPrint ("Wrote %s\n", fName);
         }
         break;
            
      case NORM_OUTPUT_16_16_16_16_ARG:
         {
            // Convert image
            short* outImg = new short[gWidth*gHeight*4];
            NmPrint ("\rConverting to 16x16x16x16 signed format\n");
            for (int y = 0; y < gHeight; y++)
            {
               ShowSpinner ();
               for (int x = 0; x < gWidth; x++)
               {
                  int fIdx = y*gWidth*numComponents + x*numComponents;
                  int idx = y*gWidth*4 + x*4;
                  outImg[idx + 0] = PACKINTOSHORT_SMINUS1TO1(img[fIdx]);
                  outImg[idx + 1] = PACKINTOSHORT_SMINUS1TO1(img[fIdx+1]);
                  outImg[idx + 2] = PACKINTOSHORT_SMINUS1TO1(img[fIdx+2]);
               }
            }
            if (gQuiet == NM_VERBOSE)
            {
               printf ("\r");
            }

            // Now write the image.
            AtiBitmap atiOut;
            atiOut.width = gWidth;
            atiOut.height = gHeight;
            atiOut.bitDepth = 64;
            atiOut.format = ATI_BITMAP_FORMAT_S16161616;
            atiOut.pixels = (unsigned char*)(outImg);
            if (!AtiWrite3DARGImageFile (&atiOut, fName))
            {
               NmPrint ("ERROR: Unable to write %s\n", fName);
               exit (-1);
            }
            delete [] outImg;
            NmPrint ("Wrote %s\n", fName);
         }
         break;
            
      case NORM_OUTPUT_10_10_10_2_ARG:
         {
            // Convert image
            long* outImg = new long[gWidth*gHeight];
            NmPrint ("\rConverting to 10x10x10x2 format\n");
            for (int y = 0; y < gHeight; y++)
            {
               ShowSpinner ();
               for (int x = 0; x < gWidth; x++)
               {
                  int fIdx = y*gWidth*numComponents + x*numComponents;
                  int idx = y*gWidth + x;
#define MASK9  0x1ff
#define MASK10 0x3ff
#define MASK11 0x7ff
                  outImg[idx] = ((((unsigned long)(img[fIdx  ]*MASK9) + MASK9)&MASK10)<<22) |
                                ((((unsigned long)(img[fIdx+1]*MASK9) + MASK9)&MASK10)<<12) |
                                ((((unsigned long)(img[fIdx+2]*MASK9) + MASK9)&MASK10)<<2);
               }
            }
            if (gQuiet == NM_VERBOSE)
            {
               printf ("\r");
            }

            // Now write the image.
            AtiBitmap atiOut;
            atiOut.width = gWidth;
            atiOut.height = gHeight;
            atiOut.bitDepth = 32;
            atiOut.format = ATI_BITMAP_FORMAT_1010102;
            atiOut.pixels = (unsigned char*)(outImg);
            if (!AtiWrite3DARGImageFile (&atiOut, fName))
            {
               NmPrint ("ERROR: Unable to write %s\n", fName);
               exit (-1);
            }
            delete [] outImg;
            NmPrint ("Wrote %s\n", fName);
         }
         break;

      case NORM_OUTPUT_10_10_10_2_ARG_MS:
         {
            // Convert image
            short* outImg = new short[gWidth*gHeight*4];
            NmPrint ("\rConverting to 10x10x10x2 test format (bit chopped 16bpc)\n");
            for (int y = 0; y < gHeight; y++)
            {
               ShowSpinner ();
               for (int x = 0; x < gWidth; x++)
               {
#define MASK_10BITS 0xffc0
                  int fIdx = y*gWidth*numComponents + x*numComponents;
                  int idx = y*gWidth*4 + x*4;
                  outImg[idx + 0] = (PACKINTOSHORT_MINUS1TO1(img[fIdx]))&MASK_10BITS;
                  outImg[idx + 1] = (PACKINTOSHORT_MINUS1TO1(img[fIdx+1]))&MASK_10BITS;
                  outImg[idx + 2] = (PACKINTOSHORT_MINUS1TO1(img[fIdx+2]))&MASK_10BITS;
                  outImg[idx + 3] = PACKINTOSHORT_MINUS1TO1(0);
               }
            }
            if (gQuiet == NM_VERBOSE)
            {
               printf ("\r");
            }

            // Now write the image.
            AtiBitmap atiOut;
            atiOut.width = gWidth;
            atiOut.height = gHeight;
            atiOut.bitDepth = 64;
            atiOut.format = ATI_BITMAP_FORMAT_16161616;
            atiOut.pixels = (unsigned char*)(outImg);
            if (!AtiWrite3DARGImageFile (&atiOut, fName))
            {
               NmPrint ("ERROR: Unable to write %s\n", fName);
               exit (-1);
            }
            delete [] outImg;
            NmPrint ("Wrote %s\n", fName);
         }
         break;

      case NORM_OUTPUT_11_11_10_ARG_MS:
         {
            // Convert image
            short* outImg = new short[gWidth*gHeight*4];
            NmPrint ("\rConverting to 11x11x10 test format (bit chopped 16bpc)\n");
            for (int y = 0; y < gHeight; y++)
            {
               ShowSpinner ();
               for (int x = 0; x < gWidth; x++)
               {
#define MASK_11BITS 0xffe0
                  int fIdx = y*gWidth*numComponents + x*numComponents;
                  int idx = y*gWidth*4 + x*4;
                  outImg[idx + 0] = (PACKINTOSHORT_MINUS1TO1(img[fIdx]))&MASK_11BITS;
                  outImg[idx + 1] = (PACKINTOSHORT_MINUS1TO1(img[fIdx+1]))&MASK_11BITS;
                  outImg[idx + 2] = (PACKINTOSHORT_MINUS1TO1(img[fIdx+2]))&MASK_10BITS;
                  outImg[idx + 3] = PACKINTOSHORT_MINUS1TO1(0);
               }
            }
            if (gQuiet == NM_VERBOSE)
            {
               printf ("\r");
            }
            
            // Now write the image.
            AtiBitmap atiOut;
            atiOut.width = gWidth;
            atiOut.height = gHeight;
            atiOut.bitDepth = 64;
            atiOut.format = ATI_BITMAP_FORMAT_16161616;
            atiOut.pixels = (unsigned char*)(outImg);
            if (!AtiWrite3DARGImageFile (&atiOut, fName))
            {
               NmPrint ("ERROR: Unable to write %s\n", fName);
               exit (-1);
            }
            delete [] outImg;
            NmPrint ("Wrote %s\n", fName);
         }
         break;
            
      default:
         NmPrint ("ERROR: Unknown output format!\n");
         exit (-1);
   } // end switch on output type.

} // WriteOutputImage

//////////////////////////////////////////////////////////////////////////
// Check argument flags and set the appropriate values.
//////////////////////////////////////////////////////////////////////////
static void
TestArgs (char* args)
{
   // Misc flags
   if (strstr (args, "m") != NULL)
   {
      gComputeMipLevels = MIP_RECOMPUTE;
   }
   if (strstr (args, "M") != NULL)
   {
      gComputeMipLevels = MIP_BOX;
   }
   if (strstr (args, "e") != NULL)
   {
      gExpandTexels = false;
   }
   if (strstr (args, "E") != NULL)
   {
      gDilateTexels = 15;
   }
   if (strstr (args, "g") != NULL)
   {
      gDilateTexels = 20;
   }
   if (strstr (args, "G") != NULL)
   {
      gDilateTexels = 30;
   }
   if (strstr (args, "h") != NULL)
   {
      gOutput = NORM_OUTPUT_16_16_ARG;
   }
   if (strstr (args, "H") != NULL)
   {
      gOutput = NORM_OUTPUT_16_16_16_16_ARG;
   }
   if (strstr (args, "i") != NULL)
   {
      gOutput = NORM_OUTPUT_10_10_10_2_ARG;
   }
   if (strstr (args, "s") != NULL)
   {
      gOutput = NORM_OUTPUT_10_10_10_2_ARG_MS;
   }
   if (strstr (args, "S") != NULL)
   {
      gOutput = NORM_OUTPUT_11_11_10_ARG_MS;
   }
   if (strstr (args, "B") != NULL)
   {
      gBoxFilter = true;
   }
   if (strstr (args, "q") != NULL)
   {
      gQuiet = NM_QUIET;
   }
   if (strstr (args, "Q") != NULL)
   {
      gQuiet = NM_SILENT;
   }
} // TestArgs

//////////////////////////////////////////////////////////////////////////
// Process arguments
//////////////////////////////////////////////////////////////////////////
static void
ProcessArgs (int argc, char **argv, char** nm1Name, char** nm2Name,
             char** outName, double* bumpScale)
{
   // A common usage line so everything is consistent.
   char* flags = NULL;
   static char* usage = "Usage: NormalMapCombiner [-BeEgGhHimMqQ] normalmap1 bump/normalMap2 [heightscale] outputname\n";

   // Print out verbose message if 0 or 1 arguments given.
   if ((argc == 1) || (argc == 2))
   {
      NmPrint (usage);
      NmPrint ("                     h  - Output 16x16 .arg texture\n");
      NmPrint ("                     H  - Output 16x16x16x16 .arg texture\n");
      NmPrint ("                     i  - Output 10x10x10x2 .arg texture\n");
      NmPrint ("                     m  - Create mip chain (remap)\n");
      NmPrint ("                     M  - Create mip chain (box filter)\n");
      NmPrint ("                     B  - Box filter final image\n");
      NmPrint ("                     e  - Don't expand border texels\n");
      NmPrint ("                     E  - expand border texels more (15 texels)\n");
      NmPrint ("                     g  - more dilation (20 texels)\n");
      NmPrint ("                     G  - even more dilation (30 texels)\n");
      NmPrint ("                     q  - Quiet mode, no spinners or percentages\n");
      NmPrint ("                     Q  - Very quiet mode, no messages\n");
      exit (-1);
   }

   // Make sure the right number of arguments are present.
   if ((argc != 4) && (argc != 5) && (argc != 6))
   {
      NmPrint (usage);
      exit (-1);
   }

   // Straight up, nothing fancy
   if (argc == 4)
   {
      (*nm1Name) = argv[1];
      (*nm2Name) = argv[2];
      (*outName) = argv[3];
   }

   // With flags or heightscale
   if (argc == 5)
   {
      if (strstr (argv[1], "-") != NULL)
      {
         (*nm1Name) = argv[2];
         (*nm2Name) = argv[3];
         (*outName) = argv[4];
         TestArgs (argv[1]);
         flags = argv[1];
      }
      else
      {
         gBumpMap = true;
         (*nm1Name) = argv[1];
         (*nm2Name) = argv[2];
         (*bumpScale) = atof (argv[3]);
         (*outName) = argv[4];
      }
   }

   // With flags and heightscale
   if (argc == 6)
   {
      (*nm1Name) = argv[2];
      (*nm2Name) = argv[3];
      (*bumpScale) = atof (argv[4]);
      (*outName) = argv[5];
      TestArgs (argv[1]);
      flags = argv[1];
   }

   // Print out options
   NmPrint (versionString);
   if (flags != NULL)
   {
      NmPrint ("Flags: %s\n", flags);
   }
   else
   {
      NmPrint ("Flags:\n");
   }
   switch (gComputeMipLevels)
   {
      case MIP_NONE:
         NmPrint ("No mipmap generation\n");
         break;
      case MIP_RECOMPUTE:
         NmPrint ("Re-cast mipmap generation.\n");
         break;
      case MIP_BOX:
         NmPrint ("Box filter mip map generation.\n");
         break;
      default:
         NmPrint ("Unknown mip map generation\n");
         break;
   }
   if (gExpandTexels)
   {
      NmPrint ("Expand border texels (%d texels)\n", gDilateTexels);
   }
   else
   {
      NmPrint ("Don't Expand border texels\n");
   }
   if (gBoxFilter)
   {
      NmPrint ("Postprocess box filter\n");
   }
   else
   {
      NmPrint ("No postprocess filter\n");
   }
} // ProcessArgs

//////////////////////////////////////////////////////////////////////////
// Entry point
//////////////////////////////////////////////////////////////////////////
int 
main (int argc, char **argv)
{

   // Get the arguments.
   char* nm1Name = NULL;
   char* nm2Name = NULL;
   char* outName = NULL;
   double bs;
   ProcessArgs (argc, argv, &nm1Name, &nm2Name, &outName, &bs);
   float bumpScale = (float)(bs);

   // Read in the starting map
   float* nm1Normals = NULL;
   int numComponents = 3;
   ReadNormalMap (nm1Name, &gWidth, &gHeight, &numComponents, &nm1Normals);

   // Read in the map to be merged onto original map
   float* nm2Normals = NULL;
   int width = 0;
   int height = 0;
   if (gBumpMap)
   {
      GetBumpMapFromHeightMap (nm2Name, &width,  &height, &nm2Normals,
                               bumpScale);
   }
   else
   {
      int nc = 3;
      ReadNormalMap (nm2Name, &width, &height, &nc, &nm2Normals);
      if (nc != numComponents)
      {
         NmPrint ("Normal maps must have the same number of components (%d %d)!\n", numComponents, nc);
         exit (-1);
      }
   }

   // Make sure sizes match
   if ((width != gWidth) || (height != gHeight))
   {
      NmPrint ("Normal map sizes must match! (%dx%d %dx%d)\n", gWidth, gHeight,
               width, height);
   }
   int fullWidth = gWidth;
   int fullHeight = gHeight;

   // Create images
   float* img = new float[gWidth*gHeight*numComponents+numComponents];
   if (img == NULL)
   {
      NmPrint ("ERROR: Unable to allocate texture\n");
      exit (-1);
   }
   float* img2 = new float[gWidth*gHeight*numComponents+numComponents];
   if (img2 == NULL)
   {
      NmPrint ("ERROR: Unable to allocate texture\n");
      exit (-1);
   }

   // loop over mip levels
   int mipCount = 0;
   int lastWidth = gWidth;
   int lastHeight = gHeight;
   while ((gWidth > 0) && (gHeight > 0))
   {
      // A little info
      NmPrint ("Output normal map %d by %d\n", gWidth, gHeight);

      // If this is the first time, or we are recomputing each mip level
      // find the normals by ray casting. SHHH, just box filter...
      if (/*(gComputeMipLevels == MIP_RECOMPUTE) ||*/ (mipCount == 0))
      {
         // Zero out memory
         memset (img, 0, sizeof (float)*gWidth*gHeight*numComponents);
         memset (img2, 0, sizeof (float)*gWidth*gHeight*numComponents);
         
         // Loop over the triangles in the low res model and rasterize them
         // into the normal texture based on the texture coordinates.
         NmPrint ("Merging normals\n");
         if (gQuiet == NM_VERBOSE)
         {
            printf ("  0%%");
         }
         for (int p = 0; p < (gWidth*gHeight); p++)
         {
            // Figure out first normal
            float v0[3];
            v0[0] = nm1Normals[p*numComponents + 0];
            v0[1] = nm1Normals[p*numComponents + 1];
            v0[2] = nm1Normals[p*numComponents + 2];
            float len2 = v0[0]*v0[0] + v0[1]*v0[1] + v0[2]* v0[2];

            // Check to make sure we actually have a vector
            if (len2 > EPSILON)
            {
               // Figure out second normal
               float v1[3];
               v1[0] = nm2Normals[p*numComponents + 0];
               v1[1] = nm2Normals[p*numComponents + 1];
               v1[2] = nm2Normals[p*numComponents + 2];
            
               // This could probably be simplified since we know thtat the
               // from vector never changes in our case.
               float mtx[3][3];
               float from[3] = {0.0, 0.0, 1.0};
               fromToRotation (from, v0, mtx);

               // Now rotate the new normal into position
               float r[3];
               r[0] = v1[0]*mtx[0][0] + v1[1]*mtx[0][1] + v1[2]*mtx[0][2];
               r[1] = v1[0]*mtx[1][0] + v1[1]*mtx[1][1] + v1[2]*mtx[1][2];
               r[2] = v1[0]*mtx[2][0] + v1[1]*mtx[2][1] + v1[2]*mtx[2][2];
               //r[0] = v1[0]*mtx[0][0] + v1[1]*mtx[1][0] + v1[2]*mtx[2][0];
               //r[1] = v1[0]*mtx[0][1] + v1[1]*mtx[1][1] + v1[2]*mtx[2][1];
               //r[2] = v1[0]*mtx[0][2] + v1[1]*mtx[1][2] + v1[2]*mtx[2][2];
               //r[0] = v0[0];
               //r[1] = v0[1];
               //r[2] = v0[2];
               //r[0] = v1[0];
               //r[1] = v1[1];
               //r[2] = v1[2];
               
               // Copy into place
               img[p*numComponents + 0] = r[0];
               img[p*numComponents + 1] = r[1];
               img[p*numComponents + 2] = r[2];
            } // end if we have a vector in the orginal map
         } // end for p (pixels in the normal maps)
         if (gQuiet == NM_VERBOSE)
         {
            printf ("\r100.00%%\r");
         }

         // Now renormalize
         NormalizeImage (img, numComponents);
         
         // Fill unused areas based on surrounding colors to prevent artifacts.
         if (gExpandTexels)
         {
            DilateImage (&img, &img2, numComponents);
         }

         // Box filter image
         if (gBoxFilter)
         {
            BoxFilter (img, img2, gWidth, numComponents);
            float* i1 = img2;
            img2 = img;
            img = i1;
         }
      } // end if we are recomputing mip chains or this is the first time.
      else // Could do compute mip == box here, but it's our only option ATM.
      {
         BoxFilter (img2, img, lastWidth, numComponents);
         float* i1 = img2;
         img2 = img;
         img = i1;
      } // end box filter old image down for mip levels

      // Tack mip chain number onto name if needed then write image.
      char fName[256];
      GetOutputFilename (fName, outName, mipCount);
      WriteOutputImage (fName, img, numComponents);
      // Update gWidth & gHeight here to go to the next mip level
      ComputeNextMipLevel (&lastWidth, &lastHeight);
      mipCount++;
   } // end while we still have mip levels to generate.
   
   return 0;
} // end main

#define ERROR_LOG_FILE "WorldSpaceifierLog.txt"
//=============================================================================
void 
NmErrorLog (const char *szFmt, ...)
{
   // If we are being silent don't print anything.
   if (gQuiet == NM_SILENT)
   {
      return;
   }

   // Print into a local buffer.
   char sz[4096], szMsg[4096];
   va_list va;
   va_start(va, szFmt);
   vsprintf(szMsg, szFmt, va);
   va_end(va);
   
   sprintf(sz, "%s", szMsg);
   
   sz[sizeof(sz)-1] = '\0';
   
#if 1
   printf (sz);
#endif
#ifdef _DEBUG
#if 1
   OutputDebugString(sz);
#endif
#endif
#if 0
   static bool first = true;
   FILE *fp;
   if (first == true)
   {
      fp = fopen(ERROR_LOG_FILE, "w");
      if (fp == NULL)
         return;
      first = false;
   }
   else
   {
      fp = fopen(ERROR_LOG_FILE, "a");
      if (fp == NULL)
         return;
   }
   
   fprintf (fp, sz);
   fclose (fp);
   fflush (stdout);
#endif
}

//=============================================================================
void 
NmErrorLog (char *szFmt)
{
   NmErrorLog((const char *)szFmt);
}
