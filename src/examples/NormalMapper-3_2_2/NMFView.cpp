//=============================================================================
// NMFView.cpp -- A GLUT app to view NMF files.
//=============================================================================
// $File: //depot/3darg/Tools/NormalMapper/NMFView.cpp $ $Revision: 1.1 $ $Author: hax22 $
//=============================================================================
// (C) 2002 ATI Research, Inc., All rights reserved.
//=============================================================================

#ifndef ATI_MAC_OS
 #include <windows.h>
 #include <GL/gl.h>
 #include <GL/glu.h>
 #include <GL/glut.h>
#else
 #include <OpenGL/gl.h>
 #include <OpenGL/glext.h>
 #include <GLUT/glut.h>
 #include <Carbon/Carbon.h>
 #include <string.h>
 
 #include "MacSpecific.h"
#endif
#include <stdio.h>
#include <float.h>
#include <math.h>

#ifndef ATI_MAC_OS
 #include <GL/glati.h>
#endif

#include "NmFileIO.h"
#include "TGAIO.h"
#include "ArgFileIO.h"

// Limit the scope of the for statement
#define for if(0) ; else for

//#define USE_SMD_FILES

// Light parameters
GLfloat gLightpos[] = {0.0f, 0.0f, 0.0f, 1.0f};

// Viewing parameters
GLfloat gViewMat[16];
GLfloat gFov = 50.0;
GLfloat gClipNear = 1.0;
GLfloat gClipFar = 200.0;
GLint gWidth = 512;
GLint gHeight = 512;

// Viewing mode.
enum
{
   NMF_VIEW_DIFFUSE_BUMP = 0,
   NMF_VIEW_NORMAL_MAP,
   NMF_VIEW_LIT_NORMALS,
   NMF_VIEW_ALPHA,
   NMF_VIEW_MAX // Leave last!
};
int gViewMode = NMF_VIEW_DIFFUSE_BUMP;

// A couple of handy colors
static GLfloat gWhite[] = { 1.0f, 1.0f, 1.0f, 1.0f };
static GLfloat gBlack[] = { 0.0f, 0.0f, 0.0f, 1.0f };

// Flags
bool gWireframe = false;
bool gShowNormals = false;
bool gShowTanCoord = false;
bool gShowVectors = false;
bool gInTangentSpace = true;

// Movement variables
GLint gLeftX = 0;
GLint gLeftY = 0;
GLuint gLeftDown = GL_FALSE;
GLint gRightX = 0;
GLint gRightY = 0;
GLuint gRightDown = GL_FALSE;
GLint gMiddleX = 0;
GLint gMiddleY = 0;
GLuint gMiddleDown = GL_FALSE;

// Geomtery data
int gNumTris = 0;
NmRawTriangle* gTriangles = NULL;
int gNumVerts = 0;
NmTangentPointD* gVertices = NULL;
NmIndex* gIndices = NULL;
double* gLightVec = NULL;

// Texture data.
AtiBitmap gTexInfo = { 0, 0, 0, ATI_BITMAP_FORMAT_UNKNOWN, NULL };
bool gHaveTexture = false;
GLuint gTextureID = 0;

#ifndef ATI_MAC_OS
///////////////////////////////////////////////////////////////////////////////
// The purpose of this routine is to read a TGA filename from the user.
// Ideally this comes up as one of those touchy feely Windows windows.
//    hWnd -- The handle of our main window (or NULL)
//    filename -- the place to put the filename
///////////////////////////////////////////////////////////////////////////////
BOOL
GetTextureFileName (HWND hWnd, char *filename)
{
   OPENFILENAME ofn;       // common dialog box structure

   // Initialize OPENFILENAME
   ZeroMemory(&ofn, sizeof(OPENFILENAME));
   ofn.lStructSize = sizeof(OPENFILENAME);
   ofn.hwndOwner = hWnd;
   ofn.lpstrFile = filename;
   ofn.nMaxFile = _MAX_PATH;;
   ofn.lpstrFilter = "Targa(*.TGA)\0*.TGA\0All(*.*)\0*.*\0";
   ofn.nFilterIndex = 1;
   ofn.lpstrFileTitle = NULL;
   ofn.nMaxFileTitle = 0;
   ofn.lpstrInitialDir = NULL;
   ofn.lpstrFileTitle = "NMFView Open Texture File";
   ofn.Flags = OFN_PATHMUSTEXIST | OFN_FILEMUSTEXIST;

   // Display the Open dialog box. 
   return GetOpenFileName(&ofn);
}

///////////////////////////////////////////////////////////////////////////////
// The purpose of this routine is to read a NMF filename from the user.
// Ideally this comes up as one of those touchy feely windows windows.
//    hWnd -- The handle of our main window (or NULL)
//    filename -- the place to put the filename
///////////////////////////////////////////////////////////////////////////////
BOOL
GetNMFFileName (HWND hWnd, char *filename)
{
   OPENFILENAME ofn;       // common dialog box structure

   // Initialize OPENFILENAME
   ZeroMemory(&ofn, sizeof(OPENFILENAME));
   ofn.lStructSize = sizeof(OPENFILENAME);
   ofn.hwndOwner = hWnd;
   ofn.lpstrFile = filename;
   ofn.nMaxFile = _MAX_PATH;;
#ifdef USE_SMD_FILES
   ofn.lpstrFileTitle = "NMFView Open SMD File";
   ofn.lpstrFilter = "SMD(*.SMD)\0*.SMD\0All(*.*)\0*.*\0";
#else
   ofn.lpstrFileTitle = "NMFView Open NMF File";
   ofn.lpstrFilter = "NMF(*.NMF)\0*.NMF\0All(*.*)\0*.*\0";
#endif
   ofn.nFilterIndex = 1;
   ofn.lpstrFileTitle = NULL;
   ofn.nMaxFileTitle = 0;
   ofn.lpstrInitialDir = NULL;
   ofn.Flags = OFN_PATHMUSTEXIST | OFN_FILEMUSTEXIST;

   // Display the Open dialog box. 
   return GetOpenFileName(&ofn);
}
#else
BOOL
GetTextureFileName (int* dummy, char *filename)
{
   // dialog options
   NavDialogOptions dialogOptions;
   NavGetDefaultDialogOptions(&dialogOptions);
   PLstrcpy(dialogOptions.windowTitle, "\pSelect Normal Map Texture (.tga)");
   
   //AEDesc location;
   FSRef fsr;
   AEKeyword key;
   DescType type;
   Size size;
   NavReplyRecord output;
   NavTypeListHandle typeList = (NavTypeListHandle) NewHandleClear (sizeof(NavTypeList) + sizeof(OSType) * 1);
   (**typeList).osTypeCount = 1;
   (**typeList).componentSignature = kNavGenericSignature;
   (**typeList).osType[0] = '????';
   
   NavChooseFile(NULL, &output, &dialogOptions, NULL, NULL, NULL, /*typeList*/NULL, NULL);
   if (output.validRecord)
   {
      AEGetNthPtr(&output.selection, 1, typeFSRef, &key, &type, &fsr, sizeof(fsr), &size);
      
      // get the full file path
      CFStringRef strRef = CFURLGetString(CFURLCreateFromFSRef(NULL, &fsr));     
      char* buffer = (char*)CFStringGetCStringPtr(strRef, 0);
      
      if (buffer)
         buffer += 16; // drop the 'file://localhost/' string

      strcpy(filename, buffer);
   }
   else
   {
      printf("Error: Invalid or no file chosen.\n");     
      exit(-1);
   }
   
   return TRUE;
}

BOOL
GetNMFFileName (int* dummy, char *filename)
{   
   // dialog options
   NavDialogOptions dialogOptions;
   NavGetDefaultDialogOptions(&dialogOptions);
   PLstrcpy(dialogOptions.windowTitle, "\pSelect NormalMapper Data File (.nmf)");

   //AEDesc location;
   FSRef fsr;
   AEKeyword key;
   DescType type;
   Size size;
   NavReplyRecord output;
   NavTypeListHandle typeList = (NavTypeListHandle) NewHandleClear (sizeof(NavTypeList) + sizeof(OSType) * 1);
   (**typeList).osTypeCount = 1;
   (**typeList).componentSignature = kNavGenericSignature;
   (**typeList).osType[0] = '????';
   
   NavChooseFile(NULL, &output, &dialogOptions, NULL, NULL, NULL, /*typeList*/NULL, NULL);
   if (output.validRecord)
   {
      AEGetNthPtr(&output.selection, 1, typeFSRef, &key, &type, &fsr, sizeof(fsr), &size);
      
      // get the full file path
      CFStringRef strRef = CFURLGetString(CFURLCreateFromFSRef(NULL, &fsr));     
      char* buffer = (char*)CFStringGetCStringPtr(strRef, 0);
      
      if (buffer)
         buffer += 16; // drop the 'file://localhost/' string

      strcpy(filename, buffer);
   }
   else
   {
      printf("Error: Invalid or no file chosen.\n");     
      exit(-1);
   }
   
   return TRUE;
}
#endif // ATI_MAC_OS

///////////////////////////////////////////////////////////////////////////////
// Set matrix to identity
//   matrix - Input/Output matrix
///////////////////////////////////////////////////////////////////////////////
void 
matrixIdentity (GLfloat matrix[16])
{
   matrix[ 0] = 1.0;  matrix[ 1] = 0.0;  matrix[ 2] = 0.0;  matrix[ 3] = 0.0;
   matrix[ 4] = 0.0;  matrix[ 5] = 1.0;  matrix[ 6] = 0.0;  matrix[ 7] = 0.0;
   matrix[ 8] = 0.0;  matrix[ 9] = 0.0;  matrix[10] = 1.0;  matrix[11] = 0.0;
   matrix[12] = 0.0;  matrix[13] = 0.0;  matrix[14] = 0.0;  matrix[15] = 1.0;
}

///////////////////////////////////////////////////////////////////////////////
// Invert a matrix. (Matrix MUST be orhtonormal!)
//   in - Input matrix
//   out - Output matrix
///////////////////////////////////////////////////////////////////////////////
void
matrixInvert (GLfloat in[16], GLfloat out[16])
{
   // Transpose rotation
   out[ 0] = in[ 0];  out[ 1] = in[ 4];  out[ 2] = in[ 8];
   out[ 4] = in[ 1];  out[ 5] = in[ 5];  out[ 6] = in[ 9];
   out[ 8] = in[ 2];  out[ 9] = in[ 6];  out[10] = in[10];
  
   // Clear shearing terms
   out[3] = 0.0f; out[7] = 0.0f; out[11] = 0.0f; out[15] = 1.0f;

   // Translation is minus the dot of tranlation and rotations
   out[12] = -(in[12]*in[ 0]) - (in[13]*in[ 1]) - (in[14]*in[ 2]);
   out[13] = -(in[12]*in[ 4]) - (in[13]*in[ 5]) - (in[14]*in[ 6]);
   out[14] = -(in[12]*in[ 8]) - (in[13]*in[ 9]) - (in[14]*in[10]);
}

///////////////////////////////////////////////////////////////////////////////
// Multiply a vector by a matrix.
//   vecIn - Input vector
//   m - Input matrix
//   vecOut - Output vector
///////////////////////////////////////////////////////////////////////////////
void
vecMatMult (GLfloat vecIn[3], GLfloat m[16], GLfloat vecOut[3]) 
{
   vecOut[0] = (vecIn[0]*m[ 0]) + (vecIn[1]*m[ 4]) + (vecIn[2]*m[ 8]) + m[12];
   vecOut[1] = (vecIn[0]*m[ 1]) + (vecIn[1]*m[ 5]) + (vecIn[2]*m[ 9]) + m[13];
   vecOut[2] = (vecIn[0]*m[ 2]) + (vecIn[1]*m[ 6]) + (vecIn[2]*m[10]) + m[14];
}

///////////////////////////////////////////////////////////////////////////////
// Multiply a vector by just the 3x3 portion of a matrix.
//   vecIn - Input vector
//   m - Input matrix
//   vecOut - Output vector
///////////////////////////////////////////////////////////////////////////////
void
vecMat3x3Mult (GLfloat vecIn[3], GLfloat m[16], GLfloat vecOut[3]) 
{
   vecOut[0] = (vecIn[0]*m[ 0]) + (vecIn[1]*m[ 4]) + (vecIn[2]*m[ 8]);
   vecOut[1] = (vecIn[0]*m[ 1]) + (vecIn[1]*m[ 5]) + (vecIn[2]*m[ 9]);
   vecOut[2] = (vecIn[0]*m[ 2]) + (vecIn[1]*m[ 6]) + (vecIn[2]*m[10]);
}

///////////////////////////////////////////////////////////////////////////////
// Normalize vector
///////////////////////////////////////////////////////////////////////////////
void
Normalize (GLfloat vec[3])
{
   GLfloat sz = sqrt((vec[0]*vec[0]) + (vec[1]*vec[1]) + (vec[2]*vec[2]));
   if ((sz > -0.0000001) && (sz < 0.0000001)) 
   { // Don't divide by zero
      vec[0] = 0.0f;
      vec[1] = 0.0f;
      vec[2] = 1.0f;
   } 
   else 
   {
      vec[0] = vec[0]/sz;
      vec[1] = vec[1]/sz;
      vec[2] = vec[2]/sz;
   }
}

///////////////////////////////////////////////////////////////////////////////
// Read in object file.
///////////////////////////////////////////////////////////////////////////////
bool
LoadObjectFile (char* objectFile)
{
   // Delete old object if it exists.
   gNumTris = 0;
   delete [] gTriangles;
   gTriangles = NULL;
   gNumVerts = 0;
   delete [] gVertices;
   gVertices = NULL;
   delete [] gIndices;
   gIndices = NULL;

   // Read object file
   FILE* fp = fopen (objectFile, "rb");
   if (fp == NULL)
   {
      printf ("ERROR: Unable to open file: %s\n", objectFile);
      return false;
   }
#ifdef USE_SMD_FILES
   extern bool SMDReadTriangles (FILE* fp, int* numTris, NmRawTriangle** tris);
   if (!SMDReadTriangles (fp, &gNumTris, &gTriangles))
#else
   if (!NmReadTriangles (fp, &gNumTris, &gTriangles))
#endif
   {
      printf ("ERROR: Unable to read file: %s\n", objectFile);
      gNumTris = 0;
      delete [] gTriangles;
      gTriangles = NULL;
      return false;
   }
   fclose (fp);

   // Generate vertex buffers.
   if (!NmCreateVertexBuffers (gNumTris, gTriangles, &gNumVerts, &gVertices,
                               &gIndices))
   {
      printf ("ERROR: Unable to read file: %s\n", objectFile);
      gNumTris = 0;
      delete [] gTriangles;
      gTriangles = NULL;
      gNumVerts = 0;
      delete [] gVertices;
      gVertices = NULL;
      delete [] gIndices;
      gIndices = NULL;
      return false;
   }

   // Find bounds
   float bbox[6];
   bbox[0] = FLT_MAX; // X min
   bbox[1] = FLT_MAX; // Y min
   bbox[2] = FLT_MAX; // Z min
   bbox[3] = -FLT_MAX; // X max
   bbox[4] = -FLT_MAX; // Y max
   bbox[5] = -FLT_MAX; // Z max
   for (int v = 0; v < gNumVerts; v++)
   {
      if (gVertices[v].vertex[0] < bbox[0])
      {
         bbox[0] = gVertices[v].vertex[0];
      }
      if (gVertices[v].vertex[0] > bbox[3])
      {
         bbox[3] = gVertices[v].vertex[0];
      }
      if (gVertices[v].vertex[1] < bbox[1])
      {
         bbox[1] = gVertices[v].vertex[1];
      }
      if (gVertices[v].vertex[1] > bbox[4])
      {
         bbox[4] = gVertices[v].vertex[1];
      }
      if (gVertices[v].vertex[2] < bbox[2])
      {
         bbox[2] = gVertices[v].vertex[2];
      }
      if (gVertices[v].vertex[2] > bbox[5])
      {
         bbox[5] = gVertices[v].vertex[2];
      }
   }
   float dx = bbox[3] - bbox[0];
   float dy = bbox[4] - bbox[1];
   float dz = bbox[5] - bbox[2];
   float center[3];
   center[0] = (dx)/2.0f + bbox[0];
   center[1] = (dy)/2.0f + bbox[1];
   center[2] = (dz)/2.0f + bbox[2];

   // Center on the origin and rescale to be 50 units maximum
   float scale = 1.0f;
   if (dx > dy)
   {
      if (dx > dz)
      {
         scale = 50.0f / dx;
      }
      else
      {
         scale = 50.0f / dz;
      }
   }
   else
   {
      if (dy > dz)
      {
         scale = 50.0f / dy;
      }
      else
      {
         scale = 50.0f / dz;
      }
   }
   for (int v = 0; v < gNumVerts; v++)
   {
      gVertices[v].vertex[0] -= center[0];
      gVertices[v].vertex[1] -= center[1];
      gVertices[v].vertex[2] -= center[2];
      gVertices[v].vertex[0] *= scale;
      gVertices[v].vertex[1] *= scale;
      gVertices[v].vertex[2] *= scale;
   }

   // All finished
   return true;
} // LoadObjectFile

///////////////////////////////////////////////////////////////////////////////
// Load in a texture.
///////////////////////////////////////////////////////////////////////////////
bool
LoadTextureFile (char* textureFile)
{
   // Delete old data
   gTexInfo.width = 0;
   gTexInfo.height = 0;
   gTexInfo.bitDepth = 0;
   gTexInfo.format = ATI_BITMAP_FORMAT_UNKNOWN;
   delete [] gTexInfo.pixels;
   gTexInfo.pixels = NULL;
 
   // Load the texture from the file.
   FILE* fp = fopen (textureFile, "rb");
   if (fp == NULL)
   {
      printf ("ERROR: Unable to open file: %s\n", textureFile);
      return false;
   }
   int width, height, bitDepth;
   if (!TGAReadImage (fp, &width, &height, &bitDepth, &(gTexInfo.pixels)))
   {
      printf ("ERROR: Unable to open file: %s\n", textureFile);
      return false;
   }
   fclose (fp);

   // Fill in header data.
   gTexInfo.width = width;
   gTexInfo.height = height;
   gTexInfo.bitDepth = bitDepth;
   switch (bitDepth)
   {
      case 24:
         gTexInfo.format = ATI_BITMAP_FORMAT_888;
         for (int i = 0; i < width*height; i++)
         {
            GLubyte blue = gTexInfo.pixels[i*3];
            GLubyte red = gTexInfo.pixels[i*3 + 2];
            gTexInfo.pixels[i*3] = red;
            gTexInfo.pixels[i*3 + 2] = blue;
         }
         break;
      case 32:
         gTexInfo.format = ATI_BITMAP_FORMAT_8888;
         for (int i = 0; i < width*height; i++)
         {
            GLubyte blue = gTexInfo.pixels[i*4];
            GLubyte red = gTexInfo.pixels[i*4 + 2];
            gTexInfo.pixels[i*4] = red;
            gTexInfo.pixels[i*4 + 2] = blue;
         }
         break;
      default:
         printf ("ERROR: Unhandled Targa bitdepth: %d\n", bitDepth);
         return false;
   }
   
   // All finished.
   return true;
} // end of LoadTextureFile

///////////////////////////////////////////////////////////////////////////////
// Load up a new object and normal map
///////////////////////////////////////////////////////////////////////////////
void
LoadNewObjectAndMap ()
{
   // Object first
   bool done = false;
   while (!done)
   {
      char filename[1024];
      filename[0] = '\0';
      if (GetNMFFileName (NULL, filename))
      {
         if (LoadObjectFile (filename))
         {
            done = true;
         }
      }
   }

   // Normal map next
   done = false;
   while (!done)
   {
      char filename[1024];
      filename[0] = '\0';
      if (GetTextureFileName (NULL, filename))
      {
         if (LoadTextureFile (filename))
         {
            done = true;
         }
      }
   }
} // LoadNewObjectAndMap

///////////////////////////////////////////////////////////////////////////////
// Print a GL error if one exists
///////////////////////////////////////////////////////////////////////////////
void
PrintError (char* header = NULL)
{
   int ec = glGetError ();
   switch (ec)
   {
      case GL_NO_ERROR:
         // Do nothing, no error
         break;
      case GL_INVALID_ENUM:
         if (header != NULL)
         {
            printf ("GL_INVALID_ENUM: %s\n", header);
         }
         else
         {
            printf ("GL_INVALID_ENUM\n");
         }
         break;
      case GL_INVALID_VALUE:
         if (header != NULL)
         {
            printf ("GL_INVALID_VALUE: %s\n", header);
         }
         else
         {
            printf ("GL_INVALID_VALUE\n");
         }
         break;
      case GL_INVALID_OPERATION:
         if (header != NULL)
         {
            printf ("GL_INVALID_OPERATION: %s\n", header);
         }
         else
         {
            printf ("GL_INVALID_OPERATION\n");
         }
         break;
      case GL_STACK_OVERFLOW:
         if (header != NULL)
         {
            printf ("GL_STACK_OVERFLOW: %s\n", header);
         }
         else
         {
            printf ("GL_STACK_OVERFLOW\n");
         }
         break;
      case GL_STACK_UNDERFLOW:
         if (header != NULL)
         {
            printf ("GL_STACK_UNDERFLOW: %s\n", header);
         }
         else
         {
            printf ("GL_STACK_UNDERFLOW\n");
         }
         break;
      case GL_OUT_OF_MEMORY: 
         if (header != NULL)
         {
            printf ("GL_OUT_OF_MEMORY: %s\n", header);
         }
         else
         {
            printf ("GL_OUT_OF_MEMORY\n");
         }
         break;
      default:
         if (header != NULL)
         {
            printf ("UNKNOWN GL ERROR! %s\n", header);
         }
         else
         {
            printf ("UNKNOWN GL ERROR!\n");
         }
         break;
   }
}

///////////////////////////////////////////////////////////////////////////////
// Initialize GL state.
///////////////////////////////////////////////////////////////////////////////
void
StateInit (void)
{
   glClearColor (0.0f, 0.0f, 0.0f, 0.0f);
   glShadeModel (GL_SMOOTH);
   glDisable (GL_CULL_FACE);
   glEnable (GL_DEPTH_TEST);
   glDepthFunc (GL_LEQUAL);
}

///////////////////////////////////////////////////////////////////////////////
// Initialize object.
///////////////////////////////////////////////////////////////////////////////
void
ObjectInit (void)
{
   // Make sure we have verts.
   if (gNumVerts < 1)
   {
      printf ("ERROR: No vertices!\n");
      Sleep (5000);
      exit (-1);
   }
   
   // Delete old array.
   delete [] gLightVec;
   gLightVec = NULL;
   
   // Create a light vector array.
   gLightVec = new double [gNumVerts*3];
   if (gLightVec == NULL)
   {
      printf ("ERROR: Unable to allocate light vector array!\n");
      Sleep (5000);
      exit (-1);
   }
   memset (gLightVec, 0, sizeof (double) * gNumVerts * 3);
}
 
///////////////////////////////////////////////////////////////////////////////
// Initialize texture.
///////////////////////////////////////////////////////////////////////////////
void
TextureInit ()
{
   // Remove old texture
   if (gHaveTexture)
   {
      if (glIsTexture (gTextureID))
      {
         glDeleteTextures (1, &gTextureID);
         gHaveTexture = true;
      }
   }
   glGenTextures (1, &gTextureID);

   // Load the texture into GL
   glBindTexture (GL_TEXTURE_2D, gTextureID);
   switch (gTexInfo.format)
   {
      case ATI_BITMAP_FORMAT_888:
         gluBuild2DMipmaps (GL_TEXTURE_2D, GL_RGB8, gTexInfo.width,
                            gTexInfo.height, GL_RGB, GL_UNSIGNED_BYTE,
                            gTexInfo.pixels);
         break;
      case ATI_BITMAP_FORMAT_8888:
         gluBuild2DMipmaps (GL_TEXTURE_2D, GL_RGBA8, gTexInfo.width,
                            gTexInfo.height, GL_RGBA, GL_UNSIGNED_BYTE,
                            gTexInfo.pixels);
         break;
      default:
         printf ("ERROR: Unhandled texture type!\n");
         Sleep (5000);
         exit (-1);
   }
}

///////////////////////////////////////////////////////////////////////////////
// Initialize view matrix
///////////////////////////////////////////////////////////////////////////////
void
MatrixInit (void)
{
   matrixIdentity (gViewMat);
   gViewMat[14] = -65.0f;
}

///////////////////////////////////////////////////////////////////////////////
// Object drawing function
///////////////////////////////////////////////////////////////////////////////
void 
DrawObject (bool withColor) 
{
   // Setup data streams.
   glEnableClientState (GL_VERTEX_ARRAY);
   int stride = sizeof (NmTangentPointD);
   glVertexPointer (3, GL_DOUBLE, stride, &(gVertices[0].vertex[0]));
   glEnableClientState (GL_NORMAL_ARRAY);
   glNormalPointer (GL_DOUBLE, stride, &(gVertices[0].normal[0]));
   glEnableClientState (GL_TEXTURE_COORD_ARRAY);
   glTexCoordPointer (2, GL_DOUBLE, stride, &(gVertices[0].uv[0]));
   if (withColor)
   {
      glEnableClientState (GL_COLOR_ARRAY);
      glColorPointer (3, GL_DOUBLE, 0, gLightVec);
   }

   // Draw
   glDrawElements (GL_TRIANGLES, gNumTris*3, GL_UNSIGNED_INT, gIndices);

   // Clear client states.
   glDisableClientState(GL_VERTEX_ARRAY);
   glDisableClientState(GL_NORMAL_ARRAY);
   glDisableClientState(GL_TEXTURE_COORD_ARRAY);
   if (withColor)
   {
      glDisableClientState(GL_COLOR_ARRAY);
   }
}

///////////////////////////////////////////////////////////////////////////////
// Normal drawing
///////////////////////////////////////////////////////////////////////////////
void 
DrawNormal (double offset) 
{
   glBegin (GL_LINES);
      for (int v = 0; v < gNumVerts; v++)
      {
         glVertex3dv (gVertices[v].vertex);
         glVertex3d (gVertices[v].vertex[0] + gVertices[v].normal[0]*offset,
                     gVertices[v].vertex[1] + gVertices[v].normal[1]*offset,
                     gVertices[v].vertex[2] + gVertices[v].normal[2]*offset);
      }
   glEnd ();
}

///////////////////////////////////////////////////////////////////////////////
// Tangent drawing
///////////////////////////////////////////////////////////////////////////////
void 
DrawTangent (double offset) 
{
   glBegin (GL_LINES);
      for (int v = 0; v < gNumVerts; v++)
      {
         glVertex3dv (gVertices[v].vertex);
         glVertex3d (gVertices[v].vertex[0] + gVertices[v].tangent[0]*offset,
                     gVertices[v].vertex[1] + gVertices[v].tangent[1]*offset,
                     gVertices[v].vertex[2] + gVertices[v].tangent[2]*offset);
      }
   glEnd ();
}

///////////////////////////////////////////////////////////////////////////////
// Binormal drawing
///////////////////////////////////////////////////////////////////////////////
void 
DrawBinormal (double offset) 
{
   glBegin (GL_LINES);
      for (int v = 0; v < gNumVerts; v++)
      {
         glVertex3dv (gVertices[v].vertex);
         glVertex3d (gVertices[v].vertex[0] + gVertices[v].binormal[0]*offset,
                     gVertices[v].vertex[1] + gVertices[v].binormal[1]*offset,
                     gVertices[v].vertex[2] + gVertices[v].binormal[2]*offset);
      }
   glEnd ();
}

///////////////////////////////////////////////////////////////////////////////
// Light vector drawing
///////////////////////////////////////////////////////////////////////////////
void
DrawLightVector (double offset, float objLightPos[3])
{
   glBegin (GL_LINES);
      for (int v = 0; v < gNumVerts; v++)
      {
         glVertex3dv (gVertices[v].vertex);
         float vec[3];
#ifdef TAN_SPACE_TEST
         GLfloat cvec[3];
         cvec[0] = (gLightVec[v*3] - 0.5) * 0.5;
         cvec[1] = (gLightVec[v*3 + 1] - 0.5) * 0.5;
         cvec[2] = (gLightVec[v*3 + 2] - 0.5) * 0.5;
         GLfloat m[16]; 
         GLfloat mi[16]; 
         matrixIdentity(m);
         matrixIdentity(mi);
         m[0] = gVertices[v].tangent[0];
         m[1] = gVertices[v].binormal[0];
         m[2] = gVertices[v].normal[0];
         m[4] = gVertices[v].tangent[1];
         m[5] = gVertices[v].binormal[1];
         m[6] = gVertices[v].normal[1];
         m[8] = gVertices[v].tangent[2];
         m[9] = gVertices[v].binormal[2];
         m[10]= gVertices[v].normal[2];
         matrixInvert (m, mi);
         vecMat3x3Mult (cvec, mi, vec);
#else
         vec[0] = (objLightPos[0] - gVertices[v].vertex[0]);
         vec[1] = (objLightPos[1] - gVertices[v].vertex[1]);
         vec[2] = (objLightPos[2] - gVertices[v].vertex[2]);
#endif
         Normalize (vec);
         glVertex3d (gVertices[v].vertex[0] + vec[0]*offset,
                     gVertices[v].vertex[1] + vec[1]*offset,
                     gVertices[v].vertex[2] + vec[2]*offset);
      }
   glEnd ();
}

///////////////////////////////////////////////////////////////////////////////
// Draw a cube at the light position
///////////////////////////////////////////////////////////////////////////////
void
DrawLightCube (float worldLightPos[3])
{
   glBegin (GL_QUADS);
      float px = worldLightPos[0] + 0.5f;
      float nx = worldLightPos[0] - 0.5f;
      float py = worldLightPos[1] + 0.5f;
      float ny = worldLightPos[1] - 0.5f;
      float pz = worldLightPos[2] + 0.5f;
      float nz = worldLightPos[2] - 0.5f;

      glVertex3d (nx, py, nz);
      glVertex3d (px, py, nz);
      glVertex3d (px, py, pz);
      glVertex3d (nx, py, pz);

      glVertex3d (nx, ny, pz);
      glVertex3d (nx, ny, nz);
      glVertex3d (nx, py, nz);
      glVertex3d (nx, py, pz);

      glVertex3d (px, ny, pz);
      glVertex3d (nx, ny, pz);
      glVertex3d (nx, py, pz);
      glVertex3d (px, py, pz);

      glVertex3d (px, ny, nz);
      glVertex3d (px, ny, pz);
      glVertex3d (px, py, pz);
      glVertex3d (px, py, nz);

      glVertex3d (nx, ny, nz);
      glVertex3d (px, ny, nz);
      glVertex3d (px, py, nz);
      glVertex3d (nx, py, nz);

      glVertex3d (nx, ny, pz);
      glVertex3d (px, ny, pz);
      glVertex3d (px, ny, nz);
      glVertex3d (nx, ny, nz);
   glEnd ();
}

///////////////////////////////////////////////////////////////////////////////
// Compute light vectors in tangent space
///////////////////////////////////////////////////////////////////////////////
void
ComputeLightVectors (float objLightPos[3], bool inTangentSpace)
{
   for (int v = 0; v < gNumVerts; v++)
   {
      // Compute light vector
      float lv[3];
      lv[0] = (objLightPos[0] - gVertices[v].vertex[0]);
      lv[1] = (objLightPos[1] - gVertices[v].vertex[1]);
      lv[2] = (objLightPos[2] - gVertices[v].vertex[2]);
      Normalize (lv);

      // Convert to tangent space, or not as requested.
      GLfloat vec[3];
      if (inTangentSpace)
      {
         // Convert the light vector into tangent space
         //
         // T - Tangent
         // N - Normal
         // B - Bi-Normal
         // color = lightvec  | T(x) B(x) N(x) |
         //                   | T(y) B(y) N(y) |
         //                   | T(z) B(z) N(z) |
         GLfloat m[16]; 
         matrixIdentity(m);
         m[0] = gVertices[v].tangent[0];
         m[1] = gVertices[v].binormal[0];
         m[2] = gVertices[v].normal[0];
         m[4] = gVertices[v].tangent[1];
         m[5] = gVertices[v].binormal[1];
         m[6] = gVertices[v].normal[1];
         m[8] = gVertices[v].tangent[2];
         m[9] = gVertices[v].binormal[2];
         m[10]= gVertices[v].normal[2];
         vecMat3x3Mult (lv, m, vec);
         Normalize (vec);
      }
      else
      {
         vec[0] = lv[0];
         vec[1] = lv[1];
         vec[2] = lv[2];
      }

      // Add into array
      gLightVec[v*3]     =  (vec[0] * 0.5) + 0.5;
      gLightVec[v*3 + 1] =  (vec[1] * 0.5) + 0.5;
      gLightVec[v*3 + 2] =  (vec[2] * 0.5) + 0.5;
   }
}

///////////////////////////////////////////////////////////////////////////////
// Setup state for showing the normal map
///////////////////////////////////////////////////////////////////////////////
void
SetupNormalMapState (void)
{
   glEnable (GL_TEXTURE_2D);
   glBindTexture (GL_TEXTURE_2D, gTextureID);
   glTexEnvf (GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_DECAL);
   glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
   glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
   glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
   glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
}

///////////////////////////////////////////////////////////////////////////////
// Clear state from showing the normal map
///////////////////////////////////////////////////////////////////////////////
void
ClearNormalMapState (void)
{
   glDisable (GL_TEXTURE_2D);
}

///////////////////////////////////////////////////////////////////////////////
// Setup state for diffuse bump mapping
///////////////////////////////////////////////////////////////////////////////
void
SetupBumpState (void)
{
   // Enable texture
   glEnable (GL_TEXTURE_2D);
   glBindTexture (GL_TEXTURE_2D, gTextureID);
   glTexEnvf (GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_DECAL);
   glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
   glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
   glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
   glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);

   // Setup texture stages to dot the bump map with the light vector (color)
   glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE_EXT);
   glTexEnvf(GL_TEXTURE_ENV, GL_COMBINE_RGB_EXT, GL_DOT3_RGBA_EXT);
   
   glTexEnvf(GL_TEXTURE_ENV, GL_SOURCE0_RGB_EXT, GL_PRIMARY_COLOR_EXT);
   glTexEnvf(GL_TEXTURE_ENV, GL_OPERAND0_RGB_EXT, GL_SRC_COLOR);
    
   glTexEnvf(GL_TEXTURE_ENV, GL_SOURCE1_RGB_EXT, GL_TEXTURE);
   glTexEnvf(GL_TEXTURE_ENV, GL_OPERAND1_RGB_EXT, GL_SRC_COLOR);
}

///////////////////////////////////////////////////////////////////////////////
// Clear state from diffuse bump mapping
///////////////////////////////////////////////////////////////////////////////
void
ClearBumpState (void)
{
   glDisable (GL_TEXTURE_2D);
}

///////////////////////////////////////////////////////////////////////////////
// Setup state for HW lighting
///////////////////////////////////////////////////////////////////////////////
void
SetupLightState  (float worldLightPos[3])
{
   // Setup one time state.
   static bool once = false;
   if (!once)
   {
      static GLfloat light1ambient[] = {0.5f, 0.5f, 0.5f, 1.0f};
      static GLfloat light1diffuse[] = {1.0f, 1.0f, 1.0f, 1.0f};
      static GLfloat light1specular[] = {1.0f, 1.0f, 1.0f, 1.0f};
      glLightfv (GL_LIGHT0, GL_AMBIENT, light1ambient);
      glLightfv (GL_LIGHT0, GL_DIFFUSE, light1diffuse);
      glLightfv (GL_LIGHT0, GL_SPECULAR, light1specular);
      once = true;
   }

   // Lighting states
   glMaterialfv (GL_FRONT_AND_BACK, GL_SPECULAR, gWhite);
   glMaterialfv (GL_FRONT_AND_BACK, GL_EMISSION, gBlack);
   glLightModeli  (GL_LIGHT_MODEL_LOCAL_VIEWER, GL_TRUE);
   glEnable (GL_LIGHTING);
   glEnable (GL_LIGHT0);
   float lp[4] = {worldLightPos[0], worldLightPos[1], worldLightPos[2], 1.0f};
   glLightfv (GL_LIGHT0, GL_POSITION, lp);
   glMaterialfv (GL_FRONT_AND_BACK, GL_SPECULAR, gWhite);
   glMaterialf (GL_FRONT_AND_BACK, GL_SHININESS, 128.0f);
}

///////////////////////////////////////////////////////////////////////////////
// Clears state assoicated with lighting
///////////////////////////////////////////////////////////////////////////////
void
ClearLightState (void)
{
   glDisable (GL_LIGHT0);
   glDisable (GL_LIGHT0);
   glDisable (GL_LIGHTING);
}

///////////////////////////////////////////////////////////////////////////////
// Setup state for diffuse bump mapping
///////////////////////////////////////////////////////////////////////////////
void
SetupAlphaState (void)
{
   // Enable texture
   glEnable (GL_TEXTURE_2D);
   glBindTexture (GL_TEXTURE_2D, gTextureID);
   glTexEnvf (GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_DECAL);
   glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
   glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
   glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
   glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);

   // Setup texture stages to dot the bump map with the light vector (color)
   glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE_EXT);
   glTexEnvf(GL_TEXTURE_ENV, GL_COMBINE_RGB_EXT, GL_REPLACE);
   
   glTexEnvf(GL_TEXTURE_ENV, GL_SOURCE0_RGB_EXT, GL_TEXTURE);
   glTexEnvf(GL_TEXTURE_ENV, GL_OPERAND0_RGB_EXT, GL_SRC_ALPHA);
    
   glTexEnvf(GL_TEXTURE_ENV, GL_SOURCE1_RGB_EXT, GL_TEXTURE);
   glTexEnvf(GL_TEXTURE_ENV, GL_OPERAND1_RGB_EXT, GL_SRC_ALPHA);
}

///////////////////////////////////////////////////////////////////////////////
// Clear state from diffuse bump mapping
///////////////////////////////////////////////////////////////////////////////
void
ClearAlphaState (void)
{
   glDisable (GL_TEXTURE_2D);
}

///////////////////////////////////////////////////////////////////////////////
// Scene drawing function
///////////////////////////////////////////////////////////////////////////////
void 
Display (void) 
{
   // See if we've gotten any errors since last draw.
   PrintError ("Display");

   // Compute world space light position.
   float worldLightPos[3];
   GLfloat m[16];
   matrixInvert (gViewMat, m);
   vecMatMult (gLightpos, m, worldLightPos);
   ComputeLightVectors (worldLightPos, gInTangentSpace);

   // Clear buffers
   glClear (GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

   // Setup matrices
   glViewport (0, 0, gWidth, gHeight);
   glMatrixMode (GL_PROJECTION);
   glLoadIdentity ();
   gluPerspective (gFov, (double)gWidth/(double)gHeight, gClipNear, gClipFar);
   glMatrixMode (GL_MODELVIEW);
   glLoadIdentity ();
   glLoadMatrixf (gViewMat);

   // Wireframe or solid
   if (gWireframe)
   {
      glPolygonMode (GL_FRONT_AND_BACK, GL_LINE);
      glColor3f (1.0f, 1.0f, 1.0f);
      DrawObject (false);
   }
   else // Solid is based on the viewing mode
   {
      switch (gViewMode)
      {
         default:
            gViewMode = NMF_VIEW_DIFFUSE_BUMP;
         case NMF_VIEW_DIFFUSE_BUMP:
            glPolygonMode (GL_FRONT_AND_BACK, GL_FILL);
            SetupBumpState ();
            DrawObject (true);
            ClearBumpState ();
            break;

         case NMF_VIEW_NORMAL_MAP:
            glPolygonMode (GL_FRONT_AND_BACK, GL_FILL);
            SetupNormalMapState ();
            DrawObject (false);
            ClearNormalMapState ();
            break;

         case NMF_VIEW_LIT_NORMALS:
            glPolygonMode (GL_FRONT_AND_BACK, GL_FILL);
            SetupLightState (worldLightPos);
            glColor3f (1.0f, 1.0f, 1.0f);
            DrawObject (false);
            ClearLightState ();
            break;

         case NMF_VIEW_ALPHA:
            glPolygonMode (GL_FRONT_AND_BACK, GL_FILL);
            SetupAlphaState ();
            glColor3f (1.0f, 1.0f, 1.0f);
            DrawObject (false);
            ClearAlphaState ();
            break;
      }
   }

   // Draw normal
   if (gShowNormals)
   {
      glPolygonMode (GL_FRONT_AND_BACK, GL_LINE);
      glColor3f (1.0f, 0.0f, 0.0f);
      DrawNormal (0.5f);
   }

   // Draw tangent/binormal
   if (gShowTanCoord)
   {
      glPolygonMode (GL_FRONT_AND_BACK, GL_LINE);
      glColor3f(0.0f, 0.0f, 1.0f);
      DrawTangent (0.4f);
      glColor3f (0.0f, 1.0f, 0.0f);
      DrawBinormal (0.4f);
   }

   // Draw vector to light
   if (gShowVectors)
   {
      glPolygonMode (GL_FRONT_AND_BACK, GL_LINE);
      glColor3f (1.0f, 1.0f, 0.0f);
      DrawLightVector (0.4f, worldLightPos);
   }

   // Draw light
#if 0 // Light is now fixed at view position
   glPolygonMode (GL_FRONT_AND_BACK, GL_FILL);
   glColor3f(1.0f, 1.0f, 0.5f);
   DrawLightCube (worldLightPos);
#endif
   
   // Finish up
   glFinish ();
   glutSwapBuffers ();
}

///////////////////////////////////////////////////////////////////////////////
// Get new window parameters
///////////////////////////////////////////////////////////////////////////////
void 
Reshape(int w, int h)
{
   gWidth = w;
   gHeight = h;
}

///////////////////////////////////////////////////////////////////////////////
// Key input handler
//   key - Which key pressed
///////////////////////////////////////////////////////////////////////////////
void 
KeyFunc (unsigned char key, int x, int y)
{
   switch(key)
   {
      case 'a':
      case 'A':
         gViewMode = (gViewMode + 1) % NMF_VIEW_MAX;
         switch (gViewMode)
         {
            default:
               gViewMode = NMF_VIEW_DIFFUSE_BUMP;
            case NMF_VIEW_DIFFUSE_BUMP:
               printf ("Diffuse lit normal map.\n");
               break;
            case NMF_VIEW_NORMAL_MAP:
               printf ("Normal map.\n");
               break;
            case NMF_VIEW_LIT_NORMALS:
               printf ("Vertex lighting.\n");
               break;
            case NMF_VIEW_ALPHA:
               if (gTexInfo.format == ATI_BITMAP_FORMAT_8888)
               {
                  printf ("Alpha.\n");
               }
               else
               {
                  gViewMode = NMF_VIEW_DIFFUSE_BUMP;
                  printf ("Diffuse lit normal map.\n");
               }
               break;
         }
         break;
      case 'b':
      case 'B':
         {
            static int background = 0;
            switch (background)
            {
               default:
               case 0:
                  glClearColor (0.0f, 0.0f, 0.5f, 1.0f);
                  background = (background + 1) % 4;
                  break;
               case 1:
                  glClearColor (0.5f, 0.5f, 0.5f, 1.0f);
                  background = (background + 1) % 4;
                  break;
               case 2:
                  glClearColor (1.0f, 1.0f, 1.0f, 1.0f);
                  background = (background + 1) % 4;
                  break;
               case 3:
                  glClearColor (0.0f, 0.0f, 0.0f, 1.0f);
                  background = (background + 1) % 4;
                  break;
            }
         }
         break;
      case 'c':
      case 'C':
         gInTangentSpace = !gInTangentSpace;
         if (gInTangentSpace)
         {
            printf ("Converting light into tangent space\n");
         }
         else
         {
            printf ("Not converting light into tangent space\n");
         }
         break;
      case 'l':
      case 'L':
         LoadNewObjectAndMap ();
         ObjectInit ();
         TextureInit ();
         break;
      case 'n':
      case 'N':
         gShowNormals = !gShowNormals;
         break;
      case 'r':
      case 'R':
         MatrixInit ();
         break;
      case 't':
      case 'T':
         gShowTanCoord = !gShowTanCoord;
         break;
#if 0 // Not really useful with the light always behind viewer
      case 'v':
      case 'V':
         gShowVectors = !gShowVectors;
         break;
#endif
      case 'w':
      case 'W':
         gWireframe = !gWireframe;
         break;
      case '\x1b': // ESC
      case 'Q':
      case 'q':
         exit(0);
         break;
   };
   glutPostRedisplay();
}

///////////////////////////////////////////////////////////////////////////////
// Handle mouse movement.
//   x - current x position.
//   y - current y position.
///////////////////////////////////////////////////////////////////////////////
void 
MouseMove (int x, int y)
{
   // Left button = rotate
   if (gLeftDown) 
   {
      glMatrixMode(GL_MODELVIEW);
      GLfloat m[16];
      matrixInvert (gViewMat, m);
      GLfloat v[3] = {(y - gLeftY) * 0.3f, (x - gLeftX) * 0.3f, 0.0f };
      GLfloat l[3] = {(y - gLeftY) * 0.3f, (x - gLeftX) * 0.3f, 0.0f };
      GLfloat o[3];
      vecMat3x3Mult (v, m, o);
      glPushMatrix ();
         glLoadIdentity ();
         glMultMatrixf (gViewMat);
         glRotatef (o[0], 1.0f, 0.0f, 0.0f);
         glRotatef (o[1], 0.0f, 1.0f, 0.0f);
         glRotatef (o[2], 0.0f, 0.0f, 1.0f);
         glGetFloatv (GL_MODELVIEW_MATRIX, gViewMat);
         glLoadIdentity ();
      glPopMatrix();
      gLeftX = x;
      gLeftY = y;
      glutPostRedisplay();
   }

   // Middle button = Pan
   if (gMiddleDown) 
   {
      glMatrixMode(GL_MODELVIEW);
      GLfloat m[16];
      matrixInvert (gViewMat, m);
      GLfloat v[3] = {(x - gMiddleX) * 0.1f, (y - gMiddleY) * -0.1f, 0.0f };
      GLfloat o[3];
      vecMat3x3Mult (v, m, o);
      glPushMatrix ();
         glLoadIdentity ();
         glMultMatrixf (gViewMat);
         glTranslatef (o[0], o[1], o[2]);
         glGetFloatv (GL_MODELVIEW_MATRIX, gViewMat);
      glPopMatrix();
      gMiddleX = x;
      gMiddleY = y;
      glutPostRedisplay();
   }

   // Right button = Zoom
   if (gRightDown) 
   {
      glMatrixMode(GL_MODELVIEW);
      GLfloat m[16];
      matrixInvert (gViewMat, m);
      GLfloat v[3] = {0.0f, 0.0f, 
                      ((x - gRightX) * 0.1f +
                       (y - gRightY) * 0.1f)};
      GLfloat o[3];
      vecMat3x3Mult (v, m, o);
      glPushMatrix ();
         glLoadIdentity ();
         glMultMatrixf (gViewMat);
         glTranslatef (o[0], o[1], o[2]);
         glGetFloatv (GL_MODELVIEW_MATRIX, gViewMat);
      glPopMatrix();
      gRightX = x;
      gRightY = y;
      glutPostRedisplay();
   }
} // MouseMove

///////////////////////////////////////////////////////////////////////////////
// Handle mouse button presses/releases.
//   button - The button pressed.
//   state - Was the button pressed or release.
//   x - current x position.
//   y - current y position.
///////////////////////////////////////////////////////////////////////////////
void 
MouseUpDown (int button, int state, int x, int y) 
{
   if (button == GLUT_LEFT_BUTTON) 
   {
      if (state == GLUT_DOWN) 
      {
         gLeftDown = GL_TRUE;
         gLeftX = x;
         gLeftY = y;
      }
      else if (state == GLUT_UP) 
      {
         gLeftDown = GL_FALSE;
      }
   }
   if (button == GLUT_RIGHT_BUTTON) 
   {
      if (state == GLUT_DOWN) 
      {
         gRightDown = GL_TRUE;
         gRightX = x;
         gRightY = y;
      }
      else if (state == GLUT_UP)
      {
         gRightDown = GL_FALSE;
      }
   }
   if (button == GLUT_MIDDLE_BUTTON) 
   {
      if (state == GLUT_DOWN) 
      {
         gMiddleDown = GL_TRUE;
         gMiddleX = x;
         gMiddleY = y;
      }
      else if (state == GLUT_UP)
      {
         gMiddleDown = GL_FALSE;
      }
   }
}

///////////////////////////////////////////////////////////////////////////////
// Determine if a particular extension is supported
//   extension -- Name of the extension to check
///////////////////////////////////////////////////////////////////////////////
int 
extensionSupported (const char *extension)
{
   static const GLubyte *extensions = NULL;
   const GLubyte *start;
   GLubyte *where, *terminator;

   where = (GLubyte *) strchr(extension, ' ');
   if (where || *extension == '\0')
   {
      return 0;
   }

   if (!extensions)
   {
      extensions = glGetString(GL_EXTENSIONS);
   }

   start = extensions;
   for (;;) 
   {
      where = (GLubyte *) strstr((const char *) start, extension);
      if (!where)
      {
         break;
      }
      terminator = where + strlen(extension);
      if (where == start || *(where - 1) == ' ') 
      {
         if (*terminator == ' ' || *terminator == '\0') 
         {
            return 1;
         }
      }
      start = terminator;
   }
   return 0;
}

///////////////////////////////////////////////////////////////////////////////
// Check to make sure the extensions we need are available.
///////////////////////////////////////////////////////////////////////////////
void
checkExtensions (void)
{
#ifndef ATI_MAC_OS
   if (!extensionSupported("GL_EXT_texture_env_combine"))
#else
   if (!extensionSupported("GL_ARB_texture_env_combine"))
#endif
   {
      printf("No GL_EXT_texture_env_combine support!\n");
      Sleep(5000);
      exit(-1);
   }
#ifndef ATI_MAC_OS
   if (!extensionSupported ("GL_EXT_texture_env_dot3"))
#else
   if (!extensionSupported ("GL_ARB_texture_env_dot3"))
#endif
   {
      printf ("No GL_EXT_texture_env_dot3 support!\n");
      Sleep (5000);
      exit (-1);
   }
}

///////////////////////////////////////////////////////////////////////////////
// Entry point
///////////////////////////////////////////////////////////////////////////////
int 
main (int argc,char **argv)
{
   printf ("NMF Viewer v01.03.01\n");
   printf (" a              - Cycle showing normal map, diffuse bump, or vertex lit, or occlusion term (if present)\n");
   printf (" b              - Cycle background color\n");
   printf (" c              - Toggle converting light vector to tangent space\n");
   printf (" l              - Load a new object and normal map\n");
   printf (" n              - Toggle display of normals\n");
   printf (" r              - Reset view/light position\n");
   printf (" t              - Toggle display of tangent/binormal\n");
   // Not really useful with the light always behind viewer
   //printf (" v              - Toggle showing vector to light\n");
   printf (" w              - Toggle wireframe\n");
   printf (" LEFT + MOUSE   - Rotate \n");
   printf (" MIDDLE + MOUSE - Pan \n");
   printf (" RIGHT + MOUSE  - Zoom\n");
   printf (" ESC or q       - Quit program\n");
   printf ("\n");

   // If the user specified a file name on command line, use it.
   if (argc == 3) 
   {
      if (!LoadObjectFile (argv[1]))
      {
         bool done = false;
         while (!done)
         {
            char filename[1024];
            filename[0] = '\0';
            if (GetNMFFileName (NULL, filename))
            {
               if (LoadObjectFile (filename))
               {
                  done = true;
               }
            }
         }
      }
      if (!LoadTextureFile (argv[2]))
      {
         bool done = false;
         while (!done)
         {
            char filename[1024];
            filename[0] = '\0';
            if (GetTextureFileName (NULL, filename))
            {
               if (LoadTextureFile (filename))
               {
                  done = true;
               }
            }
         }
      }
   } 
   else // Otherwise put up a dialog box.
   {
      LoadNewObjectAndMap ();
   }

   // Create window.
   glutInit (&argc, argv);
   glutInitDisplayMode (GLUT_RGBA | GLUT_DOUBLE | GLUT_DEPTH);
   glutInitWindowSize (gWidth, gHeight);
   glutCreateWindow ("NMF Viewer");

   // Check extensions.
   checkExtensions();

   // Setup GLUT callbacks.
   glutDisplayFunc (Display);
   glutReshapeFunc (Reshape);
   glutKeyboardFunc (KeyFunc);
   glutMotionFunc (MouseMove);
   glutMouseFunc (MouseUpDown);

   // Initialize state.
   StateInit ();
   ObjectInit ();
   TextureInit ();
   MatrixInit ();
   
   // Start drawing.
   glutMainLoop ();
   
   return 0;
}
