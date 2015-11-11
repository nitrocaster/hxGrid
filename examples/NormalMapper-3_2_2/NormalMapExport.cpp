/******************************************************************************
 *  SOLExport.cpp -- Maya Exporter for Sushi Objects.
 ******************************************************************************
 $Header: /cvsroot-fuse/hxgrid/hxgrid/examples/NormalMapper-3_2_2/NormalMapExport.cpp,v 1.1 2008/08/23 17:01:42 hax22 Exp $
 ******************************************************************************
 *  (C) 2000 ATI Research, Inc.  All rights reserved.
 ******************************************************************************/

#include <stdarg.h>
#include <stdlib.h>
#include <iostream.h>
#include <fstream.h>
#include <string.h>
#include <math.h>

#include <maya/MComputation.h>
#include <maya/MDagPath.h>
#include <maya/MFloatArray.h>
#include <maya/MFloatPoint.h>
#include <maya/MFloatPointArray.h>
#include <maya/MFloatVector.h>
#include <maya/MFloatVectorArray.h>
#include <maya/MFnDagNode.h>
#include <maya/MFnMesh.h>
#include <maya/MFnPlugin.h>
#include <maya/MFnSet.h>
#include <maya/MGlobal.h>
#include <maya/MItDag.h>
#include <maya/MItMeshPolygon.h>
#include <maya/MItSelectionList.h>
#include <maya/MObject.h>
#include <maya/MObjectArray.h>
#include <maya/MPoint.h>
#include <maya/MPointArray.h>
#include <maya/MPxFileTranslator.h>
#include <maya/MSelectionList.h>
#include <maya/MStatus.h>
#include <maya/MString.h>
#include <maya/MStringArray.h>

#include "NmFileIO.h"

#define NMF_EXPORT_VERSION "01.01"

// Constant for initial triangle array size
const int NM_MAX_NUM_TRIANGLES = 20000;
const int NM_MAX_VERTICES_PER_FACE = 1024;

// Structure to give vertex index to normal and UV index.
typedef struct
{
   int vertIdx;
   int nIdx;
   int uvIdx;
   int cIdx;
} FaceIndex;

// The Maya plug-in class for our exporter.
class NMFExport : public MPxFileTranslator 
{
public:
   NMFExport (void);
   virtual ~NMFExport (void);

   // Maya interface functions.
   static void *creator (void);
   MStatus reader (const MFileObject& file,
                   const MString& optionsString,
                   MPxFileTranslator::FileAccessMode mode);
   MStatus writer (const MFileObject& file,
                   const MString& optionsString,
                   MPxFileTranslator::FileAccessMode mode);
   bool haveReadMethod (void) const;
   bool haveWriteMethod (void) const;
   virtual MString defaultExtension (void) const;
   bool canBeOpened (void) const;
   MFileKind identifyFile (const MFileObject& fileName,
                           const char *buffer,
                           short size) const;
   
protected:
   static MString  magic;

   // Polygon data storage
   int m_numTriangles;
   int m_maxNumTriangles;
   NmRawTriangle* m_triList;
   NmRawTriangle* GetNextTriangle ();

   // Array of face index parameters
   FaceIndex m_faceIndices[NM_MAX_VERTICES_PER_FACE];

   // Helpers for node selection
   bool isObjectSelected (const MDagPath& path);
   bool isObjectOrParentSelected (const MDagPath & path);

   // Geometry routines
   bool ProcessGeom (MPxFileTranslator::FileAccessMode mode, 
                     MComputation& computation);
};

////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////
NMFExport::NMFExport (void) 
{
   m_numTriangles = 0;
   m_maxNumTriangles = 0;
   m_triList = NULL;
} // constructor

////////////////////////////////////////////////////////////////////
// Destructor
////////////////////////////////////////////////////////////////////
NMFExport::~NMFExport (void) 
{
   m_numTriangles = 0;
   m_maxNumTriangles = 0;
   delete [] m_triList;
   m_triList = NULL;
} // destructor

////////////////////////////////////////////////////////////////////
// Check if an object is selected
////////////////////////////////////////////////////////////////////
bool 
NMFExport::isObjectSelected (const MDagPath& path)
{
   MDagPath sDagPath;
   
   MSelectionList activeList;
   MGlobal::getActiveSelectionList (activeList);
   
   MItSelectionList iter (activeList, MFn::kDagNode);
   
   while (!iter.isDone())
   {
      if (iter.getDagPath (sDagPath))
      {
         if (sDagPath == path)
            return true;
      }
      iter.next();
   }
   return false;
} // isObjectSelected

////////////////////////////////////////////////////////////////////
// Check if this object and all of its parents are selected.
////////////////////////////////////////////////////////////////////
bool 
NMFExport::isObjectOrParentSelected (const MDagPath & path)
{   
   bool result = false;
   MDagPath searchPath (path);
   while (true) 
   {
      if (isObjectSelected (searchPath))
      {
         result = true;
         break;
      }    
      if (searchPath.length () <= 1)
         break;
      searchPath.pop ();
   }   
   return result;
} // isObjectOrParentSelected

////////////////////////////////////////////////////////////////////
// This routine gets the next triangle structure from internal data
// if there aren't enough triangles it re-allocates.
////////////////////////////////////////////////////////////////////
NmRawTriangle*
NMFExport::GetNextTriangle ()
{
   if (m_triList == NULL)
   {
      m_triList = new NmRawTriangle [NM_MAX_NUM_TRIANGLES];
      m_maxNumTriangles = NM_MAX_NUM_TRIANGLES;
   }
   if (m_numTriangles >= m_maxNumTriangles)
   {
      int last = m_maxNumTriangles * sizeof (NmRawTriangle);
      m_maxNumTriangles += NM_MAX_NUM_TRIANGLES;
      NmRawTriangle* tri = new NmRawTriangle [m_maxNumTriangles];
      memcpy (tri, m_triList, last);
      delete [] m_triList;
      m_triList = tri;
   }
   NmRawTriangle* ret = &(m_triList[m_numTriangles]);
   m_numTriangles++;
   return ret;
} // GetNextTriangle

////////////////////////////////////////////////////////////////////
// This routine traverses the DAG and fills in the internal polygon
// array with the current transform applied.
////////////////////////////////////////////////////////////////////
bool 
NMFExport::ProcessGeom (MPxFileTranslator::FileAccessMode mode,
                        MComputation& computation)
{
   // Create the DAG iterator.
   MStatus stat = MS::kSuccess;
   MItDag dagIterator (MItDag::kDepthFirst, MFn::kInvalid, &stat);
   if (MS::kSuccess != stat)
   {
      return false;
   }
   
   // Walk the DAG
   MDagPath dagPath;
   MDagPath currentPath;
   MObject lastNode = MObject::kNullObj;
   for (; !dagIterator.isDone (); dagIterator.next ())
   {
      // Retrieve the the current item pointed to by the iterator.
      MObject currentNode = dagIterator.item (&stat);
      stat = dagIterator.getPath (currentPath);
      if (!stat)
      {
         stat.perror ("MItDag::getPath");
         continue;
      }
      
      // If we are called with export selected only export active nodes
      if ((mode == MPxFileTranslator::kExportActiveAccessMode) &&
          !isObjectOrParentSelected (currentPath))
      {
         lastNode = currentNode;
         continue;
      }
      
      // Create the function set interface to the current DAG node
      // to access DAG node methods.
      MFnDagNode fnDagNode (currentPath, &stat);
      if (!stat)
      {
         stat.perror("MFnDagNode::constructor");
         continue;
      }
      
      // See if this node path is a type we understand.
      if (currentPath.hasFn (MFn::kMesh))
      {
         // Create the mesh object and choose the appropriate space.
         MFnMesh fnPoly;
         MSpace::Space space = MSpace::kWorld;
         fnPoly.setObject (currentPath);

         // Get Positions array for this mesh
         MFloatPointArray pointArray;
         int totalNumCoordinates = fnPoly.numVertices (&stat);
         fnPoly.getPoints (pointArray, space);

         // Get the Normals array for this mesh.
         MFloatVectorArray nArray;
         int totalNumNorms = fnPoly.numNormals (&stat);
         fnPoly.getNormals (nArray, space);
         
         // Get the Texture coordinates array for this mesh
         int totalNumTex = fnPoly.numUVs (&stat);
         MFloatArray us;
         MFloatArray vs;
         fnPoly.getUVs (us, vs);

         // Now the connectivity sets.
         MFnMesh fnMidBody (currentNode, &stat);
         int maxPolygons = fnMidBody.numPolygons (&stat);
         MObjectArray sets;
         MObjectArray comps;
         unsigned instanceNumber = 0;
         fnMidBody.getConnectedSetsAndMembers (instanceNumber, 
                                               sets, comps, 
                                               true);
         
         // Loop over sets of faces.
         unsigned int sk;
         MObject comp;
         MObject set;
         for (sk = 0; sk < sets.length(); sk++)
         {
            // Loop over the faces.
            set = sets[sk];
            comp = comps[sk];
            MFnSet fnSet (set, &stat);
            MItMeshPolygon faceIter (currentPath, comp, &stat);
            for (; !faceIter.isDone (); faceIter.next ())
            {
               // Compile a list for the vertex, normal, and tex coord indices
               // for this face so we can get the info back when we triangulate
               int numVerts = faceIter.polygonVertexCount (&stat);
               if (numVerts > NM_MAX_VERTICES_PER_FACE)
               {
                  cerr << "** Too many vertices on this face "<<numVerts<<"\n";
                  continue;
               }
               for (int v = 0; v < numVerts; v++)
               {
                  m_faceIndices[v].vertIdx = faceIter.vertexIndex (v, &stat);
                  m_faceIndices[v].nIdx = faceIter.normalIndex (v, &stat);
                  m_faceIndices[v].uvIdx = -1;
                  stat = faceIter.getUVIndex (v, m_faceIndices[v].uvIdx);
                  stat = fnMidBody.getFaceVertexColorIndex (faceIter.index(),
                                                            v,
                                                            m_faceIndices[v].cIdx);
                  if (stat != MS::kSuccess)
                  {
                     m_faceIndices[v].cIdx = -1;
                  }
               }

               // Loop over the triangles in this face
               int numTris;
               stat = faceIter.numTriangles (numTris);
               for (int q = 0; q < numTris; q++)
               {
                  // Get triangle data.
                  MPointArray points;
                  MIntArray indices;
                  faceIter.getTriangle (q, points, indices, space);

                  // Start a new triangle
                  NmRawTriangle* newTri = GetNextTriangle ();
                  memset (newTri, 0, sizeof (NmRawTriangle));

                  // Handle each vertex
                  for (int i = 0; i < 3; i++)
                  {
                     // Fill in position
                     int v1 = indices[i];
                     newTri->vert[i].x = pointArray[v1][0];
                     newTri->vert[i].y = pointArray[v1][1];
                     newTri->vert[i].z = pointArray[v1][2];

                     // Find normal and uv indices based on vertex index.
                     // Currently just brute force.
                     int n1 = -1;
                     int t1 = -1;
                     int c1 = -1;
                     for (int v = 0; v < numVerts; v++)
                     {
                        if (m_faceIndices[v].vertIdx == v1)
                        {
                           n1 = m_faceIndices[v].nIdx;
                           t1 = m_faceIndices[v].uvIdx;
                           c1 = m_faceIndices[v].cIdx;
                           break;
                        }
                     }
                     if (n1 == -1)
                     {
                        cerr << "** Error: Unable to find matching vertex index\n";
                        continue;
                     }

                     // Fill in Normal
                     newTri->norm[i].x = nArray[n1][0];
                     newTri->norm[i].y = nArray[n1][1];
                     newTri->norm[i].z = nArray[n1][2];

                     // Fill in texture coordinates.
                     if (t1 != -1)
                     {
                        newTri->texCoord[i].u = us[t1];
                        newTri->texCoord[i].v = vs[t1];
                     }
                     // Texture coords set to 0, 0 by default with memset.
                  } // end for i
               } // end for q
            } // end face iteration
         } // end sets iteration
      } // end if this is a mesh
      
      // See if we've been interupted.
      if (computation.isInterruptRequested())
      {
         computation.endComputation ();
         return false;
      }
      
      // remember who was last
      lastNode = currentNode;
   }
   
   return true;
} // ProcessGeom


//***************************************************************************//
//***************************************************************************//
//                        Plugin functions                                   //
//***************************************************************************//
//***************************************************************************//

////////////////////////////////////////////////////////////////////
// New export object.
////////////////////////////////////////////////////////////////////
void* 
NMFExport::creator ()
{
   return new NMFExport();
}

////////////////////////////////////////////////////////////////////
// No reader.
////////////////////////////////////////////////////////////////////
MStatus 
NMFExport::reader (const MFileObject & file,
                   const MString & options,
                   MPxFileTranslator::FileAccessMode mode)
{
   MStatus rval (MS::kSuccess);
   return rval;
}

////////////////////////////////////////////////////////////////////
// Writer - Main entrypoint
////////////////////////////////////////////////////////////////////
MStatus 
NMFExport::writer (const MFileObject& fileObject,
                   const MString& options,
                   MPxFileTranslator::FileAccessMode mode)
{
   // Setup for being interruptable
   MComputation computation;
   computation.beginComputation();
   
   // Figure out if this is an object or animation file.
   MString mname = fileObject.fullName();
   const char* fname = mname.asChar();
   
   // Find the geometry
   m_numTriangles = 0;
   if (!ProcessGeom (mode, computation))
   {
      printf ("Unable to process geometry");
      computation.endComputation ();
      return MS::kFailure;
   }
      
   // See if we've been interupted.
   if (computation.isInterruptRequested())
   {
      computation.endComputation ();
      return MS::kFailure;
   }
      
   // Open the file
   FILE* fp = fopen (fname, "wb");
   if (fp == NULL)
   {
     cerr << "Error: The file " << fname <<
       " could not be opened for writing." << endl;
     computation.endComputation ();
     return MS::kFailure;
   }
      
   // Now write out the file.
   if (!NmWriteTriangles (fp, m_numTriangles, m_triList))
   {
      computation.endComputation ();
      printf ("Unable to write geometry");
      return MS::kFailure;
   }

   // Close file
   fclose (fp);

   // We're done!
   computation.endComputation ();
   return MS::kSuccess;
} // Writer

////////////////////////////////////////////////////////////////////
// We don't support reading
////////////////////////////////////////////////////////////////////
bool 
NMFExport::haveReadMethod () const
{
   return false;
}

////////////////////////////////////////////////////////////////////
// We do support reading
////////////////////////////////////////////////////////////////////
bool 
NMFExport::haveWriteMethod () const
{
   return true;
}

////////////////////////////////////////////////////////////////////
// The extension that will be used if none is entered
////////////////////////////////////////////////////////////////////
MString 
NMFExport::defaultExtension () const
{
   return "nmf";
}

////////////////////////////////////////////////////////////////////
// This method tells Maya whether the translator can open and import files
// (returns true) or only import  files (returns false)
////////////////////////////////////////////////////////////////////
bool 
NMFExport::canBeOpened () const
{
   return true;
}

////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////
MPxFileTranslator::MFileKind 
NMFExport::identifyFile (const MFileObject & fileName,
                         const char *buffer,
                         short size) const
{
   MFileKind rval = kNotMyFileType;
   return rval;
}

////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////
//extern "C" 
MStatus 
initializePlugin (MObject obj)
{
   MStatus status;
   char version[256];
   
   strcpy (version, NMF_EXPORT_VERSION);
   
   MFnPlugin plugin (obj, "Normal Map Geometry Exporter for Maya", version, "Any");
   
   //Register the translator with the system
   status = plugin.registerFileTranslator ("nmf",
                                           "NMFExport.nmf",
                                           NMFExport::creator);
   if (!status)
   {
      status.perror ("registerFileTranslator");
   }
   return status;
}

////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////
extern "C" 
//MStatus 
uninitializePlugin (MObject obj)
{
   MStatus status;
   MFnPlugin plugin (obj);
   
   status = plugin.deregisterFileTranslator ("nmf");
   if (!status)
   {
      status.perror ("deregisterFileTranslator");
   }
   return status;
}
