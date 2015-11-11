//=============================================================================
// NMFExport.cpp -- 3D Studio MAX exporter for Normal Mapper
//=============================================================================
// $File: //depot/3darg/Tools/NormalMapper/MAXExportNMF.cpp $ $Revision: 1.1 $ $Author: hax22 $
//=============================================================================
// (C) 2002 ATI Research, Inc., All rights reserved.
//=============================================================================

// MAX headers
#include "Max.h"
#include "resource.h"
#include "istdplug.h"
#include "iparamb2.h"
#include "iparamm2.h"
#include <direct.h>
#include <commdlg.h>

// Normal Mapper includes
#include "NmFileIO.h"

extern TCHAR *GetString(int id);
extern HINSTANCE hInstance;

const int NMF_MAX_ROOTS = 1024;
const int NMF_MAX_NUM_POLYGONS = 20000;

#define NMFEXPORT_CLASS_ID Class_ID(0x2622470b, 0x16d11531)

class NMFExport : public SceneExport 
{
public:
   //Constructor/Destructor
   NMFExport();
   virtual ~NMFExport();

   // Overrides
   int ExtCount (); // Number of extensions supported
   const TCHAR * Ext (int n); // Extension #n (i.e. "3DS")
   const TCHAR * LongDesc (); // Long ASCII description 
                              // (i.e. "Autodesk 3D Studio File")
   const TCHAR * ShortDesc (); // Short ASCII description (i.e. "3D Studio")
   const TCHAR * AuthorName (); // ASCII Author name
   const TCHAR * CopyrightMessage (); // ASCII Copyright message
   const TCHAR * OtherMessage1 (); // Other message #1
   const TCHAR * OtherMessage2 (); // Other message #2
   unsigned int Version (); // Version number * 100 (i.e. v3.01 = 301)
   void ShowAbout (HWND hWnd); // Show DLL's "About..." box
   int DoExport (const TCHAR* name, ExpInterface *ei, Interface *i,
                 BOOL suppressPrompts, DWORD options);
   BOOL SupportsOptions(int ext, DWORD options);
   static HWND hParams;

private:
   int m_numRoots;               // How many selected nodes were found.
   INode *m_root[NMF_MAX_ROOTS]; // Root of the exported nodes.
   Interface* m_ip;              // Pointer to the interface
   bool m_interactive;           // Are we supressing prompts?

   // Data lists
   int m_numPolygons;
   int m_maxNumPolygons;
   NmRawTriangle* m_pgonList;

   // Geometry related routines
   bool ProcessGeom (INode *node, bool exportWorld);
   bool ExportGeom (INode* node);
   void GetTexCoords (NmRawTriangle* newPgon, int idx, Mesh* mesh, int face);
   NmRawTriangle* GetNextPolygon (void);

   // Helper functions.
   TriObject* GetTriObjectFromNode (INode *node, int &deleteIt);
   bool FindRootNode (INode *node);
   float FindVertexAngle (Point3 vList[3], int face, int corner);
};

// Max Exporter class description.
class MAXExportNMFClassDesc:public ClassDesc2 
{
public:
   int IsPublic() {return 1;}
   void * Create (BOOL loading = FALSE) {return new NMFExport ();}
   const TCHAR * ClassName () {return GetString (IDS_CLASS_NAME);}
   SClass_ID SuperClassID () {return SCENE_EXPORT_CLASS_ID;}
   Class_ID ClassID () {return NMFEXPORT_CLASS_ID;}
   const TCHAR* Category () {return GetString (IDS_CATEGORY);}
   // returns fixed parsable name (scripter-visible name)
   const TCHAR* InternalName () { return _T("NMFExport"); }
   // returns owning module handle
   HINSTANCE HInstance () { return hInstance; }
};

// Class to generate smooth normals
class VNormal
{
   public:
      int numUsed;
      int init;
      Point3 norm[33];
      DWORD smooth[33];
      Point3 smoothNormal;

      VNormal() { init=FALSE; numUsed=0; smooth[0]=0; norm[0]=Point3(0,0,0);
                  smoothNormal=Point3(0,0,0); }
      VNormal(Point3 &n,DWORD s) { init=TRUE; norm[0]=n; smooth[0]=s;
                                   numUsed=1; smoothNormal=Point3(0,0,0); }
      ~VNormal() { }
      void AddNormal(Point3 &n,DWORD s);
      Point3 &GetNormal(DWORD s);
      Point3 &GetSmoothNormal() { return smoothNormal; }
      void Normalize(int idx=0);
};

// Variables
bool gExportWorld = true;
static MAXExportNMFClassDesc MAXExportNMFDesc;
ClassDesc2* GetMAXExportNMFDesc() {return &MAXExportNMFDesc;}

// Fuction
float AtiMAXFindVertexAngle (Mesh *mesh, int face, int corner);

////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////
NMFExport::NMFExport ()
{
   m_numRoots = 0;
   for (int r = 0; r < NMF_MAX_ROOTS; r++)
   {
      m_root[r] = NULL;
   }
   m_ip = NULL;
   m_interactive = false;
   m_numPolygons = 0;
   m_maxNumPolygons = 0;
   m_pgonList = NULL;
}

////////////////////////////////////////////////////////////////////
// Destructor
////////////////////////////////////////////////////////////////////
NMFExport::~NMFExport () 
{
   m_numRoots = 0;
   for (int r = 0; r < NMF_MAX_ROOTS; r++)
   {
      m_root[r] = NULL;
   }
   m_ip = NULL;
   m_interactive = false;
   m_numPolygons = 0;
   m_maxNumPolygons = 0;
   delete [] m_pgonList;
   m_pgonList = NULL;
}

////////////////////////////////////////////////////////////////////
// Returns the number of file name extensions supported by the plug-in.
////////////////////////////////////////////////////////////////////
int 
NMFExport::ExtCount ()
{
   return 1;
}

////////////////////////////////////////////////////////////////////
//  Return the 'i-th' file name extension (i.e. "3DS").
////////////////////////////////////////////////////////////////////
const TCHAR *
NMFExport::Ext (int n)
{
   switch (n)
   {
      case 0:
         return _T("NMF");
         break;
      default:
         return _T("HUH");
         break;
   }
}

////////////////////////////////////////////////////////////////////
// Return long ASCII description (i.e. "Targa 2.0 Image File")
////////////////////////////////////////////////////////////////////
const TCHAR *
NMFExport::LongDesc ()
{
   return _T("Normal Mapper File");
}

////////////////////////////////////////////////////////////////////
// Return short ASCII description (i.e. "Targa")
////////////////////////////////////////////////////////////////////
const TCHAR *
NMFExport::ShortDesc () 
{
   return _T("NMF");
}

////////////////////////////////////////////////////////////////////
// Return ASCII Author name
////////////////////////////////////////////////////////////////////
const TCHAR *
NMFExport::AuthorName ()
{
   return _T("Dave Gosselin");
}

////////////////////////////////////////////////////////////////////
// Return ASCII Copyright message
////////////////////////////////////////////////////////////////////
const TCHAR *
NMFExport::CopyrightMessage () 
{
   return _T("(C) 2000 ATI Research, Inc.  All rights reserved.");
}

////////////////////////////////////////////////////////////////////
// Return Other message #1 if any
////////////////////////////////////////////////////////////////////
const TCHAR *
NMFExport::OtherMessage1 () 
{
   return _T("");
}

////////////////////////////////////////////////////////////////////
// Return other message #2 in any
////////////////////////////////////////////////////////////////////
const TCHAR *
NMFExport::OtherMessage2 () 
{
   return _T("");
}

////////////////////////////////////////////////////////////////////
//TODO: Return Version number * 100 (i.e. v3.01 = 301)
////////////////////////////////////////////////////////////////////
unsigned int 
NMFExport::Version ()
{
   
   return 201;
}

////////////////////////////////////////////////////////////////////
//
////////////////////////////////////////////////////////////////////
void 
NMFExport::ShowAbout (HWND hWnd)
{
}

////////////////////////////////////////////////////////////////////
// Magic function to make export selected work.
////////////////////////////////////////////////////////////////////
BOOL 
NMFExport::SupportsOptions (int ext, DWORD options)
{
   if (options == SCENE_EXPORT_SELECTED)
   {
      return TRUE;
   }
   else
   {
      return FALSE;
   }
}

////////////////////////////////////////////////////////////////////
// The main entrypoint into the exporter.
////////////////////////////////////////////////////////////////////
int 
NMFExport::DoExport (const TCHAR* name, ExpInterface* ei, Interface* i, 
                     BOOL suppressPrompts, DWORD options)
{   
   // Save off some values.
   m_ip = i;
   if (options & SCENE_EXPORT_SELECTED)
   {
      gExportWorld = false;
   }

   // Get the root node and recursively gather the information we need.
   m_numRoots = 0;
   for (int root = 0; root < NMF_MAX_ROOTS; root++)
   {
      m_root[root] = NULL;
   }
   FindRootNode (i->GetRootNode());
   if (m_numRoots < 1)
   {
      MessageBox (NULL, "Unable to find selected object", "NMF Error", MB_OK);
      return FALSE;
   }

   // Clear out our counters
   m_numPolygons = 0;

   // Loop over the number of max objects selected.
   for (root = 0; root < m_numRoots; root++)
   {
      // Process raw geometry
      if (!ProcessGeom (m_root[root], gExportWorld))
      {
         return FALSE;
      }
   }
     
   // Open output file.
   FILE *fp = fopen (name, "wb");
   if (fp == NULL) 
   {
      MessageBox (NULL, "Unable to open output file", "NMF Error", MB_OK);
      return FALSE;
   }
   if (!NmWriteTriangles (fp, m_numPolygons, m_pgonList))
   {
      MessageBox (NULL, "Unable to write output file", "NMF Error", MB_OK);
      fclose (fp);
      return FALSE;
   }
   fclose (fp);

   // All finished
   return TRUE;
} // end DoExport

////////////////////////////////////////////////////////////////////
// This routine traverses the node structure and added all the triangles
// from the meshes into m_pgonList using their state at the given time.
// If usebones is true the full position is determined, if false
// just the "morph" is computed (bones are at basically at time zero).
////////////////////////////////////////////////////////////////////
bool
NMFExport::ProcessGeom (INode *node, bool exportWorld)
{
   // Export this node if selected or we are exporting the whole world
   ObjectState os = node->EvalWorldState (0);
   if (os.obj)
   {
      switch (os.obj->SuperClassID ())
      {
         case GEOMOBJECT_CLASS_ID:
            if (!ExportGeom (node))
            {
               return false;
            }
            break;
            
         default:
            break;
      }
   }
   
   // Export the node's children.
   for (int i = 0; i < node->NumberOfChildren (); i++)
   {
      if (!ProcessGeom (node->GetChildNode (i), exportWorld))
      {
         return false;
      }
   }
   return true;
}

////////////////////////////////////////////////////////////////////
// This routine gets the texture coordinate(s) for vertex i in
// the new polygon
////////////////////////////////////////////////////////////////////
void
NMFExport::GetTexCoords (NmRawTriangle* newPgon, int idx, Mesh* mesh, int face)
{
   newPgon->texCoord[idx].u = 0.0f;
   newPgon->texCoord[idx].v = 0.0f;
   if (mesh->getNumMaps() > 0)
   {
      UVVert* tv = mesh->mapVerts(1);
      TVFace* tf = mesh->mapFaces(1);
      if ((tv != NULL) && (tf != NULL))
      {
         newPgon->texCoord[idx].u = tv[tf[face].t[idx]].x;
         newPgon->texCoord[idx].v = tv[tf[face].t[idx]].y;
      }
   }
}

////////////////////////////////////////////////////////////////////
// This routine pulls all the triangles out of the mesh.
////////////////////////////////////////////////////////////////////
bool
NMFExport::ExportGeom (INode* node)
{
   // Get the TriObject
   BOOL needDel = FALSE;
   TriObject* tri = GetTriObjectFromNode (node, needDel);
   if (!tri) 
   {
      return true;
   }
   
   // Get the mesh
   Mesh* mesh = &tri->mesh;
   Matrix3 tm = node->GetObjTMAfterWSM (0);
   VNormal* vertNorm = new VNormal [mesh->getNumVerts()];
   int first = m_numPolygons; // Save off where we started adding polys
   
   // Run through the mesh pulling out the polygons.
   for (int i = 0; i < mesh->getNumFaces(); i++)
   {
      // Create a new polygon object
      NmRawTriangle* newPgon = GetNextPolygon ();
      memset (newPgon, 0, sizeof (NmRawTriangle));
         
      // Vertex 0
      Point3 v0 = (tm * mesh->verts[mesh->faces[i].v[0]]);
      newPgon->vert[0].x = v0.x;
      newPgon->vert[0].y = v0.y;
      newPgon->vert[0].z = v0.z;
      GetTexCoords (newPgon, 0, mesh, i);
         
      // Vertex 1
      Point3 v1 = (tm * mesh->verts[mesh->faces[i].v[1]]);
      newPgon->vert[1].x = v1.x;
      newPgon->vert[1].y = v1.y;
      newPgon->vert[1].z = v1.z;
      GetTexCoords (newPgon, 1, mesh, i);
         
      // Vertex 2
      Point3 v2 = (tm * mesh->verts[mesh->faces[i].v[2]]);
      newPgon->vert[2].x = v2.x;
      newPgon->vert[2].y = v2.y;
      newPgon->vert[2].z = v2.z;
      GetTexCoords (newPgon, 2, mesh, i);
         
      // Compute face normal.
      Point3 fnorm = Normalize((v1-v0)^(v2-v1));
      newPgon->norm[0].x = fnorm.x;
      newPgon->norm[0].y = fnorm.y;
      newPgon->norm[0].z = fnorm.z;
      newPgon->norm[1].x = fnorm.x;
      newPgon->norm[1].y = fnorm.y;
      newPgon->norm[1].z = fnorm.z;
      newPgon->norm[2].x = fnorm.x;
      newPgon->norm[2].y = fnorm.y;
      newPgon->norm[2].z = fnorm.z;
      
      // Add normal into per-vertex normals
      Point3 ptList[3];
      ptList[0] = v0;
      ptList[1] = v1;
      ptList[2] = v2;
      for (int j = 0; j < 3; j++)
      {
         Point3 tmpPoint = fnorm * FindVertexAngle (ptList, i, j);
         vertNorm[mesh->faces[i].v[j]].AddNormal (tmpPoint,
                                                  mesh->faces[i].smGroup);
      }
   }
   
   // Make another pass over the new polygons adding in the smoothed normal
   // otherwise the face normal will be used (computed above)
   for (i = first; i < m_numPolygons; i++)
   {
      int idx = i - first;
      if (mesh->faces[idx].smGroup != 0) // Smoothed
      {
         for (int j = 0; j < 3; j++)
         {
            m_pgonList[i].norm[j].x = vertNorm[mesh->faces[idx].v[j]].GetNormal(mesh->faces[idx].smGroup).x;
            m_pgonList[i].norm[j].y = vertNorm[mesh->faces[idx].v[j]].GetNormal(mesh->faces[idx].smGroup).y;
            m_pgonList[i].norm[j].z = vertNorm[mesh->faces[idx].v[j]].GetNormal(mesh->faces[idx].smGroup).z;
            float tmpf = 1.0f / (float)sqrt(
                              m_pgonList[i].norm[j].x*m_pgonList[i].norm[j].x +
                              m_pgonList[i].norm[j].y*m_pgonList[i].norm[j].y +
                              m_pgonList[i].norm[j].z*m_pgonList[i].norm[j].z);
            m_pgonList[i].norm[j].x *= tmpf;
            m_pgonList[i].norm[j].y *= tmpf;
            m_pgonList[i].norm[j].z *= tmpf;
         }
      }
   }
   delete [] vertNorm;
   
   // Free the tris.
   if (needDel)
   {
      tri->DeleteMe ();
   }
   
   return true;
}

////////////////////////////////////////////////////////////////////
// This routine adds the material to the list of materials (if needed)
// and returns the index for that material.
////////////////////////////////////////////////////////////////////
NmRawTriangle*
NMFExport::GetNextPolygon ()
{
   if (m_pgonList == NULL)
   {
      m_pgonList = new NmRawTriangle [NMF_MAX_NUM_POLYGONS];
      m_maxNumPolygons = NMF_MAX_NUM_POLYGONS;
   }
   if (m_numPolygons >= m_maxNumPolygons)
   {
      int last = m_maxNumPolygons * sizeof (NmRawTriangle);
      m_maxNumPolygons += NMF_MAX_NUM_POLYGONS;
      NmRawTriangle* pgn = new NmRawTriangle [m_maxNumPolygons];
      memcpy (pgn, m_pgonList, last);
      delete [] m_pgonList;
      m_pgonList = pgn;
   }
   NmRawTriangle* ret = &(m_pgonList[m_numPolygons]);
   m_numPolygons++;
   return ret;
}

////////////////////////////////////////////////////////////////////
// Return a pointer to a TriObject given an INode or return NULL
// if the node cannot be converted to a TriObject
////////////////////////////////////////////////////////////////////
TriObject* 
NMFExport::GetTriObjectFromNode (INode *node, int &deleteIt)
{
   deleteIt = FALSE;
   Object *obj = node->EvalWorldState(0).obj;
   if (obj->CanConvertToType(Class_ID(TRIOBJ_CLASS_ID, 0))) 
   { 
      TriObject *tri = (TriObject *) obj->ConvertToType(0, 
                                                 Class_ID(TRIOBJ_CLASS_ID, 0));
      
      // Note that the TriObject should only be deleted
      // if the pointer to it is not equal to the object
      // pointer that called ConvertToType()
      if (obj != tri) 
      {
         deleteIt = TRUE;
      }
      return tri;
   }
   else 
   {
      return NULL;
   }
}

////////////////////////////////////////////////////////////////////
// This routine finds the root slected node.
////////////////////////////////////////////////////////////////////
bool 
NMFExport::FindRootNode (INode *node)
{
   // Export this node if selected or we are exporting the whole world
   ObjectState os = node->EvalWorldState (0);
   if (gExportWorld || (node->Selected()))
   {
      m_root[m_numRoots] = node;
      m_numRoots++;
      return true;
   }
   
   // Export the node's children.
   for (int i = 0; i < node->NumberOfChildren (); i++)
   {
      FindRootNode (node->GetChildNode (i));
   }
   
   return false;
}

//=============================================================================
// Corner is 0, 1, or 2 -- which corner do we want the angle of?
//=============================================================================
float
NMFExport::FindVertexAngle (Point3 vList[3], int face, int corner)
{
   int cnext = (corner+1)%3;
   int cprev = (corner+2)%3;
   
   // Get edge vectors:
   Point3 A = vList[cnext] - vList[corner];
   Point3 B = vList[corner] - vList[cprev];
   
   // Normalize the edge-vectors, but return 0 if either has 0 length.
   float len = Length(A);
   if (!len)
      return len;
   A = A/len;
   
   len = Length(B);
   if (!len)
      return len;
   B = B/len;
   
   // The dot product gives the cosine of the angle:
   float dp = DotProd (A,B);
   if (dp>1) dp=1.0f;   // shouldn't happen, but might
   if (dp<-1) dp=-1.0f;   // shouldn't happen, but might
   return acos(dp);
}


//=============================================================================
// Add a normal to the list if the smoothing group bits overlap, 
// otherwise create a new vertex normal in the list
//=============================================================================
void 
VNormal::AddNormal (Point3 &n, DWORD s)
{
   int i;

   smoothNormal += n;

   //Add first
   if (init == FALSE)
   {
      norm[0]   += n;
      smooth[0] |= s;
      init    = TRUE;
      numUsed = 1;
      return;
   }

   //Search for a shared smoothing group
   for (i=0; i<numUsed; i++)
   {
      if ((smooth[i] & s))
      {
         norm[i]   += n;
         smooth[i] |= s;
         return;
      }
   }

   //Add it
   if (numUsed < 33)
   {
      norm[numUsed]   = n;
      smooth[numUsed] = s;
      numUsed++;
   }
}

//=============================================================================
// Retrieves a normal if the smoothing groups overlap or there is 
// only one in the list
//=============================================================================
Point3& 
VNormal::GetNormal (DWORD s)
{
   int i;

   for (i=0; i<numUsed; i++)
   {
      if ((smooth[i] & s))
      {
         return norm[i];
      }
   }

   //Else, return first normal
   return norm[0];
}

//=============================================================================
// Normalize each normal in the list
//=============================================================================
void 
VNormal::Normalize (int idx)
{
   int i;

   smoothNormal = ::Normalize(smoothNormal);

   for (i=idx+1; i<numUsed; i++)
   {
      if ((smooth[i] & smooth[idx])) //If these share a smoothing group
      {
         //Combine normal and smoothing bits
         norm[idx] += norm[i];
         smooth[idx] |= smooth[i];

         //Remove [i] elements
         memmove(&(norm[i]), &(norm[i+1]), sizeof(Point3)*(numUsed-i-1));
         memmove(&(smooth[i]), &(smooth[i+1]), sizeof(DWORD)*(numUsed-i-1));
      }
   }

   norm[idx] = ::Normalize(norm[idx]);

   if (idx < (numUsed-1))
      Normalize(idx+1);
}

//=============================================================================
// Corner is 0, 1, or 2 -- which corner do we want the angle of?
//=============================================================================
float 
AtiMAXFindVertexAngle (Mesh *mesh, int face, int corner)
{
   int cnext = (corner+1)%3;
   int cprev = (corner+2)%3;
   DWORD *vv = mesh->faces[face].v;

   // Get edge vectors:
   Point3 A = mesh->verts[vv[cnext]] - mesh->verts[vv[corner]];
   Point3 B = mesh->verts[vv[corner]] - mesh->verts[vv[cprev]];

   // Normalize the edge-vectors, but return 0 if either has 0 length.
   float len = Length(A);
   if (!len)
      return len;
   A = A/len;

   len = Length(B);
   if (!len)
      return len;
   B = B/len;

   // The dot product gives the cosine of the angle:
   float dp = DotProd (A,B);
   if (dp>1) dp=1.0f;   // shouldn't happen, but might
   if (dp<-1) dp=-1.0f;   // shouldn't happen, but might
   return acos(dp);
}
