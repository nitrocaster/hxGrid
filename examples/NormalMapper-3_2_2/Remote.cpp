#ifndef ATI_MAC_OS
#include <windows.h>
#endif
#include <stdio.h> 
#include <math.h>
#include <float.h>
#include <map>

#include "NmFileIO.h"
#include "TGAIO.h"
#include "ArgFileIO.h"
#include "AtiOctree.h"
#include "AtiTriBoxMoller.h"
#include "windows.h"
#include "conio.h"
#include "remote.h"
#include "nmCommon.h"
#include "IAgent.h"

// Octree stack.
static const int MAX_TREE_NODES = 64;

// Occlusion constants
static double gDistanceOffset = 0.00001;
static double gZVec[3] = {0.0f, 0.0f, 1.0f};

static const char* dataDesc = "NormalMapperData";

//---------------------------------------
class TSessionData
{
public:
 AtiOctree* octree;
 NmRawTriangle* highTris;
 int highNumTris;
 NmTangentMatrix* hTangentSpace;
 float* bumpMap;
 int bumpWidth;
 int bumpHeight;
 int gNumSamples;
 NmSample* gSamples;
 int numRays;
 NmRawPointD* rays;
 double* rayWeights;

 TSessionData()
 {
  octree = NULL;
  highTris = NULL;
  hTangentSpace = NULL;
  bumpMap = NULL;
  gSamples = NULL;
  rays = NULL;
  rayWeights = NULL;
 }

 ~TSessionData()
 {
  delete octree;
  delete[] highTris;                 
  delete[] gSamples;                 
  if (hTangentSpace!=NULL) delete[] hTangentSpace;
  if (bumpMap!=NULL) delete[] bumpMap;
  if (rays!=NULL) delete[] rays;
  if (rayWeights!=NULL) delete[] rayWeights;
 }
}; 
//---------------------------------------

typedef std::map<DWORD, TSessionData> TSessionDataCache;

static TSessionDataCache sessionDataCache;

//session data cache access
static CRITICAL_SECTION CS;

//////////////////////////////////////////////////////////////////////////
// Add a cell into our list.
//////////////////////////////////////////////////////////////////////////
static inline void
AddCell (AtiOctreeCell* cell, int* numCells, int& gMaxCells, AtiOctreeCell** &gCell)
{
 // See if we have enough space first.
 if ((*numCells) >= gMaxCells)
 {
  gMaxCells += MAX_TREE_NODES;
  AtiOctreeCell** tmp = new AtiOctreeCell* [gMaxCells];
  if (tmp == NULL)
  {
   //NmPrint ("ERROR: Unable to allocate cell stack!\n");
   exit (-1);
  }
  memset (tmp, 0, sizeof (AtiOctreeCell*) * gMaxCells);
  memcpy (tmp, gCell, sizeof (AtiOctreeCell*) * (*numCells));
  delete [] gCell;
  gCell = tmp;
  tmp = NULL;
 }

 // Place the pointer in the list
 gCell[(*numCells)] = cell;
 (*numCells)++;
}

//////////////////////////////////////////////////////////////////////////
// Test if the new normal is a better fit than the last one.
//////////////////////////////////////////////////////////////////////////
static inline bool
IntersectionIsBetter (int rule, NmRawPointD* norm,
                      double nNorm[3], NmRawPointD* nIntersect,
                      double lNorm[3], NmRawPointD* lIntersect,
                      double gMaxAngle, double gDistance,
                      double gEpsilon)
{
 // First see if the normal is roughly in the same direction as the low
 // resoultion normal.
 if (VEC_DotProduct (nNorm, norm->v) > gMaxAngle)
 {
  // If this is the first intersection we've found.
  bool first = false;
  if ((lNorm[0] == 0.0) && (lNorm[1] == 0.0) && (lNorm[2] == 0.0))
  {
   first = true;
   //return true;
  }

  // Which ruleset to use.
  switch (rule)
  {
  default:
   //            NmPrint ("Error: Unknown rules set (%d)!\n", rule);
   exit (-1);

  case NORM_RULE_CLOSEST:
   // Pick the closest
   if ( (fabs (nIntersect->x) < gDistance) &&
    ((fabs(nIntersect->x) < fabs(lIntersect->x)) || first) )
   {
    return true;
   }
   return false;

  case NORM_RULE_FRONT_CLOSEST:
   // Pick the closest in front of the low res.
   if ( (nIntersect->x >= -gEpsilon) && 
    (nIntersect->x < gDistance) &&
    ((nIntersect->x < lIntersect->x) || first) )
   {
    return true;
   }
   return false;


  case NORM_RULE_BEST_CLOSEST:
   // Pick the closest, if equal pick the one closer to low res norm
   if ( (fabs(nIntersect->x) < gDistance) &&
    ((fabs(nIntersect->x) < fabs(lIntersect->x)) || first) )
   {
    return true;
   }
   else if ( (fabs(nIntersect->x) < gDistance) &&
    (fabs(nIntersect->x) == fabs(lIntersect->x)) &&
    (VEC_DotProduct (nNorm, norm->v) >
    VEC_DotProduct (lNorm, norm->v)) )
   {
    return true;
   }
   return false;

  case NORM_RULE_FRONT_BEST_CLOSEST:
   // Pick the closest in front of low res,
   // if equal pick the one closer to low res norm
   if ( (nIntersect->x >= -gEpsilon) && 
    (nIntersect->x < gDistance) &&
    ((nIntersect->x < lIntersect->x) || first) )
   {
    return true;
   }
   else if ( (nIntersect->x < gDistance) &&
    (nIntersect->x == lIntersect->x) &&
    (VEC_DotProduct (nNorm, norm->v) >
    VEC_DotProduct (lNorm, norm->v)))
   {
    return true;
   }
   return false;

  case NORM_RULE_FARTHEST:
   // Pick the furthest
   if ( (fabs(nIntersect->x) < gDistance) &&
    ((fabs(nIntersect->x) > fabs(lIntersect->x)) || first) )
   {
    return true;
   }
   return false;

  case NORM_RULE_FRONT_FURTHEST:
   // Pick the furthest in front of low res
   if ( (nIntersect->x >= -gEpsilon) && 
    (nIntersect->x < gDistance) &&
    ((nIntersect->x > lIntersect->x) || first) )
   {
    return true;
   }
   return false;

  case NORM_RULE_BEST_FARTHEST:
   // Pick the furthest, if equal pick the one closer to low res norm
   if ( (fabs (nIntersect->x) < gDistance) &&
    ((fabs(nIntersect->x) > fabs(lIntersect->x)) || first) )
   {
    return true;
   }
   else if ( (fabs (nIntersect->x) < gDistance) && 
    (fabs(nIntersect->x) == fabs(lIntersect->x)) &&
    (VEC_DotProduct (nNorm, norm->v) >
    VEC_DotProduct (lNorm, norm->v)) )
   {
    return true;
   }
   return false;

  case NORM_RULE_FRONT_BEST_FURTHEST:
   // Pick the furthest in front of low res,
   // if equal pick the one closer to low res norm
   if ( (nIntersect->x >= -gEpsilon) && 
    (nIntersect->x < gDistance) && 
    ((nIntersect->x > lIntersect->x) || first) )
   {
    return true;
   }
   else if ( (nIntersect->x < gDistance) &&  
    (fabs(nIntersect->x) == fabs(lIntersect->x)) &&
    (VEC_DotProduct (nNorm, norm->v) >
    VEC_DotProduct (lNorm, norm->v)) )
   {
    return true;
   }
   return false;

  case NORM_RULE_MIXED:
   // See if this is an intersection behind low res face
   if (nIntersect->x <= 0)
   {
    // We prefer normals that are in front of the low res face, if
    // the last intersection was in front ignore this one.
    if ((lIntersect->x > 0) && !first)
    {
     return false;
    }

    // Pick the closer of the two since this intersection is behind
    // the low res face.
    if (  (fabs (nIntersect->x) < gDistance) &&  
     ((fabs(nIntersect->x) < fabs(lIntersect->x)) || first) )
    {
     return true;
    }
   }
   else // Intersection in front of low res face
   {
    if ( (lIntersect->x < 0) && (fabs (nIntersect->x) < gDistance) )
    {
     // We automatically accept this one since it's in front
     // and the last one was behind.
     return true;
    } 
    else if ( (fabs (nIntersect->x) < gDistance) &&  
     ((fabs(nIntersect->x) > fabs(lIntersect->x)) ||
     first) )
    {
     // This one is the furthest in front so pick this one.
     return true;
    }
   }
   return false;

  case NORM_RULE_BEST_MIXED:
   // See if this is an intersection behind low res face
   if (nIntersect->x <= 0)
   {
    // We prefer normals that are in front of the low res face, if
    // the last intersection was in front ignore this one.
    if ((lIntersect->x > 0) && !first)
    {
     return false;
    }

    // Pick the closer of the two since this intersection is behind
    // the low res face. If the two intersections are equidistant
    // pick the normal that more closely matches the low res normal
    if ( (lIntersect->x < 0) && (fabs (nIntersect->x) < gDistance) )
    {
     // We automatically accept this one since it's in front
     // and the last one was behind.
     return true;
    }
    else  if ( (fabs (nIntersect->x) < gDistance) &&  
     ((fabs(nIntersect->x) < fabs(lIntersect->x)) || first) )
    {
     return true;
    }
    else if ( (fabs (nIntersect->x) < gDistance) &&  
     (fabs(nIntersect->x) == fabs(lIntersect->x)) &&
     (VEC_DotProduct (nNorm, norm->v) >
     VEC_DotProduct (lNorm, norm->v)) )
    {
     return true;
    }
   }
   else // Intersection in front of low res face
   {
    // Pick the furthest intersection since the intersection is in
    // front of the low res face. If they are equidistant pick the
    // normal that most closely matches the low res normal.
    if ((fabs(nIntersect->x) > fabs(lIntersect->x)) || first)
    {
     return true;
    }
    else if ( (fabs (nIntersect->x) < gDistance) &&  
     (fabs(nIntersect->x) == fabs(lIntersect->x)) &&
     (VEC_DotProduct (nNorm, norm->v) >
     VEC_DotProduct (lNorm, norm->v)) )
    {
     return true;
    }
   }
   return false;

  case NORM_RULE_BEST:
   // Pick the one closer to low res norm
   if ( (fabs (nIntersect->x) < gDistance) &&  
    (VEC_DotProduct (nNorm, norm->v) >
    VEC_DotProduct (lNorm, norm->v)) )
   {
    return true;
   }
   return false;
  }
 } // end if we pass angle test

 return false;
}

//////////////////////////////////////////////////////////////////////////
// Figure out if we have an intersection from the given point, if we do
// make sure it's the "best" one. Returns true if it found an intersection
// false otherwise. It places the normal into newNormal and position into
// newPos.
//////////////////////////////////////////////////////////////////////////
inline bool
FindBestIntersection (NmRawPointD& pos, NmRawPointD& norm, AtiOctree* octree,
                      NmRawTriangle* highTris, NmTangentMatrix* hTangentSpace,
                      float* bumpMap, int bumpHeight, int bumpWidth,
                      double newNorm[3], double newPos[3],
                      double* displacement,
                      int gNormalRules,
                      double gMaxAngle,
                      double gDistance,
                      double gEpsilon,
                      int& gMaxCells, AtiOctreeCell** &gCell)
{
 // Clear outputs.
 newNorm[0] = 0.0;
 newNorm[1] = 0.0;
 newNorm[2] = 0.0;
 newPos[0] = 0.0;
 newPos[1] = 0.0;
 newPos[2] = 0.0;

 // Create negative normal
 NmRawPointD negNorm;
 negNorm.x = -norm.x;
 negNorm.y = -norm.y;
 negNorm.z = -norm.z;

 // Some stats.
 int cellCount = 0;
 int hitTriCount = 0;
 int triCount = 0;

 // Walk the octree looking for intersections.
 NmRawPointD intersect;
 NmRawPointD lastIntersect;
 int numCells = 0;
 AddCell (octree->m_root, &numCells, gMaxCells, gCell);
 while (numCells > 0)
 {
  // Take the cell from the list.
  cellCount++;
  numCells--;
  AtiOctreeCell* currCell = gCell[numCells];

  // See if this is a leaf node
  bool leaf = true;
  for (int c = 0; c < 8; c++)
  {
   if (currCell->m_children[c] != NULL)
   {
    leaf = false;
    break;
   }
  }

  // If we are a leaf check the triangles
  if (leaf)
  {
   // Run through the triangles seeing if the ray intersects.
   for (int t = 0; t < currCell->m_numItems; t++)
   {
    // Save off current triangle.
    NmRawTriangle* hTri = &highTris[currCell->m_item[t]];

    // See if it intersects.
    triCount++;
    if (IntersectTriangle (pos.v, norm.v, hTri->vert[0].v,
     hTri->vert[1].v, hTri->vert[2].v,
     &intersect.x, &intersect.y,
     &intersect.z))
    {
     // Keep some statistics.
     hitTriCount++;

     // Figure out new normal and position
     double b0 = 1.0 - intersect.y - intersect.z;
     double np[3]; 
     double nn[3];
     BaryInterpolate (hTri, b0, intersect.y, intersect.z, np, nn);

     // Debug this intersection test if requested.

     // See if this should be the normal for the map.
     if (IntersectionIsBetter (gNormalRules, &norm,
      nn, &intersect,
      newNorm, &lastIntersect,
      gMaxAngle,
      gDistance,
      gEpsilon))
     {
      // Perturb by bump map
      if (bumpMap != NULL)
      {
       GetPerturbedNormal (hTri, b0, intersect.y,
        intersect.z, bumpMap,
        bumpWidth, bumpHeight,
        hTangentSpace[currCell->m_item[t]].m,
        nn);
      }

      // Copy over values
      memcpy (newNorm, nn, sizeof (double)*3);
      memcpy (newPos, np, sizeof (double)*3);
      memcpy (&lastIntersect, &intersect,
       sizeof (NmRawPointD));
     } // end if this intersection is better
#ifdef DEBUG_INTERSECTION
     else if (gDbgIntersection)
     {
      NmPrint ("      <<< Intersection is worse!\n");
     }
#endif
    } // end if we have an intersection
   } // end for t (num triangles in this cell)
  } // end if leaf
  else
  {  // Non-leaf, add the children to the list if their bounding
   // box intersects the ray.
   for (int c = 0; c < 8; c++)
   {
    if (currCell->m_children[c] != NULL)
    {
     // Save off current child.
     AtiOctreeCell* child = currCell->m_children[c];

     // If the ray intersects the box
     if (RayIntersectsBox (&pos, &norm, &child->m_boundingBox))
     {
      AddCell (child, &numCells, gMaxCells, gCell);
     } // end if the ray intersects this bounding box.
     else if (RayIntersectsBox (&pos, &negNorm, &child->m_boundingBox))
     {
      AddCell (child, &numCells, gMaxCells, gCell);
     } // end if the ray intersects this bounding box.
     // else do nothing, we'll never intersect any triangles
     // for it's children.
    } // end if we have a cell
   } // end for c (8 children)
  } // end else non-leaf node.
 } // end while cells

 /*
 // Save off some stats.
 if (triCount > gMaxTrisTested)
 {
 gMaxTrisTested = triCount;
 }
 if (hitTriCount > gMaxTrisHit)
 {
 gMaxTrisHit = hitTriCount;
 }
 if (cellCount > gMaxCellsTested)
 {
 gMaxCellsTested = cellCount;
 }
 */
 // Test if we found an intersection.
 if ( (newNorm[0] != 0.0) || (newNorm[1] != 0.0) || (newNorm[2] != 0.0) )
 {
  (*displacement) = lastIntersect.x;
  return true;
 }
 return false;
} // end of FindBestIntersection


//////////////////////////////////////////////////////////////////////////
// Given the position, normal, and set of rays compute the bent normal and
// occlusion term.
//////////////////////////////////////////////////////////////////////////
void
ComputeOcclusion (NmRawPointD* newPos, NmRawPointD* newNorm, int numTris,
                  NmRawTriangle* tri, AtiOctree* octree, int numRays,
                  NmRawPointD* rays, double* rayWeights,
                  NmRawPointD* bentNormal, double* occlusion,
                  int& gMaxCells, AtiOctreeCell** &gCell)
{
#ifdef _DEBUG
 if ((newPos == NULL) || (newNorm == NULL) || (tri == NULL) ||
  (octree == NULL) || (rays == NULL) || (rayWeights == NULL) ||
  (bentNormal == NULL) || (occlusion == NULL))
 {
  //NmPrint ("ERROR: Incorrect arguments passed!\n");
  exit (-1);
 }
#endif
 // Clear results. 
 // Bent normal should at minimum be the regular normal (I think).
 bentNormal->x = newNorm->x;
 bentNormal->y = newNorm->y;
 bentNormal->z = newNorm->z;
 (*occlusion) = 0.0;
 double hit = 0.0f;
 double num = 0.0f;

 // Compute offset vertex
 NmRawPointD pos;
 pos.x = newPos->x + (newNorm->x * gDistanceOffset);
 pos.y = newPos->y + (newNorm->y * gDistanceOffset);
 pos.z = newPos->z + (newNorm->z * gDistanceOffset);

 // Compute rotation matrix to match hemisphere to normal
 double rotMat[16];
 FromToRotation (rotMat, gZVec, newNorm->v);

 // Shoot the rays
 for (int r = 0; r < numRays; r++)
 {
  // First rotate the ray into position
  NmRawPointD oRay;
  oRay.x = rays[r].x*rotMat[0] + rays[r].y*rotMat[4] + rays[r].z*rotMat[8];
  oRay.y = rays[r].x*rotMat[1] + rays[r].y*rotMat[5] + rays[r].z*rotMat[9];
  oRay.z = rays[r].x*rotMat[2] + rays[r].y*rotMat[6] + rays[r].z*rotMat[10];

  // Walk the Octree to find triangle intersections.
  bool intersect = false;
  int numCells = 0;
  int cellCount = 0;
  int triCount = 0;
  AddCell (octree->m_root, &numCells, gMaxCells, gCell);
  while ((numCells > 0) && !intersect)
  {
   // Take the cell from the list.
   cellCount++;
   numCells--;
   AtiOctreeCell* currCell = gCell[numCells];

   // See if this is a leaf node
   bool leaf = true;
   for (int c = 0; c < 8; c++)
   {
    if (currCell->m_children[c] != NULL)
    {
     leaf = false;
     break;
    }
   }

   // If we are a leaf check the triangles
   if (leaf)
   {
    // Run through the triangles seeing if the ray intersects.
    for (int t = 0; t < currCell->m_numItems; t++)
    {
     // Save off current triangle.
     NmRawTriangle* currTri = &(tri[currCell->m_item[t]]);
     triCount++;

     // See if it intersects.
     double oT, oU, oV;
     if (IntersectTriangle (pos.v, oRay.v, currTri->vert[0].v,
      currTri->vert[1].v, currTri->vert[2].v,
      &oT, &oU, &oV))
     {
      if (oT > 0.0f)
      {
       intersect = true;
       break;
      }
     }
    } // end for t (num triangles in this cell)
   } // end if leaf
   else
   {  // Non-leaf, add the children to the list if their bounding
    // box intersects the ray.
    for (int c = 0; c < 8; c++)
    {
     if (currCell->m_children[c] != NULL)
     {
      // Save off current child.
      AtiOctreeCell* child = currCell->m_children[c];

      // If the ray intersects the box
      if (RayIntersectsBox (&pos, &oRay, &child->m_boundingBox))
      {
       AddCell (child, &numCells, gMaxCells, gCell);
      } // end if the ray intersects this bounding box.
      // else do nothing, we'll never intersect any triangles
      // for it's children.
     } // end if we have a cell
    } // end for c (8 children)
   } // end else non-leaf node.
  } // end while cells

  // Update our running results based on if we found and intersection.
  num += rayWeights[r];
  if (!intersect)
  {
   bentNormal->x += (oRay.x * rayWeights[r]);
   bentNormal->y += (oRay.y * rayWeights[r]);
   bentNormal->z += (oRay.z * rayWeights[r]);
  }
  else
  {
   hit += rayWeights[r];
  }
  /*      
  // Save off some stats.
  if (triCount > gAOMaxTrisTested)
  {
  gAOMaxTrisTested = triCount;
  }
  if (cellCount > gAOMaxCellsTested)
  {
  gAOMaxCellsTested = cellCount;
  }
  */
 } // end for r (number of rays)

 // Normalize result
 Normalize (bentNormal->v);
 (*occlusion) = (num - hit) / num;
} // end of ComputeOcclusion


/*
//================================================================
//================================================================
bool __cdecl StartSession(IAgent* agent, DWORD sessionId)
{
IGenericStream* globalDataStream;

HRESULT rz = agent->GetData(sessionId, "NormalMapperData", &globalDataStream);

if (rz!=S_OK) return false;

octree = new AtiOctree(globalDataStream);

globalDataStream->Read(&highNumTris,sizeof(highNumTris));
highTris = new NmRawTriangle[highNumTris];
globalDataStream->Read(highTris,highNumTris*sizeof(NmRawTriangle));

globalDataStream->Read(&gNumSamples,sizeof(gNumSamples));
gSamples = new NmSample[gNumSamples];
globalDataStream->Read(gSamples,gNumSamples*sizeof(NmSample));

globalDataStream->Read(&hTangentSpace,sizeof(hTangentSpace));
if (hTangentSpace!=NULL)
{
hTangentSpace = new NmTangentMatrix[highNumTris];
globalDataStream->Read(hTangentSpace,highNumTris*sizeof(NmTangentMatrix));
}

globalDataStream->Read(&bumpWidth,sizeof(bumpWidth));
globalDataStream->Read(&bumpHeight,sizeof(bumpHeight));
globalDataStream->Read(&bumpMap,sizeof(bumpMap));
if (bumpMap!=NULL)
{
bumpMap = new float[bumpWidth*bumpHeight*3];
globalDataStream->Read(bumpMap,bumpWidth*bumpHeight*3*sizeof(float));
}

globalDataStream->Read(&numRays,sizeof(numRays));
if (numRays!=0)
{
rays = new NmRawPointD[numRays];
globalDataStream->Read(rays,numRays*sizeof(NmRawPointD));

rayWeights = new double[numRays];
globalDataStream->Read(rayWeights,numRays*sizeof(double));
}


{
globalDataStream->AddRef();
DWORD d = globalDataStream->Release();
}

globalDataStream->Release();

return true;
}
*/

//================================================================
//================================================================
void __cdecl EndSession(IAgent* agent, DWORD sessionId)
{
 TSessionDataCache::iterator i = sessionDataCache.find(sessionId);

 if (i!=sessionDataCache.end())
 { 
  sessionDataCache.erase(i);
 } 
}

//================================================================
//================================================================
bool __cdecl RunTask(IAgent* agent, DWORD sessionId, IGenericStream* inStream, IGenericStream* outStream)
{
 EnterCriticalSection(&CS);

 DWORD dataKey = sessionId;
 
 //uncomment to allow each thread from pool to have separate global data
 //dataKey |= GetCurrentThreadId();

 TSessionDataCache::iterator i = sessionDataCache.find(dataKey);

 if (i==sessionDataCache.end())
 { 
  sessionDataCache.insert(std::make_pair(dataKey,TSessionData()));


  //----------- read global data -------------
  IGenericStream* globalDataStream;

  HRESULT rz = agent->GetData(sessionId, dataDesc, &globalDataStream);

  if (rz!=S_OK) 
  {
   LeaveCriticalSection(&CS);
   return false;
  } 

  i = sessionDataCache.find(dataKey);
   assert(i!=sessionDataCache.end());

   TSessionData* sd = &i->second;

   sd->octree = new AtiOctree(globalDataStream);

   globalDataStream->Read(&sd->highNumTris,sizeof(sd->highNumTris));
   sd->highTris = new NmRawTriangle[sd->highNumTris];
   globalDataStream->Read(sd->highTris,sd->highNumTris*sizeof(NmRawTriangle));

   globalDataStream->Read(&sd->gNumSamples,sizeof(sd->gNumSamples));
   sd->gSamples = new NmSample[sd->gNumSamples];
   globalDataStream->Read(sd->gSamples,sd->gNumSamples*sizeof(NmSample));

   globalDataStream->Read(&sd->hTangentSpace,sizeof(sd->hTangentSpace));
   if (sd->hTangentSpace!=NULL)
    {
     sd->hTangentSpace = new NmTangentMatrix[sd->highNumTris];
     globalDataStream->Read(sd->hTangentSpace,sd->highNumTris*sizeof(NmTangentMatrix));
    }

   globalDataStream->Read(&sd->bumpWidth,sizeof(sd->bumpWidth));
   globalDataStream->Read(&sd->bumpHeight,sizeof(sd->bumpHeight));
   globalDataStream->Read(&sd->bumpMap,sizeof(sd->bumpMap));
   if (sd->bumpMap!=NULL)
    {
     sd->bumpMap = new float[sd->bumpWidth*sd->bumpHeight*3];
     globalDataStream->Read(sd->bumpMap,sd->bumpWidth*sd->bumpHeight*3*sizeof(float));
    }

   globalDataStream->Read(&sd->numRays,sizeof(sd->numRays));
   if (sd->numRays!=0)
    {
     sd->rays = new NmRawPointD[sd->numRays];
     globalDataStream->Read(sd->rays,sd->numRays*sizeof(NmRawPointD));
     
     sd->rayWeights = new double[sd->numRays];
     globalDataStream->Read(sd->rayWeights,sd->numRays*sizeof(double));
    }


   {
    globalDataStream->AddRef();
    DWORD d = globalDataStream->Release();
   }
   
   globalDataStream->Release();
   
   agent->FreeCachedData(sessionId, dataDesc);
  }  
 
 TSessionData* sd = &i->second;

 LeaveCriticalSection(&CS);

//------------------------------------------

 float* img;
 float* img2;
 int minX;
 int pad;
 int maxX;
 int gWidth;
 int y;
 double x1,x2,x3,y1,y2,y3;
 double b0;
 bool gInTangentSpace;
 NmTangentMatrix tangentSpace_l;
 NmRawTriangle lTri;
 bool gOcclusion;
 bool gBentNormal;
 int numComponents;
 int gOcclIdx;
 bool gDisplacements;
 int gDispIdx;
 bool gEdgeCopy;
 int gNormalRules;
 double gMaxAngle;
 double gDistance;
 double gEpsilon;
 
 int gMaxCells = 0;
 AtiOctreeCell** gCell = NULL;

 DWORD intag;
 
 bool bFirstPass=true;
 int l;

 inStream->Read(&intag, sizeof(DWORD));

 do
 {

   inStream->Read(&img, sizeof(float*));
   inStream->Read(&img2, sizeof(float*));

   if (bFirstPass==true)
    {
     outStream->Write(&img, sizeof(float*));
     outStream->Write(&img2, sizeof(float*));
    }

   inStream->Read(&minX, sizeof(minX));
   inStream->Read(&pad, sizeof(pad));
   inStream->Read(&maxX, sizeof(maxX));
   inStream->Read(&gWidth, sizeof(gWidth));
   inStream->Read(&y, sizeof(y));
   
   inStream->Read(&x1, sizeof(x1));
   inStream->Read(&x2, sizeof(x2));
   inStream->Read(&x3, sizeof(x3));

   inStream->Read(&y1, sizeof(y1));
   inStream->Read(&y2, sizeof(y2));
   inStream->Read(&y3, sizeof(y3));
   
   inStream->Read(&b0, sizeof(b0));

   inStream->Read(&gInTangentSpace, sizeof(gInTangentSpace));

   inStream->Read(&tangentSpace_l,sizeof(NmTangentMatrix));

   inStream->Read(&lTri, sizeof(NmRawTriangle));
/*   
   if (bFirstPass)                  
    {
     octree = new AtiOctree(inStream);

     inStream->Read(&highNumTris,sizeof(highNumTris));
     highTris = new NmRawTriangle[highNumTris];
     inStream->Read(highTris,highNumTris*sizeof(NmRawTriangle));

     inStream->Read(&gNumSamples,sizeof(gNumSamples));
     gSamples = new NmSample[gNumSamples];
     inStream->Read(gSamples,gNumSamples*sizeof(NmSample));

     inStream->Read(&hTangentSpace,sizeof(hTangentSpace));
     if (hTangentSpace!=NULL)
      {
       hTangentSpace = new NmTangentMatrix[highNumTris];
       inStream->Read(hTangentSpace,highNumTris*sizeof(NmTangentMatrix));
      }

     inStream->Read(&bumpWidth,sizeof(bumpWidth));
     inStream->Read(&bumpHeight,sizeof(bumpHeight));
     inStream->Read(&bumpMap,sizeof(bumpMap));
     if (bumpMap!=NULL)
      {
       bumpMap = new float[bumpWidth*bumpHeight*3];
       inStream->Read(bumpMap,bumpWidth*bumpHeight*3*sizeof(float));
      }

     inStream->Read(&numRays,sizeof(numRays));
     if (numRays!=0)
      {
       rays = new NmRawPointD[numRays];
       inStream->Read(rays,numRays*sizeof(NmRawPointD));
       
       rayWeights = new double[numRays];
       inStream->Read(rayWeights,numRays*sizeof(double));
      }

   }
*/
   inStream->Read(&gOcclusion,sizeof(gOcclusion));
   inStream->Read(&gBentNormal,sizeof(gBentNormal));

   inStream->Read(&numComponents,sizeof(numComponents));

   inStream->Read(&gOcclIdx,sizeof(gOcclIdx));

   inStream->Read(&gDisplacements ,sizeof(gDisplacements));

   inStream->Read(&gDispIdx ,sizeof(gDispIdx));

   inStream->Read(&gEdgeCopy, sizeof(gEdgeCopy));

   inStream->Read(&gNormalRules, sizeof(gNormalRules));

   inStream->Read(&gMaxAngle, sizeof(gMaxAngle));

   inStream->Read(&gDistance, sizeof(gDistance));

   inStream->Read(&gEpsilon, sizeof(gEpsilon));

   inStream->Read(&l, sizeof(l));

   if (bFirstPass==true)
    {
     outStream->Write(&l, sizeof(l));
    }

   bFirstPass=false;



   //---------------------------------------


                 // Loop over the Xs filling in each texel of the normal map
                 
                 for (int x = (minX - pad); x <= (maxX + pad); x++)
                 {

                  if (agent->TestConnection(sessionId)!=S_OK)
                  {
                   delete[] gCell;
                   return false;
                  }

                    // Make sure this is a valid texture coordinate
                    if ((x < 0) || (x >= gWidth))
                    {
                       continue;
                    }

                    // Find Barycentric coordinates.
                    double b1 = ((x2-x) * (y3-y) - (x3-x) * (y2-y)) / b0;
                    double b2 = ((x3-x) * (y1-y) - (x1-x) * (y3-y)) / b0;
                    double b3 = ((x1-x) * (y2-y) - (x2-x) * (y1-y)) / b0;

                    // Interpolate tangent space.
                    double ts[9];
                    if (gInTangentSpace == true)
                    {
                       for (int t = 0; t < 9; t++)
                       {
  //                        ts[t] = (tangentSpace[l].m[0][t] * b1) + 
  //                                (tangentSpace[l].m[1][t] * b2) +
  //                                (tangentSpace[l].m[2][t] * b3);
                          ts[t] = (tangentSpace_l.m[0][t] * b1) + 
                                  (tangentSpace_l.m[1][t] * b2) +
                                  (tangentSpace_l.m[2][t] * b3);
                       }
                    }
                    
                    // For each sample accumulate the normal
                    double sNorm[3] = {0.0, 0.0, 0.0};
                    double sOcclusion = 0.0;
                    double sDisplacement = 0.0;
                    int sFound = 0;
                    double aNorm[3] = {0.0, 0.0, 0.0};
                    for (int s = 0; s < sd->gNumSamples; s++)
                    {
                       // Compute new x & y
                       double sX = (double)(x) + sd->gSamples[s].x;
                       double sY = (double)(y) + sd->gSamples[s].y;
                       
                       // Find Barycentric coordinates.
                       double b1 = ((x2-sX) * (y3-sY) - (x3-sX) * (y2-sY)) / b0;
                       double b2 = ((x3-sX) * (y1-sY) - (x1-sX) * (y3-sY)) / b0;
                       double b3 = ((x1-sX) * (y2-sY) - (x2-sX) * (y1-sY)) / b0;

                       // Use Barycentric coordinates to deterimine if we are
                       // are outside of the triangle.
                       if ((b1 > 1.0) || (b1 < 0.0) ||
                           (b2 > 1.0) || (b2 < 0.0) ||
                           (b3 > 1.0) || (b3 < 0.0))
                       {
                          continue;
                       }

                       // Compute position and normal
                       NmRawPointD pos;
                       NmRawPointD norm;
                       BaryInterpolate (&lTri, b1, b2, b3, pos.v, norm.v);

                       // First see if we even have an intersection.
                       double newNorm[3];
                       double newPos[3];
                       double newDisplacement = 0.0f;
                       if (FindBestIntersection (pos, norm, sd->octree, sd->highTris,
                                                 sd->hTangentSpace, sd->bumpMap, 
                                                 sd->bumpHeight, sd->bumpWidth, newNorm, 
                                                 newPos, &newDisplacement,
                                                 gNormalRules,
                                                 gMaxAngle, gDistance, gEpsilon,
                                                 gMaxCells, gCell))
                       {
                          // Normalize the new normal
                          sDisplacement += newDisplacement;
                          Normalize (newNorm);
                          
                          // Do bent normal/occlusion if needed.
                          if (gOcclusion || gBentNormal)
                          {
                             // First do the ray casting.
                             NmRawPointD bentNormal;
                             double occlusion;
                             ComputeOcclusion ((NmRawPointD*)(newPos),
                                               (NmRawPointD*)(newNorm),
                                               sd->highNumTris, sd->highTris, sd->octree,
                                               sd->numRays, sd->rays, sd->rayWeights,
                                               &bentNormal, &occlusion,
                                               gMaxCells, gCell);
                             
                             // Add it to our sample sum.
                             if (gBentNormal)
                             {
                                sNorm[0] += bentNormal.x;
                                sNorm[1] += bentNormal.y;
                                sNorm[2] += bentNormal.z;
                             }
                             else
                             {
                                sNorm[0] += newNorm[0];
                                sNorm[1] += newNorm[1];
                                sNorm[2] += newNorm[2];
                             }
                             sOcclusion += occlusion;
                             sFound++;
                          }
                          else
                          {  // Plain old normal map
                             sNorm[0] += newNorm[0];
                             sNorm[1] += newNorm[1];
                             sNorm[2] += newNorm[2];
                             sFound++;
                          }
                       } // end if we found a normal
                    } // end for samples (s);

                    // If we found a normal put it in the image.
                    if (sFound > 0)
                    {
                       // Convert to tangent space if needed
                       if (gInTangentSpace == true)
                       {
                          ConvertToTangentSpace (ts, sNorm, sNorm);
                       }

                       // Add it to the image.
                       int idx = y*gWidth*numComponents + x*numComponents;
                       
                       //======== write result ==============
                       BYTE ostag;
                       DWORD osindex;
                       float osval;
                       
                       ostag = 3;
                       osindex = idx;
                       osval = (float)sNorm[0];
                       outStream->Write(&ostag,sizeof(ostag));
                       outStream->Write(&osindex,sizeof(osindex));
                       outStream->Write(&osval,sizeof(osval));

                       ostag = 3;
                       osindex = idx+1;
                       osval = (float)sNorm[1];
                       outStream->Write(&ostag,sizeof(ostag));
                       outStream->Write(&osindex,sizeof(osindex));
                       outStream->Write(&osval,sizeof(osval));

                       ostag = 3;
                       osindex = idx+2;
                       osval = (float)sNorm[2];
                       outStream->Write(&ostag,sizeof(ostag));
                       outStream->Write(&osindex,sizeof(osindex));
                       outStream->Write(&osval,sizeof(osval));
                       
                       
  //                     img[idx + 0] += (float)sNorm[0];
  //                     img[idx + 1] += (float)sNorm[1];
  //                     img[idx + 2] += (float)sNorm[2];

                       if (gOcclusion)
                       {
                        ostag = 1;
                        osindex = idx + gOcclIdx;
                        osval = (float)(sOcclusion/sFound);
                        outStream->Write(&ostag,sizeof(ostag));
                        outStream->Write(&osindex,sizeof(osindex));
                        outStream->Write(&osval,sizeof(osval));
                       
  //                        img[idx + gOcclIdx] = (float)(sOcclusion/sFound);
                       }
                       if (gDisplacements)
                       {
                        ostag = 1;
                        osindex = idx + gDispIdx;
                        osval = (float)(sDisplacement/sFound);
                        outStream->Write(&ostag,sizeof(ostag));
                        outStream->Write(&osindex,sizeof(osindex));
                        outStream->Write(&osval,sizeof(osval));
  //                        img[idx + gDispIdx] = (float)(sDisplacement/sFound);
                       }

                       //====================================
                    } // end if we found a normal
                    else if (gEdgeCopy)
                    {
                       // Since we didn't find a normal, "snap" the center
                       // sample's barycentric coordinates to the edge of the
                       // triangle and find the normal there. Store this in
                       // a temporary buffer that we'll use to dilate the image
                       // in a way that doesn't produce texture seams.
                       double sX = (double)x;
                       double sY = (double)y;
                       double b1 = ((x2-sX) * (y3-sY) - (x3-sX) * (y2-sY)) / b0;
                       double b2 = ((x3-sX) * (y1-sY) - (x1-sX) * (y3-sY)) / b0;
                       double b3 = ((x1-sX) * (y2-sY) - (x2-sX) * (y1-sY)) / b0;
                       
                       // "Snap" barycentric coordinates.
                       if (b1 > 1.0)
                       {
                          double diff = b1 - 1.0;
                          b1 = 1.0;
                          b2 += (diff/2.0);
                          b3 += (diff/2.0);
                       }
                       if (b1 < 0.0)
                       {
                          double diff = b1;
                          b1 = 0.0;
                          b2 += (diff/2.0);
                          b3 += (diff/2.0);
                       }
                       if (b2 > 1.0)
                       {
                          double diff = b2 - 1.0;
                          b1 = 1.0;
                          b1 += (diff/2.0);
                          b3 += (diff/2.0);
                       }
                       if (b2 < 0.0)
                       {
                          double diff = b2;
                          b2 = 0.0;
                          b1 += (diff/2.0);
                          b3 += (diff/2.0);
                       }
                       if (b3 > 1.0)
                       {
                          double diff = b3 - 1.0;
                          b3 = 1.0;
                          b2 += (diff/2.0);
                          b1 += (diff/2.0);
                       }
                       if (b3 < 0.0)
                       {
                          double diff = b3;
                          b3 = 0.0;
                          b2 += (diff/2.0);
                          b1 += (diff/2.0);
                       }

                       // Compute position and normal
                       NmRawPointD pos;
                       NmRawPointD norm;
                       BaryInterpolate (&lTri, b1, b2, b3, pos.v, norm.v);

                       // First see if we even have an intersection.
                       double newNorm[3];
                       double newPos[3];
                       double newOcclusion = 0.0;
                       double newDisplacement = 0.0;
                       if (FindBestIntersection (pos, norm, sd->octree, sd->highTris,
                                                 sd->hTangentSpace, sd->bumpMap, 
                                                 sd->bumpHeight, sd->bumpWidth, newNorm, 
                                                 newPos, &newDisplacement,
                                                 gNormalRules,
                                                 gMaxAngle, gDistance, gEpsilon,
                                                 gMaxCells, gCell))
                       {
                          // Normalize the new normal
                          Normalize (newNorm);
                          
                          // Do bent normal/occlusion if needed.
                          if (gOcclusion || gBentNormal)
                          {
                             // First do the ray casting.
                             NmRawPointD bentNormal;
                             ComputeOcclusion ((NmRawPointD*)(newPos),
                                               (NmRawPointD*)(newNorm),
                                               sd->highNumTris, sd->highTris, sd->octree,
                                               sd->numRays, sd->rays, sd->rayWeights,
                                               &bentNormal, &newOcclusion,
                                               gMaxCells, gCell);
                             
                             // Replace normal with bent normal if needed
                             if (gBentNormal)
                             {
                                newNorm[0] += bentNormal.x;
                                newNorm[1] += bentNormal.y;
                                newNorm[2] += bentNormal.z;
                             }
                          } // do ambient occlusion if needed

                          
                          // Convert to tangent space if needed
                          if (gInTangentSpace == true)
                          {
                             ConvertToTangentSpace (ts, newNorm, newNorm);
                          }

                          // Add it to the image.
                          int idx = y*gWidth*numComponents + x*numComponents;

                          BYTE ostag;
                          DWORD osindex;
                          float osval;
                          
                          ostag = 4;
                          osindex = idx;
                          osval = (float)newNorm[0];
                          outStream->Write(&ostag,sizeof(ostag));
                          outStream->Write(&osindex,sizeof(osindex));
                          outStream->Write(&osval,sizeof(osval));

                          ostag = 4;
                          osindex = idx+1;
                          osval = (float)newNorm[1];
                          outStream->Write(&ostag,sizeof(ostag));
                          outStream->Write(&osindex,sizeof(osindex));
                          outStream->Write(&osval,sizeof(osval));

                          ostag = 4;
                          osindex = idx+2;
                          osval = (float)newNorm[2];
                          outStream->Write(&ostag,sizeof(ostag));
                          outStream->Write(&osindex,sizeof(osindex));
                          outStream->Write(&osval,sizeof(osval));

  //                        img2[idx + 0] += (float)newNorm[0];
  //                        img2[idx + 1] += (float)newNorm[1];
  //                        img2[idx + 2] += (float)newNorm[2];

                          if (gOcclusion)
                          {
                           ostag = 2;
                           osindex = idx + gOcclIdx;
                           osval = (float)(newOcclusion);
                           outStream->Write(&ostag,sizeof(ostag));
                           outStream->Write(&osindex,sizeof(osindex));
                           outStream->Write(&osval,sizeof(osval));
                          
  //                           img2[idx + gOcclIdx] = (float)(newOcclusion);
                          }
                          if (gDisplacements)
                          {
                           ostag = 2;
                           osindex = idx + gDispIdx;
                           osval = (float)(newDisplacement);
                           outStream->Write(&ostag,sizeof(ostag));
                           outStream->Write(&osindex,sizeof(osindex));
                           outStream->Write(&osval,sizeof(osval));

  //                           img2[idx + gDispIdx] = (float)(newDisplacement);
                          }
                       } // end if we found a normal
                       // else don't add anything to either image.
                    } // end else find a "snapped" normal
                    // Spin!
  //                  ShowSpinner (prog);
                 } // end for x
                    
   inStream->Read(&intag, sizeof(DWORD));

  } while (intag==1);

 delete[] gCell;
 gMaxCells = 0;
 gCell=NULL;
 

 BYTE ostag;
 ostag = 0;
 outStream->Write(&ostag,sizeof(ostag));

 DWORD tricount;
 inStream->Read(&tricount, sizeof(tricount));
 outStream->Write(&tricount, sizeof(tricount));

 return true;
}

//===========================================================
// bool DllMain()
//===========================================================
//should be outside namespace !!!
BOOL APIENTRY DllMain(HINSTANCE hInst       ,
                      DWORD     dwReason    ,
                      LPVOID    /*lpReserved*/ )
{																																																		 
 switch ( dwReason )															 
 {
  case DLL_PROCESS_ATTACH :                 
   InitializeCriticalSection(&CS);
  break;

  case DLL_PROCESS_DETACH :
   DeleteCriticalSection(&CS);
  break;
 }

 return TRUE;    
}
