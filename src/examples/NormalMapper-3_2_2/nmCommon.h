#pragma once

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


// Rules for figuring out which normal is better.
enum
{
   NORM_RULE_CLOSEST = 0,
   NORM_RULE_BEST_CLOSEST,
   NORM_RULE_BEST_FARTHEST,
   NORM_RULE_FARTHEST,
   NORM_RULE_MIXED,
   NORM_RULE_BEST_MIXED,
   NORM_RULE_BEST,
   NORM_RULE_FRONT_FURTHEST,
   NORM_RULE_FRONT_BEST_FURTHEST,
   NORM_RULE_FRONT_CLOSEST,
   NORM_RULE_FRONT_BEST_CLOSEST
};

// Tangent space structure.
typedef struct
{
   double m[3][9];
} NmTangentMatrix;

typedef union
{
   struct { double x, y; };
   struct { double v[2]; };
} NmSample;

extern inline void
BaryInterpolate (NmRawTriangle* tri, double b1, double b2, double b3,
                 double pos[3], double nrm[3]);
                 
extern void
Normalize(double v[3]);

extern void
ConvertToTangentSpace (double* m, double *vec, double *result);

extern inline bool
IntersectTriangle (double *orig, double *dir,
                   float *v1, float *v2, float *v3,
                   double *t, double *u, double *v);

extern inline void
GetPerturbedNormal (NmRawTriangle* tri, double b0, double b1, double b2,
                    float* bumpMap, int bumpWidth, int bumpHeight,
                    double m[3][9], double norm[3]);

extern inline bool
RayIntersectsBox (NmRawPointD* position, NmRawPointD* direction,
                  AtiOctBoundingBox* box);

extern void 
FromToRotation (double mtx[16], double from[3], double to[3]);
