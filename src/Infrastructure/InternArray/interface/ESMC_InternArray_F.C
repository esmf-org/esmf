// $Id: ESMC_InternArray_F.C,v 1.7.2.3 2009/01/21 21:25:21 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
//
//==============================================================================
//
// This file contains the Fortran interface code to link F90 and C++.
//
//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------
#include "ESMC_Start.h"
#include "ESMC_Base.h"
#include "ESMC_LogErr.h"
#include "ESMC_InternArray.h"
#include "ESMCI_DELayout.h"
#include <stdio.h>
#include <string.h>

#define MIN(a,b) (((a)<(b))?(a):(b))

#ifdef ESMC_DATA_ADDR_NEEDS_INDIR
#define XD *
#else
#define XD
#endif

//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
// The code in this file implements the inter-language code which
//  allows F90 to call C++ for supporting {\tt InternArray} class functions.
//
//EOP

// the interface subroutine names MUST be in lower case
extern "C" {

     void FTN(c_esmc_iarraycreateall)(ESMC_InternArray **ptr, int *rank, 
                                     ESMC_TypeKind *dk,
                                     int *counts, int *lbounds, int *ubounds,
                                     int *status)  {
         (*ptr) = ESMC_InternArrayCreate_F(*rank, *dk, counts, 
                                    NULL, NULL, ESMC_DATA_NONE, 
                                    lbounds, ubounds, status);

             (*status) = (*ptr != NULL) ? ESMF_SUCCESS : ESMF_FAILURE;
     }

     void FTN(c_esmc_iarraycreatenodata)(ESMC_InternArray **ptr, int *rank, 
                                        ESMC_TypeKind *dk, 
                                        ESMC_ArrayOrigin *oflag, int *status) {
             
             (*ptr) = ESMC_InternArrayCreateNoData(*rank, *dk, *oflag, status);

             (*status) = (*ptr != NULL) ? ESMF_SUCCESS : ESMF_FAILURE;
     }
 
     void FTN(c_esmc_iarraysetinfo)(ESMC_InternArray **ptr, 
                               struct c_F90ptr *fptr, void XD *base, int *counts,
                               int *lbounds, int *ubounds, int *offsets,
                               ESMC_Logical *contig, ESMC_Logical *dealloc,
			       int *hwidth, int *status) {
      
          if ((ptr == NULL) || (*ptr == NULL)) {
              *status = ESMF_FAILURE;
              return;
          }

         *status = (*ptr)->ESMC_ArraySetInfo(fptr, XD base, counts, 
                                            lbounds, ubounds, offsets, 
                                            *contig, *dealloc, *hwidth);
     }

     void FTN(c_esmc_iarraysetlengths)(ESMC_InternArray **ptr, int *rank, int *lengths, int *status) {
      
          if ((ptr == NULL) || (*ptr == NULL)) {
              *status = ESMF_FAILURE;
              return;
          }

         *status = (*ptr)->ESMC_ArraySetLengths(*rank, lengths);
     }

     void FTN(c_esmc_iarraygetlengths)(ESMC_InternArray **ptr, int *rank, int *lengths, int *status) {
      
          if ((ptr == NULL) || (*ptr == NULL)) {
              *status = ESMF_FAILURE;
              return;
          }

         *status = (*ptr)->ESMC_ArrayGetLengths(*rank, lengths);
     }

     void FTN(c_esmc_iarraygetlbounds)(ESMC_InternArray **ptr, int *rank, int *lbounds, int *status) {
      
          if ((ptr == NULL) || (*ptr == NULL)) {
              *status = ESMF_FAILURE;
              return;
          }

         *status = (*ptr)->ESMC_ArrayGetLbounds(*rank, lbounds);
     }

     void FTN(c_esmc_iarraygetubounds)(ESMC_InternArray **ptr, int *rank, int *ubounds, int *status) {
      
          if ((ptr == NULL) || (*ptr == NULL)) {
              *status = ESMF_FAILURE;
              return;
          }

         *status = (*ptr)->ESMC_ArrayGetUbounds(*rank, ubounds);
     }

     void FTN(c_esmc_iarraygethwidth)(ESMC_InternArray **ptr, int *hwidth, int *status) {
      
          if ((ptr == NULL) || (*ptr == NULL)) {
              *status = ESMF_FAILURE;
              return;
          }

         *status = (*ptr)->ESMC_ArrayGetHWidth(hwidth);
     }

     void FTN(c_esmc_iarraygethwidthlist)(ESMC_InternArray **ptr, int *hwidth, int *status) {
      
          if ((ptr == NULL) || (*ptr == NULL)) {
              *status = ESMF_FAILURE;
              return;
          }

         *status = (*ptr)->ESMC_ArrayGetHWidthList(hwidth);
     }

     void FTN(c_esmc_iarraygetrank)(ESMC_InternArray **ptr, int *rank, int *status) {
      
          if ((ptr == NULL) || (*ptr == NULL)) {
              *status = ESMF_FAILURE;
              return;
          }

         *rank = (*ptr)->ESMC_ArrayGetRank();
         *status = ESMF_SUCCESS;
     }

     void FTN(c_esmc_iarraygettypekind)(ESMC_InternArray **ptr, int *kind, int *status) {
      
          if ((ptr == NULL) || (*ptr == NULL)) {
              *status = ESMF_FAILURE;
              return;
          }

         *kind = (*ptr)->ESMC_ArrayGetTypeKind();
         *status = ESMF_SUCCESS;
     }

     void FTN(c_esmc_iarraygetname)(ESMC_InternArray **ptr, char *name, int *status,
                                                                   int nlen) {
          if ((ptr == NULL) || (*ptr == NULL)) {
              *status = ESMF_FAILURE;
              return;
          }

         if ((nlen <= 0) || (name == NULL)) {
             if (status) *status = ESMF_FAILURE;
             return;
         }
           
         *status = ESMC_CtoF90string((*ptr)->ESMC_BaseGetName(), name, nlen);
     }


     void FTN(c_esmc_iarraydestroy)(ESMC_InternArray **ptr, int *status) {
          if ((ptr == NULL) || (*ptr == NULL)) {
              *status = ESMF_FAILURE;
              return;
          }

         *status = ESMC_InternArrayDestroy(*ptr);
     }

//     void FTN(c_esmc_iarraycomputeaxisindex)(ESMC_InternArray **ptr, 
//                                        ESMC_DELayout **delayout, 
//                                        int *decompids, int *dlen, 
//                                        int *status) {
//          if ((ptr == NULL) || (*ptr == NULL)) {
//              *status = ESMF_FAILURE;
//              return;
//          }
//
//          *status = (*ptr)->ESMC_ArrayComputeAxisIndex(*delayout, 
//                                                       decompids, *dlen);
//     }

     void FTN(c_esmc_iarraysetaxisindex)(ESMC_InternArray **ptr, ESMC_DomainType *dt, 
                                        ESMC_AxisIndex *ai, int *status) {
          if ((ptr == NULL) || (*ptr == NULL)) {
              *status = ESMF_FAILURE;
              return;
          }

          *status = (*ptr)->ESMC_ArraySetAxisIndex(*dt, ai);
     }

     void FTN(c_esmc_iarraygetaxisindex)(ESMC_InternArray **ptr, ESMC_DomainType *dt, 
                                        ESMC_AxisIndex *ai, int *status) {
          if ((ptr == NULL) || (*ptr == NULL)) {
              *status = ESMF_FAILURE;
              return;
          }

          *status = (*ptr)->ESMC_ArrayGetAxisIndex(*dt, ai);
     }

// obsolete when arbitrary distribution stops needing this.  nsc 03nov05
     void FTN(c_esmc_iarraygetallaxisindices)(ESMC_InternArray **ptr, 
                                     ESMC_AxisIndex *global, int *nDEs,
                                     int *rank, ESMC_AxisIndex *total,
                                     ESMC_AxisIndex *comp, ESMC_AxisIndex *excl,
                                     int *status) {
          if ((ptr == NULL) || (*ptr == NULL)) {
              *status = ESMF_FAILURE;
              return;
          }

          *status = (*ptr)->ESMC_ArrayGetAllAxisIndices(global, *nDEs, *rank,
                                     total, comp, excl);
     }

     void FTN(c_esmc_iarraygetallaxisindex)(ESMC_InternArray **ptr, ESMC_DomainType *dt, 
                                     ESMC_AxisIndex *global, int *nDEs,
                                     int *rank, ESMC_AxisIndex *ai,
                                     int *status) {
          if ((ptr == NULL) || (*ptr == NULL)) {
              *status = ESMF_FAILURE;
              return;
          }

          switch(*dt) {
            case ESMC_DOMAIN_TOTAL:
               *status = (*ptr)->ESMC_ArrayGetAllAxisIndices(global, *nDEs, 
                                                   *rank, ai, NULL, NULL);
               break;
            case ESMC_DOMAIN_COMPUTATIONAL:
               *status = (*ptr)->ESMC_ArrayGetAllAxisIndices(global, *nDEs, 
                                                   *rank, NULL, ai, NULL);
               break;
            case ESMC_DOMAIN_EXCLUSIVE:
               *status = (*ptr)->ESMC_ArrayGetAllAxisIndices(global, *nDEs, 
                                                   *rank, NULL, NULL, ai);
               break;
         }
     }

     void FTN(c_esmc_iarraysetbaseaddr)(ESMC_InternArray **ptr, void XD *base, int *status) {
          if ((ptr == NULL) || (*ptr == NULL)) {
              *status = ESMF_FAILURE;
              return;
          }

          *status = (*ptr)->ESMC_ArraySetBaseAddr(XD base);
     }

     void FTN(c_esmc_iarraygetbaseaddr)(ESMC_InternArray **ptr, void **base, int *status) {
          if ((ptr == NULL) || (*ptr == NULL)) {
              *status = ESMF_FAILURE;
              return;
          }

          *status = (*ptr)->ESMC_ArrayGetBaseAddr((void **)base);
     }

     void FTN(c_esmc_iarraysetf90ptr)(ESMC_InternArray **ptr, struct c_F90ptr *p, int *status) {
        //fprintf(stderr, "interface code, setting f90 ptr to %lx, this = %lx, &this = %lx\n", 
        //                                (ESMC_I8)p, (ESMC_I8)(*ptr), (ESMC_I8)ptr);
          if ((ptr == NULL) || (*ptr == NULL)) {
              *status = ESMF_FAILURE;
              return;
          }

          *status = (*ptr)->ESMC_ArraySetF90Ptr(p);
     }

     void FTN(c_esmc_iarraygetf90ptr)(ESMC_InternArray **ptr, struct c_F90ptr *p, int *status) {
        //fprintf(stderr, "interface code, getting f90 ptr into %lx, this = %lx, &this = %lx\n", 
        //                                (ESMC_I8)p, (ESMC_I8)(*ptr), (ESMC_I8)ptr);
          if ((ptr == NULL) || (*ptr == NULL)) {
              *status = ESMF_FAILURE;
              return;
          }

          *status = (*ptr)->ESMC_ArrayGetF90Ptr(p);
     }

     void FTN(c_esmc_iarraysetdealloc)(ESMC_InternArray **ptr, int *status) {
          if ((ptr == NULL) || (*ptr == NULL)) {
              *status = ESMF_FAILURE;
              return;
          }

          *status = (*ptr)->ESMC_ArraySetDealloc();
     }

     void FTN(c_esmc_iarraysetnodealloc)(ESMC_InternArray **ptr, int *status) {
          if ((ptr == NULL) || (*ptr == NULL)) {
              *status = ESMF_FAILURE;
              return;
          }

          *status = (*ptr)->ESMC_ArraySetNoDealloc();
     }

     void FTN(c_esmc_iarrayneedsdealloc)(ESMC_InternArray **ptr, int flag, int *status) {
      
          if ((ptr == NULL) || (*ptr == NULL)) {
              *status = ESMF_FAILURE;
              return;
          }

          flag = (*ptr)->ESMC_ArrayNeedsDealloc();
          *status = ESMF_SUCCESS;
     }

     void FTN(c_esmc_iarrayprint)(ESMC_InternArray **ptr, char *opts, int *status, 
                                                                 int clen) {
         char *temp = NULL;

          if ((ptr == NULL) || (*ptr == NULL)) {
              *status = ESMF_FAILURE;
              return;
          }

         // make a local copy because opts may be non-writable or not
         // long enough to add a trailing null safely.
         if (opts && (clen > 0)) {
             temp = new char[clen+1];
             strncpy(temp, opts, clen);
             temp[clen] = '\0';
         }

         *status = (*ptr)->ESMC_ArrayPrint(temp);

         if (temp)
             delete[] temp;
     }

     void FTN(c_esmc_iarraywrite)(ESMC_InternArray **ptr, char *opts, char *fname,
                                    int *status, int optlen, int flen) {
         char *opttemp = NULL;
         char *filetemp = NULL;

          if ((ptr == NULL) || (*ptr == NULL)) {
              *status = ESMF_FAILURE;
              return;
          }

         // make a local copy because opts may be non-writable or not
         // long enough to add a trailing null safely.
         if (opts && (optlen > 0)) {
             opttemp = new char[optlen+1];
             strncpy(opttemp, opts, optlen);
             opttemp[optlen] = '\0';
         }

         if (fname && (flen > 0)) {
             filetemp = new char[flen+1];
             strncpy(filetemp, fname, flen);
             filetemp[flen] = '\0';
         }

         *status = (*ptr)->ESMC_ArrayWrite(opttemp, filetemp);

         if (opttemp)
             delete[] opttemp;
         if (filetemp)
             delete[] filetemp;
     }

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_iarrayserialize"
//BOP
// !IROUTINE:  c_ESMC_IArraySerialize - Serialize InternArray object 
//
// !INTERFACE:
      void FTN(c_esmc_iarrayserialize)(
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_InternArray **iarray,       // in - iarray object
      char *buf,                // in/out - a byte stream buffer
      int *length,              // in/out - number of allocated bytes
      int *offset,              // in/out - current offset in the stream
      int *rc) {                // out - return code
// 
// !DESCRIPTION:
//     Serialize the contents of a iarray object.
//     Warning!!  Not completely implemented yet.
//
//EOP

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!iarray) {
    //printf("uninitialized InternArray object\n");
    ESMC_LogDefault.ESMC_LogWrite("InternArray object uninitialized", ESMC_LOG_INFO);
    if (rc) *rc = ESMF_SUCCESS;
    return;
  }

  *rc = (*iarray)->ESMC_ArraySerialize(buf, length, offset);

  return;

}  // end c_ESMC_IArraySerialize


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_iarraydeserialize"
//BOP
// !IROUTINE:  c_ESMC_IArrayDeserialize - Deserialize InternArray object 
//
// !INTERFACE:
      void FTN(c_esmc_iarraydeserialize)(
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_InternArray **iarray,       // in/out - empty iarray object to fill in
      char *buf,                // in - byte stream buffer
      int *offset,              // in/out - current offset in the stream
      int *rc) {                // out - return code
// 
// !DESCRIPTION:
//     Deserialize the contents of a iarray object.
//
//EOP

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  // create a new iarray object to deserialize into
  *iarray = new ESMC_InternArray;

  (*iarray)->ESMC_ArrayDeserialize(buf, offset);

  if (rc) *rc = ESMF_SUCCESS;

  return;

}  // end c_ESMC_IArrayDeserialize


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_iarrayserializenodata"
//BOP
// !IROUTINE:  c_ESMC_IArraySerializeNoData - Serialize InternArray object 
//
// !INTERFACE:
      void FTN(c_esmc_iarrayserializenodata)(
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_InternArray **iarray,       // in - iarray object
      char *buf,                // in/out - a byte stream buffer
      int *length,              // in/out - number of allocated bytes
      int *offset,              // in/out - current offset in the stream
      int *rc) {                // out - return code
// 
// !DESCRIPTION:
//     Serialize the contents of a iarray object, without preserving
//     the data values.
//
//EOP

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  if (!iarray) {
    //printf("uninitialized InternArray object\n");
    ESMC_LogDefault.ESMC_LogWrite("InternArray object uninitialized", ESMC_LOG_INFO);
    if (rc) *rc = ESMF_SUCCESS;
    return;
  }

  *rc = (*iarray)->ESMC_ArraySerializeNoData(buf, length, offset);

  return;

}  // end c_ESMC_InternArraySerializeNoData


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_iarraydeserializenodata"
//BOP
// !IROUTINE:  c_ESMC_IArrayDeserializeNoData - Deserialize InternArray object 
//
// !INTERFACE:
      void FTN(c_esmc_iarraydeserializenodata)(
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list.
// 
// !ARGUMENTS:
      ESMC_InternArray **iarray,       // in/out - empty iarray object to fill in
      char *buf,                // in - byte stream buffer
      int *offset,              // in/out - current offset in the stream
      int *rc) {                // out - return code
// 
// !DESCRIPTION:
//     Deserialize the contents of a iarray object, without preserving
//     any of the data (counts explicitly set to 0).
//
//EOP

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;
 
  // create a new iarray object to deserialize into
  *iarray = new ESMC_InternArray;

  (*iarray)->ESMC_ArrayDeserializeNoData(buf, offset);

  if (rc) *rc = ESMF_SUCCESS;

  return;

}  // end c_ESMC_InternArrayDeserializeNoData


#undef  ESMC_METHOD
}


