// $Id: ESMC_Array_F.C,v 1.36.2.3 2007/10/18 02:41:55 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research, 
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
#include "ESMC_Array.h"
#include "ESMC_DELayout.h"
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
//  allows F90 to call C++ for supporting {\tt Array} class functions.
//
//EOP

// the interface subroutine names MUST be in lower case
extern "C" {

     void FTN(c_esmc_arraycreateall)(ESMC_Array **ptr, int *rank, 
                                     ESMC_DataType *dt, ESMC_DataKind *dk,
                                     int *counts, int *lbounds, int *ubounds,
                                     int *status)  {
         (*ptr) = ESMC_ArrayCreate_F(*rank, *dt, *dk, counts, 
                                    NULL, NULL, ESMC_DATA_NONE, 
                                    lbounds, ubounds, status);

             (*status) = (*ptr != NULL) ? ESMF_SUCCESS : ESMF_FAILURE;
     }

     void FTN(c_esmc_arraycreatenodata)(ESMC_Array **ptr, int *rank, 
                                        ESMC_DataType *dt, ESMC_DataKind *dk, 
                                        ESMC_ArrayOrigin *oflag, int *status) {
             
             (*ptr) = ESMC_ArrayCreateNoData(*rank, *dt, *dk, *oflag, status);

             (*status) = (*ptr != NULL) ? ESMF_SUCCESS : ESMF_FAILURE;
     }
 
     void FTN(c_esmc_arraysetinfo)(ESMC_Array **ptr, 
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

     void FTN(c_esmc_arraysetlengths)(ESMC_Array **ptr, int *rank, int *lengths, int *status) {
      
          if ((ptr == NULL) || (*ptr == NULL)) {
              *status = ESMF_FAILURE;
              return;
          }

         *status = (*ptr)->ESMC_ArraySetLengths(*rank, lengths);
     }

     void FTN(c_esmc_arraygetlengths)(ESMC_Array **ptr, int *rank, int *lengths, int *status) {
      
          if ((ptr == NULL) || (*ptr == NULL)) {
              *status = ESMF_FAILURE;
              return;
          }

         *status = (*ptr)->ESMC_ArrayGetLengths(*rank, lengths);
     }

     void FTN(c_esmc_arraygetlbounds)(ESMC_Array **ptr, int *rank, int *lbounds, int *status) {
      
          if ((ptr == NULL) || (*ptr == NULL)) {
              *status = ESMF_FAILURE;
              return;
          }

         *status = (*ptr)->ESMC_ArrayGetLbounds(*rank, lbounds);
     }

     void FTN(c_esmc_arraygetubounds)(ESMC_Array **ptr, int *rank, int *ubounds, int *status) {
      
          if ((ptr == NULL) || (*ptr == NULL)) {
              *status = ESMF_FAILURE;
              return;
          }

         *status = (*ptr)->ESMC_ArrayGetUbounds(*rank, ubounds);
     }

     void FTN(c_esmc_arraygethwidth)(ESMC_Array **ptr, int *hwidth, int *status) {
      
          if ((ptr == NULL) || (*ptr == NULL)) {
              *status = ESMF_FAILURE;
              return;
          }

         *status = (*ptr)->ESMC_ArrayGetHWidth(hwidth);
     }

     void FTN(c_esmc_arraygethwidthlist)(ESMC_Array **ptr, int *hwidth, int *status) {
      
          if ((ptr == NULL) || (*ptr == NULL)) {
              *status = ESMF_FAILURE;
              return;
          }

         *status = (*ptr)->ESMC_ArrayGetHWidthList(hwidth);
     }

     void FTN(c_esmc_arraygetrank)(ESMC_Array **ptr, int *rank, int *status) {
      
          if ((ptr == NULL) || (*ptr == NULL)) {
              *status = ESMF_FAILURE;
              return;
          }

         *rank = (*ptr)->ESMC_ArrayGetRank();
         *status = ESMF_SUCCESS;
     }

     void FTN(c_esmc_arraygettype)(ESMC_Array **ptr, int *type, int *status) {
      
          if ((ptr == NULL) || (*ptr == NULL)) {
              *status = ESMF_FAILURE;
              return;
          }

         *type = (*ptr)->ESMC_ArrayGetType();
         *status = ESMF_SUCCESS;
     }

     void FTN(c_esmc_arraygetkind)(ESMC_Array **ptr, int *kind, int *status) {
      
          if ((ptr == NULL) || (*ptr == NULL)) {
              *status = ESMF_FAILURE;
              return;
          }

         *kind = (*ptr)->ESMC_ArrayGetKind();
         *status = ESMF_SUCCESS;
     }

     void FTN(c_esmc_arraygetname)(ESMC_Array **ptr, char *name, int *status,
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


     void FTN(c_esmc_arraydestroy)(ESMC_Array **ptr, int *status) {
          if ((ptr == NULL) || (*ptr == NULL)) {
              *status = ESMF_FAILURE;
              return;
          }

         *status = ESMC_ArrayDestroy(*ptr);
     }

//     void FTN(c_esmc_arraycomputeaxisindex)(ESMC_Array **ptr, 
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

     void FTN(c_esmc_arraysetaxisindex)(ESMC_Array **ptr, ESMC_DomainType *dt, 
                                        ESMC_AxisIndex *ai, int *status) {
          if ((ptr == NULL) || (*ptr == NULL)) {
              *status = ESMF_FAILURE;
              return;
          }

          *status = (*ptr)->ESMC_ArraySetAxisIndex(*dt, ai);
     }

     void FTN(c_esmc_arraygetaxisindex)(ESMC_Array **ptr, ESMC_DomainType *dt, 
                                        ESMC_AxisIndex *ai, int *status) {
          if ((ptr == NULL) || (*ptr == NULL)) {
              *status = ESMF_FAILURE;
              return;
          }

          *status = (*ptr)->ESMC_ArrayGetAxisIndex(*dt, ai);
     }

// obsolete when arbitrary distribution stops needing this.  nsc 03nov05
     void FTN(c_esmc_arraygetallaxisindices)(ESMC_Array **ptr, 
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

     void FTN(c_esmc_arraygetallaxisindex)(ESMC_Array **ptr, ESMC_DomainType *dt, 
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

     void FTN(c_esmc_arraysetbaseaddr)(ESMC_Array **ptr, void XD *base, int *status) {
          if ((ptr == NULL) || (*ptr == NULL)) {
              *status = ESMF_FAILURE;
              return;
          }

          *status = (*ptr)->ESMC_ArraySetBaseAddr(XD base);
     }

     void FTN(c_esmc_arraygetbaseaddr)(ESMC_Array **ptr, void **base, int *status) {
          if ((ptr == NULL) || (*ptr == NULL)) {
              *status = ESMF_FAILURE;
              return;
          }

          *status = (*ptr)->ESMC_ArrayGetBaseAddr((void **)base);
     }

     void FTN(c_esmc_arraysetf90ptr)(ESMC_Array **ptr, struct c_F90ptr *p, int *status) {
        //fprintf(stderr, "interface code, setting f90 ptr to %lx, this = %lx, &this = %lx\n", 
        //                                (long int)p, (long int)(*ptr), (long int)ptr);
          if ((ptr == NULL) || (*ptr == NULL)) {
              *status = ESMF_FAILURE;
              return;
          }

          *status = (*ptr)->ESMC_ArraySetF90Ptr(p);
     }

     void FTN(c_esmc_arraygetf90ptr)(ESMC_Array **ptr, struct c_F90ptr *p, int *status) {
        //fprintf(stderr, "interface code, getting f90 ptr into %lx, this = %lx, &this = %lx\n", 
        //                                (long int)p, (long int)(*ptr), (long int)ptr);
          if ((ptr == NULL) || (*ptr == NULL)) {
              *status = ESMF_FAILURE;
              return;
          }

          *status = (*ptr)->ESMC_ArrayGetF90Ptr(p);
     }

     void FTN(c_esmc_arraysetdealloc)(ESMC_Array **ptr, int *status) {
          if ((ptr == NULL) || (*ptr == NULL)) {
              *status = ESMF_FAILURE;
              return;
          }

          *status = (*ptr)->ESMC_ArraySetDealloc();
     }

     void FTN(c_esmc_arraysetnodealloc)(ESMC_Array **ptr, int *status) {
          if ((ptr == NULL) || (*ptr == NULL)) {
              *status = ESMF_FAILURE;
              return;
          }

          *status = (*ptr)->ESMC_ArraySetNoDealloc();
     }

     void FTN(c_esmc_arrayneedsdealloc)(ESMC_Array **ptr, int flag, int *status) {
      
          if ((ptr == NULL) || (*ptr == NULL)) {
              *status = ESMF_FAILURE;
              return;
          }

          flag = (*ptr)->ESMC_ArrayNeedsDealloc();
          *status = ESMF_SUCCESS;
     }

     void FTN(c_esmc_arrayprint)(ESMC_Array **ptr, char *opts, int *status, 
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

     void FTN(c_esmc_arraywrite)(ESMC_Array **ptr, char *opts, char *fname,
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
#define ESMC_METHOD "c_esmc_arrayserialize"
//BOP
// !IROUTINE:  c_ESMC_ArraySerialize - Serialize Array object 
//
// !INTERFACE:
      void FTN(c_esmc_arrayserialize)(
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Array **array,       // in - array object
      char *buf,                // in/out - a byte stream buffer
      int *length,              // in/out - number of allocated bytes
      int *offset,              // in/out - current offset in the stream
      int *rc) {                // out - return code
// 
// !DESCRIPTION:
//     Serialize the contents of a array object.
//     Warning!!  Not completely implemented yet.
//
//EOP

  if (!array) {
    //printf("uninitialized Array object\n");
    ESMC_LogDefault.ESMC_LogWrite("Array object uninitialized", ESMC_LOG_INFO);
    if (rc) *rc = ESMF_SUCCESS;
    return;
  }

  *rc = (*array)->ESMC_ArraySerialize(buf, length, offset);

  return;

}  // end c_ESMC_ArraySerialize


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraydeserialize"
//BOP
// !IROUTINE:  c_ESMC_ArrayDeserialize - Deserialize Array object 
//
// !INTERFACE:
      void FTN(c_esmc_arraydeserialize)(
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Array **array,       // in/out - empty array object to fill in
      char *buf,                // in - byte stream buffer
      int *offset,              // in/out - current offset in the stream
      int *rc) {                // out - return code
// 
// !DESCRIPTION:
//     Deserialize the contents of a array object.
//
//EOP

  // create a new array object to deserialize into
  *array = new ESMC_Array;

  (*array)->ESMC_ArrayDeserialize(buf, offset);

  if (rc) *rc = ESMF_SUCCESS;

  return;

}  // end c_ESMC_ArrayDeserialize


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arrayserializenodata"
//BOP
// !IROUTINE:  c_ESMC_ArraySerializeNoData - Serialize Array object 
//
// !INTERFACE:
      void FTN(c_esmc_arrayserializenodata)(
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_Array **array,       // in - array object
      char *buf,                // in/out - a byte stream buffer
      int *length,              // in/out - number of allocated bytes
      int *offset,              // in/out - current offset in the stream
      int *rc) {                // out - return code
// 
// !DESCRIPTION:
//     Serialize the contents of a array object, without preserving
//     the data values.
//
//EOP

  if (!array) {
    //printf("uninitialized Array object\n");
    ESMC_LogDefault.ESMC_LogWrite("Array object uninitialized", ESMC_LOG_INFO);
    if (rc) *rc = ESMF_SUCCESS;
    return;
  }

  *rc = (*array)->ESMC_ArraySerializeNoData(buf, length, offset);

  return;

}  // end c_ESMC_ArraySerializeNoData


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_arraydeserializenodata"
//BOP
// !IROUTINE:  c_ESMC_ArrayDeserializeNoData - Deserialize Array object 
//
// !INTERFACE:
      void FTN(c_esmc_arraydeserializenodata)(
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list.
// 
// !ARGUMENTS:
      ESMC_Array **array,       // in/out - empty array object to fill in
      char *buf,                // in - byte stream buffer
      int *offset,              // in/out - current offset in the stream
      int *rc) {                // out - return code
// 
// !DESCRIPTION:
//     Deserialize the contents of a array object, without preserving
//     any of the data (counts explicitly set to 0).
//
//EOP

  // create a new array object to deserialize into
  *array = new ESMC_Array;

  (*array)->ESMC_ArrayDeserializeNoData(buf, offset);

  if (rc) *rc = ESMF_SUCCESS;

  return;

}  // end c_ESMC_ArrayDeserializeNoData


#undef  ESMC_METHOD
}


