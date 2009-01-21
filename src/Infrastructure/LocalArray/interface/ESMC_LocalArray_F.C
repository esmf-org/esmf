// $Id: ESMC_LocalArray_F.C,v 1.23.2.6 2009/01/21 21:25:22 cdeluca Exp $
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
#include "ESMC_LocalArray.h"
#include <stdio.h>
#include <string.h>
//------------------------------------------------------------------------------
//BOPI
// !DESCRIPTION:
//
// The code in this file implements the inter-language code which allows F90 to
// call C++ for supporting {\tt ESMC\_LocalArray} class functions.
//
//EOPI

#ifdef ESMC_DATA_ADDR_NEEDS_INDIR
#define XD *
#else
#define XD
#endif

// the interface subroutine names MUST be in lower case
extern "C" {

char *name = NULL;

     void FTN(c_esmc_localarraycreateall)(ESMC_LocalArray **ptr, int *rank, 
                                     ESMC_TypeKind *dk,
                                     int *counts, int *lbounds, int *ubounds,
                                     int *localrc)  {
         (*ptr) = ESMC_LocalArray::ESMC_LocalArrayCreate_F(*rank, *dk, counts, 
                                    NULL, NULL, ESMC_DATA_NONE, name,
                                    lbounds, ubounds, NULL, localrc);

             (*localrc) = (*ptr != NULL) ? ESMF_SUCCESS : ESMF_FAILURE;
     }

     void FTN(c_esmc_localarraycreatenodata)(ESMC_LocalArray **ptr, int *rank, 
                                        ESMC_TypeKind *dk, 
                                        ESMC_ArrayOrigin *oflag, int *localrc) {
             
             (*ptr) = ESMC_LocalArray::ESMC_LocalArrayCreateNoData(*rank, *dk,
                  *oflag, name, localrc);

             (*localrc) = (*ptr != NULL) ? ESMF_SUCCESS : ESMF_FAILURE;
     }
 
     void FTN(c_esmc_localarraycreatecopy)(ESMC_LocalArray **ptr, 
       ESMC_LocalArray **larrayOut, int *localrc) {
             
             *larrayOut =
               ESMC_LocalArray::ESMC_LocalArrayCreate(*ptr, localrc);

             (*localrc) = (*ptr != NULL) ? ESMF_SUCCESS : ESMF_FAILURE;
     }
 
     void FTN(c_esmc_localarraysetinternal)(ESMC_LocalArray **ptr, 
                               struct c_F90ptr *fptr,
                               void XD *base, 
                               int *counts,
                               int *lbounds, int *ubounds, int *offsets,
                               ESMC_Logical *contig, ESMC_Logical *dealloc,
                               int *localrc) {
      
          if ((ptr == NULL) || (*ptr == NULL)) {
              *localrc = ESMF_FAILURE;
              return;
          }
         *localrc = (*ptr)->ESMC_LocalArraySetInfo(fptr, XD base, counts, 
                                                  lbounds, ubounds, offsets, 
                                                  contig, dealloc);
     }

     void FTN(c_esmc_localarraysetinfo)(ESMC_LocalArray **ptr, 
                                        int *counts, int *lbounds, 
                                        int *ubounds, int *offsets,
                                        int *localrc) {
      
          if ((ptr == NULL) || (*ptr == NULL)) {
              *localrc = ESMF_FAILURE;
              return;
          }
         *localrc = (*ptr)->ESMC_LocalArraySetInfo(NULL, NULL, counts, 
                                                  lbounds, ubounds, offsets, 
                                                  NULL, NULL);
     }

     void FTN(c_esmc_localarraygetinfo)(ESMC_LocalArray **ptr, 
                               void **base, int *counts,
                               int *lbounds, int *ubounds, int *offsets,
                               int *localrc) {
      
          if ((ptr == NULL) || (*ptr == NULL)) {
              *localrc = ESMF_FAILURE;
              return;
          }
         *localrc = (*ptr)->ESMC_LocalArrayGetInfo(NULL, base, counts, 
                                                  lbounds, ubounds, offsets);
     }

     void FTN(c_esmc_localarraysetlengths)(ESMC_LocalArray **ptr, int *rank, int *lengths, int *localrc) {
      
          if ((ptr == NULL) || (*ptr == NULL)) {
              *localrc = ESMF_FAILURE;
              return;
          }
         *localrc = (*ptr)->ESMC_LocalArraySetLengths(*rank, lengths);
     }

     void FTN(c_esmc_localarraygetlengths)(ESMC_LocalArray **ptr, int *rank, int *lengths, int *localrc) {
      
          if ((ptr == NULL) || (*ptr == NULL)) {
              *localrc = ESMF_FAILURE;
              return;
          }
         *localrc = (*ptr)->ESMC_LocalArrayGetLengths(*rank, lengths);
     }

     void FTN(c_esmc_localarraygetlbounds)(ESMC_LocalArray **ptr, int *rank, int *lbounds, int *localrc) {
          if ((ptr == NULL) || (*ptr == NULL)) {
              *localrc = ESMF_FAILURE;
              return;
          }
         *localrc = (*ptr)->ESMC_LocalArrayGetLbounds(*rank, lbounds);
     }

     void FTN(c_esmc_localarraygetubounds)(ESMC_LocalArray **ptr, int *rank, int *ubounds, int *localrc) {
          if ((ptr == NULL) || (*ptr == NULL)) {
              *localrc = ESMF_FAILURE;
              return;
          }
         *localrc = (*ptr)->ESMC_LocalArrayGetUbounds(*rank, ubounds);
     }


     void FTN(c_esmc_localarraygetrank)(ESMC_LocalArray **ptr, int *rank, int *localrc) {
          if ((ptr == NULL) || (*ptr == NULL)) {
              *localrc = ESMF_FAILURE;
              return;
          }
         *rank = (*ptr)->ESMC_LocalArrayGetRank();
         *localrc = ESMF_SUCCESS;
     }

     void FTN(c_esmc_localarraygettypekind)(ESMC_LocalArray **ptr, int *kind, int *localrc) {
      
          if ((ptr == NULL) || (*ptr == NULL)) {
              *localrc = ESMF_FAILURE;
              return;
          }
         *kind = (*ptr)->ESMC_LocalArrayGetTypeKind();
         *localrc = ESMF_SUCCESS;
     }

     void FTN(c_esmc_localarraygetname)(ESMC_LocalArray **ptr, 
                                        char *name, int *localrc, int nlen) {
         if ((nlen <= 0) || (name == NULL)) {
             if (localrc) *localrc = ESMF_FAILURE;
             return;
         }

         memset(name, ' ', nlen);
         strncpy(name, (*ptr)->ESMC_BaseGetName(),
                                   MIN(nlen, strlen((*ptr)->ESMC_BaseGetName())));
         *localrc = ESMF_SUCCESS;
     }


     void FTN(c_esmc_localarraydestroy)(ESMC_LocalArray **ptr, int *localrc) {
          if ((ptr == NULL) || (*ptr == NULL)) {
              *localrc = ESMF_FAILURE;
              return;
          }
         *localrc = ESMC_LocalArray::ESMC_LocalArrayDestroy(*ptr);
     }

     void FTN(c_esmc_localarraysetbaseaddr)(ESMC_LocalArray **ptr, void XD *base, int *localrc) {
          if ((ptr == NULL) || (*ptr == NULL)) {
              *localrc = ESMF_FAILURE;
              return;
          }
          *localrc = (*ptr)->ESMC_LocalArraySetBaseAddr(XD base);
     }

     void FTN(c_esmc_localarraygetbaseaddr)(ESMC_LocalArray **ptr, float **base, int *localrc) {
          if ((ptr == NULL) || (*ptr == NULL)) {
              *localrc = ESMF_FAILURE;
              return;
          }
          *localrc = (*ptr)->ESMC_LocalArrayGetBaseAddr((void **)base);
     }

     void FTN(c_esmc_localarraysetf90ptr)(ESMC_LocalArray **ptr, struct c_F90ptr *p, int *localrc) {
          if ((ptr == NULL) || (*ptr == NULL)) {
              *localrc = ESMF_FAILURE;
              return;
          }
        //fprintf(stderr, "interface code, setting f90 ptr to %lx, this = %lx, &this = %lx\n", 
        //                                (ESMC_I8)p, (ESMC_I8)(*ptr), (ESMC_I8)ptr);
          *localrc = (*ptr)->ESMC_LocalArraySetF90Ptr(p);
     }

     void FTN(c_esmc_localarraygetf90ptr)(ESMC_LocalArray **ptr, struct c_F90ptr *p, int *localrc) {
          if ((ptr == NULL) || (*ptr == NULL)) {
              *localrc = ESMF_FAILURE;
              return;
          }
        //fprintf(stderr, "interface code, getting f90 ptr into %lx, this = %lx, &this = %lx\n", 
        //                                (ESMC_I8)p, (ESMC_I8)(*ptr), (ESMC_I8)ptr);
          *localrc = (*ptr)->ESMC_LocalArrayGetF90Ptr(p);
     }

     void FTN(c_esmc_localarrayforcef90ptr)(ESMC_LocalArray **ptr, 
        void XD *base, int *localrc) {
          if ((ptr == NULL) || (*ptr == NULL)) {
              *localrc = ESMF_FAILURE;
              return;
          }
          *localrc = (*ptr)->ESMC_LocalArrayForceF90Ptr(XD base);
     }

     void FTN(c_esmc_localarraysetdealloc)(ESMC_LocalArray **ptr, int *localrc) {
          if ((ptr == NULL) || (*ptr == NULL)) {
              *localrc = ESMF_FAILURE;
              return;
          }
          *localrc = (*ptr)->ESMC_LocalArraySetDealloc();
     }

     void FTN(c_esmc_localarraysetnodealloc)(ESMC_LocalArray **ptr, int *localrc) {
          if ((ptr == NULL) || (*ptr == NULL)) {
              *localrc = ESMF_FAILURE;
              return;
          }
          *localrc = (*ptr)->ESMC_LocalArraySetNoDealloc();
     }

     void FTN(c_esmc_localarrayneedsdealloc)(ESMC_LocalArray **ptr, int flag, int *localrc) {
          if ((ptr == NULL) || (*ptr == NULL)) {
              flag = ESMC_ARRAY_NO_ALLOCATE;
              *localrc = ESMF_FAILURE;
              return;
          }
          flag = (*ptr)->ESMC_LocalArrayNeedsDealloc();
          *localrc = ESMF_SUCCESS;
     }

     void FTN(c_esmc_localarrayprint)(ESMC_LocalArray **ptr, char *opts, int *localrc, 
                                                                 int clen) {
         char *temp = NULL;

          if ((ptr == NULL) || (*ptr == NULL)) {
              *localrc = ESMF_FAILURE;
              return;
          }
         // make a local copy because opts may be non-writable or not
         // long enough to add a trailing null safely.
         if (opts && (clen > 0)) {
             temp = new char[clen+1];
             strncpy(temp, opts, clen);
             temp[clen] = '\0';
         }

         *localrc = (*ptr)->ESMC_LocalArrayPrint(temp);

         if (temp)
             delete[] temp;
     }

     void FTN(c_esmc_localarraywrite)(ESMC_LocalArray **ptr, char *opts, char *fname,
                                    int *localrc, int optlen, int flen) {
         char *opttemp = NULL;
         char *filetemp = NULL;

          if ((ptr == NULL) || (*ptr == NULL)) {
              *localrc = ESMF_FAILURE;
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

         *localrc = (*ptr)->ESMC_LocalArrayWrite(opttemp, filetemp);

         if (opttemp)
             delete[] opttemp;
         if (filetemp)
             delete[] filetemp;
     }

     // compare actual pointer size computed at run time, vs. the
     // compile-time fixed size specified in ESMC_Conf.h
     void FTN(c_esmf_f90ptrsizeprint)(char *p1, char *p2, int *rank, int *rc) {
         int rsize = (int)(p2 - p1);

         int fixed = ESMF_F90_PTR_BASE_SIZE;
         for (int i=1; i<*rank; i++)
             fixed += ESMF_F90_PTR_PLUS_RANK;

         if (rsize != fixed) {
            printf("No Match: rank %d=%d, computed=%d (diff=%d)\n",
                             *rank, fixed, rsize, rsize-fixed);
            printf(" full details: \n");
            printf("  rank %d: compiled-in size is %d bytes, run-time computed size is %d bytes (diff=%d)\n", 
                             *rank, fixed, rsize, rsize-fixed);
            printf("  in platform specific conf.h file, ESMF_F90_PTR_BASE_SIZE = %d\n",
                             ESMF_F90_PTR_BASE_SIZE);
            printf("  increment per rank ESMF_F90_PTR_PLUS_RANK = %d\n",
                             ESMF_F90_PTR_PLUS_RANK);

            *rc = ESMF_FAILURE;
;
         } else {
            printf("Match: rank %d Fortran 90 pointer is %d bytes, matches computed size ok\n", 
                          *rank, fixed);
            *rc = ESMF_SUCCESS;
         } 
     }


     // subtract 2 pointers and return the actual number of bytes between them.
     void FTN(c_esmf_f90ptrsizeget)(char *p1, char *p2, int *psize, int *rc) {

         if (psize)
             *psize = (int)(p2 - p1);
         if (rc)
             *rc = ESMF_SUCCESS;
     }


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_localarrayserialize"
//BOP
// !IROUTINE:  c_ESMC_LocalArraySerialize - Serialize LocalArray object 
//
// !INTERFACE:
      void FTN(c_esmc_localarrayserialize)(
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_LocalArray **localarray,       // in - localarray object
      char *buf,                // in/out - a byte stream buffer
      int *length,              // in/out - number of allocated bytes
      int *offset,              // in/out - current offset in the stream
      int *rc) {                // out - return code
// 
// !DESCRIPTION:
//     Serialize the contents of a localarray object.
//     Warning!!  Not completely implemented yet.
//
//EOP

  if (!localarray) {
    //printf("uninitialized LocalArray object\n");
    ESMC_LogDefault.ESMC_LogWrite("LocalArray object uninitialized", ESMC_LOG_INFO);
    if (rc) *rc = ESMF_SUCCESS;
    return;
  }

  *rc = (*localarray)->ESMC_LocalArraySerialize(buf, length, offset);

  return;

}  // end c_ESMC_LocalArraySerialize


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_localarraydeserialize"
//BOP
// !IROUTINE:  c_ESMC_LocalArrayDeserialize - Deserialize LocalArray object 
//
// !INTERFACE:
      void FTN(c_esmc_localarraydeserialize)(
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_LocalArray **localarray,       // in/out - empty localarray object to fill in
      char *buf,                // in - byte stream buffer
      int *offset,              // in/out - current offset in the stream
      int *rc) {                // out - return code
// 
// !DESCRIPTION:
//     Deserialize the contents of a localarray object.
//
//EOP

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;       

  // create a new localarray object to deserialize into
  *localarray = new ESMC_LocalArray;

  (*localarray)->ESMC_LocalArrayDeserialize(buf, offset);

  if (rc) *rc = ESMF_SUCCESS;

  return;

}  // end c_ESMC_LocalArrayDeserialize


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_localarrayserializenodata"
//BOP
// !IROUTINE:  c_ESMC_LocalArraySerializeNoData - Serialize LocalArray object 
//
// !INTERFACE:
      void FTN(c_esmc_localarrayserializenodata)(
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list
// 
// !ARGUMENTS:
      ESMC_LocalArray **localarray,       // in - localarray object
      char *buf,                // in/out - a byte stream buffer
      int *length,              // in/out - number of allocated bytes
      int *offset,              // in/out - current offset in the stream
      int *rc) {                // out - return code
// 
// !DESCRIPTION:
//     Serialize the contents of a localarray object, without preserving
//     the data values.
//
//EOP

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;       

  if (!localarray) {
    //printf("uninitialized LocalArray object\n");
    ESMC_LogDefault.ESMC_LogWrite("LocalArray object uninitialized", ESMC_LOG_INFO);
    if (rc) *rc = ESMF_SUCCESS;
    return;
  }

  *rc = (*localarray)->ESMC_LocalArraySerializeNoData(buf, length, offset);

  return;

}  // end c_ESMC_LocalArraySerializeNoData


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_localarraydeserializenodata"
//BOP
// !IROUTINE:  c_ESMC_LocalArrayDeserializeNoData - Deserialize LocalArray object 
//
// !INTERFACE:
      void FTN(c_esmc_localarraydeserializenodata)(
//
// !RETURN VALUE:
//    none.  return code is passed thru the parameter list.
// 
// !ARGUMENTS:
      ESMC_LocalArray **localarray,       // in/out - empty localarray object to fill in
      char *buf,                // in - byte stream buffer
      int *offset,              // in/out - current offset in the stream
      int *rc) {                // out - return code
// 
// !DESCRIPTION:
//     Deserialize the contents of a localarray object, without preserving
//     any of the data (counts explicitly set to 0).
//
//EOP

  // Initialize return code; assume routine not implemented
  if (rc) *rc = ESMC_RC_NOT_IMPL;

  // create a new localarray object to deserialize into
  *localarray = new ESMC_LocalArray;

  (*localarray)->ESMC_LocalArrayDeserializeNoData(buf, offset);

  if (rc) *rc = ESMF_SUCCESS;

  return;

}  // end c_ESMC_LocalArrayDeserializeNoData


#undef  ESMC_METHOD

}


