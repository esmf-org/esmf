// $Id: ESMCI_LocalArray_F.C,v 1.6 2009/06/16 20:54:47 theurich Exp $
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
#define ESMC_FILENAME "ESMCI_LocalArray_F.C"
//==============================================================================
//
// This file contains the Fortran interface code to link F90 and C++.
//
//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------
#include <stdio.h>
#include <string.h>

#include "ESMC_Start.h"
#include "ESMC_Base.h"

#include "ESMCI_LocalArray.h"

#include "ESMCI_LogErr.h"
#include "ESMC_LogMacros.inc"             // for LogErr
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

     void FTN(c_esmc_localarraycreateall)(ESMCI::LocalArray **ptr, int *rank, 
                                     ESMC_TypeKind *dk,
                                     int *counts, int *lbounds, int *ubounds,
                                     int *localrc)  {
         (*ptr) = ESMCI::LocalArray::create_F(*rank, *dk, counts,
                                    NULL, NULL, ESMCI::DATA_NONE, name,
                                    lbounds, ubounds, NULL, localrc);

             (*localrc) = (*ptr != NULL) ? ESMF_SUCCESS : ESMF_FAILURE;
     }

     void FTN(c_esmc_localarraycreatenodata)(ESMCI::LocalArray **ptr, int *rank,
                                        ESMC_TypeKind *dk, 
                                        ESMCI::LocalArrayOrigin *oflag, 
         int *localrc) {
             
             (*ptr) = ESMCI::LocalArray::createNoData(*rank, *dk,
                  *oflag, name, localrc);

             (*localrc) = (*ptr != NULL) ? ESMF_SUCCESS : ESMF_FAILURE;
     }
 
     void FTN(c_esmc_localarraycreatecopy)(ESMCI::LocalArray **ptr, 
       ESMCI::LocalArray **larrayOut, int *localrc) {
             
             *larrayOut =
               ESMCI::LocalArray::create(*ptr, NULL, NULL, localrc);

             (*localrc) = (*ptr != NULL) ? ESMF_SUCCESS : ESMF_FAILURE;
     }
 
     void FTN(c_esmc_localarraysetinternal)(ESMCI::LocalArray **ptr, 
                               struct ESMCI::c_F90ptr *fptr,
                               void XD *base, 
                               int *counts,
                               int *lbounds, int *ubounds, int *offsets,
                               ESMC_Logical *contig, ESMC_Logical *dealloc,
                               int *localrc) {
      
          if ((ptr == NULL) || (*ptr == NULL)) {
              *localrc = ESMF_FAILURE;
              return;
          }
         *localrc = (*ptr)->setInfo(fptr, XD base, counts, 
                                                  lbounds, ubounds, offsets, 
                                                  contig, dealloc);
     }

     void FTN(c_esmc_localarraysetinfo)(ESMCI::LocalArray **ptr, 
                                        int *counts, int *lbounds, 
                                        int *ubounds, int *offsets,
                                        int *localrc) {
      
          if ((ptr == NULL) || (*ptr == NULL)) {
              *localrc = ESMF_FAILURE;
              return;
          }
         *localrc = (*ptr)->setInfo(NULL, NULL, counts, 
                                                  lbounds, ubounds, offsets, 
                                                  NULL, NULL);
     }

     void FTN(c_esmc_localarraygetinfo)(ESMCI::LocalArray **ptr, 
                               void **base, int *counts,
                               int *lbounds, int *ubounds, int *offsets,
                               int *localrc) {
      
          if ((ptr == NULL) || (*ptr == NULL)) {
              *localrc = ESMF_FAILURE;
              return;
          }
         *localrc = (*ptr)->getInfo(NULL, base, counts, 
                                                  lbounds, ubounds, offsets);
     }

     void FTN(c_esmc_localarraysetlengths)(ESMCI::LocalArray **ptr, int *rank,
          int *lengths, int *localrc) {
      
          if ((ptr == NULL) || (*ptr == NULL)) {
              *localrc = ESMF_FAILURE;
              return;
          }
         *localrc = (*ptr)->ESMC_LocalArraySetLengths(*rank, lengths);
     }

     void FTN(c_esmc_localarraygetlengths)(ESMCI::LocalArray **ptr, int *rank,
          int *lengths, int *localrc) {
      
          if ((ptr == NULL) || (*ptr == NULL)) {
              *localrc = ESMF_FAILURE;
              return;
          }
         *localrc = (*ptr)->ESMC_LocalArrayGetLengths(*rank, lengths);
     }

     void FTN(c_esmc_localarraygetlbounds)(ESMCI::LocalArray **ptr, int *rank,
          int *lbounds, int *localrc) {
          if ((ptr == NULL) || (*ptr == NULL)) {
              *localrc = ESMF_FAILURE;
              return;
          }
         *localrc = (*ptr)->ESMC_LocalArrayGetLbounds(*rank, lbounds);
     }

     void FTN(c_esmc_localarraygetubounds)(ESMCI::LocalArray **ptr, int *rank,
          int *ubounds, int *localrc) {
          if ((ptr == NULL) || (*ptr == NULL)) {
              *localrc = ESMF_FAILURE;
              return;
          }
         *localrc = (*ptr)->ESMC_LocalArrayGetUbounds(*rank, ubounds);
     }


     void FTN(c_esmc_localarraygetrank)(ESMCI::LocalArray **ptr, int *rank,
         int *localrc) {
          if ((ptr == NULL) || (*ptr == NULL)) {
              *localrc = ESMF_FAILURE;
              return;
          }
         *rank = (*ptr)->getRank();
         *localrc = ESMF_SUCCESS;
     }

     void FTN(c_esmc_localarraygettypekind)(ESMCI::LocalArray **ptr, int *kind, int *localrc) {
      
          if ((ptr == NULL) || (*ptr == NULL)) {
              *localrc = ESMF_FAILURE;
              return;
          }
         *kind = (*ptr)->getTypeKind();
         *localrc = ESMF_SUCCESS;
     }

     void FTN(c_esmc_localarraygetname)(ESMCI::LocalArray **ptr, 
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


     void FTN(c_esmc_localarraydestroy)(ESMCI::LocalArray **ptr, int *localrc) {
          if ((ptr == NULL) || (*ptr == NULL)) {
              *localrc = ESMF_FAILURE;
              return;
          }
         *localrc = ESMCI::LocalArray::destroy(*ptr);
     }

     void FTN(c_esmc_localarraysetbaseaddr)(ESMCI::LocalArray **ptr, void XD *base, int *localrc) {
          if ((ptr == NULL) || (*ptr == NULL)) {
              *localrc = ESMF_FAILURE;
              return;
          }
          (*ptr)->setBaseAddr(XD base);
          *localrc = ESMF_SUCCESS;
     }

     void FTN(c_esmc_localarraygetbaseaddr)(ESMCI::LocalArray **ptr,
         float **base, int *localrc) {
          if ((ptr == NULL) || (*ptr == NULL)) {
              *localrc = ESMF_FAILURE;
              return;
          }
          *base = (float *)(*ptr)->getBaseAddr();
          *localrc = ESMF_SUCCESS;
     }

     void FTN(c_esmc_localarraysetfptr)(ESMCI::LocalArray **ptr,
         struct ESMCI::c_F90ptr *p, int *localrc) {
          if ((ptr == NULL) || (*ptr == NULL)) {
              *localrc = ESMF_FAILURE;
              return;
          }
        //fprintf(stderr, "interface code, setting f90 ptr to %lx, this = %lx, &this = %lx\n", 
        //                                (ESMC_I8)p, (ESMC_I8)(*ptr), (ESMC_I8)ptr);
          *localrc = (*ptr)->setFortranPtr(p);
     }

     void FTN(c_esmc_localarraygetfptr)(ESMCI::LocalArray **ptr,
         struct ESMCI::c_F90ptr *p, int *localrc) {
          if ((ptr == NULL) || (*ptr == NULL)) {
              *localrc = ESMF_FAILURE;
              return;
          }
        //fprintf(stderr, "interface code, getting f90 ptr into %lx, this = %lx, &this = %lx\n", 
        //                                (ESMC_I8)p, (ESMC_I8)(*ptr), (ESMC_I8)ptr);
          *localrc = (*ptr)->getFortranPtr(p);
     }

     void FTN(c_esmc_localarrayforcefptr)(ESMCI::LocalArray **ptr, 
        void XD *base, int *localrc) {
          if ((ptr == NULL) || (*ptr == NULL)) {
              *localrc = ESMF_FAILURE;
              return;
          }
          *localrc = (*ptr)->forceFortranPtr(XD base);
     }

     void FTN(c_esmc_localarraysetdealloc)(ESMCI::LocalArray **ptr,
          int *localrc) {
          if ((ptr == NULL) || (*ptr == NULL)) {
              *localrc = ESMF_FAILURE;
              return;
          }
          (*ptr)->setDealloc();
          *localrc = ESMF_SUCCESS;
     }

     void FTN(c_esmc_localarraysetnodealloc)(ESMCI::LocalArray **ptr, int *localrc) {
          if ((ptr == NULL) || (*ptr == NULL)) {
              *localrc = ESMF_FAILURE;
              return;
          }
          (*ptr)->setNoDealloc();
          *localrc = ESMF_SUCCESS;
     }

     void FTN(c_esmc_localarrayneedsdealloc)(ESMCI::LocalArray **ptr, int flag, int *localrc) {
          if ((ptr == NULL) || (*ptr == NULL)) {
              flag = ESMCI::NO_ALLOCATE;
              *localrc = ESMF_FAILURE;
              return;
          }
          if ((*ptr)->needsDealloc())
            flag = 1;
          else
            flag = 0;
          *localrc = ESMF_SUCCESS;
     }

     void FTN(c_esmc_localarrayprint)(ESMCI::LocalArray **ptr, char *opts, int *localrc, 
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

         *localrc = (*ptr)->print(temp);

         if (temp)
             delete[] temp;
     }

     void FTN(c_esmc_localarraywrite)(ESMCI::LocalArray **ptr, char *opts,
         char *fname,
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

         *localrc = (*ptr)->write(opttemp, filetemp);

         if (opttemp)
             delete[] opttemp;
         if (filetemp)
             delete[] filetemp;
     }

     // compare actual size of Fortran dope vector computed at run time,
     // vs. the compile-time memory allocation in the LocalArray object.
     void FTN(c_esmf_f90ptrsizeprint)(char *p1, char *p2, int *rank, int *rc){
         int rsize = (int)(p2 - p1);
         int fixed = ESMF_F90_PTR_BASE_SIZE
           + ESMF_F90_MAXRANK_POSSIBLE*ESMF_F90_PTR_PLUS_RANK;
         
         if (rsize > fixed) {
            printf("dope vector allocation too small: rank %d\n", *rank);
            printf(" full details: \n");
            printf("  rank %d: compiled-in size is %d bytes, run-time computed"
              " size is %d bytes (diff=%d)\n", *rank, fixed, rsize,
              rsize-fixed);
            *rc = ESMF_FAILURE;
         } else {
            printf("dope vector allocation is sufficient: rank %d %d bytes <= "
              "%d bytes\n", *rank, rsize, fixed);
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

#undef  ESMC_METHOD

}


