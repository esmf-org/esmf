// $Id: ESMC_Array_F.C,v 1.14 2003/10/07 22:33:11 nscollins Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.
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
#include <stdio.h>
#include <string.h>
#include <iostream.h>
#include "ESMC.h"
#include "ESMC_Base.h"
#include "ESMC_Array.h"
#include "ESMC_DELayout.h"
#include "ESMC_Grid.h" 
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
                               struct c_F90ptr *fptr, void *base, int *counts,
                               int *lbounds, int *ubounds, int *offsets,
                               ESMC_Logical *contig, ESMC_Logical *dealloc,
			       int *hwidth, int *status) {
      
         *status = (*ptr)->ESMC_ArraySetInfo(fptr, base, counts, 
                                            lbounds, ubounds, offsets, 
                                            *contig, *dealloc, *hwidth);
     }

     void FTN(c_esmc_arraysetlengths)(ESMC_Array **ptr, int *rank, int *lengths, int *status) {
      
         *status = (*ptr)->ESMC_ArraySetLengths(*rank, lengths);
     }

     void FTN(c_esmc_arraygetlengths)(ESMC_Array **ptr, int *rank, int *lengths, int *status) {
      
         *status = (*ptr)->ESMC_ArrayGetLengths(*rank, lengths);
     }

     void FTN(c_esmc_arraygetrank)(ESMC_Array **ptr, int *rank, int *status) {
      
         *rank = (*ptr)->ESMC_ArrayGetRank();
         *status = ESMF_SUCCESS;
     }

     void FTN(c_esmc_arraygettype)(ESMC_Array **ptr, int *type, int *status) {
      
         *type = (*ptr)->ESMC_ArrayGetType();
         *status = ESMF_SUCCESS;
     }

     void FTN(c_esmc_arraygetkind)(ESMC_Array **ptr, int *kind, int *status) {
      
         *kind = (*ptr)->ESMC_ArrayGetKind();
         *status = ESMF_SUCCESS;
     }

     void FTN(c_esmc_arraydestroy)(ESMC_Array **ptr, int *status) {
         *status = ESMC_ArrayDestroy(*ptr);
     }

     void FTN(c_esmc_arraysetaxisindex)(ESMC_Array **ptr, ESMC_DomainType *dt, 
                                        ESMC_AxisIndex *ai, int *status) {
          *status = (*ptr)->ESMC_ArraySetAxisIndex(*dt, ai);
     }

     void FTN(c_esmc_arraygetaxisindex)(ESMC_Array **ptr, ESMC_DomainType *dt, 
                                        ESMC_AxisIndex *ai, int *status) {
          *status = (*ptr)->ESMC_ArrayGetAxisIndex(*dt, ai);
     }

     void FTN(c_esmc_arraygetallaxisindices)(ESMC_Array **ptr, 
                                     ESMC_AxisIndex *global, int *nDEs,
                                     ESMC_AxisIndex *total, ESMC_AxisIndex *comp, 
                                     ESMC_AxisIndex *excl, int *status) {
          *status = (*ptr)->ESMC_ArrayGetAllAxisIndices(global, *nDEs, 
                                     total, comp, excl);
     }

     void FTN(c_esmc_arraygetallaxisindex)(ESMC_Array **ptr, ESMC_DomainType *dt, 
                                     ESMC_AxisIndex *global, int *nDEs,
                                     ESMC_AxisIndex *ai, int *status) {
          switch(*dt) {
            case ESMC_DOMAIN_TOTAL:
               *status = (*ptr)->ESMC_ArrayGetAllAxisIndices(global, *nDEs, 
                                                           ai, NULL, NULL);
               break;
            case ESMC_DOMAIN_COMPUTATIONAL:
               *status = (*ptr)->ESMC_ArrayGetAllAxisIndices(global, *nDEs, 
                                                           NULL, ai, NULL);
               break;
            case ESMC_DOMAIN_EXCLUSIVE:
               *status = (*ptr)->ESMC_ArrayGetAllAxisIndices(global, *nDEs, 
                                                           NULL, NULL, ai);
               break;
         }
     }

     void FTN(c_esmc_arraysetbaseaddr)(ESMC_Array **ptr, void *base, int *status) {
          *status = (*ptr)->ESMC_ArraySetBaseAddr((void *)(base));
     }

     void FTN(c_esmc_arraygetbaseaddr)(ESMC_Array **ptr, void **base, int *status) {
          *status = (*ptr)->ESMC_ArrayGetBaseAddr((void **)base);
     }

     void FTN(c_esmc_arraysetf90ptr)(ESMC_Array **ptr, struct c_F90ptr *p, int *status) {
        //fprintf(stderr, "interface code, setting f90 ptr to %lx, this = %lx, &this = %lx\n", 
        //                                (long int)p, (long int)(*ptr), (long int)ptr);
          *status = (*ptr)->ESMC_ArraySetF90Ptr(p);
     }

     void FTN(c_esmc_arraygetf90ptr)(ESMC_Array **ptr, struct c_F90ptr *p, int *status) {
        //fprintf(stderr, "interface code, getting f90 ptr into %lx, this = %lx, &this = %lx\n", 
        //                                (long int)p, (long int)(*ptr), (long int)ptr);
          *status = (*ptr)->ESMC_ArrayGetF90Ptr(p);
     }

     void FTN(c_esmc_arraysetdealloc)(ESMC_Array **ptr, int *status) {
          *status = (*ptr)->ESMC_ArraySetDealloc();
     }

     void FTN(c_esmc_arraysetnodealloc)(ESMC_Array **ptr, int *status) {
          *status = (*ptr)->ESMC_ArraySetNoDealloc();
     }

     void FTN(c_esmc_arrayneedsdealloc)(ESMC_Array **ptr, int flag, int *status) {
      
          flag = (*ptr)->ESMC_ArrayNeedsDealloc();
          *status = ESMF_SUCCESS;
     }

     void FTN(c_esmc_arrayprint)(ESMC_Array **ptr, char *opts, int *status, 
                                                                 int clen) {
         char *temp = NULL;

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

};


