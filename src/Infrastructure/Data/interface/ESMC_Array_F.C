// $Id: ESMC_Array_F.C,v 1.17 2003/02/10 16:43:37 nscollins Exp $
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
#include "ESMC.h"
#include "ESMC_Base.h"
#include "ESMC_Array.h"
#include "ESMC_Alloc.h"
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
     void FTN(c_esmc_storeallocfunc)(void (*func)(struct c_F90ptr *, int *, int *, int *), int *status) {
         *status = ESMC_AllocFuncStore(func);
     }
     void FTN(c_esmc_storedeallocfunc)(void (*func)(struct c_F90ptr *, int *, int *, int *), int *status) {
         *status = ESMC_DeallocFuncStore(func);
     }
     void FTN(c_esmc_arraycreate)(ESMC_Array **ptr, int rank, 
                                        enum ESMC_DataType type, 
                                        enum ESMC_DataKind kind,
                                        int *lbounds, int *ubounds, 
                                        int *strides, int *status) {
             (*ptr) = ESMC_ArrayCreate(rank, type, kind, NULL,
                                     lbounds, ubounds, strides, status);
     }

     void FTN(c_esmc_arrayconstructbyspec)() {
     }

     void FTN(c_esmc_arraycreatebyptr)(ESMC_Array **ptr, 
                                 enum ESMC_DataType *dt, enum ESMC_DataKind *dk,
                                 int *rank, int *lengths, int *status) {
             
             (*ptr) = ESMC_ArrayCreate_F(*rank, *dt, *dk, NULL, NULL, lengths,
                                      NULL, NULL, status);

     }
 
     void FTN(c_esmc_arraydestroy)(ESMC_Array **ptr, int *status) {
         *status = ESMC_ArrayDestroy(*ptr);
     }

     void FTN(c_esmc_arraysetaxisindex)(ESMC_Array **ptr, ESMC_AxisIndex *ai, int *status) {
          *status = (*ptr)->ESMC_ArraySetAxisIndex(ai);
     }

     void FTN(c_esmc_arraygetaxisindex)(ESMC_Array **ptr, ESMC_AxisIndex *ai, int *status) {
          *status = (*ptr)->ESMC_ArrayGetAxisIndex(ai);
     }

     void FTN(c_esmc_arraysetbaseaddr)(ESMC_Array **ptr, float *base, int *status) {
          (*ptr)->ESMC_ArraySetBaseAddr((void *)(base));
          *status = ESMF_SUCCESS;
     }

     void FTN(c_esmc_arraygetbaseaddr)(ESMC_Array **ptr, float *base, int *status) {
          (*ptr)->ESMC_ArrayGetBaseAddr((void *)(base));
          *status = ESMF_SUCCESS;
     }

     void FTN(c_esmc_arraysetf90ptr)(ESMC_Array **ptr, struct c_F90ptr *p, int *status) {
          (*ptr)->ESMC_ArraySetF90Ptr(p);
          *status = ESMF_SUCCESS;
     }

     void FTN(c_esmc_arraygetf90ptr)(ESMC_Array **ptr, struct c_F90ptr *p, int *status) {
          (*ptr)->ESMC_ArrayGetF90Ptr(p);
          *status = ESMF_SUCCESS;
     }

     void FTN(c_esmc_arraysetdealloc)(ESMC_Array **ptr, int *status) {
          (*ptr)->ESMC_ArraySetDealloc();
          *status = ESMF_SUCCESS;
     }

     void FTN(c_esmc_arraysetnodealloc)(ESMC_Array **ptr, int *status) {
          (*ptr)->ESMC_ArraySetNoDealloc();
          *status = ESMF_SUCCESS;
     }

     void FTN(c_esmc_arrayneedsdealloc)(ESMC_Array **ptr, int flag, int *status) {
      
          flag = (*ptr)->ESMC_ArrayNeedsDealloc();
          *status = ESMF_SUCCESS;
     }

     void FTN(c_esmc_arrayprint)(ESMC_Array **ptr, char *opts, int *status) {
         *status = (*ptr)->ESMC_ArrayPrint(opts);
     }

     void FTN(c_esmf_sizeprint)(char *p1, char *p2) {
         int psize = (int)(p2 - p1);
         printf("number of bytes in a Fortran 90 pointer is %d\n", psize);

         if (psize != ESMF_F90_PTR_SIZE) {
            printf("!! Error!  need to fix ESMF_F90_PTR_SIZE in conf.h\n");
            printf("real size is %d, #define is %d\n", psize, ESMF_F90_PTR_SIZE)
;
         }
     }


};



