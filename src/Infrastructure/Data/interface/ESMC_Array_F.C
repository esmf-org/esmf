// $Id: ESMC_Array_F.C,v 1.6 2002/12/10 18:13:03 nscollins Exp $
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

 // dummy structure which is the right size for an F90 pointer on
 //  the alpha architcture (halem)
 // TODO : find pointer size exactly for each system and put it into
 // build/*/conf.h for each arch.
struct c_F90ptr {
   char pad[88];
};

// the interface subroutine names MUST be in lower case
extern "C" {
     void FTN(c_esmc_storeallocfunc)(void (*func)(struct c_F90ptr *, int *, int *, int *), int *status) {
         *status = ESMC_AllocFuncStore(func);
     }
     void FTN(c_esmc_storedeallocfunc)(void (*func)(struct c_F90ptr *, int *, int *, int *), int *status) {
         *status = ESMC_DeallocFuncStore(func);
     }
     void FTN(c_esmc_arraycreate)(ESMC_Array *ptr, int rank, 
                                        enum ESMC_DataType type, 
                                        enum ESMC_DataKind kind,
                                        int *lbounds, int *ubounds, 
                                        int *strides, int *status) {
             ptr = ESMC_ArrayCreate(rank, type, kind, NULL,
                                     lbounds, ubounds, strides, status);
     }

     void FTN(c_esmc_arrayconstructbyspec)() {
     }

     void FTN(c_esmc_arraycreatebyptr2d)(ESMC_Array *ptr, int *ni, int *nj, int *status) {
             ESMC_Array *lptr;
             int lengths[2];
             enum ESMC_DataType dt;
             enum ESMC_DataKind dk;

             lengths[0] = *ni;
             lengths[1] = *nj;

             dt = ESMF_DATA_REAL;
             dk = ESMF_KIND_4;
             
             lptr = ESMC_ArrayCreate_F(2, dt, dk, NULL, NULL, lengths,
                                      NULL, NULL, status);

             *(unsigned long *)ptr = (unsigned long)lptr;
             printf("ptr = 0x%08lx, lptr = 0x%08lx, *lptr = 0x%08lx\n", 
                      (unsigned long)ptr, (unsigned long)lptr, *(unsigned long *)lptr);

             printf("status = %d\n", *status);
     }
 
     void FTN(c_esmc_arraydestroy)(ESMC_Array *ptr, int *status) {
         *status = ESMC_ArrayDestroy(ptr);
     }

     void FTN(c_esmc_arraysetbaseaddr)(ESMC_Array *ptr, float *base, int *status) {
          ptr->ESMC_ArraySetBaseAddr((void *)(base));
          *status = ESMF_SUCCESS;
     }

     void FTN(c_esmc_arrayprint)(ESMC_Array *ptr, int *status) {
         *status = ptr->ESMC_ArrayPrint("");
     }

};



