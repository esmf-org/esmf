// $Id: ESMC_Array_F.C,v 1.31 2003/04/14 16:01:32 nscollins Exp $
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
#include "ESMC_Alloc.h"
#include "ESMC_DELayout.h"
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

             (*status) = (*ptr != NULL) ? ESMF_SUCCESS : ESMF_FAILURE;
     }

     void FTN(c_esmc_arrayconstructbyspec)() {
     }

     void FTN(c_esmc_arraycreatebyptr)(ESMC_Array **ptr, 
                                 enum ESMC_DataType *dt, enum ESMC_DataKind *dk,
                                 int *rank, int *lengths, int *status) {
             
             (*ptr) = ESMC_ArrayCreate_F(*rank, *dt, *dk, NULL, NULL, lengths,
                                      NULL, NULL, status);

             (*status) = (*ptr != NULL) ? ESMF_SUCCESS : ESMF_FAILURE;
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

     void FTN(c_esmc_arraysetaxisindex)(ESMC_Array **ptr, ESMC_AxisIndex *ai, int *status) {
          *status = (*ptr)->ESMC_ArraySetAxisIndex(ai);
     }

     void FTN(c_esmc_arraygetaxisindex)(ESMC_Array **ptr, ESMC_AxisIndex *ai, int *status) {
          *status = (*ptr)->ESMC_ArrayGetAxisIndex(ai);
     }

     void FTN(c_esmc_arrayredist)(ESMC_Array **ptr, ESMC_DELayout **layout,
                                  int *rank_trans, int *size_rank_trans, 
                                  int *olddecompids, int *decompids,  int *size_decomp,
                                  ESMC_Array **RedistArray, int *status) {
          *status = (*ptr)->ESMC_ArrayRedist(*layout, rank_trans, *size_rank_trans,
                                             olddecompids, decompids, *size_decomp,
                                             *RedistArray);
     }

     void FTN(c_esmc_arrayhalo)(ESMC_Array **ptr, ESMC_DELayout **layout,
                                int *decompids,  int *size_decomp,
                                ESMC_AxisIndex *AI_exc, ESMC_AxisIndex *AI_tot,
                                int *status) {
          *status = (*ptr)->ESMC_ArrayHalo(*layout, decompids, *size_decomp,
                                           AI_exc, AI_tot);
     }

     void FTN(c_esmc_arraysetbaseaddr)(ESMC_Array **ptr, float *base, int *status) {
          *status = (*ptr)->ESMC_ArraySetBaseAddr((void *)(base));
     }

     void FTN(c_esmc_arraygetbaseaddr)(ESMC_Array **ptr, float **base, int *status) {
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
         cout << "array print called, clen = " << clen << " ";
         // TODO: make sure this isn't writing past end of buffer
         if (opts && (clen > 0)) {
             temp = new char[clen];
             strncpy(temp, opts, clen);
             temp[clen] = '\0';
             cout << "null term opts = " << opts << endl;
         } else cout << endl;
         *status = (*ptr)->ESMC_ArrayPrint(temp);
         if (temp)
             delete[] temp;
     }

     void FTN(c_esmf_sizeprint)(char *p1, char *p2, int *rank, int *total) {
         int psize = (int)(p2 - p1);

         int bytes = ESMF_F90_PTR_BASE_SIZE;
         for (int i=1; i<*rank; i++)
             bytes += ESMF_F90_PTR_PLUS_RANK;

         if (psize != bytes) {
            printf("!! Error!  need to fix ESMF_F90_PTR_xxx in conf.h\n");
            printf("real size is %d, computed size for rank %d is %d (diff=%d)\n", 
                                       psize, *rank, bytes, psize-bytes);
;
         } else
            printf("rank %d Fortran 90 pointer is %d bytes, matches computed size ok\n", 
                          *rank, psize);
     }


};



