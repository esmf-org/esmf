// $Id: ESMC_Array_F.C,v 1.1 2002/11/07 22:29:35 nscollins Exp $
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
#include "stdio.h"
#include "ESMC.h"
#include "ESMC_Base.h"
#include "ESMC_Array.h"
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

     void FTN(c_esmc_arraycreatebyptr2d)(ESMC_Array *ptr, void *f90ptr, 
                                              int ni, int nj, int *status) {
             int lengths[2];
             enum ESMC_DataType dt;
             enum ESMC_DataKind dk;

             lengths[0] = ni;
             lengths[1] = nj;

             dt = ESMF_DATA_REAL;
             dk = ESMF_KIND_4;
             
             ptr = ESMC_ArrayCreate_F(2, dt, dk, NULL, NULL, lengths,
                                      NULL, f90ptr, status);
     }
 
     void FTN(c_esmc_arraydestroy)(ESMC_Array *ptr, int *status) {
         *status = ESMC_ArrayDestroy(ptr);
     }
};



