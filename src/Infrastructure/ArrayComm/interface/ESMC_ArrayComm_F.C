// $Id: ESMC_ArrayComm_F.C,v 1.12.6.3 2007/10/18 02:42:09 cdeluca Exp $
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
#include <stdio.h>
#include <string.h>
#include "ESMC_Start.h"
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

     void FTN(c_esmc_arrayredist)(ESMC_Array **ptr, ESMC_DELayout **delayout,
                                  int *global_start, int *global_dimlengths, 
                                  int *rank_trans, int *size_rank_trans, 
                                  int *olddecompids, int *decompids,  int *size_decomp,
                                  ESMC_Array **RedistArray, int *status) {
          *status = (*ptr)->ESMC_ArrayRedist(*delayout, global_start, 
                                  global_dimlengths, rank_trans,
                                  *size_rank_trans, olddecompids, decompids, 
                                  *size_decomp, *RedistArray);
     }

     void FTN(c_esmc_arrayhalo)(ESMC_Array **ptr, ESMC_DELayout **delayout,
                                ESMC_AxisIndex *ai_global, 
                                int *global_dimlengths,
                                int *decompids,  int *size_decomp, 
                                ESMC_Logical *periodic, int *status) {
          *status = (*ptr)->ESMC_ArrayHalo(*delayout, ai_global, 
                                  global_dimlengths, decompids, *size_decomp, 
                                  periodic);
     }


     void FTN(c_esmc_arraygather)(ESMC_Array **ptr, ESMC_DELayout **delayout,
                                  int *decompids,  int *size_decomp,
                                  int *local_axislengths, int *global_dimlengths, 
                                  int *local_maxlengths, int *deid,
                                  ESMC_Array **Array_out, int *status) {
          *status = (*ptr)->ESMC_ArrayGather(*delayout, decompids, *size_decomp,
                                             local_axislengths, global_dimlengths,
                                             local_maxlengths, *deid, Array_out);
     }

     void FTN(c_esmc_arrayscatter)(ESMC_Array **ptr, ESMC_DELayout **delayout,
                                   int *decompids,  int *size_decomp, int *deid,
                                   ESMC_Array **Array_out, int *status) {
          *status = (*ptr)->ESMC_ArrayScatter(*delayout, decompids, *size_decomp,
                                              *deid, Array_out);
     }

};


