// $Id: ESMC_Layout_F.C,v 1.6 2003/02/21 21:01:53 jwolfe Exp $
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
#include "ESMC_Layout.h"
//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
// The code in this file implements the inter-language code which
//  allows F90 to call C++ for supporting {\tt Layout} class functions.
//
//EOP


// the interface subroutine names MUST be in lower case
extern "C" {

       void FTN(c_esmc_layoutcreate)(ESMC_Layout **ptr, int *nx, int *ny,
                int *delist, int *commhint, int *status) {
         *ptr = ESMC_LayoutCreate(*nx, *ny, delist, (ESMC_CommHint_e) *commhint,
                                  status);
       }

       void FTN(c_esmc_layoutdestroy)(ESMC_Layout **ptr, int *status) {
           *status = ESMC_LayoutDestroy(*ptr);
       }

       void FTN(c_esmc_layoutgetsize)(ESMC_Layout **ptr, int *nx, int *ny,
                int *status) {
           *status = (*ptr)->ESMC_LayoutGetSize(nx, ny);
       }

       void FTN(c_esmc_layoutgetdeposition)(ESMC_Layout **ptr, int *x, int *y,
                int *status) {
           *status = (*ptr)->ESMC_LayoutGetDEPosition(x, y);
       }

       void FTN(c_esmc_layoutgetdeid)(ESMC_Layout **ptr, int *id, int *status) {
           *status = (*ptr)->ESMC_LayoutGetDEid(id);
       }

       void FTN(c_esmc_layoutsetaxisindex)(ESMC_Layout **ptr,
                                           int *global_counts, 
                                           int *size_gcount, int *decompids, 
                                           int *size_decomp, 
                                           ESMC_AxisIndex *AIPtr, int *status) {
           *status = (*ptr)->ESMC_LayoutSetAxisIndex(global_counts, *size_gcount, 
                                                     decompids, *size_decomp,
                                                     AIPtr);
       }

       void FTN(c_esmc_layoutgatherarrayi)(ESMC_Layout **ptr,
                                           int *DistArray, int *decompids, 
                                           int *size_decomp, 
                                           ESMC_AxisIndex *AIPtr, 
                                           int *GlobalArray, int *status) {
           *status = (*ptr)->ESMC_LayoutGatherArrayI(DistArray, decompids, 
                                                     *size_decomp, AIPtr,
                                                     GlobalArray);
       }

//     void FTN(c_esmc_layoutprint)(ESMC_Layout **ptr, char *opts, int *status){
//         *status = (*ptr)->ESMC_LayoutPrint(opts);
//     }

       void FTN(c_esmc_layoutallreduce)(ESMC_Layout **ptr, int *array,
                int *result, int *len, int *op, int *status) {
           *status = (*ptr)->ESMC_LayoutAllReduce(array, result, *len,
                                                  (ESMC_Op_e) *op);
       }
       void FTN(c_esmc_layoutallgathervi)(ESMC_Layout **ptr,
                                 int *sndarray, int *slen, 
                                 int *rcvarray, int *rlen, int *rcvdispls, 
                                 int *status) {
           *status = (*ptr)->ESMC_LayoutAllGatherVI(sndarray, *slen,
                                                    rcvarray,  rlen, rcvdispls);
       }
};
