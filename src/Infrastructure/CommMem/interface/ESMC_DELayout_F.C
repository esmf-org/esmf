// $Id: ESMC_DELayout_F.C,v 1.12 2003/03/27 20:41:20 cdeluca Exp $
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
#include "ESMC_DELayout.h"
//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
// The code in this file implements the inter-language code which
//  allows F90 to call C++ for supporting {\tt DELayout} class functions.
//
//EOP


// the interface subroutine names MUST be in lower case
extern "C" {

       void FTN(c_esmc_delayoutcreatedefault1d)(ESMC_DELayout **ptr,
                                                int *status) {
         *ptr = ESMC_DELayoutCreate(status);
       }

       void FTN(c_esmc_delayoutcreatelayout2d)(ESMC_DELayout **ptr, int *nx,
                                               int *ny,
                                               ESMC_DELayout **parentlayout,
                                               int *commhint, int *exclusive,
                                               int *status) {
         *ptr = ESMC_DELayoutCreate(*nx, *ny, *parentlayout,
                                    (ESMC_CommHint_e) *commhint,
                                    (ESMC_Exclusivity_e) *exclusive,
                                    status);
       }

       void FTN(c_esmc_delayoutcreatelayout2dne)(ESMC_DELayout **ptr, int *nx,
                                                 int *ny,
                                                 ESMC_DELayout **parentlayout,
                                                 int *commhint,
                                                 int *status) {
         *ptr = ESMC_DELayoutCreate(*nx, *ny, *parentlayout,
                                    (ESMC_CommHint_e) *commhint,
                                    status);
       }

       void FTN(c_esmc_delayoutcreate)(ESMC_DELayout **ptr, int *nx, int *ny,
                                     int *delist, int *commhint, int *status) {
         *ptr = ESMC_DELayoutCreate(*nx, *ny, delist,
                                    (ESMC_CommHint_e) *commhint, status);
       }

       void FTN(c_esmc_delayoutdestroy)(ESMC_DELayout **ptr, int *status) {
           *status = ESMC_DELayoutDestroy(*ptr);
       }

       void FTN(c_esmc_delayoutgetnumdes)(ESMC_DELayout **ptr, int *ndes,
                                          int *status) {
           *status = (*ptr)->ESMC_DELayoutGetNumDEs(ndes);
       }

       void FTN(c_esmc_delayoutgetsize)(ESMC_DELayout **ptr, int *nx, int *ny,
                                        int *status) {
           *status = (*ptr)->ESMC_DELayoutGetSize(nx, ny);
       }

       void FTN(c_esmc_delayoutislocal)(ESMC_DELayout **ptr, int *islocal, 
                                        int *status) {
	 //   *status = (*ptr)->ESMC_DELayoutIsLocal(islocal);
       }

       void FTN(c_esmc_delayoutgetdeposition)(ESMC_DELayout **ptr, int *x,
                                              int *y, int *status) {
           *status = (*ptr)->ESMC_DELayoutGetDEPosition(x, y);
       }

       void FTN(c_esmc_delayoutgetdeid)(ESMC_DELayout **ptr, int *id,
                                        int *status) {
           *status = (*ptr)->ESMC_DELayoutGetDEID(id);
       }

       void FTN(c_esmc_delayoutsetaxisindex)(ESMC_DELayout **ptr,
                                           int *global_counts, 
                                           int *size_gcount, int *decompids, 
                                           int *size_decomp, 
                                           ESMC_AxisIndex *AIPtr, int *status) {
           *status = (*ptr)->ESMC_DELayoutSetAxisIndex(global_counts,
                                                       *size_gcount, 
                                                       decompids, *size_decomp,
                                                       AIPtr);
       }

       void FTN(c_esmc_delayoutgatherarrayi)(ESMC_DELayout **ptr,
                                             int *DistArray, int *decompids, 
                                             int *size_decomp, 
                                             ESMC_AxisIndex *AIPtr, 
                                             ESMC_AxisIndex *AIPtr2, 
                                             int *GlobalArray, int *status) {
           *status = (*ptr)->ESMC_DELayoutGatherArrayI(DistArray, decompids, 
                                                       *size_decomp, AIPtr,
                                                       AIPtr2, GlobalArray);
       }

       void FTN(c_esmc_delayoutprint)(ESMC_DELayout **ptr, char *opts,
                                      int *status){
           *status = (*ptr)->ESMC_DELayoutPrint();
           //*status = (*ptr)->ESMC_DELayoutPrint(opts);
       }

       void FTN(c_esmc_delayoutallreduce)(ESMC_DELayout **ptr, int *array,
                                 int *result, int *len, int *op, int *status) {
           *status = (*ptr)->ESMC_DELayoutAllReduce(array, result, *len,
                                                  (ESMC_Op) *op);
       }

       void FTN(c_esmc_delayoutsendrecv)(ESMC_DELayout **ptr, void *sarray,
                                 void *rarray, int *sarraylen, int *rarraylen,
                                 int *sde, int *rde, int *status) {
           *status = (*ptr)->ESMC_DELayoutSendRecv(sarray, rarray, *sarraylen,
                                 *rarraylen, *sde, *rde);
       }
       void FTN(c_esmc_delayoutallgathervi)(ESMC_DELayout **ptr,
                                 int *sndarray, int *slen, 
                                 int *rcvarray, int *rlen, int *rcvdispls, 
                                 int *status) {
           *status = (*ptr)->ESMC_DELayoutAllGatherVI(sndarray, *slen,
                                                    rcvarray,  rlen, rcvdispls);
       }
};









