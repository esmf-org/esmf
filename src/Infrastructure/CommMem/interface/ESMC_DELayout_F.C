// $Id: ESMC_DELayout_F.C,v 1.15 2003/04/07 16:54:12 nscollins Exp $
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

       void FTN(c_esmc_delayoutcreatefparent)(ESMC_DELayout **ptr, ESMC_DELayout *parent,
					      int *parent_offsets, int *de_indices,
					      int *ndim, int *lengths, ESMC_CommType 
					      *commtypes, int *status) {
  	   *ptr = ESMC_DELayoutCreate(parent, parent_offsets, de_indices, *ndim, lengths, 
				      commtypes, status);
       }

       void FTN(c_esmc_delayoutcreatefde)(ESMC_DELayout **ptr, int *delist,
					      int *ndim, int *lengths, ESMC_CommType 
					      *commtypes, int *status) {
  	   *ptr = ESMC_DELayoutCreate(delist, *ndim, lengths, commtypes, status);
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

       void FTN(c_esmc_delayoutsendrecv)(ESMC_DELayout **ptr, void *sbuf,
                                 void *rbuf, int *snum, int *rnum, int *sde_index, 
				 int *rde_index, ESMC_Datatype *type, int *status) {
           *status = (*ptr)->ESMC_DELayoutSendRecv(sbuf, rbuf, *snum, *rnum, 
                                 *sde_index, *rde_index, *type);
       }

       void FTN(c_esmc_delayoutbcast)(ESMC_DELayout **ptr, void *buf, int *num, 
                                 int *rootde_index, ESMC_Datatype *type, int *status) {
           *status = (*ptr)->ESMC_DELayoutBcast(buf, *num, *rootde_index, *type);
       }

       void FTN(c_esmc_delayoutallgathervi)(ESMC_DELayout **ptr,
                                 int *sndarray, int *slen, 
                                 int *rcvarray, int *rlen, int *rcvdispls, 
                                 int *status) {
           *status = (*ptr)->ESMC_DELayoutAllGatherVI(sndarray, *slen,
                                                    rcvarray,  rlen, rcvdispls);
       }
};









