// $Id: ESMC_DELayout_F.C,v 1.22 2003/07/18 21:03:27 eschwab Exp $
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
//  allows F90 to call C++ for supporting {\tt ESMC\_DELayout} class functions.
//
//EOP

// the interface subroutine names MUST be in lower case
extern "C" {

       void FTN(c_esmc_delayoutcreatedefault1d)(ESMC_DELayout **ptr,
                                                int *status) {
           *ptr = ESMC_DELayoutCreate(status);
       }

       void FTN(c_esmc_delayoutcreatefparent)(ESMC_DELayout **ptr, ESMC_DELayout **parent,
					      int *parent_offsets, int *de_indices,
					      int *ndim, int *lengths, ESMC_CommType 
					      *commtypes, int *status) {
  	   *ptr = ESMC_DELayoutCreate(*parent, parent_offsets, de_indices, *ndim, lengths, 
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

       void FTN(c_esmc_delayoutgetdeidat)(ESMC_DELayout **ptr, int *x,
                                  int *y, int *z, int *id, int *status) {
           *status = (*ptr)->ESMC_DELayoutGetDEIDat(*x, *y, *z, id);
       }

       void FTN(c_esmc_delayoutgetparentdeid)(ESMC_DELayout **child, int *cid, 
                                            ESMC_DELayout **parent, int *pid,
                                            int *status) {
           *status = (*child)->ESMC_DELayoutGetParentDEID(*cid, *parent, pid);
       }

       void FTN(c_esmc_delayoutgetchilddeid)(ESMC_DELayout **parent, int *pid, 
                                            ESMC_DELayout **child, int *cid,
                                            int *status) {
           *status = (*parent)->ESMC_DELayoutGetParentDEID(*pid, *child, cid);
       }

       void FTN(c_esmc_delayoutgetdeexists)(ESMC_DELayout **ptr, int *deid, 
                                   ESMC_DELayout **other, ESMC_Logical *exists,
                                   int *status) {
           *status = (*ptr)->ESMC_DELayoutGetDEExists(*deid, *other, exists);
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

       void FTN(c_esmc_delayoutgatherarrayr)(ESMC_DELayout **ptr,
                                             float *DistArray, int *decompids, 
                                             int *size_decomp, 
                                             ESMC_AxisIndex *AIPtr, 
                                             ESMC_AxisIndex *AIPtr2, 
                                             float *GlobalArray, int *status) {
           *status = (*ptr)->ESMC_DELayoutGatherArrayF(DistArray, decompids, 
                                                       *size_decomp, AIPtr,
                                                       AIPtr2, GlobalArray);
       }

       void FTN(c_esmc_delayoutprint)(ESMC_DELayout **ptr, char *opts,
                                      int *status){
           *status = (*ptr)->ESMC_DELayoutPrint();
           //*status = (*ptr)->ESMC_DELayoutPrint(opts);
       }

       void FTN(c_esmc_delayoutscatterna)(ESMC_DELayout **ptr,
                                 void *sndarray, void *rcvarray, int *len,
                                 ESMC_DataKind *type, int *srcdeid,
                                 int *status) {
           *status = (*ptr)->ESMC_DELayoutScatter(sndarray, rcvarray, *len,
                                                  *type, *srcdeid);
       }

       void FTN(c_esmc_delayoutscatterla)(ESMC_DELayout **ptr,
                                 ESMC_LocalArray *sndarray,
                                 ESMC_LocalArray *rcvarray, int *len,
                                 int *srcdeid, int *status) {
           *status = (*ptr)->ESMC_DELayoutScatter(sndarray, rcvarray, *len,
                                                  *srcdeid);
       }

       void FTN(c_esmc_delayoutallreduce)(ESMC_DELayout **ptr, int *array,
                                 int *result, int *len, int *op, int *status) {
           *status = (*ptr)->ESMC_DELayoutAllReduce(array, result, *len,
                                                  (ESMC_Op) *op);
       }

       void FTN(c_esmc_delayoutsendrecv)(ESMC_DELayout **ptr, void *sbuf,
                                 void *rbuf, int *snum, int *rnum, int *sde_index, 
				 int *rde_index, ESMC_DataKind *type, int *status) {
           *status = (*ptr)->ESMC_DELayoutSendRecv(sbuf, rbuf, *snum, *rnum, 
                                 *sde_index, *rde_index, *type);
       }

       void FTN(c_esmc_delayoutbcast)(ESMC_DELayout **ptr, void *buf, int *num, 
                                 int *srcde_index, ESMC_DataKind *type, int *status) {
           *status = (*ptr)->ESMC_DELayoutBcast(buf, *num, *srcde_index, *type);
       }

       void FTN(c_esmc_delayoutallgathervi)(ESMC_DELayout **ptr,
                                 int *sndarray, int *slen, 
                                 int *rcvarray, int *rlen, int *rcvdispls, 
                                 int *status) {
           *status = (*ptr)->ESMC_DELayoutAllGatherVI(sndarray, *slen,
                                                    rcvarray,  rlen, rcvdispls);
       }

       void FTN(c_esmc_delayoutallgathervr)(ESMC_DELayout **ptr,
                                 float *sndarray, int *slen, 
                                 float *rcvarray, int *rlen, int *rcvdispls, 
                                 int *status) {
           *status = (*ptr)->ESMC_DELayoutAllGatherVF(sndarray, *slen,
                                                    rcvarray,  rlen, rcvdispls);
       }
};
