// $Id: ESMC_FieldDataMap_F.C,v 1.6 2007/03/31 05:51:07 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// ESMC interface routines

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ {\tt ESMC\_FieldDataMap} methods 
// declared in the companion file ESMC_FieldDataMap.h
//
// 
//
//-----------------------------------------------------------------------------
//
 // insert any higher level, 3rd party or system includes here
#include "ESMC_Start.h"
#include "ESMC_LogErr.h"

 // associated class definition files
#include "ESMC_InternArrayDataMap.h"
#include "ESMC_FieldDataMap.h"

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = 
             "$Id: ESMC_FieldDataMap_F.C,v 1.6 2007/03/31 05:51:07 cdeluca Exp $";
//-----------------------------------------------------------------------------

extern "C" {
//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the FieldDataMap routines
//
//

#if 0
      type ESMF_InterleaveType
         type(ESMF_InterleaveFlag) :: il_type
         integer :: il_start
         integer :: il_end
         integer :: il_strides

      type ESMF_ArrayDataMap
        type(ESMF_Status) :: status
        integer :: dataRank                             ! scalar, vector, etc.
        integer, dimension(ESMF_MAXDIM) :: dataDimOrder ! 0 = not a data dim
        integer, dimension(ESMF_MAXDIM) :: dataNonGridCounts ! for non-grid

      type ESMF_FieldDataMapDataMap
        type(ESMF_Status) :: status 
        type(ESMF_ArrayDataMap) :: adm
        type(ESMF_Logical) :: isScalar                  ! scalar values
        integer, dimension(ESMF_MAXDIM) :: rankLength   ! len if > scalar
        type(ESMF_InterleaveType) :: interleave         ! if > scalar
        type(ESMF_RelLoc) :: horzRelloc                 ! data item loc/cell
        type(ESMF_RelLoc) :: vertRelloc                 ! data item loc/cell
#endif

// non-method functions
void FTN(c_esmc_arraydatamapserialize)(int *status, int *dataRank,
                                  int *dataDimOrder,      /* ESMF_MAXDIM ints */
                                  int *dataNonGridCounts, /* ESMF_MAXDIM ints */
                         void *buffer, int *length, int *offset, int *localrc) {

    int i, *ip;

    // TODO: verify length > needed, and if not, make room.
    int fixedpart = 16 * sizeof(int);
    if ((*length - *offset) < fixedpart) {
         
         ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                         "Buffer too short to add a DataMap object", localrc);
         return;
 
        //buffer = (char *)realloc((void *)buffer,
        //                         *length + 2*fixedpart + byte_count);
        //*length += 2 * fixedpart;
    }


    ip = (int *)((char *)(buffer) + *offset);
    *ip++ = *status;
    *ip++ = *dataRank;
    for (i=0; i<ESMF_MAXDIM; i++)
        *ip++ = *dataDimOrder++;
    for (i=0; i<ESMF_MAXDIM; i++)
        *ip++ = *dataNonGridCounts++;

    *offset = (char *)ip - (char *)buffer;

    if (localrc) *localrc = ESMF_SUCCESS;

    return;
} 


void FTN(c_esmc_arraydatamapdeserialize)(int *status, int *dataRank,
                                  int *dataDimOrder,      /* ESMF_MAXDIM ints */
                                  int *dataNonGridCounts, /* ESMF_MAXDIM ints */
                                  void *buffer, int *offset, int *localrc) {

    int i, *ip;

    ip = (int *)((char *)(buffer) + *offset);
    *status = *ip++;
    *dataRank = *ip++;
    for (i=0; i<ESMF_MAXDIM; i++)
        *dataDimOrder++ = *ip++;
    for (i=0; i<ESMF_MAXDIM; i++)
        *dataNonGridCounts++ = *ip++;

    *offset = (char *)ip - (char *)buffer;

    if (localrc) *localrc = ESMF_SUCCESS;

    return;
} 


// non-method functions
void FTN(c_esmc_fielddatamapserialize)(int *status, int *isScalar,
                                  int *rankLength,  /* ESMF_MAXDIM ints */
                                  int *interleave,  /* 4 ints */
                                  int *horzRelloc, int *vertRelloc,
                         void *buffer, int *length, int *offset, int *localrc) {

    int i, *ip;

    // TODO: verify length > needed, and if not, make room.

    ip = (int *)((char *)(buffer) + *offset);
    *ip++ = *status;
    *ip++ = *isScalar;
    for (i=0; i<ESMF_MAXDIM; i++)
        *ip++ = *rankLength++;
    for (i=0; i<4; i++)
        *ip++ = *interleave++;
    *ip++ = *horzRelloc;
    *ip++ = *vertRelloc;

    *offset = (char *)ip - (char *)buffer;

    if (localrc) *localrc = ESMF_SUCCESS;

    return;
} 


// non-method functions
void FTN(c_esmc_fielddatamapdeserialize)(int *status, int *isScalar,
                                    int *rankLength,  /* ESMF_MAXDIM ints */
                                    int *interleave,  /* 4 ints */
                                    int *horzRelloc, int *vertRelloc,
                                    void *buffer, int *offset, int *localrc) {

    int i, *ip;

    ip = (int *)((char *)(buffer) + *offset);
    *status = *ip++;
    *isScalar = *ip++;
    for (i=0; i<ESMF_MAXDIM; i++)
        *rankLength++ = *ip++;
    for (i=0; i<4; i++)
        *interleave++ = *ip++;
    *horzRelloc = *ip++;
    *vertRelloc = *ip++;

    *offset = (char *)ip - (char *)buffer;

    if (localrc) *localrc = ESMF_SUCCESS;

    return;
} 



} // extern "C"
