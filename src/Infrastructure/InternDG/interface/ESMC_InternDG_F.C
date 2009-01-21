// $Id: ESMC_InternDG_F.C,v 1.8.2.2 2009/01/21 21:25:22 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
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
// The code in this file implements the C++ {\tt ESMC\_InternDG} methods declared
// in the companion file ESMC_InternDG.h
//
// 
//
//-----------------------------------------------------------------------------
//
 // insert any higher level, 3rd party or system includes here
#include "ESMC_Start.h"
#include "ESMC_LogErr.h"


 // associated class definition file
#include "ESMC_InternDG.h"

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = 
             "$Id: ESMC_InternDG_F.C,v 1.8.2.2 2009/01/21 21:25:22 cdeluca Exp $";
//-----------------------------------------------------------------------------

extern "C" {
//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the InternDG routines
//
//

// non-method functions
void FTN(c_esmc_interndgserialize)(int *dimCount, int *ndes,
                                   int *decompIDs,
                                   int *cellCountPerDEPerDim,
                                   void *buffer, int *length,
                                   int *offset, int *localrc){

    int *ip, i;

    // Initialize return code; assume routine not implemented
    if (localrc) *localrc = ESMC_RC_NOT_IMPL;

    // TODO: verify length > needed, and if not, make room.
    int fixedpart = 40 * sizeof(int *);
    if ((*length - *offset) < fixedpart) {
         
         ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                             "Buffer too short to add a IGrid object", localrc);
         return;
 
        //buffer = (char *)realloc((void *)buffer,
        //                         *length + 2*fixedpart + byte_count);
        //*length += 2 * fixedpart;
    }


    ip = (int *)((char *)(buffer) + *offset);
    *ip++ = *dimCount;
    *ip++ = *ndes;
    for (i=0; i<*dimCount; i++)
      *ip++ = decompIDs[i];
    for (i=0; i<*dimCount * *ndes; i++)
      *ip++ = cellCountPerDEPerDim[i];

    *offset = (char *)ip - (char *)buffer;

    if (localrc) *localrc = ESMF_SUCCESS;

    return;
} 


void FTN(c_esmc_interndgdeserialize)(int *decompIDs,
                                     int *cellCountPerDEPerDim,
                                     void *buffer, int *offset, int *localrc){

    int *ip, i, dimCount, ndes;

    // Initialize return code; assume routine not implemented
    if (localrc) *localrc = ESMC_RC_NOT_IMPL;

    ip = (int *)((char *)(buffer) + *offset);
    dimCount = *ip++;
    ndes     = *ip++;
    for (i=0; i<dimCount; i++)
      decompIDs[i]            = *ip++; 
    for (i=0; i<dimCount * ndes; i++)
      cellCountPerDEPerDim[i] = *ip++; 

    *offset = (char *)ip - (char *)buffer;

    if (localrc) *localrc = ESMF_SUCCESS;

    return;
} 


} // extern "C"
