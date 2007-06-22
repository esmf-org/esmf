// $Id: ESMC_InternGrid_F.C,v 1.1 2007/06/22 23:21:36 cdeluca Exp $
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
// The code in this file implements the C++ {\tt ESMC\_InternGrid} methods declared
// in the companion file ESMC_InternGrid.h
//
// 
//
//-----------------------------------------------------------------------------
//
 // insert any higher level, 3rd party or system includes here
#include "ESMC_Start.h"
#include "ESMC_LogErr.h"

 // associated class definition file
#include "ESMC_InternGrid.h"

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = 
             "$Id: ESMC_InternGrid_F.C,v 1.1 2007/06/22 23:21:36 cdeluca Exp $";
//-----------------------------------------------------------------------------

extern "C" {
//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the InternGrid routines
//
//

// non-method functions
void FTN(c_esmc_interngridserialize)(int *dimCount, 
                               int *interngridStructure, 
                               int *horzInternGridType, 
                               int *vertInternGridType, 
                               int *horzStagger, 
                               int *vertStagger,
                               int *interngridStorage,
                               int *horzCoordSystem,
                               int *vertCoordSystem,
                               int *coordOrder,
                               int *coordIndex,
                               int *periodic,                  // array of logicals
                               ESMC_R8 *minGlobalCoordPerDim,   // array of reals
                               ESMC_R8 *maxGlobalCoordPerDim,   // array of reals
                               void *buffer, int *length, int *offset, int *localrc){

    int *ip, i;
     ESMC_I8 l;
    ESMC_R8 *dp;

    // Initialize return code; assume routine not implemented
    if (localrc) *localrc = ESMC_RC_NOT_IMPL;


    // TODO: verify length > needed, and if not, make room.
    int fixedpart =  24 * sizeof(int *);   // rough estimate
    if ((*length - *offset) < fixedpart) {
         
         ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                           "Buffer too short to add a InternGrid object", localrc);
         return;
 
        //buffer = (char *)realloc((void *)buffer,
        //                         *length + 2*fixedpart + byte_count);
        //*length += 2 * fixedpart;
    }
    

    ip = (int *)((char *)(buffer) + *offset);
    *ip++ = *dimCount; 
    *ip++ = *interngridStructure; 
    *ip++ = *horzInternGridType; 
    *ip++ = *vertInternGridType; 
    *ip++ = *horzStagger; 
    *ip++ = *vertStagger; 
    *ip++ = *interngridStorage;
    *ip++ = *horzCoordSystem;
    *ip++ = *vertCoordSystem;
    *ip++ = *coordOrder;
    *ip++ = *coordIndex;
    for (i=0; i<*dimCount; i++) 
      *ip++ = periodic[i];               // array of logicals

    // make sure pointer is aligned on good ESMC_R8 address before the cast
    l = (ESMC_I8) ip;
    if (l%8) {
        l += 8 - (l%8);
        ip = (int *)l;
    }

    dp = (ESMC_R8 *)ip;
    for (i=0; i<*dimCount; i++) {
      *dp++ = minGlobalCoordPerDim[i];   // array of reals
      *dp++ = maxGlobalCoordPerDim[i];   // array of reals
    }
   
    *offset = (char *)dp - (char *)buffer;

    if (localrc) *localrc = ESMF_SUCCESS;

    return;
} 


void FTN(c_esmc_interngriddeserialize)(int *dimCount, 
                                 int *interngridStructure, 
                                 int *horzInternGridType, 
                                 int *vertInternGridType, 
                                 int *horzStagger, 
                                 int *vertStagger,
                                 int *interngridStorage,
                                 int *horzCoordSystem,
                                 int *vertCoordSystem,
                                 int *coordOrder,
                                 int *coordIndex,
                                 int *periodic,                  // array of logicals
                                 ESMC_R8 *minGlobalCoordPerDim,   // array of reals
                                 ESMC_R8 *maxGlobalCoordPerDim,   // array of reals
                                 void *buffer, int *offset, int *localrc){

    int *ip, i;
     ESMC_I8 l;
    ESMC_R8 *dp;

    // Initialize return code; assume routine not implemented
    if (localrc) *localrc = ESMC_RC_NOT_IMPL;

    ip = (int *)((char *)(buffer) + *offset);
    *dimCount        = *ip++; 
    *interngridStructure   = *ip++; 
    *horzInternGridType    = *ip++; 
    *vertInternGridType    = *ip++; 
    *horzStagger     = *ip++; 
    *vertStagger     = *ip++; 
    *interngridStorage     = *ip++;
    *horzCoordSystem = *ip++;
    *vertCoordSystem = *ip++;
    *coordOrder      = *ip++;
    *coordIndex      = *ip++;

    for (i=0; i<*dimCount; i++)
      periodic[i] = *ip++;    // array of logicals

    // make sure pointer is aligned on good ESMC_R8 address before the cast
    l = (ESMC_I8) ip;
    if (l%8) {
        l += 8 - (l%8);
        ip = (int *)l;
    }

    dp = (ESMC_R8 *)ip;
    for (i=0; i<*dimCount; i++) {
      minGlobalCoordPerDim[i] = *dp++;    // array of reals
      maxGlobalCoordPerDim[i] = *dp++;    // array of reals
    }

    *offset = (char *)dp - (char *)buffer;

    if (localrc) *localrc = ESMF_SUCCESS;

    return;
} 


} // extern "C"
