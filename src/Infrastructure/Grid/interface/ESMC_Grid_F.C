// $Id: ESMC_Grid_F.C,v 1.3 2004/12/04 00:30:27 jwolfe Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMC interface routines

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ {\tt ESMC\_Grid} methods declared
// in the companion file ESMC_Grid.h
//
// 
//
//-----------------------------------------------------------------------------
//
 // insert any higher level, 3rd party or system includes here
#include "ESMC_Start.h"

 // associated class definition file
#include "ESMC_Grid.h"

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = 
             "$Id: ESMC_Grid_F.C,v 1.3 2004/12/04 00:30:27 jwolfe Exp $";
//-----------------------------------------------------------------------------

extern "C" {
//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the Grid routines
//
//

// non-method functions
void FTN(c_esmc_gridserialize)(int *dimCount, 
                               int *gridStructure, 
                               int *horzGridType, 
                               int *vertGridType, 
                               int *horzStagger, 
                               int *vertStagger,
                               int *gridStorage,
                               int *horzCoordSystem,
                               int *vertCoordSystem,
                               int *coordOrder,
                               int *coordIndex,
                               int *periodic,                  // array of logicals
                               double *minGlobalCoordPerDim,   // array of reals
                               double *maxGlobalCoordPerDim,   // array of reals
                               void *buffer, int *length, int *offset, int *localrc){

    int *ip, i;
    double *dp;

    // TODO: verify length > needed, and if not, make room.

    ip = (int *)((char *)(buffer) + *offset);
    *ip++ = *dimCount; 
    *ip++ = *gridStructure; 
    *ip++ = *horzGridType; 
    *ip++ = *vertGridType; 
    *ip++ = *horzStagger; 
    *ip++ = *vertStagger; 
    *ip++ = *gridStorage;
    *ip++ = *horzCoordSystem;
    *ip++ = *vertCoordSystem;
    *ip++ = *coordOrder;
    *ip++ = *coordIndex;
    for (i=0; i<*dimCount; i++) 
      *ip++ = periodic[i];               // array of logicals

    dp = (double *)ip;
    for (i=0; i<*dimCount; i++) {
      *dp++ = minGlobalCoordPerDim[i];   // array of reals
      *dp++ = maxGlobalCoordPerDim[i];   // array of reals
    }
   
    *offset = (char *)dp - (char *)buffer;

    if (localrc) *localrc = ESMF_SUCCESS;

    return;
} 


void FTN(c_esmc_griddeserialize)(int *dimCount, 
                                 int *gridStructure, 
                                 int *horzGridType, 
                                 int *vertGridType, 
                                 int *horzStagger, 
                                 int *vertStagger,
                                 int *gridStorage,
                                 int *horzCoordSystem,
                                 int *vertCoordSystem,
                                 int *coordOrder,
                                 int *coordIndex,
                                 int *periodic,                  // array of logicals
                                 double *minGlobalCoordPerDim,   // array of reals
                                 double *maxGlobalCoordPerDim,   // array of reals
                                 void *buffer, int *offset, int *localrc){

    int *ip, i;
    double *dp;

    ip = (int *)((char *)(buffer) + *offset);
    *dimCount        = *ip++; 
    *gridStructure   = *ip++; 
    *horzGridType    = *ip++; 
    *vertGridType    = *ip++; 
    *horzStagger     = *ip++; 
    *vertStagger     = *ip++; 
    *gridStorage     = *ip++;
    *horzCoordSystem = *ip++;
    *vertCoordSystem = *ip++;
    *coordOrder      = *ip++;
    *coordIndex      = *ip++;

    for (i=0; i<*dimCount; i++)
      periodic[i] = *ip++;    // array of logicals

    dp = (double *)ip;
    for (i=0; i<*dimCount; i++) {
      minGlobalCoordPerDim[i] = *dp++;    // array of reals
      maxGlobalCoordPerDim[i] = *dp++;    // array of reals
    }

    *offset = (char *)dp - (char *)buffer;

    if (localrc) *localrc = ESMF_SUCCESS;

    return;
} 


} // extern "C"
