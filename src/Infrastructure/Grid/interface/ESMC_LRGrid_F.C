// $Id: ESMC_LRGrid_F.C,v 1.3.2.3 2007/10/18 02:42:50 cdeluca Exp $
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
// The code in this file implements the C++ {\tt ESMC\_LRGrid} methods declared
// in the companion file ESMC_LRGrid.h
//
// 
//
//-----------------------------------------------------------------------------
//
 // insert any higher level, 3rd party or system includes here
#include "ESMC_Start.h"
#include "ESMC_LogErr.h"

 // associated class definition file
//#include "ESMC_LRGrid.h"

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = 
             "$Id: ESMC_LRGrid_F.C,v 1.3.2.3 2007/10/18 02:42:50 cdeluca Exp $";
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
void FTN(c_esmc_lrgridserialize)(int *dimCount,
                                 int *countPerDim,               // array of ints
                                 double *deltaPerDim,            // array of reals
                                 void *buffer, int *length, 
                                 int *offset, int *localrc){

    int *ip, i;
    long l;
    double *dp;

    // TODO: verify length > needed, and if not, make room.
    int fixedpart = 12 * sizeof (int *);
    if ((*length - *offset) < fixedpart) {
         
         ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                            "Buffer too short to add a Grid object", localrc);
         return;
 
        //buffer = (char *)realloc((void *)buffer,
        //                         *length + 2*fixedpart + byte_count);
        //*length += 2 * fixedpart;
    }


    ip = (int *)((char *)(buffer) + *offset);
    *ip++ = *dimCount; 
    for (i=0; i<*dimCount; i++)
      *ip++ = countPerDim[i]; 

    // make sure pointer is aligned on good double address before the cast
    l = (long) ip;
    if (l%8) {
        l += 8 - (l%8);
        ip = (int *)l;
    }

    dp = (double *) ip;
    for (i=0; i<*dimCount; i++)
      *dp++ = deltaPerDim[i];

    *offset = (char *)dp - (char *)buffer;

    if (localrc) *localrc = ESMF_SUCCESS;

    return;
} 


void FTN(c_esmc_lrgriddeserialize)(int *countPerDim,            // array of ints
                                   double *deltaPerDim,            // array of reals
                                   void *buffer, int *offset, int *localrc){

    int *ip, i, dimCount;
    long l;
    double *dp;

    ip = (int *)((char *)(buffer) + *offset);
    dimCount = *ip++;
    for (i=0; i<dimCount; i++)
      countPerDim[i] = *ip++; 

    // make sure pointer is aligned on good double address before the cast
    l = (long) ip;
    if (l%8) {
        l += 8 - (l%8);
        ip = (int *)l;
    }

    dp = (double *) ip;
    for (i=0; i<dimCount; i++)
      deltaPerDim[i] = *dp++; 

    *offset = (char *)dp - (char *)buffer;

    if (localrc) *localrc = ESMF_SUCCESS;

    return;
} 


} // extern "C"
