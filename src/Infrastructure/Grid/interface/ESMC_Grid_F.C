// $Id: ESMC_Grid_F.C,v 1.1 2004/11/30 20:58:01 nscollins Exp $
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
             "$Id: ESMC_Grid_F.C,v 1.1 2004/11/30 20:58:01 nscollins Exp $";
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
void c_esmc_gridserialize(int *gridStatus, 
                          int *dimCount, 
                          int *hasLocalData, 
                          int *gridStructure, 
                          int *horzGridType, 
                          int *vertGridType, 
                          int *horzStagger, 
                          int *vertStagger,
                          void *buffer, int *length, int *offset, int *localrc){

    int *ip;

    // TODO: verify length > needed, and if not, make room.

    ip = (int *)((char *)(buffer) + *offset);
    *ip++ = *gridStatus;
    *ip++ = *dimCount; 
    *ip++ = *hasLocalData; 
    *ip++ = *gridStructure; 
    *ip++ = *horzGridType; 
    *ip++ = *vertGridType; 
    *ip++ = *horzStagger; 
    *ip++ = *vertStagger; 

    *offset = (char *)ip - (char *)buffer;

    if (localrc) *localrc = ESMF_SUCCESS;

    return;
} 


void c_esmc_griddeserialize(int *gridStatus, 
                            int *dimCount, 
                            int *hasLocalData, 
                            int *gridStructure, 
                            int *horzGridType, 
                            int *vertGridType, 
                            int *horzStagger, 
                            int *vertStagger,
                            void *buffer, int *offset, int *localrc){

    int *ip;

    ip = (int *)((char *)(buffer) + *offset);
    *gridStatus = *ip++;
    *dimCount = *ip++; 
    *hasLocalData = *ip++; 
    *gridStructure = *ip++; 
    *horzGridType = *ip++; 
    *vertGridType = *ip++; 
    *horzStagger = *ip++; 
    *vertStagger = *ip++; 

    *offset = (char *)ip - (char *)buffer;

    if (localrc) *localrc = ESMF_SUCCESS;

    return;
} 


} // extern "C"
