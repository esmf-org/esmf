// $Id: ESMCI_GeomBase_F.C,v 1.5 2011/01/05 20:05:43 svasquez Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2011, University Corporation for Atmospheric Research, 
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
//
//-----------------------------------------------------------------------------
//
// insert any higher level, 3rd party or system includes here

#include <cstring>
using namespace std;

#include "ESMCI_Macros.h"
#include "ESMCI_LogErr.h"


//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = 
             "$Id: ESMCI_GeomBase_F.C,v 1.5 2011/01/05 20:05:43 svasquez Exp $";
//-----------------------------------------------------------------------------

extern "C" {
//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the Field routines
//
//

// non-method functions
  void FTN(c_esmc_geombaseserialize)(int *geomtype, int * staggerloc,
				     int *meshloc, int *xgridside, int *xgridindex, 
				     char *buffer, int *length, int *offset,
				     ESMC_InquireFlag *inquireflag, int *localrc,
				     ESMCI_FortranStrLenArg buffer_l){
    int *ip;

    // Initialize return code; assume routine not implemented
    if (localrc) *localrc = ESMC_RC_NOT_IMPL;

    // TODO: verify length > 4 status vars, and if not, make room.
    int fixedpart = 5 * sizeof(int);
    if (*inquireflag != ESMF_INQUIREONLY) {
      if ((*length - *offset) < fixedpart) {     
         ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                    "Buffer too short to add a GeomBase object", localrc);
         return;
      }
    }


    // Fill in data
    ip = (int *)(buffer + *offset);
    if (*inquireflag != ESMF_INQUIREONLY) {
      *ip++ = *geomtype;
      *ip++ = *staggerloc; 
      *ip++ = *meshloc; 
      *ip++ = *xgridside; 
      *ip++ = *xgridindex; 
    }

    // Adjust offset
    *offset += 5 * sizeof(int);

    // return success
    if (localrc) *localrc = ESMF_SUCCESS;

    return;
} 


  void FTN(c_esmc_geombasedeserialize)(int *geomtype, int * staggerloc,
				       int *meshloc, int *xgridside, int *xgridindex, 
				       char *buffer, int *offset, int *localrc,
				       ESMCI_FortranStrLenArg buffer_l){

    int *ip;

    // Initialize return code; assume routine not implemented
    if (localrc) *localrc = ESMC_RC_NOT_IMPL;

    // Get data out
    ip = (int *)(buffer + *offset);
    *geomtype = *ip++;
    *staggerloc = *ip++; 
    *meshloc = *ip++; 
    *xgridside = *ip++; 
    *xgridindex = *ip++; 

    // Adjust offset
    *offset += 5 * sizeof(int);

    if (localrc) *localrc = ESMF_SUCCESS;

    return;
} 


}
