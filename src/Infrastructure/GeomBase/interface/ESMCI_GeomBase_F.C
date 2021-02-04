// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2021, University Corporation for Atmospheric Research,
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
             "$Id$";
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
  void FTN_X(c_esmc_geombaseserialize)(int *geomtype, int * staggerloc,
                                     int *meshloc, int *xgridside, int *xgridindex,
                                     char *buffer, int *length, int *offset,
                                     ESMC_InquireFlag *inquireflag, int *rc,
                                     ESMCI_FortranStrLenArg buffer_l){
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_geombaseserialize()"
    int *ip;

    // Initialize return code; assume routine not implemented
    if (rc) *rc = ESMC_RC_NOT_IMPL;

    // TODO: verify length > 4 status vars, and if not, make room.
    int fixedpart = 5 * sizeof(int);
    if (*inquireflag != ESMF_INQUIREONLY) {
      if ((*length - *offset) < fixedpart) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
          "Buffer too short to add a GeomBase object", ESMC_CONTEXT, rc);
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
    if (rc) *rc = ESMF_SUCCESS;

    return;
}


  void FTN_X(c_esmc_geombasedeserialize)(int *geomtype, int * staggerloc,
                                       int *meshloc, int *xgridside, int *xgridindex,
                                       const char *buffer, int *offset, int *rc,
                                       ESMCI_FortranStrLenArg buffer_l){

    int *ip;

    // Initialize return code; assume routine not implemented
    if (rc) *rc = ESMC_RC_NOT_IMPL;

    // Get data out
    ip = (int *)(buffer + *offset);
    *geomtype = *ip++;
    *staggerloc = *ip++;
    *meshloc = *ip++;
    *xgridside = *ip++;
    *xgridindex = *ip++;

    // Adjust offset
    *offset += 5 * sizeof(int);

    if (rc) *rc = ESMF_SUCCESS;

    return;
}


}
