// $Id: ESMCI_GeomBase_F.C,v 1.2.2.2 2010/03/10 06:33:08 oehmke Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research, 
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

#include "ESMC_Start.h"
#include "ESMCI_LogErr.h"


//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = 
             "$Id: ESMCI_GeomBase_F.C,v 1.2.2.2 2010/03/10 06:33:08 oehmke Exp $";
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
		    void *buffer, int *length, int *offset,
                    ESMC_InquireFlag *inquireflag, int *localrc){
    int *ip;

    // Initialize return code; assume routine not implemented
    if (localrc) *localrc = ESMC_RC_NOT_IMPL;

    // TODO: verify length > 4 status vars, and if not, make room.
    int fixedpart = 2 * sizeof(int);
    if (*inquireflag != ESMF_INQUIREONLY) {
      if ((*length - *offset) < fixedpart) {     
         ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                    "Buffer too short to add a GeomBase object", localrc);
         return;
      }
    }


    // Fill in data
    ip = (int *)((char *)(buffer) + *offset);
    if (*inquireflag != ESMF_INQUIREONLY) {
      *ip++ = *geomtype;
      *ip++ = *staggerloc; 
    }

    // Adjust offset
    *offset += 2 * sizeof(int);

    // return success
    if (localrc) *localrc = ESMF_SUCCESS;

    return;
} 


  void FTN(c_esmc_geombasedeserialize)(int *geomtype, int * staggerloc,
	       void *buffer, int *offset, int *localrc){

    int *ip;

    // Initialize return code; assume routine not implemented
    if (localrc) *localrc = ESMC_RC_NOT_IMPL;

    // Get data out
    ip = (int *)((char *)(buffer) + *offset);
    *geomtype = *ip++;
    *staggerloc = *ip++; 

    // Adjust offset
    *offset += 2 * sizeof(int);

    if (localrc) *localrc = ESMF_SUCCESS;

    return;
} 


}
