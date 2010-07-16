// $Id: ESMCI_XGrid_F.C,v 1.1 2010/07/16 14:15:08 feiliu Exp $
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

#include "ESMC_Macros.h"
#include "ESMCI_LogErr.h"

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = 
             "$Id: ESMCI_XGrid_F.C,v 1.1 2010/07/16 14:15:08 feiliu Exp $";
//-----------------------------------------------------------------------------

extern "C" {
//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the XGrid routines
//
//

// non-method functions
void FTN(c_esmc_xgridserialize)(
                char *buffer, int *length, int *offset,
                ESMC_InquireFlag *inquireflag, int *localrc,
                ESMCI_FortranStrLenArg buf_l){

    ESMC_InquireFlag linquireflag = *inquireflag;
    int i;
 
    // Initialize return code; assume routine not implemented
    if (localrc) *localrc = ESMC_RC_NOT_IMPL;
    // either put the code here, or call into a real C++ function
    ESMC_Status *sp;

    // TODO: verify length > 4 status vars, and if not, make room.
    int fixedpart = 4 * sizeof(int *) + sizeof(int) + 5 * ESMF_MAXDIM * sizeof(int);
    if ((*inquireflag != ESMF_INQUIREONLY) && (*length - *offset) < fixedpart) {
         
         ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                            "Buffer too short to add a XGrid object", localrc);
         return;
 
        //buffer = (char *)realloc((void *)buffer,
        //                         *length + 2*fixedpart + byte_count);
        //*length += 2 * fixedpart;
    }


    sp = (ESMC_Status *)(buffer + *offset);
    char * ptr = (char *)sp;

    *offset = ptr - buffer;

    if (localrc) *localrc = ESMF_SUCCESS;

    return;
} 


void FTN(c_esmc_xgriddeserialize)(
                char *buffer, int *offset, int *localrc,
                ESMCI_FortranStrLenArg buffer_l){

    int i;

    // Initialize return code; assume routine not implemented
    if (localrc) *localrc = ESMC_RC_NOT_IMPL;
    // either put the code here, or call into a real C++ function
    ESMC_Status *sp;

    sp = (ESMC_Status *)(buffer + *offset);
    char * ptr = (char *)sp;

    *offset = ptr - buffer;

    if (localrc) *localrc = ESMF_SUCCESS;

    return;
} 


}
