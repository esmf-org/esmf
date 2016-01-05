// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2016, University Corporation for Atmospheric Research, 
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
// The code in this file implements the C serialize and deserialize methods
// 
//
//-----------------------------------------------------------------------------
//
 // insert any higher level, 3rd party or system includes here
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
// This section includes all the FieldBundle routines
//
//

// non-method functions
void FTN_X(c_esmc_fieldbundleserialize)(
                            int *status,
                            int *field_count,
                            char *buffer, int *length, int *offset,
                            ESMC_InquireFlag *inquireflag, int *localrc,
                            ESMCI_FortranStrLenArg buffer_l){

#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_fieldbundleserialize()"
    // either put the code here, or call into a real C++ function
    ESMC_Status *sp;
    int *ip;

    // TODO: verify length > need, and if not, make room.
    int fixedpart = 8 * sizeof(int *);
    if (*inquireflag != ESMF_INQUIREONLY) {
      if ((*length - *offset) < fixedpart) {
         
         ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
          "Buffer too short to add a FieldBundle object", ESMC_CONTEXT,
          localrc);
         return;
 
        //buffer = (char *)realloc((void *)buffer,
        //                         *length + 2*fixedpart + byte_count);
        //*length += 2 * fixedpart;
      }
    }


    ip = (int *)(buffer + *offset);
    if (*inquireflag != ESMF_INQUIREONLY) {
      *ip++ = *status;
      *ip++ = *field_count; 
    } else
      ip += 2;

    *offset = (char *)ip - buffer;

    if (localrc) *localrc = ESMF_SUCCESS;
    return;
} 


// non-method functions
void FTN_X(c_esmc_fieldbundledeserialize)( 
                              int *status,
                              int *field_count, 
                              char *buffer, int *offset, int *localrc,
                              ESMCI_FortranStrLenArg buffer_l){

#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_fieldbundledeserialize()"
    // either put the code here, or call into a real C++ function
    ESMC_Status *sp;
    int *ip;

    ip = (int *)(buffer + *offset);
    *status = *ip++;
    *field_count = *ip++;

    *offset = (char *)ip - buffer;

    if (localrc) *localrc = ESMF_SUCCESS;
    return;
} 


}
