// $Id: ESMCI_FieldBundle_F.C,v 1.6.2.1 2010/02/05 19:56:19 svasquez Exp $
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
// The code in this file implements the C serialize and deserialize methods
// 
//
//-----------------------------------------------------------------------------
//
 // insert any higher level, 3rd party or system includes here
#include "ESMC_Start.h"
#include "ESMCI_LogErr.h"

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = 
             "$Id: ESMCI_FieldBundle_F.C,v 1.6.2.1 2010/02/05 19:56:19 svasquez Exp $";
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
void FTN(c_esmc_fieldbundleserialize)(
                            ESMC_Status *gridstatus,
                            ESMC_Status *iostatus,
                            int *field_count,
                            int *pack_flag,
                            int *isCongruent,
                            int *hasPattern,
                            void *buffer, int *length, int *offset,
                            ESMC_InquireFlag *inquireflag, int *localrc){

    // either put the code here, or call into a real C++ function
    ESMC_Status *sp;
    int *ip;

    // TODO: verify length > need, and if not, make room.
    int fixedpart = 8 * sizeof(int *);
    if (*inquireflag != ESMF_INQUIREONLY) {
      if ((*length - *offset) < fixedpart) {
         
         ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
                            "Buffer too short to add a FieldBundle object", localrc);
         return;
 
        //buffer = (char *)realloc((void *)buffer,
        //                         *length + 2*fixedpart + byte_count);
        //*length += 2 * fixedpart;
      }
    }


    sp = (ESMC_Status *)((char *)(buffer) + *offset);
    if (*inquireflag != ESMF_INQUIREONLY) {
      *sp++ = *gridstatus; 
      *sp++ = *iostatus; 
    } else
      sp += 2;

    ip = (int *)sp;
    if (*inquireflag != ESMF_INQUIREONLY) {
      *ip++ = *field_count; 
      *ip++ = *pack_flag; 
      *ip++ = *isCongruent; 
      *ip++ = *hasPattern; 
    } else
      ip += 4;

    *offset = (char *)ip - (char *)buffer;

    if (localrc) *localrc = ESMF_SUCCESS;
    return;
} 


// non-method functions
void FTN(c_esmc_fieldbundledeserialize)( 
                              ESMC_Status *gridstatus, 
                              ESMC_Status *iostatus, 
                              int *field_count, 
                              int *pack_flag, 
                              int *isCongruent,
                              int *hasPattern,
                              void *buffer, int *offset, int *localrc){

    // either put the code here, or call into a real C++ function
    ESMC_Status *sp;
    int *ip;

    sp = (ESMC_Status *)((char *)(buffer) + *offset);
    *gridstatus = *sp++;
    *iostatus = *sp++;
    ip = (int *)sp;
    *field_count = *ip++;
    *pack_flag = *ip++;
    *isCongruent = *ip++;
    *hasPattern = *ip++;

    *offset = (char *)ip - (char *)buffer;

    if (localrc) *localrc = ESMF_SUCCESS;
    return;
} 


}
