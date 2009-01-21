// $Id: ESMCI_FieldBundle_F.C,v 1.1.2.3 2009/01/21 21:25:20 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
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
#include "ESMC_LogErr.h"

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = 
             "$Id: ESMCI_FieldBundle_F.C,v 1.1.2.3 2009/01/21 21:25:20 cdeluca Exp $";
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
void FTN(c_esmc_fieldbundleserialize)(ESMC_Status *bundlestatus,
                            ESMC_Status *gridstatus,
                            ESMC_Status *iostatus,
                            int *field_count,
                            int *pack_flag,
                            int *isCongruent,
                            int *hasPattern,
                            void *buffer, int *length, int *offset, int *localrc){

    // either put the code here, or call into a real C++ function
    ESMC_Status *sp;
    int *ip;

    // TODO: verify length > need, and if not, make room.
    int fixedpart = 8 * sizeof(int *);
    if ((*length - *offset) < fixedpart) {
         
         ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                            "Buffer too short to add a FieldBundle object", localrc);
         return;
 
        //buffer = (char *)realloc((void *)buffer,
        //                         *length + 2*fixedpart + byte_count);
        //*length += 2 * fixedpart;
    }


    sp = (ESMC_Status *)((char *)(buffer) + *offset);
    *sp++ = *bundlestatus;
    *sp++ = *gridstatus; 
    *sp++ = *iostatus; 
    ip = (int *)sp;
    *ip++ = *field_count; 
    *ip++ = *pack_flag; 
    *ip++ = *isCongruent; 
    *ip++ = *hasPattern; 

    *offset = (char *)ip - (char *)buffer;

    if (localrc) *localrc = ESMF_SUCCESS;
    return;
} 


// non-method functions
void FTN(c_esmc_fieldbundledeserialize)(ESMC_Status *bundlestatus, 
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
    *bundlestatus = *sp++;
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
