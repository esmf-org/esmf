// $Id: ESMCI_Field_F.C,v 1.3 2008/04/05 03:38:16 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2008, University Corporation for Atmospheric Research, 
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
#include "ESMC_Start.h"
#include "ESMC_LogErr.h"


//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = 
             "$Id: ESMCI_Field_F.C,v 1.3 2008/04/05 03:38:16 cdeluca Exp $";
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
void FTN(c_esmc_fieldserialize)(ESMC_Status *fieldstatus, 
				ESMC_Status *gridstatus, 
				ESMC_Status *datastatus, 
				ESMC_Status *iostatus,
				void *buffer, int *length, int *offset, int *localrc){
    int i;


    // Initialize return code; assume routine not implemented
    if (localrc) *localrc = ESMC_RC_NOT_IMPL;
    // either put the code here, or call into a real C++ function
    ESMC_Status *sp;

    // TODO: verify length > 4 status vars, and if not, make room.
    int fixedpart = 4 * sizeof(int *);
    if ((*length - *offset) < fixedpart) {
         
         ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                            "Buffer too short to add a Field object", localrc);
         return;
 
        //buffer = (char *)realloc((void *)buffer,
        //                         *length + 2*fixedpart + byte_count);
        //*length += 2 * fixedpart;
    }


    sp = (ESMC_Status *)((char *)(buffer) + *offset);
    *sp++ = *fieldstatus;
    *sp++ = *gridstatus; 
    *sp++ = *datastatus; 
    *sp++ = *iostatus; 

    *offset = (char *)sp - (char *)buffer;

    if (localrc) *localrc = ESMF_SUCCESS;

    return;
} 


void FTN(c_esmc_fielddeserialize)(ESMC_Status *fieldstatus, 
				  ESMC_Status *gridstatus, 
				  ESMC_Status *datastatus, 
				  ESMC_Status *iostatus, 
				  void *buffer, int *offset, int *localrc){

    int i;

    // Initialize return code; assume routine not implemented
    if (localrc) *localrc = ESMC_RC_NOT_IMPL;
    // either put the code here, or call into a real C++ function
    ESMC_Status *sp;

    sp = (ESMC_Status *)((char *)(buffer) + *offset);
    *fieldstatus = *sp++;
    *gridstatus = *sp++;
    *datastatus = *sp++;
    *iostatus = *sp++;

    *offset = (char *)sp - (char *)buffer;

    if (localrc) *localrc = ESMF_SUCCESS;

    return;
} 


}
