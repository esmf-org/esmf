// $Id: ESMC_Field_F.C,v 1.7 2007/06/22 23:21:29 cdeluca Exp $
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
// The code in this file implements the C++ {\tt ESMC\_Field} methods declared
// in the companion file ESMC_Field.h
//
// 
//
//-----------------------------------------------------------------------------
//
 // insert any higher level, 3rd party or system includes here
#include "ESMC_Start.h"
#include "ESMC_LogErr.h"

 // associated class definition file
#include "ESMC_Field.h"

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = 
             "$Id: ESMC_Field_F.C,v 1.7 2007/06/22 23:21:29 cdeluca Exp $";
//-----------------------------------------------------------------------------

extern "C" {
//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the Field routines
//
//

#if 0
     type ESMF_FieldType
        type (ESMF_Base) :: base             ! base class object
        type (ESMF_Status) :: fieldstatus
        type (ESMF_Status) :: interngridstatus
        type (ESMF_Status) :: datastatus
        type (ESMF_Status) :: datamapstatus
        type (ESMF_InternGrid) :: interngrid             ! save to satisfy query routines
        type (ESMF_LocalField) :: localfield ! this differs per DE
        type (ESMF_FieldDataMap) :: mapping  ! mapping of array indices to interngrid
        type (ESMF_IOSpec) :: iospec         ! iospec values
        type (ESMF_Status) :: iostatus       ! if unset, inherit from gcomp
#endif

// non-method functions
void FTN(c_esmc_fieldserialize)(ESMC_Status *fieldstatus, 
                           ESMC_Status *interngridstatus, 
                           ESMC_Status *datastatus, 
                           ESMC_Status *datamapstatus, 
                           ESMC_Status *iostatus, 
                           void *buffer, int *length, int *offset, int *localrc){

    // Initialize return code; assume routine not implemented
    if (localrc) *localrc = ESMC_RC_NOT_IMPL;
    // either put the code here, or call into a real C++ function
    ESMC_Status *sp;

    // TODO: verify length > 5 status vars, and if not, make room.
    int fixedpart = 5 * sizeof(int *);
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
    *sp++ = *interngridstatus; 
    *sp++ = *datastatus; 
    *sp++ = *datamapstatus; 
    *sp++ = *iostatus; 

    *offset = (char *)sp - (char *)buffer;

    if (localrc) *localrc = ESMF_SUCCESS;

    return;
} 


void FTN(c_esmc_fielddeserialize)(ESMC_Status *fieldstatus, 
                             ESMC_Status *interngridstatus, 
                             ESMC_Status *datastatus, 
                             ESMC_Status *datamapstatus, 
                             ESMC_Status *iostatus, 
                             void *buffer, int *offset, int *localrc){

    // Initialize return code; assume routine not implemented
    if (localrc) *localrc = ESMC_RC_NOT_IMPL;
    // either put the code here, or call into a real C++ function
    ESMC_Status *sp;

    sp = (ESMC_Status *)((char *)(buffer) + *offset);
    *fieldstatus = *sp++;
    *interngridstatus = *sp++;
    *datastatus = *sp++;
    *datamapstatus = *sp++;
    *iostatus = *sp++;

    *offset = (char *)sp - (char *)buffer;

    if (localrc) *localrc = ESMF_SUCCESS;

    return;
} 


}
