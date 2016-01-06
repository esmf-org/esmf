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
// The code in this file implements the C++ {\tt ESMC\_State} methods declared
// in the companion file ESMC_State.h
//
// 
//
//-----------------------------------------------------------------------------
 // associated class definition file
#include "ESMCI_State.h"

 // insert any higher level, 3rd party or system includes here
#include <string>
#include "ESMCI_Macros.h"
#include "ESMCI_LogErr.h"

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = 
             "$Id$";
//-----------------------------------------------------------------------------

namespace ESMCI {

extern "C" {
//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the State routines
//
//

// non-method functions
void FTN_X(c_esmc_stateserialize)(
                           int *st, 
                           int *datacount, 
                           char *buffer, int *length, int *offset,
                           ESMC_InquireFlag *inquireflag, int *localrc,
                           ESMCI_FortranStrLenArg buffer_l){

#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_stateserialize()"

    int *ip;

    if (localrc) *localrc = ESMC_RC_NOT_IMPL;

    // TODO: verify length > needed, else realloc longer
    int fixedpart = 10 * sizeof (int *);
    if ((*inquireflag != ESMF_INQUIREONLY) && (*length - *offset) < fixedpart) {
         
         ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
           "Buffer too short to add a State object", ESMC_CONTEXT, localrc);
         return;
 
        //buffer = (char *)realloc((void *)buffer,
        //                         *length + 2*fixedpart + byte_count);
        //*length += 2 * fixedpart;
    }


    ip = (int *)(buffer + *offset);
    if (*inquireflag != ESMF_INQUIREONLY) {
      *ip++ = *st; 
      *ip++ = *datacount; 
    } else
      ip += 2;

    *offset = (char *)ip - buffer;

    if (localrc) *localrc = ESMF_SUCCESS;

    return;
} 


void FTN_X(c_esmc_statedeserialize)(
                             int *st, 
                             int *datacount, 
                             char *buffer, int *offset, int *localrc,
                             ESMCI_FortranStrLenArg buffer_l){

#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_statedeserialize()"

    int *ip;

    if (localrc) *localrc = ESMC_RC_NOT_IMPL;

    ip = (int *)(buffer + *offset);
    *st = *ip++; 
    *datacount = *ip++; 

    *offset = (char *)ip - buffer;

    if (localrc) *localrc = ESMF_SUCCESS;

    return;
} 

#if 0
      type ESMF_DataHolder
          type(ESMF_FieldBundle) :: bp
          type(ESMF_Field)  :: fp
          type(ESMF_Array)  :: ap
          type(ESMF_StateClass), pointer  :: spp

      type ESMF_StateItem
        type(ESMF_StateItemType) :: otype
        character(len=ESMF_MAXSTR) :: namep
        type(ESMF_DataHolder), pointer :: datap
        integer :: indirect_index
        type(ESMF_NeededFlag) :: needed
        type(ESMF_ReadyFlag) :: ready
        type(ESMF_ValidFlag) :: valid
        type(ESMF_ReqForRestartFlag) :: reqrestart

#endif

void FTN_X(c_esmc_stateitemserialize)(int *otype, 
                               char *namep, 
                               char *buffer, int *length, int *offset,
                               ESMC_InquireFlag *inquireflag, int *localrc,
                               ESMCI_FortranStrLenArg clen,
                               ESMCI_FortranStrLenArg buffer_l) {

    int *ip;
    char *cp;

    if (localrc) *localrc = ESMC_RC_NOT_IMPL;

    // TODO: verify length > needed, else realloc longer

    ip = (int *)(buffer + *offset);
    if (*inquireflag != ESMF_INQUIREONLY)
      *ip = *otype;
    ip++;

    cp = (char *)ip;
    if (*inquireflag != ESMF_INQUIREONLY)
      memcpy(cp, namep, clen);
    cp += clen;

    *offset = cp - buffer;

    if (localrc) *localrc = ESMF_SUCCESS;

    return;
} 


void FTN_X(c_esmc_stateitemdeserialize)(int *otype, 
                               char *namep, 
                               char *buffer, int *offset, int *localrc,
                               ESMCI_FortranStrLenArg clen,
                               ESMCI_FortranStrLenArg buffer_l) {

#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_stateitemdeserialize()"

    int *ip;
    char *cp;

    if (localrc) *localrc = ESMC_RC_NOT_IMPL;

    ip = (int *)(buffer + *offset);
    *otype = *ip++;

    cp = (char *)ip;
    memcpy(namep, cp, clen);
    cp += clen;

    *offset = cp - buffer;

    if (localrc) *localrc = ESMF_SUCCESS;

    return;
} 

//-----------------------------------------------------------------------------

void FTN_X(c_esmc_stateread)(State *ptr,
                           ESMC_Base **base,
                           const char *fileName,
                           int *status,
                           ESMCI_FortranStrLenArg fileName_l) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_stateread()"

         ESMF_CHECK_POINTER(ptr, status)

         std::string fn = std::string (fileName, fileName_l);

         // Read the items and attributes into the state object.
         int rc = (ptr)->State::read(*base, // always present
                          fn);              // always present

         if (ESMC_PRESENT(status)) *status = rc;
}

//-----------------------------------------------------------------------------

void FTN_X(c_esmc_statewrite)(State *ptr,
                           ESMC_Base **base,
                           const char *fileName,
                           int *status,
                           ESMCI_FortranStrLenArg fileName_l) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_statewrite()"

         ESMF_CHECK_POINTER(ptr, status)

         std::string fn = std::string (fileName, fileName_l);

         // Read the items and attributes into the state object.
         int rc = (ptr)->State::write(*base, // always present
                          fn);               // always present

         if (ESMC_PRESENT(status)) *status = rc;
}

//-----------------------------------------------------------------------------

} // extern "C"

} // namespace ESMCI
