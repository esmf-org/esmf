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
void FTN_X(c_esmc_fieldserialize)(
                ESMC_Status *status, 
                ESMC_Status *iostatus,
                int * dimCount,
                int * gridToFieldMap,
                int * ungriddedLBound,
                int * ungriddedUBound,
                int * totalLWidth,
                int * totalUWidth,
                char *buffer, int *length, int *offset,
                ESMC_InquireFlag *inquireflag, int *localrc,
                ESMCI_FortranStrLenArg buf_l){

#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_fieldserialize()"
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
          "Buffer too short to add a Field object", ESMC_CONTEXT, localrc);
         return;
 
        //buffer = (char *)realloc((void *)buffer,
        //                         *length + 2*fixedpart + byte_count);
        //*length += 2 * fixedpart;
    }


    sp = (ESMC_Status *)(buffer + *offset);
    if (linquireflag != ESMF_INQUIREONLY) {
      *sp++ = *status; 
      *sp++ = *iostatus; 
    } else
      sp += 2;

    // copy the rest of the field parameters
    // we are explicitly assuming Fortran-integer is of size C-int
    // This might be probelmatic on 64bit machines depending how compiler flag is used
    // e.g. we know gridToFieldMap is of type cpu_word *, its element maybe int32 or int64
    // depending on the size of Fortran-integer
    char * ptr = (char *)sp;
    if (linquireflag != ESMF_INQUIREONLY)
      memcpy((void *)ptr, (const void *)dimCount, sizeof(int));
    ptr += sizeof(int);
    if (linquireflag != ESMF_INQUIREONLY)
      memcpy((void *)ptr, (const void *)gridToFieldMap, ESMF_MAXDIM*sizeof(int));
    ptr += ESMF_MAXDIM*sizeof(int);
    if (linquireflag != ESMF_INQUIREONLY)
      memcpy((void *)ptr, (const void *)ungriddedLBound, ESMF_MAXDIM*sizeof(int));
    ptr += ESMF_MAXDIM*sizeof(int);
    if (linquireflag != ESMF_INQUIREONLY)
      memcpy((void *)ptr, (const void *)ungriddedUBound, ESMF_MAXDIM*sizeof(int));
    ptr += ESMF_MAXDIM*sizeof(int);
    if (linquireflag != ESMF_INQUIREONLY)
      memcpy((void *)ptr, (const void *)totalLWidth, ESMF_MAXDIM*sizeof(int));
    ptr += ESMF_MAXDIM*sizeof(int);
    if (linquireflag != ESMF_INQUIREONLY)
      memcpy((void *)ptr, (const void *)totalUWidth, ESMF_MAXDIM*sizeof(int));
    ptr += ESMF_MAXDIM*sizeof(int);

    *offset = ptr - buffer;

    if (localrc) *localrc = ESMF_SUCCESS;

    return;
} 


void FTN_X(c_esmc_fielddeserialize)(
                ESMC_Status *status, 
                ESMC_Status *iostatus, 
                int * dimCount,
                int * gridToFieldMap,
                int * ungriddedLBound,
                int * ungriddedUBound,
                int * totalLWidth,
                int * totalUWidth,
		char *buffer, int *offset, int *localrc,
                ESMCI_FortranStrLenArg buffer_l){

#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_fielddeserialize()"
    int i;

    // Initialize return code; assume routine not implemented
    if (localrc) *localrc = ESMC_RC_NOT_IMPL;
    // either put the code here, or call into a real C++ function
    ESMC_Status *sp;

    sp = (ESMC_Status *)(buffer + *offset);
    *status = *sp++;
    *iostatus = *sp++;

    char * ptr = (char *)sp;
    memcpy((void *)dimCount, (const void *)ptr, sizeof(int));
    ptr += sizeof(int);
    memcpy((void *)gridToFieldMap, (const void *)ptr, ESMF_MAXDIM*sizeof(int));
    ptr += ESMF_MAXDIM*sizeof(int);
    memcpy((void *)ungriddedLBound, (const void *)ptr, ESMF_MAXDIM*sizeof(int));
    ptr += ESMF_MAXDIM*sizeof(int);
    memcpy((void *)ungriddedUBound, (const void *)ptr, ESMF_MAXDIM*sizeof(int));
    ptr += ESMF_MAXDIM*sizeof(int);
    memcpy((void *)totalLWidth, (const void *)ptr, ESMF_MAXDIM*sizeof(int));
    ptr += ESMF_MAXDIM*sizeof(int);
    memcpy((void *)totalUWidth, (const void *)ptr, ESMF_MAXDIM*sizeof(int));
    ptr += ESMF_MAXDIM*sizeof(int);

    *offset = ptr - buffer;

    if (localrc) *localrc = ESMF_SUCCESS;

    return;
} 


}
