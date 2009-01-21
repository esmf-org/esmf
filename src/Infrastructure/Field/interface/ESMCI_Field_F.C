// $Id: ESMCI_Field_F.C,v 1.1.2.4 2009/01/21 21:25:20 cdeluca Exp $
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
//
//-----------------------------------------------------------------------------
//
// insert any higher level, 3rd party or system includes here

#include <cstring>
using namespace std;

#include "ESMC_Start.h"
#include "ESMC_LogErr.h"


//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = 
             "$Id: ESMCI_Field_F.C,v 1.1.2.4 2009/01/21 21:25:20 cdeluca Exp $";
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
                int * staggerloc,
                int * gridToFieldMap,
                int * ungriddedLBound,
                int * ungriddedUBound,
                int * maxHaloLWidth,
                int * maxHaloUWidth,
				void *buffer, int *length, int *offset, int *localrc){
    int i;


    // Initialize return code; assume routine not implemented
    if (localrc) *localrc = ESMC_RC_NOT_IMPL;
    // either put the code here, or call into a real C++ function
    ESMC_Status *sp;

    // TODO: verify length > 4 status vars, and if not, make room.
    int fixedpart = 4 * sizeof(int *) + sizeof(int) + 5 * ESMF_MAXDIM * sizeof(int);
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

    // copy the rest of the field parameters
    // we are explicitly assuming Fortran-integer is of size C-int
    // This might be probelmatic on 64bit machines depending how compiler flag is used
    // e.g. we know gridToFieldMap is of type cpu_word *, its element maybe int32 or int64
    // depending on the size of Fortran-integer
    char * ptr = (char *)sp;
    memcpy((void *)ptr, (const void *)staggerloc, sizeof(int));
    ptr += sizeof(int);
    memcpy((void *)ptr, (const void *)gridToFieldMap, ESMF_MAXDIM*sizeof(int));
    ptr += ESMF_MAXDIM*sizeof(int);
    memcpy((void *)ptr, (const void *)ungriddedLBound, ESMF_MAXDIM*sizeof(int));
    ptr += ESMF_MAXDIM*sizeof(int);
    memcpy((void *)ptr, (const void *)ungriddedUBound, ESMF_MAXDIM*sizeof(int));
    ptr += ESMF_MAXDIM*sizeof(int);
    memcpy((void *)ptr, (const void *)maxHaloLWidth, ESMF_MAXDIM*sizeof(int));
    ptr += ESMF_MAXDIM*sizeof(int);
    memcpy((void *)ptr, (const void *)maxHaloUWidth, ESMF_MAXDIM*sizeof(int));
    ptr += ESMF_MAXDIM*sizeof(int);

    *offset = ptr - (char *)buffer;

    if (localrc) *localrc = ESMF_SUCCESS;

    return;
} 


void FTN(c_esmc_fielddeserialize)(ESMC_Status *fieldstatus, 
                ESMC_Status *gridstatus, 
                ESMC_Status *datastatus, 
                ESMC_Status *iostatus, 
                int * staggerloc,
                int * gridToFieldMap,
                int * ungriddedLBound,
                int * ungriddedUBound,
                int * maxHaloLWidth,
                int * maxHaloUWidth,
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

    char * ptr = (char *)sp;
    memcpy((void *)staggerloc, (const void *)ptr, sizeof(int));
    ptr += sizeof(int);
    memcpy((void *)gridToFieldMap, (const void *)ptr, ESMF_MAXDIM*sizeof(int));
    ptr += ESMF_MAXDIM*sizeof(int);
    memcpy((void *)ungriddedLBound, (const void *)ptr, ESMF_MAXDIM*sizeof(int));
    ptr += ESMF_MAXDIM*sizeof(int);
    memcpy((void *)ungriddedUBound, (const void *)ptr, ESMF_MAXDIM*sizeof(int));
    ptr += ESMF_MAXDIM*sizeof(int);
    memcpy((void *)maxHaloLWidth, (const void *)ptr, ESMF_MAXDIM*sizeof(int));
    ptr += ESMF_MAXDIM*sizeof(int);
    memcpy((void *)maxHaloUWidth, (const void *)ptr, ESMF_MAXDIM*sizeof(int));
    ptr += ESMF_MAXDIM*sizeof(int);

    *offset = ptr - (char *)buffer;

    if (localrc) *localrc = ESMF_SUCCESS;

    return;
} 


}
