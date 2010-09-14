// $Id: ESMCI_XGrid_F.C,v 1.4 2010/09/14 21:34:48 feiliu Exp $
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
             "$Id: ESMCI_XGrid_F.C,v 1.4 2010/09/14 21:34:48 feiliu Exp $";
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
                int * s, int * cellCount, int * dimCount, 
                int * ngridA, int * ngridB,
                int * eleCountA2X, int * eleCountX2A, int * eleCountB2X, int * eleCountX2B, 
                double * area, double * centroid, 
                char *buffer, int *length, int *offset,
                ESMC_InquireFlag *inquireflag, int *localrc,
                ESMCI_FortranStrLenArg buf_l){

    ESMC_InquireFlag linquireflag = *inquireflag;
    int i, padding;
 
    // Initialize return code; assume routine not implemented
    if (localrc) *localrc = ESMC_RC_NOT_IMPL;

    char * ptr = (char *)(buffer + *offset);

#define SSIZE 10
    if (linquireflag != ESMF_INQUIREONLY)
      memcpy((void *)ptr, (const void *)s, SSIZE*sizeof(int));
    ptr += SSIZE*sizeof(int);
#undef SSIZE
    if (linquireflag != ESMF_INQUIREONLY)
      memcpy((void *)ptr, (const void *)ngridA, sizeof(int));
    ptr += sizeof(int);
    if (linquireflag != ESMF_INQUIREONLY)
      memcpy((void *)ptr, (const void *)ngridB, sizeof(int));
    ptr += sizeof(int);
    if (linquireflag != ESMF_INQUIREONLY)
      memcpy((void *)ptr, (const void *)cellCount, sizeof(int));
    ptr += sizeof(int);
    if (linquireflag != ESMF_INQUIREONLY)
      memcpy((void *)ptr, (const void *)dimCount, sizeof(int));
    ptr += sizeof(int);

    // sparseMat meta data
    if(eleCountA2X != 0) {
        if (linquireflag != ESMF_INQUIREONLY)
          memcpy((void *)ptr, (const void *)eleCountA2X, *ngridA*sizeof(int));
        ptr += *ngridA*sizeof(int);
    }
    if(eleCountX2A != 0) {
        if (linquireflag != ESMF_INQUIREONLY)
          memcpy((void *)ptr, (const void *)eleCountX2A, *ngridA*sizeof(int));
        ptr += *ngridA*sizeof(int);
    }
    if(eleCountB2X != 0) {
        if (linquireflag != ESMF_INQUIREONLY)
          memcpy((void *)ptr, (const void *)eleCountB2X, *ngridB*sizeof(int));
        ptr += *ngridB*sizeof(int);
    }
    if(eleCountX2B != 0) {
        if (linquireflag != ESMF_INQUIREONLY)
          memcpy((void *)ptr, (const void *)eleCountX2B, *ngridB*sizeof(int));
        ptr += *ngridB*sizeof(int);
    }

#define AREA_IDX 4
#define CENTROID_IDX 5
    // realign
    *offset = ptr - buffer;
    padding = (*offset)%8;
    if(padding) (*offset) += 8-padding;
    ptr = (char *)(buffer + *offset);
    if(s[AREA_IDX]){
       if (linquireflag != ESMF_INQUIREONLY)
          memcpy(reinterpret_cast<void *>(ptr), reinterpret_cast<const void *>(area), *cellCount*sizeof(double));
       ptr += *cellCount*sizeof(double);
    }

    *offset = ptr - buffer;
    padding = (*offset)%8;
    if(padding) (*offset) += 8-padding;
    ptr = (char *)(buffer + *offset);
    if(s[CENTROID_IDX]){
       if (linquireflag != ESMF_INQUIREONLY)
          memcpy((void *)ptr, (const void *)centroid, *cellCount*(*dimCount)*sizeof(double));
       ptr += *cellCount*(*dimCount)*sizeof(double);
    }
#undef AREA_IDX 
#undef CENTROID_IDX

    // realign again
    *offset = ptr - buffer;
    padding = (*offset)%8;
    if(padding) (*offset) += 8-padding;

    if (localrc) *localrc = ESMF_SUCCESS;

    return;
} 


void FTN(c_esmc_xgriddeserialize)(
                int * s, int * cellCount, int * dimCount, 
                int * ngridA, int * ngridB,
                int * eleCountA2X, int * eleCountX2A, int * eleCountB2X, int * eleCountX2B, 
                double * area, double * centroid, int * step, 
                char *buffer, int *offset, int *localrc,
                ESMCI_FortranStrLenArg buffer_l){

    int i, padding;

    // Initialize return code; assume routine not implemented
    if (localrc) *localrc = ESMC_RC_NOT_IMPL;

    if(*step != 1 && *step != 2) {
        *localrc = ESMC_RC_ARG_BAD;
        return;
    }

    char * ptr = (char *)(buffer + *offset);
    if(*step == 1){
#define SSIZE 10
        memcpy((void *)s, (const void *)ptr, SSIZE*sizeof(int));
        ptr += SSIZE*sizeof(int);
#undef SSIZE
        memcpy((void *)ngridA, (const void *)ptr, sizeof(int));
        ptr += sizeof(int);
        memcpy((void *)ngridB, (const void *)ptr, sizeof(int));
        ptr += sizeof(int);
        memcpy((void *)cellCount, (const void *)ptr, sizeof(int));
        ptr += sizeof(int);
        memcpy((void *)dimCount, (const void *)ptr, sizeof(int));
        ptr += sizeof(int);
    }

    if(*step == 2){
        // sparseMat meta data
        if(eleCountA2X != 0) {
            memcpy((void *)eleCountA2X, (const void *)ptr, *ngridA*sizeof(int));
            ptr += *ngridA*sizeof(int);
        }
        if(eleCountX2A != 0) {
            memcpy((void *)eleCountX2A, (const void *)ptr, *ngridA*sizeof(int));
            ptr += *ngridA*sizeof(int);
        }
        if(eleCountB2X != 0) {
            memcpy((void *)eleCountB2X, (const void *)ptr, *ngridB*sizeof(int));
            ptr += *ngridB*sizeof(int);
        }
        if(eleCountX2B != 0) {
            memcpy((void *)eleCountX2B, (const void *)ptr, *ngridB*sizeof(int));
            ptr += *ngridB*sizeof(int);
        }

#define AREA_IDX 4
#define CENTROID_IDX 5
        // realign
        *offset = ptr - buffer;
        padding = (*offset)%8;
        if(padding) (*offset) += 8-padding;
        ptr = (char *)(buffer + *offset);
        if(s[AREA_IDX] && area != 0){
           memcpy((void *)area, (const void *)ptr, *cellCount*sizeof(double));
           ptr += *cellCount*sizeof(double);
        }

        *offset = ptr - buffer;
        padding = (*offset)%8;
        if(padding) (*offset) += 8-padding;
        ptr = (char *)(buffer + *offset);
        if(s[CENTROID_IDX] && centroid != 0){
           memcpy((void *)centroid, (const void *)ptr, *cellCount*(*dimCount)*sizeof(double));
           ptr += *cellCount*(*dimCount)*sizeof(double);
        }
#undef AREA_IDX 
#undef CENTROID_IDX
    }

    // realign again
    *offset = ptr - buffer;
    padding = (*offset)%8;
    if(padding) (*offset) += 8-padding;

    if (localrc) *localrc = ESMF_SUCCESS;

    return;
} 

// non-method functions
void FTN(c_esmc_smmspecserialize)(
                int * cellCount, 
                int * indices, double * weights, 
                char *buffer, int *length, int *offset,
                ESMC_InquireFlag *inquireflag, int *localrc,
                ESMCI_FortranStrLenArg buf_l){

    ESMC_InquireFlag linquireflag = *inquireflag;
    int i, padding;
 
    // Initialize return code; assume routine not implemented
    if (localrc) *localrc = ESMC_RC_NOT_IMPL;

    char * ptr = (char *)(buffer + *offset);

    if (linquireflag != ESMF_INQUIREONLY)
      memcpy((void *)ptr, (const void *)cellCount, sizeof(int));
    ptr += sizeof(int);

    // realign
    *offset = ptr - buffer;
    padding = (*offset)%8;
    if(padding) (*offset) += 8-padding;
    ptr = (char *)(buffer + *offset);
    if (linquireflag != ESMF_INQUIREONLY)
       memcpy(reinterpret_cast<void *>(ptr), reinterpret_cast<const void *>(indices), *cellCount*2*sizeof(int));
    ptr += *cellCount*2*sizeof(int);

    *offset = ptr - buffer;
    padding = (*offset)%8;
    if(padding) (*offset) += 8-padding;
    ptr = (char *)(buffer + *offset);
    if (linquireflag != ESMF_INQUIREONLY)
       memcpy((void *)ptr, (const void *)weights, *cellCount*sizeof(double));
    ptr += *cellCount*sizeof(double);

    // realign again
    *offset = ptr - buffer;
    padding = (*offset)%8;
    if(padding) (*offset) += 8-padding;

    if (localrc) *localrc = ESMF_SUCCESS;

    return;
} 

// non-method functions
void FTN(c_esmc_smmspecdeserialize)(
                int * cellCount, 
                int * indices, double * weights, 
                char *buffer, int *offset,
                int *localrc,
                ESMCI_FortranStrLenArg buf_l){

    int i, padding;
 
    // Initialize return code; assume routine not implemented
    if (localrc) *localrc = ESMC_RC_NOT_IMPL;

    char * ptr = (char *)(buffer + *offset);

    // realign
    *offset = ptr - buffer;
    padding = (*offset)%8;
    if(padding) (*offset) += 8-padding;
    ptr = (char *)(buffer + *offset);

    memcpy((void  *)indices, (const void *)ptr, *cellCount*2*sizeof(int));
    ptr += *cellCount*2*sizeof(int);

    *offset = ptr - buffer;
    padding = (*offset)%8;
    if(padding) (*offset) += 8-padding;
    ptr = (char *)(buffer + *offset);

    memcpy((void *)weights, (const void *)ptr, *cellCount*sizeof(double));
    ptr += *cellCount*sizeof(double);

    // realign again
    *offset = ptr - buffer;
    padding = (*offset)%8;
    if(padding) (*offset) += 8-padding;

    if (localrc) *localrc = ESMF_SUCCESS;

    return;
} 

}
