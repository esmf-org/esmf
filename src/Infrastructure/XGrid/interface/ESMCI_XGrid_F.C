// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2018, University Corporation for Atmospheric Research, 
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
#include "ESMCI_VM.h"
#include "ESMC_Util.h"
#include "ESMCI_Array.h"
#include "Mesh/include/ESMCI_Mesh.h"
#include "Mesh/include/ESMCI_MeshCap.h"
#include "Mesh/include/ESMCI_Exception.h"
//#include "Mesh/include/ESMCI_XGridUtil.h"
//#include "Mesh/include/ESMCI_MeshRegrid.h"
#include "Mesh/include/ESMCI_MeshMerge.h"
#include "ESMCI_GridToMesh.h"
#include "ESMCI_Grid.h"

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = 
             "$Id$";
//-----------------------------------------------------------------------------

using namespace ESMCI;

#if 0
  // Defined in Mesh now
namespace ESMCI {
  struct TempWeights {
    int nentries;
    int *iientries;
    double *factors;
  };
}

#endif

extern "C" {
//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the XGrid routines
//
//

// non-method functions
void FTN_X(c_esmc_xgridserialize)(
                int * s, 
                int * ngridA, int * ngridB, int * online, int * flag,
                char *buffer, int *length, int *offset,
                ESMC_InquireFlag *inquireflag, int *localrc,
                ESMCI_FortranStrLenArg buf_l){

    ESMC_InquireFlag linquireflag = *inquireflag;
    int i, padding;
 
    // Initialize return code; assume routine not implemented
    if (localrc) *localrc = ESMC_RC_NOT_IMPL;

    char * ptr = (char *)(buffer + *offset);

#define SSIZE 4
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
      memcpy((void *)ptr, (const void *)online, sizeof(int));
      ptr += sizeof(int);
    if (linquireflag != ESMF_INQUIREONLY)
      memcpy((void *)ptr, (const void *)flag, sizeof(int));
    ptr += sizeof(int);

    // realign again
    *offset = ptr - buffer;
    padding = (*offset)%8;
    if(padding) (*offset) += 8-padding;

    if (localrc) *localrc = ESMF_SUCCESS;

    return;
} 


void FTN_X(c_esmc_xgriddeserialize)(
                int * s, 
                int * ngridA, int * ngridB, int * online, int * flag,
                char *buffer, int *offset, int *localrc,
                ESMCI_FortranStrLenArg buffer_l){

    int i, padding;

    // Initialize return code; assume routine not implemented
    if (localrc) *localrc = ESMC_RC_NOT_IMPL;

    char * ptr = (char *)(buffer + *offset);
#define SSIZE 4
    memcpy((void *)s, (const void *)ptr, SSIZE*sizeof(int));
    ptr += SSIZE*sizeof(int);
#undef SSIZE
    memcpy((void *)ngridA, (const void *)ptr, sizeof(int));
    ptr += sizeof(int);
    memcpy((void *)ngridB, (const void *)ptr, sizeof(int));
    ptr += sizeof(int);
    memcpy((void *)online, (const void *)ptr, sizeof(int));
    ptr += sizeof(int);
    memcpy((void *)flag, (const void *)ptr, sizeof(int));
    ptr += sizeof(int);

    // realign again
    *offset = ptr - buffer;
    padding = (*offset)%8;
    if(padding) (*offset) += 8-padding;

    if (localrc) *localrc = ESMF_SUCCESS;

    return;
  } 

// non-method functions
void FTN_X(c_esmc_smmspecserialize)(
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

    if (localrc) *localrc = ESMF_SUCCESS;

    return;
} 

// non-method functions
void FTN_X(c_esmc_smmspecdeserialize)(
                int * cellCount, 
                int * indices, double * weights, 
                char *buffer, int *offset,
                int *localrc,
                ESMCI_FortranStrLenArg buf_l){

    int i, padding;
 
    // Initialize return code; assume routine not implemented
    if (localrc) *localrc = ESMC_RC_NOT_IMPL;

    char * ptr = (char *)(buffer + *offset);

    if (localrc) *localrc = ESMF_SUCCESS;

    return;
} 

// xgrid regrid create method tailored for XGrid
void FTN_X(c_esmc_xgridregrid_create)(MeshCap **meshsrcpp, MeshCap **meshdstpp,
                   MeshCap **mesh,
                   int *compute_midmesh,
                   int *regridMethod, 
                    int *unmappedaction,
                   int *nentries, ESMCI::TempWeights **tweights,
                   int*rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_xgridregrid_create()" 

  MeshCap::xgridregrid_create(meshsrcpp, meshdstpp, 
                              mesh,
                              compute_midmesh,
                              regridMethod, 
                              unmappedaction,
                              nentries, tweights,
                              rc);
}

void FTN_X(c_esmc_copy_tempweights_xgrid)(ESMCI::TempWeights **_tw, int *ii, double *w) {

  ESMCI::TempWeights &tw = (**_tw);

  for (int i = 0; i < tw.nentries; ++i) {
    int two_i = i << 1;

    ii[two_i] = tw.iientries[two_i+0];
    ii[two_i+1] = tw.iientries[two_i+1];
    w[i] = tw.factors[i];
  }

  if (tw.factors != NULL) delete [] tw.factors;
  if (tw.iientries != NULL) delete [] tw.iientries;

  delete *_tw;

}

// mesh merge
void FTN_X(c_esmc_meshmerge)(MeshCap **srcmeshpp, MeshCap **dstmeshpp,
                   MeshCap **meshpp,
                   int*rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_meshmerge()" 

  *meshpp=MeshCap::merge(srcmeshpp, dstmeshpp,
                           rc);
}

// mesh set fraction
void FTN_X(c_esmc_meshsetfraction)(MeshCap **meshpp, double * fraction, 
                   int*rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_meshsetfraction()" 

  (*meshpp)->meshsetfrac(fraction,rc);

}

// Assumes array is center stagger loc
extern "C" void FTN_X(c_esmc_xgrid_getfrac)(Grid **gridpp,
                   MeshCap **meshpp, ESMCI::Array **arraypp, int *staggerLoc,
                   int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_regrid_getfrac()" 

  (*meshpp)->xgrid_getfrac(gridpp,
                           arraypp, staggerLoc,
                           rc);

}

////////////////////////////////////////////////////////////////////////////
// Assumes array is center stagger loc
extern "C" void FTN_X(c_esmc_xgrid_getfrac2)(Grid **gridpp,
                   MeshCap **meshpp, ESMCI::Array **arraypp, int *staggerLoc,
                   int *rc) {
#undef  ESMC_METHOD
#define ESMC_METHOD "c_esmc_regrid_getfrac2()" 

  (*meshpp)->xgrid_getfrac2(gridpp,
                            arraypp, staggerLoc,
                            rc);
}

} // end extern "C"
