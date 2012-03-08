// $Id: ESMC_Grid.C,v 1.5 2012/03/08 18:53:44 rokuingh Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2012, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMC_Grid.C"
//==============================================================================
//
//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
// This file contains the C interfaces' code for the {\tt Grid} class functions.
//
//EOP
//------------------------------------------------------------------------------
// INCLUDES
#include "ESMC_Grid.h"

#include "ESMCI_Grid.h"
#include "ESMCI_LogErr.h"                  // for LogErr
#include "ESMF_LogMacros.inc"             // for LogErr
#include "ESMCI_VM.h"

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_Grid.C,v 1.5 2012/03/08 18:53:44 rokuingh Exp $";
//-----------------------------------------------------------------------------

using namespace ESMCI;

extern "C" {

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_GridCreateNoPeriDim()"
ESMC_Grid ESMC_GridCreateNoPeriDim(ESMC_InterfaceInt maxIndex,
                                   enum ESMC_CoordSys coordSys,
                                   enum ESMC_TypeKind coordTypeKind, 
                                   int *rc){
  // Initialize return code. Assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;
  if(rc!=NULL) *rc=ESMC_RC_NOT_IMPL;

  // Init Grid
  ESMC_Grid grid;
  grid.ptr = NULL;
  
  grid.ptr = reinterpret_cast<ESMCI::Grid *>(ESMCI::Grid::createnoperidim(maxIndex,
                                      coordSys, coordTypeKind, &localrc));
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, rc))
    return grid; // bail out

#if 0
  ESMCI::Grid *gridp = reinterpret_cast<ESMCI::Grid *>(grid.ptr);
  printf("\n\nnoperidim gridstatus = %d\n\n", gridp->getStatus());
  printf("\n\nnoperidim gridname   = %s\n\n", gridp->getName());
#endif

  // return successfully
  if (rc) *rc = ESMF_SUCCESS;
  return grid;
}
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_GridCreate1PeriDim()"
ESMC_Grid ESMC_GridCreate1PeriDim(ESMC_InterfaceInt maxIndex,
                                   enum ESMC_CoordSys coordSys,
                                   enum ESMC_TypeKind coordTypeKind, 
                                   int *rc){
  int localrc = ESMC_RC_NOT_IMPL;
  if(rc!=NULL) *rc=ESMC_RC_NOT_IMPL;

  // Init Grid
  ESMC_Grid grid;
  grid.ptr = NULL;
  
  grid.ptr = reinterpret_cast<void *>(ESMCI::Grid::create1peridim(maxIndex,
                                      coordSys, coordTypeKind, &localrc));
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, rc))
    return grid; // bail out

  // return successfully
  if (rc) *rc = ESMF_SUCCESS;
  return grid;
}
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_GridDestroy()"
int ESMC_GridDestroy(ESMC_Grid *grid){
  
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  ESMCI::Grid *gridp = reinterpret_cast<ESMCI::Grid *>(grid->ptr);

  // Do destroy
  localrc=ESMCI::Grid::destroy(&gridp);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &rc))
    return rc;  // bail out    

  // Set to NULL
  grid->ptr=NULL;

  // return successfully
  return ESMF_SUCCESS;
}
//--------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_GridAddCoord()"
int ESMC_GridAddCoord(ESMC_Grid grid, enum ESMC_StaggerLoc staggerloc){
  // Initialize return code. Assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;
  int rc=ESMC_RC_NOT_IMPL;

  // convert the ESMC_Grid to an ESMCI::Grid
  ESMCI::Grid *gridp = reinterpret_cast<ESMCI::Grid *>(grid.ptr);

  // convert the staggerloc enum to an int
  int stagger = static_cast<int>(staggerloc);

  // add coords
  localrc=gridp->ESMCI::Grid::addCoordArray(&stagger, NULL, NULL, NULL, NULL);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &rc))
    return rc; // bail out

  // return successfully
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_GridGetCoord()"
void * ESMC_GridGetCoord(ESMC_Grid grid, int coordDim, 
                      enum ESMC_StaggerLoc staggerloc, 
                      int *exclusiveLBound,
                      int *exclusiveUBound, int *rc){

  // Initialize return code. Assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;

  // convert the ESMC_Grid to an ESMCI::Grid
  ESMCI::Grid *gridp = reinterpret_cast<ESMCI::Grid *>(grid.ptr);
  //printf("\n\ngridstatus = %d\n\n", gridp->getStatus());

  // convert the staggerloc enum to an int
  int stagger = static_cast<int>(staggerloc);

  // get coord array
  ESMCI::Array *coordArray; 
  coordArray = ((gridp)->getCoordArray(&stagger, 
                                       coordDim, NULL, &localrc));
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, rc))
    return NULL; // bail out

  // create an ESMF_Array and cast into pointer access
  ESMC_Array arrayPtr;
  arrayPtr.ptr = reinterpret_cast<void *>(coordArray);

  // get the Array pointer to return
  void *coordPtr = ESMC_ArrayGetPtr(arrayPtr, 0, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, rc))
    return NULL; // bail out

  // get the bounds
  if(exclusiveLBound && exclusiveUBound) {
    int localDe = 0;
    localrc = gridp->getExclusiveLBound(stagger, localDe, exclusiveLBound);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, rc))
      return NULL; // bail out
    localrc = gridp->getExclusiveUBound(stagger, localDe, exclusiveUBound);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, rc))
      return NULL; // bail out
  }

  // this code is technically more correct, but I can't find the symbol for some reason..
#if 0
  int localDe = 0;
  c_esmc_gridgetcoordbounds(&gridp, &localDe, &coordDim, &stagger,
                            NULL, NULL, NULL,
                            &computationalLBound, &computationalUBound,
                            NULL, NULL, NULL, NULL, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, rc))
    return NULL; // bail out
#endif

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return coordPtr;
}
//-----------------------------------------------------------------------------

} // extern "C"
// $Id: ESMC_Grid.C,v 1.5 2012/03/08 18:53:44 rokuingh Exp $
