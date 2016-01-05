// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2016, University Corporation for Atmospheric Research,
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
#include "ESMCI_LogErr.h"
#include "ESMCI_VM.h"
#include "ESMC_Array.h"

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------

using namespace ESMCI;

extern "C" {
// fortran interface functions to attribute objects
  void FTN_X(c_esmc_gridgetcoordbounds)(ESMCI::Grid **_grid, int *_localDE,
                                        int *_coord, int *_staggerloc,
                                        ESMCI::InterfaceInt **_exclusiveLBound,
                                        ESMCI::InterfaceInt **_exclusiveUBound,
                                        ESMCI::InterfaceInt **_exclusiveCount,
                                        ESMCI::InterfaceInt **_computationalLBound,
                                        ESMCI::InterfaceInt **_computationalUBound,
                                        ESMCI::InterfaceInt **_computationalCount,
                                        ESMCI::InterfaceInt **_totalLBound,
                                        ESMCI::InterfaceInt **_totalUBound,
                                        ESMCI::InterfaceInt **_totalCount,
                                        int *_rc);

void FTN_X(c_esmc_gridio)(ESMCI::Grid **gridpp, int *staggerLoc, int *num_arrays,
                          char*name, int *rc,
                          ESMCI::Array **arraypp1,
                          ESMCI::Array **arraypp2,
                          ESMCI::Array **arraypp3,
                          ESMCI::Array **arraypp4,
                          ESMCI::Array **arraypp5,
                          ESMCI::Array **arraypp6,
                          int *spherical, int *islatlondeg,
                          ESMCI_FortranStrLenArg nlen);
//-----------------------------------------------------------------------------
//TODO: InterfaceInt should be passed by value when ticket 3613642 is resolved
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_GridCreateNoPeriDim()"
ESMC_Grid ESMC_GridCreateNoPeriDim(ESMC_InterfaceInt *maxIndex,
                                   enum ESMC_CoordSys_Flag *coordSys,
                                   enum ESMC_TypeKind_Flag *coordTypeKind, 
                                   enum ESMC_IndexFlag *indexflag,
                                   int *rc){
  // Initialize return code. Assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;
  if(rc!=NULL) *rc=ESMC_RC_NOT_IMPL;

  // Init Grid
  ESMC_Grid grid;
  grid.ptr = NULL;
  
  grid.ptr = reinterpret_cast<ESMCI::Grid *>(ESMCI::Grid::createnoperidim(maxIndex,
                                      coordSys, coordTypeKind, indexflag, &localrc));
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    rc)) return grid; // bail out

  // return successfully
  if (rc) *rc = ESMF_SUCCESS;
  return grid;
}
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//TODO: InterfaceInt should be passed by value when ticket 3613642 is resolved
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_GridCreate1PeriDim()"
ESMC_Grid ESMC_GridCreate1PeriDim(ESMC_InterfaceInt *maxIndex,
                                  int *periodicDim, int *poleDim,
                                  enum ESMC_CoordSys_Flag *coordSys,
                                  enum ESMC_TypeKind_Flag *coordTypeKind, 
                                  enum ESMC_PoleKind_Flag *poleKind,
                                  enum ESMC_IndexFlag *indexflag,
                                  int *rc){
  int localrc = ESMC_RC_NOT_IMPL;
  if(rc!=NULL) *rc=ESMC_RC_NOT_IMPL;

  // Init Grid
  ESMC_Grid grid;
  grid.ptr = NULL;
  
  grid.ptr = reinterpret_cast<void *>(ESMCI::Grid::create1peridim(maxIndex,
                                                                  periodicDim, 
                                                                  poleDim,
                                                                  coordSys, 
                                                                  coordTypeKind, 
                                                                  poleKind, 
                                                                  indexflag,
                                                                  &localrc));
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    rc)) return grid; // bail out

  // return successfully
  if (rc) *rc = ESMF_SUCCESS;
  return grid;
}
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_GridCreateFromFile()"
ESMC_Grid ESMC_GridCreateFromFile(const char *filename, int fileTypeFlag, 
                  int *regDecomp, int *decompflag,
                  int *isSphere, int *addCornerStagger,
                  int *addUserArea, enum ESMC_IndexFlag *indexflag, int *addMask,
                  const char *varname, const char **coordNames,
                  int *rc) {

  int localrc = ESMC_RC_NOT_IMPL;
  if(rc!=NULL) *rc=ESMC_RC_NOT_IMPL;

  // Init Grid
  ESMC_Grid grid;
  grid.ptr = NULL;

  grid.ptr = reinterpret_cast<void *>
    (ESMCI::Grid::createfromfile(filename, fileTypeFlag, regDecomp, decompflag,
                 isSphere, addCornerStagger, addUserArea, indexflag,
                 addMask, varname, coordNames, &localrc));
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    rc)) return grid; // bail out

  // return successfully
  if (rc) *rc = ESMF_SUCCESS;
  
  int exLB[2]={-1,-1}, exUB[2]={-1,-1};
  double *gridXCoord;
  gridXCoord = (double *)ESMC_GridGetCoord(grid, 1, ESMC_STAGGERLOC_CENTER, exLB, exUB, &localrc);
  
  double *gridYCoord;
  gridYCoord = (double *)ESMC_GridGetCoord(grid, 2, ESMC_STAGGERLOC_CENTER, exLB, exUB, &localrc);
  
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
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;  // bail out    

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
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc; // bail out

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
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    rc)) return NULL; // bail out

  // create an ESMF_Array and cast into pointer access
  ESMC_Array arrayPtr;
  arrayPtr.ptr = reinterpret_cast<void *>(coordArray);

  // get the Array pointer to return
  void *coordPtr = ESMC_ArrayGetPtr(arrayPtr, 0, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    rc)) return NULL; // bail out

  // get the bounds
  if(exclusiveLBound && exclusiveUBound) {
    int localDe = 0;
    localrc = gridp->getExclusiveLBound(stagger, localDe, exclusiveLBound);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return NULL; // bail out
    localrc = gridp->getExclusiveUBound(stagger, localDe, exclusiveUBound);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return NULL; // bail out
  }

#if 0
  // TODO: use this instead of the above, it is more correct because the Fortran
  //       layer does additional bounds checking.
  ESMCI::InterfaceInt *computationalLBound;
  ESMCI::InterfaceInt *computationalUBound;
  // this code is technically more correct, but I can't find the symbol for some reason..
  int localDe = 0;
  FTN_X(c_esmc_gridgetcoordbounds)(&gridp, &localDe, &coordDim, &stagger,
                            NULL, NULL, NULL,
                            &computationalLBound, &computationalUBound,
                            NULL, NULL, NULL, NULL, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    rc)) return NULL; // bail out
#endif

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return coordPtr;
}
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_GridGetCoordBounds()"
int ESMC_GridGetCoordBounds(ESMC_Grid grid, 
                      enum ESMC_StaggerLoc staggerloc, 
                      int *exclusiveLBound,
                      int *exclusiveUBound, int *rc){

  // Initialize return code. Assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;

  // convert the ESMC_Grid to an ESMCI::Grid
  ESMCI::Grid *gridp = reinterpret_cast<ESMCI::Grid *>(grid.ptr);

  // convert the staggerloc enum to an int
  int stagger = static_cast<int>(staggerloc);

  // get the bounds
  if(exclusiveLBound && exclusiveUBound) {
    int localDe = 0;
    localrc = gridp->getExclusiveLBound(stagger, localDe, exclusiveLBound);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return localrc; // bail out
    localrc = gridp->getExclusiveUBound(stagger, localDe, exclusiveUBound);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return localrc; // bail out
  }

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_GridAddItem()"
int ESMC_GridAddItem(ESMC_Grid grid, enum ESMC_GridItem_Flag itemflag, 
                                      enum ESMC_StaggerLoc staggerloc){
  // Initialize return code. Assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;
  int rc=ESMC_RC_NOT_IMPL;

  // convert the ESMC_Grid to an ESMCI::Grid
  ESMCI::Grid *gridp = reinterpret_cast<ESMCI::Grid *>(grid.ptr);

  // convert the enums to ints
  int stagger = static_cast<int>(staggerloc);
  int item = static_cast<int>(itemflag);

  // add coords
  localrc=gridp->ESMCI::Grid::addItemArray(&stagger, &item, 
                                           NULL, NULL, NULL, NULL, NULL);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc; // bail out

  // return successfully
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_GridGetItem()"
void * ESMC_GridGetItem(ESMC_Grid grid,
                      enum ESMC_GridItem_Flag itemflag, 
                      enum ESMC_StaggerLoc staggerloc, 
                      int *rc){

  // Initialize return code. Assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;

  // convert the ESMC_Grid to an ESMCI::Grid
  ESMCI::Grid *gridp = reinterpret_cast<ESMCI::Grid *>(grid.ptr);
  //printf("\n\ngridstatus = %d\n\n", gridp->getStatus());

  // convert the enums to ints
  int stagger = static_cast<int>(staggerloc);
  int item = static_cast<int>(itemflag);

  // get coord array
  ESMCI::Array *itemArray; 
  itemArray = ((gridp)->getItemArray(&stagger, &item, NULL, &localrc));
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    rc)) return NULL; // bail out

  // create an ESMF_Array and cast into pointer access
  ESMC_Array arrayPtr;
  arrayPtr.ptr = reinterpret_cast<void *>(itemArray);

  // get the Array pointer to return
  void *itemPtr = ESMC_ArrayGetPtr(arrayPtr, 0, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    rc)) return NULL; // bail out

  // return successfully
  if (rc!=NULL) *rc = ESMF_SUCCESS;
  return itemPtr;
}
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_GridWrite()"
int ESMC_GridWrite(ESMC_Grid grid,
                   enum ESMC_StaggerLoc staggerloc, 
                   const char* fname) {

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  ESMCI::Grid *gridp = reinterpret_cast<ESMCI::Grid *>(grid.ptr);

  localrc=gridp->ESMCI::Grid::write(staggerloc, fname);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;  // bail out    

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------

} // extern "C"
// $Id$
