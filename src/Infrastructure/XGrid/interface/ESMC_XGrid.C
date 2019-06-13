// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2019, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMC_XGrid.C"
//==============================================================================
//
//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
// This file contains the C interfaces' code for the {\tt XGrid} class
// functions.
//
//EOP
//------------------------------------------------------------------------------
// INCLUDES
#include "ESMC_XGrid.h"

#include "ESMCI_Macros.h"
#include "ESMCI_XGrid.h"
#include "ESMCI_F90Interface.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_Grid.h"

#include <string>
#include <iostream>

using namespace ESMCI;

extern "C" {


#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_XGridCreate()"
  ESMC_XGrid ESMC_XGridCreate(
                              int sideAGridCount,  ESMC_Grid *sideAGrid, 
                              int sideAMeshCount,  ESMC_Mesh *sideAMesh, 
                              int sideBGridCount,  ESMC_Grid *sideBGrid, 
                              int sideBMeshCount,  ESMC_Mesh *sideBMesh, 
                              ESMC_InterArrayInt *sideAGridPriority, 
                              ESMC_InterArrayInt *sideAMeshPriority, 
                              ESMC_InterArrayInt *sideBGridPriority, 
                              ESMC_InterArrayInt *sideBMeshPriority, 
                              ESMC_InterArrayInt *sideAMaskValues, 
                              ESMC_InterArrayInt *sideBMaskValues, 
                              int storeOverlay, 
                              int *rc) {  

    // Initialize return code. Assume routine not implemented
    if (rc) *rc = ESMF_RC_NOT_IMPL;
    int localrc = ESMF_RC_NOT_IMPL;

    // XGrid struct
    ESMC_XGrid xgrid;

    // Invoke the C++ interface
    xgrid.ptr = reinterpret_cast<void *>(ESMCI::XGrid::create(
                              sideAGridCount,  sideAGrid, 
                              sideAMeshCount,  sideAMesh, 
                              sideBGridCount,  sideBGrid, 
                              sideBMeshCount,  sideBMesh, 
                              sideAGridPriority, 
                              sideAMeshPriority, 
                              sideBGridPriority, 
                              sideBMeshPriority, 
                              sideAMaskValues, 
                              sideBMaskValues, 
                              storeOverlay, 
                              &localrc));
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)){
      xgrid.ptr = NULL;  // invalidate
      return xgrid; // bail out
    }

    // return successfully
    if (rc!=NULL) *rc = ESMF_SUCCESS;
    return xgrid;
  }


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_XGridDestroy()"
  int ESMC_XGridDestroy(ESMC_XGrid *xgrid){
    // Initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    
    // typecase into ESMCI type
    ESMCI::XGrid *xgridp = reinterpret_cast<ESMCI::XGrid *>(xgrid->ptr);

    // Invoke the C++ interface
    localrc = ESMCI::XGrid::destroy(xgridp);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) return rc;

    // invalidate pointer
    xgrid->ptr = NULL;

    // return successfully
    rc = ESMF_SUCCESS;
    return rc;
  }
//--------------------------------------------------------------------------


//--------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_XGridGetSideAGridCount()"
  int ESMC_XGridGetSideAGridCount(ESMC_XGrid xgrid, int *rc){
    // Initialize return code; assume routine not implemented
    if(rc) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    
    // typecast into ESMCI type
    ESMCI::XGrid *xgridp = reinterpret_cast<ESMCI::XGrid *>(xgrid.ptr);

    // Invoke the C++ interface
    int count = xgridp->getSideAGridCount(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return count;

    // return successfully
    if(rc) *rc = ESMF_SUCCESS;
    return count;
  }
//--------------------------------------------------------------------------


//--------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_XGridGetSideAMeshCount()"
  int ESMC_XGridGetSideAMeshCount(ESMC_XGrid xgrid, int *rc){
    // Initialize return code; assume routine not implemented
    if(rc) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    
    // typecast into ESMCI type
    ESMCI::XGrid *xgridp = reinterpret_cast<ESMCI::XGrid *>(xgrid.ptr);

    // Invoke the C++ interface
    int count = xgridp->getSideAMeshCount(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return count;

    // return successfully
    if(rc) *rc = ESMF_SUCCESS;
    return count;
  }
//--------------------------------------------------------------------------


//--------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_XGridGetSideBGridCount()"
  int ESMC_XGridGetSideBGridCount(ESMC_XGrid xgrid, int *rc){
    // Initialize return code; assume routine not implemented
    if(rc) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    
    // typecast into ESMCI type
    ESMCI::XGrid *xgridp = reinterpret_cast<ESMCI::XGrid *>(xgrid.ptr);

    // Invoke the C++ interface
    int count = xgridp->getSideBGridCount(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return count;

    // return successfully
    if(rc) *rc = ESMF_SUCCESS;
    return count;
  }
//--------------------------------------------------------------------------

//--------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_XGridGetSideBMeshCount()"
  int ESMC_XGridGetSideBMeshCount(ESMC_XGrid xgrid, int *rc){
    // Initialize return code; assume routine not implemented
    if(rc) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    
    // typecast into ESMCI type
    ESMCI::XGrid *xgridp = reinterpret_cast<ESMCI::XGrid *>(xgrid.ptr);

    // Invoke the C++ interface
    int count = xgridp->getSideBMeshCount(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return count;

    // return successfully
    if(rc) *rc = ESMF_SUCCESS;
    return count;
  }
//--------------------------------------------------------------------------


//--------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_XGridGetDimCount()"
  int ESMC_XGridGetDimCount(ESMC_XGrid xgrid, int *rc){
    // Initialize return code; assume routine not implemented
    if(rc) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    
    // typecast into ESMCI type
    ESMCI::XGrid *xgridp = reinterpret_cast<ESMCI::XGrid *>(xgrid.ptr);

    // Invoke the C++ interface
    int count = xgridp->getDimCount(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return count;

    // return successfully
    if(rc) *rc = ESMF_SUCCESS;
    return count;
  }
//--------------------------------------------------------------------------


//--------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_XGridGetElementCount()"
  int ESMC_XGridGetElementCount(ESMC_XGrid xgrid, int *rc){
    // Initialize return code; assume routine not implemented
    if(rc) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    
    // typecast into ESMCI type
    ESMCI::XGrid *xgridp = reinterpret_cast<ESMCI::XGrid *>(xgrid.ptr);

    // Invoke the C++ interface
    int count = xgridp->getElementCount(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return count;

    // return successfully
    if(rc) *rc = ESMF_SUCCESS;
    return count;
  }
//--------------------------------------------------------------------------

//--------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_XGridGetMesh()"
  ESMC_Mesh ESMC_XGridGetMesh(ESMC_XGrid xgrid, int *rc){
    // Initialize return code; assume routine not implemented
    if(rc) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    
    // typecase into ESMCI type
    ESMCI::XGrid *xgridp = reinterpret_cast<ESMCI::XGrid *>(xgrid.ptr);

    // Invoke the C++ interface
    ESMC_Mesh mesh = xgridp->getMesh(&localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return mesh;

    // return successfully
    if(rc) *rc = ESMF_SUCCESS;
    return mesh;
  }
//--------------------------------------------------------------------------

//--------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_XGridGetElementArea()"
  void ESMC_XGridGetElementArea(ESMC_XGrid xgrid, ESMC_R8 *area, int *rc){
    // Initialize return code; assume routine not implemented
    if(rc) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    
    // typecast into ESMCI type
    ESMCI::XGrid *xgridp = reinterpret_cast<ESMCI::XGrid *>(xgrid.ptr);

    // Invoke the C++ interface
    xgridp->getArea(area, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return;

    // return successfully
    if(rc) *rc = ESMF_SUCCESS;
    return;
  }
//--------------------------------------------------------------------------

//--------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_XGridGetElementCentroid()"
  void ESMC_XGridGetElementCentroid(ESMC_XGrid xgrid, ESMC_R8 *centroid, int *rc){
    // Initialize return code; assume routine not implemented
    if(rc) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    
    // typecast into ESMCI type
    ESMCI::XGrid *xgridp = reinterpret_cast<ESMCI::XGrid *>(xgrid.ptr);

    // Invoke the C++ interface
    xgridp->getCentroid(centroid, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return;

    // return successfully
    if(rc) *rc = ESMF_SUCCESS;
    return;
  }
//--------------------------------------------------------------------------

//--------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_XGridGetSparseMatA2X()"
  void ESMC_XGridGetSparseMatA2X(ESMC_XGrid xgrid, int sideAIndex, 
                                 int *factorListCount, 
                                 double **factorList, 
                                 int **factorIndexList, 
                                 int *rc){

    // Initialize return code; assume routine not implemented
    if(rc) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    
    // typecast into ESMCI type
    ESMCI::XGrid *xgridp = reinterpret_cast<ESMCI::XGrid *>(xgrid.ptr);

    // Invoke the C++ interface
    xgridp->getSparseMatA2X(sideAIndex, factorListCount, 
                            factorList, factorIndexList, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return;
    // return successfully
    if(rc) *rc = ESMF_SUCCESS;
    return;
  }
//--------------------------------------------------------------------------

//--------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_XGridGetSparseMatX2A()"
  void ESMC_XGridGetSparseMatX2A(ESMC_XGrid xgrid, int sideAIndex, 
                                 int *factorListCount, 
                                 double **factorList, 
                                 int **factorIndexList, 
                                 int *rc){

    // Initialize return code; assume routine not implemented
    if(rc) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    
    // typecast into ESMCI type
    ESMCI::XGrid *xgridp = reinterpret_cast<ESMCI::XGrid *>(xgrid.ptr);

    // Invoke the C++ interface
    xgridp->getSparseMatX2A(sideAIndex, factorListCount, 
                            factorList, factorIndexList, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return;
    // return successfully
    if(rc) *rc = ESMF_SUCCESS;
    return;
  }
//--------------------------------------------------------------------------

//--------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_XGridGetSparseMatB2X()"
  void ESMC_XGridGetSparseMatB2X(ESMC_XGrid xgrid, int sideBIndex, 
                                 int *factorListCount, 
                                 double **factorList, 
                                 int **factorIndexList, 
                                 int *rc){

    // Initialize return code; assume routine not implemented
    if(rc) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    
    // typecast into ESMCI type
    ESMCI::XGrid *xgridp = reinterpret_cast<ESMCI::XGrid *>(xgrid.ptr);

    // Invoke the C++ interface
    xgridp->getSparseMatB2X(sideBIndex, factorListCount, 
                            factorList, factorIndexList, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return;
    // return successfully
    if(rc) *rc = ESMF_SUCCESS;
    return;
  }
//--------------------------------------------------------------------------


//--------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_XGridGetSparseMatX2B()"
  void ESMC_XGridGetSparseMatX2B(ESMC_XGrid xgrid, int sideBIndex, 
                                 int *factorListCount, 
                                 double **factorList, 
                                 int **factorIndexList, 
                                 int *rc){

    // Initialize return code; assume routine not implemented
    if(rc) *rc = ESMC_RC_NOT_IMPL;
    int localrc = ESMC_RC_NOT_IMPL;
    
    // typecast into ESMCI type
    ESMCI::XGrid *xgridp = reinterpret_cast<ESMCI::XGrid *>(xgrid.ptr);

    // Invoke the C++ interface
    xgridp->getSparseMatX2B(sideBIndex, factorListCount, 
                            factorList, factorIndexList, &localrc);
    if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) return;
    // return successfully
    if(rc) *rc = ESMF_SUCCESS;
    return;
  }
//--------------------------------------------------------------------------



//--------------------------------------------------------------------------

}
