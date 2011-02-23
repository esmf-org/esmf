// $Id: ESMC_Mesh.C,v 1.22 2011/02/23 01:03:19 w6ws Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2011, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMC_Mesh.C"
//==============================================================================
//
//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
// This file contains the C interfaces' code for the {\tt Mesh} class functions.
//
//EOP
//------------------------------------------------------------------------------
// INCLUDES

#include "ESMCI_ParEnv.h"
#include "ESMCI_LogErr.h"                  // for LogErr
#include "ESMF_LogMacros.inc"             // for LogErr
#include "ESMCI_MeshCXX.h"
#include "ESMC_Mesh.h"
#include "ESMCI_VM.h"

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_Mesh.C,v 1.22 2011/02/23 01:03:19 w6ws Exp $";
//-----------------------------------------------------------------------------

using namespace ESMCI;

extern "C" {

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_MeshCreate()"
ESMC_Mesh ESMC_MeshCreate(int parametricDim, int spatialDim, int *rc){
  // Initialize return code. Assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;
  if(rc!=NULL) *rc=ESMC_RC_NOT_IMPL;

  // Init Mesh
  ESMC_Mesh mesh;
  mesh.ptr = NULL;

  // call into ESMCI method
  mesh.ptr = (void *)MeshCXX::create(parametricDim, spatialDim, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, rc))
    return mesh; // bail out

  // return successfully
  if (rc) *rc = ESMF_SUCCESS;
  return mesh;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_MeshAddNodes"
int ESMC_MeshAddNodes(ESMC_Mesh mesh, int nodeCount, int *nodeIds,
  double *nodeCoords, int *nodeOwners){
  
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code


  // typecast into ESMCI type
  MeshCXX* mep = (MeshCXX*)(mesh.ptr);
  
  // call into ESMCI method
  localrc = mep->addNodes(nodeCount, nodeIds, nodeCoords, nodeOwners);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &rc))
    return rc;  // bail out
  
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_MeshAddElements()"
int ESMC_MeshAddElements(ESMC_Mesh mesh, int elementCount, int *elementIds,
  int *elementTypes, int *elementConn){
   
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  // typecast into ESMCI type
  MeshCXX* mep = (MeshCXX*)(mesh.ptr);
  
  // call into ESMCI method
  localrc = mep->addElements(elementCount, elementIds, elementTypes, elementConn);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &rc))
    return rc;  // bail out

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//--------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_MeshCreateDistGrids()"
int ESMC_MeshCreateDistGrids(ESMC_Mesh mesh, int *nodeDistGrid, 
  int *elemDistGrid, int *num_nodes, int *num_elements){
  
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  // typecast into ESMCI type
  MeshCXX* mep = (MeshCXX*)(mesh.ptr);
  
  // call into ESMCI method
  localrc = mep->createDistGrids(nodeDistGrid, elemDistGrid, num_nodes, 
    num_elements);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &rc))
    return rc;  // bail out

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//--------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_MeshDestroy()"
int ESMC_MeshDestroy(ESMC_Mesh *mesh){
  
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  // Get MeshCXX pointer
  MeshCXX *mep=(MeshCXX *)mesh->ptr;

  // Do destroy
  localrc= MeshCXX::destroy(&mep);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &rc))
    return rc;  // bail out    

  // Set to NULL
  mesh->ptr=NULL;

  // return successfully
  return ESMF_SUCCESS;
}
//--------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_MeshFreeMemory()"
int ESMC_MeshFreeMemory(ESMC_Mesh mesh){
  
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  // typecast into ESMCI type
  MeshCXX* mep = (MeshCXX*)(mesh.ptr);
  
  // call into ESMCI method
  localrc = mep->freeMemory();
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &rc))
    return rc;  // bail out

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//--------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_MeshGetLocalNodeCount()"
int ESMC_MeshGetLocalNodeCount(ESMC_Mesh mesh, int* num_nodes){
  
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  // typecast into ESMCI type
  MeshCXX* mep = (MeshCXX*)(mesh.ptr);
  
  // call into ESMCI method
  *num_nodes = mep->numNodes();

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//--------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_MeshGetLocalElementCount()"
int ESMC_MeshGetLocalElementCount(ESMC_Mesh mesh, int* num_elems){
  
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  // typecast into ESMCI type
  MeshCXX* mep = (MeshCXX*)(mesh.ptr);
  
  // call into ESMCI method
  *num_elems = mep->numElements();

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//--------------------------------------------------------------------------

  //-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_MeshVTKHeader()"
int ESMC_MeshVTKHeader(const char *fname, int *num_elem, int *num_node,
  int *conn_size){
  
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  // call into ESMCI method
  localrc = MeshVTKHeader(fname, num_elem, num_node, conn_size);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &rc))
    return rc;  // bail out

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//--------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_MeshVTKBody()"
int ESMC_MeshVTKBody(const char *fname, int *nodeId, double *nodeCoord,
  int *nodeOwner, int *elemId, int *elemType, int *elemConn){
  
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  // call into ESMCI method
  localrc = MeshVTKBody(fname, nodeId, nodeCoord, nodeOwner, elemId, elemType,
    elemConn);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &rc))
    return rc;  // bail out

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//--------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_MeshWrite()"
int ESMC_MeshWrite(ESMC_Mesh mesh, const char* filename){
  
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  // typecast into ESMCI type
  MeshCXX* mep = (MeshCXX*)(mesh.ptr);
  
  // call into ESMCI method
  localrc = mep->meshWrite(filename);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, &rc))
    return rc;  // bail out

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//--------------------------------------------------------------------------

} // extern "C"
