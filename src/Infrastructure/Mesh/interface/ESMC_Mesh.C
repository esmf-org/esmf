// $Id: ESMC_Mesh.C,v 1.29 2012/10/09 00:32:48 jcjacob Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2013, University Corporation for Atmospheric Research,
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

#include "ESMF_LogMacros.inc" // TODO: remove once this comes through ESMCI_LogErr.h

#include "ESMCI_ParEnv.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_MeshCXX.h"
#include "ESMC_Mesh.h"
#include "ESMCI_VM.h"

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_Mesh.C,v 1.29 2012/10/09 00:32:48 jcjacob Exp $";
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
#define ESMC_METHOD "ESMC_MeshCreateFromFile()"
ESMC_Mesh ESMC_MeshCreateFromFile(char *filename, int fileTypeFlag, 
				  int *convert3D, 
				  int *convertToDual,
				  int *addUserArea,
				  char *meshname,
				  int *addMask,
				  char *varname,
				  int *rc) {
  // Initialize return code. Assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;

  if(rc!=NULL) *rc=ESMC_RC_NOT_IMPL;

  // Init Mesh
  ESMC_Mesh mesh;
  mesh.ptr = NULL;

  // Call into ESMCI method
  mesh.ptr = (void *)MeshCXX::createFromFile(filename, fileTypeFlag, convert3D, convertToDual, addUserArea, meshname, addMask, varname, &localrc);
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
                         int *elementTypes, int *elementConn, 
                         int *elementMask, double *elementArea){
   
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  // typecast into ESMCI type
  MeshCXX* mep = (MeshCXX*)(mesh.ptr);
  
  // call into ESMCI method
  localrc = mep->addElements(elementCount, elementIds, elementTypes, 
                             elementConn, elementMask, elementArea);
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

  // init output (because 0 could be legit)
  *num_nodes=-1;

  // make sure Mesh has had it's nodes added
  if (!mep->isNodesAdded()) {
    if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
     "- Mesh must have had its nodes added to get local node count ", &localrc)) return localrc;
  }
  
  // call into ESMCI method
  *num_nodes = mep->getNumLocalNodes();

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//--------------------------------------------------------------------------



//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_MeshGetOwnedNodeCount()"
int ESMC_MeshGetOwnedNodeCount(ESMC_Mesh mesh, int* num_nodes){
  
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  // typecast into ESMCI type
  MeshCXX* mep = (MeshCXX*)(mesh.ptr);

  // init output (because 0 could be legit)
  *num_nodes=-1;

  // make sure Mesh has been finished
  if (!mep->isMeshFinished()) {
    if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
     "- Mesh must be finished to get owned node count ", &localrc)) return localrc;
  }
  
  // call into ESMCI method
  *num_nodes = mep->getNumOwnedNodes();

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

  // init output (because 0 could be legit)
  *num_elems=-1;

  // typecast into ESMCI type
  MeshCXX* mep = (MeshCXX*)(mesh.ptr);

  // make sure Mesh has had it's elements added
  if (!mep->isElemsAdded()) {
    if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
     "- Mesh must have had its elements added to get local element count ", &localrc)) return localrc;
  }

  // call into ESMCI method
  *num_elems = mep->getNumLocalElements();

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//--------------------------------------------------------------------------



//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_MeshGetOwnedElementCount()"
int ESMC_MeshGetOwnedElementCount(ESMC_Mesh mesh, int* num_elems){
  
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;              // final return code

  // init output (because 0 could be legit)
  *num_elems=-1;

  // typecast into ESMCI type
  MeshCXX* mep = (MeshCXX*)(mesh.ptr);

  // make sure Mesh has had it's elements added
  if (!mep->isMeshFinished()) {
    if(ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE,
     "- Mesh must be finished to get owned element count ", &localrc)) return localrc;
  }

  // call into ESMCI method
  *num_elems = mep->getNumOwnedElements();

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
