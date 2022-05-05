// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2022, University Corporation for Atmospheric Research,
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

#include "ESMCI_LogErr.h"
#include "ESMCI_VM.h"
#include "ESMCI_CoordSys.h"
#include "Mesh/include/ESMC_Mesh.h"
#include "Mesh/include/ESMCI_MeshCap.h"
#include "Mesh/include/ESMCI_MeshCXX.h"
#include "Mesh/include/Legacy/ESMCI_ParEnv.h"

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id$";
//-----------------------------------------------------------------------------

using namespace ESMCI;

extern "C" {

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_MeshGetMOAB()"
void ESMC_MeshGetMOAB(bool *moabOn, int *rc){

  int moabOnInt = 0;
  MeshCap::meshGetMOAB(&moabOnInt, rc);
  
  if (moabOnInt == 0) *moabOn = false;
  else *moabOn = true;
    
  // return successfully
  if (rc) *rc = ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_MeshSetMOAB()"
void ESMC_MeshSetMOAB(bool moabOn, int *rc){

  int moabOnInt = 0;
  if (moabOn == true) moabOnInt = 1;
  
  MeshCap::meshSetMOAB(&moabOnInt, rc);
  
  // return successfully
  if (rc) *rc = ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_MeshCreate()"
ESMC_Mesh ESMC_MeshCreate(int parametricDim, int spatialDim, 
                          enum ESMC_CoordSys_Flag *coordSys,
                          int *rc){
  // Initialize return code. Assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;
  if(rc!=NULL) *rc=ESMC_RC_NOT_IMPL;

  // Init Mesh
  ESMC_Mesh mesh;
  mesh.ptr = NULL;
  MeshCap *mc;

  // Set coordSys
  ESMC_CoordSys_Flag localCoordSys;
  if (coordSys != NULL) {
    localCoordSys=*coordSys;
  } else {
    localCoordSys=ESMC_COORDSYS_SPH_DEG;
  }

  // call into ESMCI method
  mc = MeshCap::meshcreate(&parametricDim, &spatialDim, 
                           &localCoordSys, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    rc)) return mesh; // bail out

  mesh.ptr = static_cast<void*> (mc);
  
  // return successfully
  if (rc) *rc = ESMF_SUCCESS;
  return mesh;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_MeshCreateFromFile()"
ESMC_Mesh ESMC_MeshCreateFromFile(const char *filename, int fileTypeFlag,
                                  int *convertToDual,
                                  int *addUserArea,
                                  const char *meshname,
                                  int *maskFlag,
                                  const char *varname,
                                  int *rc) {
  // Initialize return code. Assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;

  if(rc!=NULL) *rc=ESMC_RC_NOT_IMPL;

  // Init Mesh
  ESMC_Mesh mesh;
  mesh.ptr = NULL;
  MeshCap *mc;

  // Call into ESMCI method
  mc = MeshCap::meshcreatefromfile(filename, fileTypeFlag,
                               convertToDual, addUserArea,
                               meshname, maskFlag, varname,
                               &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    rc)) return mesh; // bail out

  mesh.ptr = static_cast<void*> (mc);

  // return successfully
  if (rc) *rc = ESMF_SUCCESS;
  return mesh;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_MeshCreateFromPtr()"
  ESMC_Mesh ESMC_MeshCreateFromPtr(void *mesh_ptr, int parametricDim, 
                                   int spatialDim, 
                                   enum ESMC_CoordSys_Flag coordSys, int *rc){
    
  // Initialize return code. Assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;
  if(rc!=NULL) *rc=ESMC_RC_NOT_IMPL;

  // Init Mesh
  ESMC_Mesh mesh;
  MeshCap *mc;
  
  // call into ESMCI method
  mc = MeshCap::create_from_ptr(mesh_ptr, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    rc)) return mesh;

  mesh.ptr = static_cast<void*> (mc);
    
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
  int localrc = ESMC_RC_NOT_IMPL;
  int rc = ESMC_RC_NOT_IMPL;

  MeshCap *mc = static_cast<MeshCap*> (mesh.ptr);

  // Wrap node_owners in IntArray
  InterArray<int> nodeOwnersIA(nodeOwners,nodeCount);

  // call into ESMCI method
  mc->meshaddnodes(&nodeCount, nodeIds, nodeCoords, &nodeOwnersIA,
                   NULL, &(mc->coordsys_mc), &(mc->sdim_mc), &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;

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
                         int *elementMask, double *elementArea,
                         double *elementCoords){

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;
  int rc = ESMC_RC_NOT_IMPL;

  MeshCap *mc = static_cast<MeshCap*> (mesh.ptr);
  
  // count elemconn
  int ec = 0;
  for (unsigned int i = 0; i < elementCount; ++i)
    ec += elementTypes[i];

  // evaluate presence of input parameters
  int apresent = 0;
  if (elementArea != nullptr) apresent = 1;

  int cpresent = 0;
  if (elementCoords != nullptr) cpresent = 1;

  // convert elementMask to InterArray for transfer to MeshCap
  InterArray<int> *em = new InterArray<int> (elementMask, elementCount);
  
  // call into ESMCI method
  mc->meshaddelements(&elementCount, elementIds, elementTypes,
                      em,
                      &apresent, elementArea, 
                      &cpresent, elementCoords, 
                      &ec, elementConn,
                      &(mc->coordsys_mc), &(mc->sdim_mc),
                      &localrc);
                      // elementConn, elementMask, elementArea, elementCoords);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;

  delete em;

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
  int localrc = ESMC_RC_NOT_IMPL;
  int rc = ESMC_RC_NOT_IMPL;

  MeshCap *mc = static_cast<MeshCap*> (mesh->ptr);

  localrc = MeshCap::destroy(&(mc));
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;  // bail out
    
  // Set to NULL
  mesh->ptr=NULL;

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//--------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_MeshFreeMemory()"
int ESMC_MeshFreeMemory(ESMC_Mesh mesh){

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;
  int rc = ESMC_RC_NOT_IMPL;

  MeshCap *mc = static_cast<MeshCap*> (mesh.ptr);

  // call into ESMCI method
  mc->meshfreememory(&localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;  // bail out

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//--------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_MeshGetCoord()"
void ESMC_MeshGetCoord(ESMC_Mesh mesh,
           double *nodeCoord, int *num_nodes, int *num_dims, int *rc){

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;
  if(rc!=NULL) *rc=ESMC_RC_NOT_IMPL;

  MeshCap *mc = static_cast<MeshCap*> (mesh.ptr);

  // get the nodeCoord
  mc->getlocalcoords(nodeCoord, &(mc->sdim_mc), &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    rc)) return;  // bail out

  // get the num_nodes
  mc->getOwnedNodeCount(num_nodes, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    rc)) return;  // bail out

  // get the num_dims
  *num_dims = mc->sdim_mc;

  // return successfully
  if(rc!=NULL) *rc = ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_MeshGetElemCoord()"
void ESMC_MeshGetElemCoord(ESMC_Mesh mesh,
           double *elemCoord, int *num_elems, int *num_dims, int *rc){

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;
  if(rc!=NULL) *rc=ESMC_RC_NOT_IMPL;

  MeshCap *mc = static_cast<MeshCap*> (mesh.ptr);

  // get the elemCoord
  mc->getlocalelemcoords(elemCoord, &(mc->sdim_mc), &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    rc)) return;  // bail out

  // get the num_elems
  mc->getOwnedElemCount(num_elems, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    rc)) return;  // bail out

  // get the num_dims
  *num_dims = mc->sdim_mc;

  // return successfully
  if(rc!=NULL) *rc = ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_MeshGetConnectivity()"
void ESMC_MeshGetConnectivity(ESMC_Mesh mesh, double *connCoord,
                              int *nodesPerElem, int *rc){

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;
  if(rc!=NULL) *rc=ESMC_RC_NOT_IMPL;

  ESMCI::InterArray<int> elemConn;

  MeshCap *mc = static_cast<MeshCap*> (mesh.ptr);

  // get the elemtypes and elemconn
  mc->getElemCreateInfo(NULL, NULL, &elemConn, NULL, NULL, NULL, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    rc)) return;  // bail out

  // set connCoord and nodesPerElem

  // return successfully
  if(rc!=NULL) *rc = ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_MeshGetNodeCount()"
int ESMC_MeshGetNodeCount(ESMC_Mesh mesh, int *num_nodes){

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;
  int rc = ESMC_RC_NOT_IMPL;

  // init output (because 0 could be legit)
  *num_nodes=-1;

  MeshCap *mc = static_cast<MeshCap*> (mesh.ptr);

  // get node count
  mc->getNodeCount(num_nodes, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return -1;  // bail out

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//--------------------------------------------------------------------------



//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_MeshGetOwnedNodeCount()"
int ESMC_MeshGetOwnedNodeCount(ESMC_Mesh mesh, int *num_nodes){

  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;

  // init output (because 0 could be legit)
  *num_nodes=-1;

  MeshCap *mc = static_cast<MeshCap*> (mesh.ptr);

  // get node count
  mc->getOwnedNodeCount(num_nodes, &rc);
  
  return rc;
}
//--------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_MeshGetElementCount()"
int ESMC_MeshGetElementCount(ESMC_Mesh mesh, int *num_elems){

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;
  int rc = ESMC_RC_NOT_IMPL;

  // init output (because 0 could be legit)
  *num_elems=-1;

  MeshCap *mc = static_cast<MeshCap*> (mesh.ptr);

  // get elem count
  mc->getElemCount(num_elems, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return -1;  // bail out

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//--------------------------------------------------------------------------



//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_MeshGetOwnedElementCount()"
int ESMC_MeshGetOwnedElementCount(ESMC_Mesh mesh, int *num_elems){

  // initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;

  // init output (because 0 could be legit)
  *num_elems=-1;

  MeshCap *mc = static_cast<MeshCap*> (mesh.ptr);

  // get elem count
  mc->getOwnedElemCount(num_elems, &rc);

  // return successfully
  return rc;
}
//--------------------------------------------------------------------------


  //-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_MeshVTKHeader()"
int ESMC_MeshVTKHeader(const char *fname, int *num_elem, int *num_node,
  int *conn_size){

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;
  int rc = ESMC_RC_NOT_IMPL;

  // filename string
  const std::string fn(fname);

  // call into ESMCI method
  MeshCap::meshvtkheader(const_cast<char*> (fname), 
                         num_elem, num_node, conn_size, &localrc, 
                         fn.size());
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;  // bail out

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
  int localrc = ESMC_RC_NOT_IMPL;
  int rc = ESMC_RC_NOT_IMPL;

  // filename string
  const std::string fn(fname);

  // call into ESMCI method
  MeshCap::meshvtkbody(const_cast<char*> (fname), 
                       nodeId, nodeCoord, nodeOwner, elemId, elemType,
                       elemConn, &localrc, fn.size());
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;  // bail out

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//--------------------------------------------------------------------------


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_MeshWrite()"
int ESMC_MeshWrite(ESMC_Mesh mesh, const char *filename){

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;
  int rc = ESMC_RC_NOT_IMPL;

  // filename string
  const std::string fn(filename);

  MeshCap *mc = static_cast<MeshCap*> (mesh.ptr);

  // call into ESMCI method
  mc->meshwrite(const_cast<char*> (filename), &localrc, fn.size());
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
    &rc)) return rc;  // bail out

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
}
//--------------------------------------------------------------------------

} // extern "C"
