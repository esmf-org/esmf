// $Id: ESMC_Mesh.C,v 1.6 2008/11/10 19:54:34 rosalind Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2008, University Corporation for Atmospheric Research,
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

#include "ESMCI_LogErr.h"                  // for LogErr
#include "ESMF_LogMacros.inc"             // for LogErr
#include "ESMCI_MeshCXX.h"
#include "ESMC_Mesh.h"

using namespace ESMCI;

extern "C" {

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_MeshCreate()"
ESMC_Mesh *ESMC_MeshCreate(int parametricDim, int spatialDim, int *rc) {
  int localrc;
   // Initialize return code. Assume routine not implemented
   localrc = ESMC_RC_NOT_IMPL;
   if(rc!=NULL) *rc=ESMC_RC_NOT_IMPL;

  ESMC_Mesh *mesh;
  mesh = new ESMC_Mesh();

  MeshCXX* mep;
  mep = new MeshCXX();

  (mep)->create(&parametricDim, &spatialDim, &localrc);

  if (rc) *rc = localrc;
  mesh->ptr = (void*)mep;

  return mesh;

} // ESMC_MeshCreate

//--------------------------------------------------------------------------
// !BOP
// !IROUTINE: ESMC_MeshAddNodes
//
// !EOP

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_MeshAddNodes"
int ESMC_MeshAddNodes(ESMC_Mesh *mesh, int *num_nodes, int *nodeIds,
                      double *nodeCoords, int *nodeOwners) {
   int localrc;
   // Initialize return code. Assume routine not implemented
   localrc = ESMC_RC_NOT_IMPL;

   MeshCXX* mep = (MeshCXX*)(mesh->ptr);
   localrc = mep->addNodes(num_nodes, nodeIds, nodeCoords, nodeOwners);
   return localrc;

} // ESMC_MeshAddNodes

//--------------------------------------------------------------------------
// !BOP
// !IROUTINE: ESMC_MeshAddElements
//
// !EOP
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_MeshAddElements()"
int ESMC_MeshAddElements(ESMC_Mesh *mesh, int *num_elems, int *elementIds,
                         int *elementTypes, int *elementConn){
   int localrc;
   // Initialize return code. Assume routine not implemented
   localrc = ESMC_RC_NOT_IMPL;

   MeshCXX* mep = (MeshCXX*)(mesh->ptr);
   
   localrc = mep->addElements(num_elems, elementIds, elementTypes,
                  elementConn);

   return localrc;
} // ESMC_MeshAddElements

//--------------------------------------------------------------------------
// !BOP
// !IROUTINE: ESMC_MeshCreateDistGrids
//
// !EOP
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_MeshCreateDistGrids()"
int ESMC_MeshCreateDistGrids(ESMC_Mesh *mesh, int *num_nodes, int *num_elements)
{
   int localrc;
   // Initialize return code. Assume routine not implemented
   localrc = ESMC_RC_NOT_IMPL;

   MeshCXX* mep = (MeshCXX*)(mesh->ptr);

   localrc = mep->createDistGrids(num_nodes, num_elements);

   return localrc;

}  // ESMC_MeshCreateDistGrids


//--------------------------------------------------------------------------
// !BOP
// !IROUTINE: ESMC_MeshDestroy
//
// !EOP
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_MeshDestroy()"
int ESMC_MeshDestroy(ESMC_Mesh *mesh){
   int localrc;
   // Initialize return code. Assume routine not implemented
   localrc = ESMC_RC_NOT_IMPL;
   

   MeshCXX* mep = (MeshCXX*)(mesh->ptr);

   localrc = mep->destroy();

   return localrc;

} // ESMC_MeshDestroy


//--------------------------------------------------------------------------
// !BOP
// !IROUTINE: ESMC_MeshFreeMemory
//
// !EOP
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_MeshFreeMemory()"
int ESMC_MeshFreeMemory(ESMC_Mesh *mesh){
   int localrc;
   // Initialize return code. Assume routine not implemented
   localrc = ESMC_RC_NOT_IMPL;


   MeshCXX* mep = (MeshCXX*)(mesh->ptr);

   localrc = mep->freeMemory();

   return localrc;

} // ESMC_MeshDestroy


} // extern "C"

