// $Id: ESMC_Mesh.C,v 1.4 2008/11/04 21:39:28 rosalind Exp $
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

#include "ESMC_Mesh.h"

using namespace ESMCI;

extern "C" {
ESMC_Mesh *ESMC_MeshCreate(int parametricDim, int spatialDim, int *rc) {
  int localrc;
  ESMC_Mesh *mesh;
  mesh = new ESMC_Mesh();

  Mesh* me;
  me = new Mesh();

  FTN(c_esmc_meshcreate)(&me, &parametricDim, &spatialDim, &localrc);

  if (rc) *rc = localrc;
  mesh->ptr = (void*)me;

  return mesh;

} // ESMC_MeshCreate

//--------------------------------------------------------------------------
// !BOP 
// !IROUTINE: ESMC_MeshAddNodes
//
// !EOP
int ESMC_MeshAddNodes(ESMC_Mesh *mesh, int *num_nodes, int *nodeIds, 
                      double *nodeCoords, int *nodeOwners) {
   int localrc;

   Mesh* me = (Mesh*)(mesh->ptr);

   FTN(c_esmc_meshaddnodes)(&me, num_nodes, nodeIds, nodeCoords,
                            nodeOwners, &localrc);

   return localrc;

} // ESMC_MeshAddNodes

//--------------------------------------------------------------------------
// !BOP
// !IROUTINE: ESMC_MeshAddElements
//
// !EOP
int ESMC_MeshAddElements(ESMC_Mesh *mesh, int *num_elems, int *elementIds, 
                         int *elementTypes, int *elementConn){
   int localrc;

   Mesh* me = (Mesh*)(mesh->ptr);

   FTN(c_esmc_meshaddelements)(&me, num_elems, elementIds, elementTypes,
                               elementConn, &localrc);

   return localrc;

}  // ESMC_MeshAddElements

} // extern "C"

