//
// Earth System Modeling Framework
// Copyright 2002-2008, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMC_Mesh_h
#define ESMC_Mesh_h

#include "ESMCI_Util.h"
#include "ESMCI_F90Interface.h"

extern "C" {

typedef struct {
  ESMCI::F90ClassHolder *f90this;
} ESMC_Mesh;

typedef enum {ESMC_MESHELEMENT_QUAD=0, ESMC_MESHELEMENT_TRI=1,
     ESMC_MESHELEMENT_HEX=2, ESMC_MESHELEMENT_TET=3} ESMF_MeshElement;

ESMC_Mesh *ESMC_MeshCreate(int parametricDim, int spatialDim, int *rc);

int ESMC_MeshAddNodes(ESMC_Mesh *mesh, int *nodeIds, double *nodeCoords, int *nodeOwners);

int ESMC_MeshAddElements(ESMC_Mesh *mesh, int *elementIds, int *elementTypes, int *elementConn);

int ESMC_MeshCreateAll(ESMC_Mesh *mesh, parametricDim, int *nodeIds, double *nodeCoords,
             int *nodeOwners, int *elementIds, int *elementTypes, int *elementConn);

int ESMC_MeshDestroy(ESMC_Mesh *mesh);


} // extern "C"

#endif
