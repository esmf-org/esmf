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

//-----------------------------------------------------------------------------
//BOP
// !CLASS:  ESMC_Mesh - Public C interface to the ESMF Mesh class
//
// !DESCRIPTION:
//
// The code in this file defines the public C Mesh class and declares method
// signatures (prototypes).  The companion file {\tt ESMC\_Mesh.C} contains
// the definitions (full code bodies) for the Mesh methods.
//
//EOP
//-----------------------------------------------------------------------------


extern "C" {

typedef struct {
  void* ptr;
} ESMC_Mesh;

typedef enum {ESMC_MESHELEMENT_QUAD=0, ESMC_MESHELEMENT_TRI=1,
     ESMC_MESHELEMENT_HEX=2, ESMC_MESHELEMENT_TET=3} ESMF_MeshElement;

ESMC_Mesh *ESMC_MeshCreate(int parametricDim, int spatialDim, int *rc);

int ESMC_MeshCreateDistGrids(ESMC_Mesh *mesh, int *num_nodes, int *num_elements);

int ESMC_MeshAddNodes(ESMC_Mesh *mesh, int *num_nodes, int *nodeIds, 
                      double *nodeCoords, int *nodeOwners);

int ESMC_MeshAddElements(ESMC_Mesh *mesh, int *num_elems, int *elementIds, 
                         int *elementTypes, int *elementConn);

int ESMC_MeshDestroy(ESMC_Mesh *mesh);

int ESMC_MeshFreeMemory(ESMC_Mesh *mesh);


//int ESMC_MeshCreateAll(ESMC_Mesh *mesh, int parametricDim, int *nodeIds, double *nodeCoords,
//             int *nodeOwners, int *elementIds, int *elementTypes, int *elementConn);

} // extern "C"
#endif
