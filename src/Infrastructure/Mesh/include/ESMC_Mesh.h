// $Id: ESMC_Mesh.h,v 1.24.2.1 2010/02/05 19:59:29 svasquez Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// This file is part of the pure C public ESMC API
//-----------------------------------------------------------------------------

//-------------------------------------------------------------------------
// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
// these lines prevent this file from being read more than once if it
// ends up being included multiple times

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

#if defined (__cplusplus)
extern "C" {
#endif

// Class declaration type
typedef struct{
  void *ptr;
}ESMC_Mesh;

// Class API
ESMC_Mesh ESMC_MeshCreate(int *parametricDim, int *spatialDim, int *rc);

int ESMC_MeshCreateDistGrids(ESMC_Mesh mesh, int* nodeDistGrid,
  int* elemDistGrid, int *num_nodes, int *num_elements);

int ESMC_MeshDestroy(ESMC_Mesh *mesh);

int ESMC_MeshAddNodes(ESMC_Mesh mesh, int *num_nodes, int *nodeIds, 
  double *nodeCoords, int *nodeOwners);

int ESMC_MeshAddElements(ESMC_Mesh mesh, int *num_elems, int *elementIds, 
  int *elementTypes, int *elementConn);

int ESMC_MeshFreeMemory(ESMC_Mesh mesh);

int ESMC_MeshWrite(ESMC_Mesh mesh, char* fname);

int ESMC_MeshGetNumNodes(ESMC_Mesh mesh, int* num_nodes);

int ESMC_MeshGetNumElements(ESMC_Mesh mesh, int* num_elems);

// Associated methods

int ESMC_MeshVTKHeader(char *fname, int *num_elem, int *num_node,
  int *conn_size);

int ESMC_MeshVTKBody(char *fname, int *nodeId, double *nodeCoord,
  int *nodeOwner, int *elemId, int *elemType, int *elemConn);

// Associated enum types
typedef enum {ESMC_MESHELEMENT_QUAD=0, ESMC_MESHELEMENT_TRI=1,
  ESMC_MESHELEMENT_HEX=2, ESMC_MESHELEMENT_TET=3} ESMF_MeshElement;

#if defined (__cplusplus)
} // extern "C"
#endif

#endif  // ESMC_Mesh_h
