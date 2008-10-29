// $Id: ESMC_Mesh.C,v 1.1 2008/10/29 14:52:39 rosalind Exp $
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
// This file contains the C interfaces' code for the Mesh class.
//
//------------------------------------------------------------------------------
// INCLUDES

#include "ESMC_Mesh.h"

using namespace ESMCI;

extern "C" {
ESMC_Mesh *ESMC_MeshCreate(int parametricDim, int spatialDim, int *rc) {
  int localrc;
  ESMC_Mesh *mesh;

  Mesh* me;
  me = new Mesh();

  FTN(c_esmc_meshcreate)(&me, &parametricDim, &spatialDim, &localrc);

  if (rc) *rc = localrc;
  mesh->ptr = (void*)me;

  return mesh;

} // ESMC_MeshCreate

} // extern "C"

