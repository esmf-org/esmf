// $Id$
//==============================================================================
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
#ifndef MPICH_IGNORE_CXX_SEEK
#define MPICH_IGNORE_CXX_SEEK
#endif
#include <mpi.h>

// ESMF header
#include "ESMC.h"

// ESMF Test header
#include "ESMC_Test.h"

#if defined ESMF_MOAB
#include "ESMC_MBMeshTestUtilMesh.C"
#include "ESMC_MBMeshTestUtilMBMesh.C"
#include "ESMC_MBMeshTestUtilPL.C"

// other headers
#include "ESMCI_MBMesh.h"
#include "ESMCI_MBMesh_Glue.h"
#include "ESMCI_MBMesh_Bilinear.h"
#include "ESMCI_MBMesh_Util.h"

#include "MBTagConventions.hpp"
#include "moab/Core.hpp"

#include "ESMCI_WMat.h"
#endif

#include <iostream>
#include <iterator>
#include <vector>
#include <cstring>


#if !defined (M_PI)
// for Windows...
#define M_PI 3.14159265358979323846
#endif

int main(int argc, char *argv[]) {

  char name[80];
  char failMsg[80];
  int result = 0;
  int rc;
  int localPet, petCount;
  ESMC_VM vm;

  //----------------------------------------------------------------------------
  ESMC_TestStart(__FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  rc=ESMC_LogSet(true);

#if defined ESMF_MOAB
  //----------------------------------------------------------------------------
  bool cart = false;

  std::vector<double> weights;
  weights.resize(4);
#endif

  // Get parallel information
  vm=ESMC_VMGetGlobal(&rc);
  if (rc != ESMF_SUCCESS) return 0;

  rc=ESMC_VMGet(vm, &localPet, &petCount, (int *)NULL, (MPI_Comm *)NULL,
                (int *)NULL, (int *)NULL);
  if (rc != ESMF_SUCCESS) return 0;

  // --------------------------------------------------------------------------
  // quad mesh bilinear
  // --------------------------------------------------------------------------

#if defined ESMF_MOAB
  cart = true;
  // build a mesh
  MBMesh *mesh_quad_par;
  mesh_quad_par = create_mesh_quad_10_parallel(ESMC_COORDSYS_CART, rc);

  // build a pointlist
  PointList *pl_quad_par;
  pl_quad_par = create_pointlist_for_quad_parallel(rc);

  // expected result
  std::fill(weights.begin(), weights.end(), UNINITVAL);

  //----------------------------------------------------------------------------
  //NEX_UTest_Multi_Proc_Only
  strcpy(name, "Quadrilateral bilinear weight generation");
  strcpy(failMsg, "Weights were not generated correctly");
  ESMC_Test((weight_gen(mesh_quad_par, pl_quad_par, weights, cart)), name, failMsg, &result, __FILE__, __LINE__, 0);

  // clean up
  delete pl_quad_par;
  delete mesh_quad_par;
#else
  rc = ESMF_SUCCESS;
  strcpy(name, "Quadrilateral bilinear weight generation");
  strcpy(failMsg, "Weights were not generated correctly");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);
#endif

  // --------------------------------------------------------------------------
  // spherical quad mesh bilinear
  // --------------------------------------------------------------------------

#if defined ESMF_MOAB
  cart = false;
  // build a mesh
  MBMesh *mesh_quad_sph_par;
  mesh_quad_sph_par = create_mesh_quad_sph_10_parallel(ESMC_COORDSYS_SPH_RAD, rc);

  // build a pointlist
  PointList *pl_quad_sph_par;
  pl_quad_sph_par = create_pointlist_sph_for_quad_parallel(rc);

  //----------------------------------------------------------------------------
  //NEX_UTest_Multi_Proc_Only
  strcpy(name, "Spherical quadrilateral bilinear weight generation");
  strcpy(failMsg, "Weights were not generated correctly");
  ESMC_Test((weight_gen(mesh_quad_sph_par, pl_quad_sph_par, weights, cart)), name, failMsg, &result, __FILE__, __LINE__, 0);

  // clean up
  delete pl_quad_sph_par;
  delete mesh_quad_sph_par;
#else
  rc = ESMF_SUCCESS;
  strcpy(name, "Spherical quadrilateral bilinear weight generation");
  strcpy(failMsg, "Weights were not generated correctly");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);
#endif

  //----------------------------------------------------------------------------
  ESMC_TestEnd(__FILE__, __LINE__, 0);

  return 0;
}


