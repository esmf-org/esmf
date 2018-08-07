// $Id$
//==============================================================================
//
// Earth System Modeling Framework
// Copyright 2002-2016, University Corporation for Atmospheric Research,
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

using namespace std;

int main(int argc, char *argv[]) {

  char name[80];
  char failMsg[80];
  int result = 0;
  int rc;
  int localPet, petCount;
  ESMC_VM vm;

#if defined ESMF_MOAB
  // common vector for pointlist verification
  vector<double*> cv;
  bool cart = false;

  vector<double> weights;
  weights.resize(4);
#endif

  //----------------------------------------------------------------------------
  ESMC_TestStart(__FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  rc=ESMC_LogSet(true);

  //----------------------------------------------------------------------------
  //ESMC_MoabSet(true);

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
  MBMesh *mesh_quad;
  mesh_quad = create_mesh_quad(rc);
  
  // build a pointlist
  PointList *pl_quad;
  pl_quad = create_pointlist_for_quad(&cv, rc);
  
  // expected result
  std::fill(weights.begin(), weights.end(), UNINITVAL);

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Quadrilateral bilinear weight generation");
  strcpy(failMsg, "Weights were not generated correctly");
  ESMC_Test((weight_gen(mesh_quad, pl_quad, weights, cart)), name, failMsg, &result, __FILE__, __LINE__, 0);
  
  // clean up
  delete pl_quad;
  delete mesh_quad;
#else
  rc = ESMF_SUCCESS;
  strcpy(name, "Quadrilateral bilinear weight generation");
  strcpy(failMsg, "Weights were not generated correctly");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);
#endif

  // --------------------------------------------------------------------------
  // quad sph mesh bilinear
  // --------------------------------------------------------------------------

#if defined ESMF_MOAB
  cart = false;
  // build a mesh
  MBMesh *mesh_quad_sph;
  mesh_quad_sph = create_mesh_quad_sph(rc);
  
  // build a pointlist
  PointList *pl_quad_sph;
  pl_quad_sph = create_pointlist_for_quad_sph(&cv, rc);
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Spherical quadrilateral bilinear weight generation");
  strcpy(failMsg, "Weights were not generated correctly");
  ESMC_Test((weight_gen(mesh_quad_sph, pl_quad_sph, weights, cart)), name, failMsg, &result, __FILE__, __LINE__, 0);
  
  // clean up
  delete pl_quad_sph;
  delete mesh_quad_sph;
#else
  rc = ESMF_SUCCESS;
  strcpy(name, "Spherical quadrilateral bilinear weight generation");
  strcpy(failMsg, "Weights were not generated correctly");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);
#endif

  // --------------------------------------------------------------------------
  // tri mesh bilinear
  // --------------------------------------------------------------------------

#if defined ESMF_MOAB
  cart = true;
  // build a mesh
  MBMesh *mesh_tri;
  mesh_tri = create_mesh_tri(rc);

  // build a pointlist
  PointList *pl_tri;
  pl_tri = create_pointlist_for_tri(&cv, rc);

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Triangle mesh bilinear weight generation");
  strcpy(failMsg, "Weights were not generated correctly");
  ESMC_Test((weight_gen(mesh_tri, pl_tri, weights, cart)), name, failMsg, &result, __FILE__, __LINE__, 0);

  // clean up
  delete pl_tri;
  delete mesh_tri;
#else
  rc = ESMF_SUCCESS;
  strcpy(name, "Triangle mesh bilinear weight generation");
  strcpy(failMsg, "Weights were not generated correctly");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);
#endif

  // --------------------------------------------------------------------------
  // tri sph mesh bilinear
  // --------------------------------------------------------------------------

#if defined ESMF_MOAB
  cart = false;
  // build a mesh
  MBMesh *mesh_tri_sph;
  mesh_tri_sph = create_mesh_tri_sph(rc);
  
  // build a pointlist
  PointList *pl_tri_sph;
  pl_tri_sph = create_pointlist_for_tri_sph(&cv, rc);
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Spherical triangle mesh bilinear weight generation");
  strcpy(failMsg, "Weights were not generated correctly");
  ESMC_Test((weight_gen(mesh_tri_sph, pl_tri_sph, weights, cart)), name, failMsg, &result, __FILE__, __LINE__, 0);
  
  // clean up
  delete pl_tri_sph;
  delete mesh_tri_sph;
#else
  rc = ESMF_SUCCESS;
  strcpy(name, "Spherical triangle mesh bilinear weight generation");
  strcpy(failMsg, "Weights were not generated correctly");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);
#endif

  //----------------------------------------------------------------------------
  ESMC_TestEnd(__FILE__, __LINE__, 0);

  return 0;
}


