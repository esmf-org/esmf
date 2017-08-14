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

#include "ESMC_MBMeshTestUtilMBMesh.C"

// other headers
#include "ESMCI_MBMesh.h"
#include "ESMCI_MBMesh_Util.h"

#include "MBTagConventions.hpp"
#include "moab/Core.hpp"

#include <iostream>
#include <iterator>
#include <vector>


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

  // common vector for pointlist verification
  vector<double*> cv;

  // --------------------------------------------------------------------------
  // quad mesh bilinear
  // --------------------------------------------------------------------------

  // build a mesh
  MBMesh *mesh_quad;
  mesh_quad = create_mesh_quad(rc);

  // build a pointlist
  PointList *pl_quad;
  pl_quad = MBMesh_to_PointList(mesh_quad, &rc);

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Quadrilateral mesh pointlist generation");
  strcpy(failMsg, "Mesh to Pointlist did not work correctly");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);

  // clean up
  delete pl_quad;
  delete mesh_quad;

  // --------------------------------------------------------------------------
  // spherical quad mesh bilinear
  // --------------------------------------------------------------------------

  // build a mesh
  MBMesh *mesh_quad_sph;
  mesh_quad_sph = create_mesh_quad_sph(rc);

  // build a pointlist
  PointList *pl_quad_sph;
  pl_quad_sph = MBMesh_to_PointList(mesh_quad_sph, &rc);

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Spherical quadrilateral mesh pointlist generation");
  strcpy(failMsg, "Mesh to Pointlist to not work correctly");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);

  // clean up
  delete pl_quad_sph;
  delete mesh_quad_sph;

  //----------------------------------------------------------------------------
  ESMC_TestEnd(__FILE__, __LINE__, 0);

  return 0;
}


