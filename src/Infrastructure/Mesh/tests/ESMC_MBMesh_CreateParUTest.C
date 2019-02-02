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

#include "ESMC_MBMeshTestUtilMBMesh.C"

// other headers
#include "ESMCI_MBMesh.h"
#include "ESMCI_MBMesh_Glue.h"
#include "ESMCI_MBMesh_Search_EToP.h"
#include "ESMCI_MBMesh_Util.h"

#include "MBTagConventions.hpp"
#include "moab/Core.hpp"
#include "moab/ElemEvaluator.hpp"
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
  //ESMC_MoabSet(true);
#endif

  // Get parallel information
  vm=ESMC_VMGetGlobal(&rc);
  if (rc != ESMF_SUCCESS) return 0;

  rc=ESMC_VMGet(vm, &localPet, &petCount, (int *)NULL, (MPI_Comm *)NULL,
                (int *)NULL, (int *)NULL);
  if (rc != ESMF_SUCCESS) return 0;

  // --------------------------------------------------------------------------
  // get entities from meshes created with quadrilaterals
  // --------------------------------------------------------------------------
#if defined ESMF_MOAB

  //----------------------------------------------------------------------------
  //NEX_UTest_Multi_Proc_Only
  MBMesh *mesh_quad_10;
  mesh_quad_10 = create_mesh_quad_10_parallel(ESMC_COORDSYS_CART, rc);
  if (!mesh_quad_10) rc = ESMC_RC_PTR_NULL;
#else
  rc = ESMF_SUCCESS;
#endif
  strcpy(name, "Quadrilaterals 10mesh creation");
  strcpy(failMsg, "Mesh creation failed");
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest_Multi_Proc_Only
#if defined ESMF_MOAB
  Range range_quad_10;
  Interface *mb_mesh_quad_10=mesh_quad_10->mesh;
  int merr_quad_10=mb_mesh_quad_10->get_entities_by_dimension(0,mesh_quad_10->pdim,range_quad_10);
  if (merr_quad_10 != MB_SUCCESS) rc = ESMF_FAILURE;
  // clean up
  delete mesh_quad_10;
#else
  rc = ESMF_SUCCESS;
#endif

  strcpy(name, "get_entities");
  strcpy(failMsg, "Cannot get entities");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest_Multi_Proc_Only
#if defined ESMF_MOAB
  MBMesh *mesh_quad_9;
  mesh_quad_9 = create_mesh_quad_9_parallel(ESMC_COORDSYS_CART, rc);
  if (!mesh_quad_9) rc = ESMC_RC_PTR_NULL;
#else
  rc = ESMF_SUCCESS;
#endif
  strcpy(name, "Quadrilaterals 9 mesh creation");
  strcpy(failMsg, "Mesh creation failed");
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest_Multi_Proc_Only
#if defined ESMF_MOAB
  Range range_quad_9;
  Interface *mb_mesh_quad_9=mesh_quad_9->mesh;
  int merr_quad_9=mb_mesh_quad_9->get_entities_by_dimension(0,mesh_quad_9->pdim,range_quad_9);
  if (merr_quad_9 != MB_SUCCESS) rc = ESMF_FAILURE;

  // clean up
  delete mesh_quad_9;
#else
  rc = ESMF_SUCCESS;
#endif

  strcpy(name, "get_entities");
  strcpy(failMsg, "Cannot get entities");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);


  //----------------------------------------------------------------------------
  ESMC_TestEnd(__FILE__, __LINE__, 0);

  return 0;
}


