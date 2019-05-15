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

#include "ESMC_MBMeshTestUtilPL.C"

// other headers
#include "ESMCI_Regrid_Nearest.h"
#include "ESMCI_WMat.h"

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

  std::vector<double> weights;
  weights.resize(4);

  //----------------------------------------------------------------------------
  ESMC_TestStart(__FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  rc=ESMC_LogSet(true);

  //----------------------------------------------------------------------------

  // Get parallel information
  vm=ESMC_VMGetGlobal(&rc);
  if (rc != ESMF_SUCCESS) return 0;

  rc=ESMC_VMGet(vm, &localPet, &petCount, (int *)NULL, (MPI_Comm *)NULL,
                (int *)NULL, (int *)NULL);
  if (rc != ESMF_SUCCESS) return 0;

#if defined ESMF_MOAB

  // --------------------------------------------------------------------------
  // nearest neighbor Cartesian
  // --------------------------------------------------------------------------
  // build a pointlist
  PointList *pl_par;
  pl_par = create_pointlist_par(rc);
  
  // build a pointlist
  PointList *pl_quad;
  pl_quad = create_pointlist_for_quad_parallel(rc);
  
  // expected result
  std::fill(weights.begin(), weights.end(), UNINITVAL2);

  //----------------------------------------------------------------------------
  //NEX_Multi_Proc_Only_UTest
  strcpy(name, "Nearest neighbor weight generation");
  strcpy(failMsg, "Weights were not generated correctly");
  ESMC_Test((weight_gen_nearest(pl_par, pl_quad, weights)), name, failMsg, &result, __FILE__, __LINE__, 0);
  
  // clean up
  delete pl_quad;
  delete pl_par;

#else
  rc = ESMF_SUCCESS;
  strcpy(name, "Nearest neighbor weight generation");
  strcpy(failMsg, "Weights were not generated correctly");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);
#endif

  // --------------------------------------------------------------------------
  // Nearest neighbor spherical
  // --------------------------------------------------------------------------

#if defined ESMF_MOAB

  // build a mesh
  PointList *pl_par_sph;
  pl_par_sph = create_pointlist_par_sph(rc);
  
  // build a pointlist
  PointList *pl_quad_sph;
  pl_quad_sph = create_pointlist_sph_for_quad_parallel(rc);
  
  // expected result
  std::fill(weights.begin(), weights.end(), UNINITVAL2);

  //----------------------------------------------------------------------------
  //NEX_Multi_Proc_Only_UTest
  strcpy(name, "Spherical nearest neighbor weight generation");
  strcpy(failMsg, "Weights were not generated correctly");
  ESMC_Test((weight_gen_nearest(pl_par_sph, pl_quad_sph, weights)), name, failMsg, &result, __FILE__, __LINE__, 0);
  
  // clean up
  delete pl_quad_sph;
  delete pl_par_sph;

#else
  rc = ESMF_SUCCESS;
  strcpy(name, "Spherical nearest neighbor weight generation");
  strcpy(failMsg, "Weights were not generated correctly");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);
#endif

#if defined ESMF_MOAB

  // --------------------------------------------------------------------------
  // nearest neighbor N point average Cartesian
  // --------------------------------------------------------------------------
  // build a pointlist
  // PointList *pl_par;
  pl_par = create_pointlist_par(rc);
  
  // build a pointlist
  // PointList *pl_quad;
  pl_quad = create_pointlist_for_quad_parallel(rc);
  
  // expected result
  std::fill(weights.begin(), weights.end(), UNINITVAL2);

  //----------------------------------------------------------------------------
  //NEX_Multi_Proc_Only_UTest
  strcpy(name, "Nearest neighbor weight generation");
  strcpy(failMsg, "Weights were not generated correctly");
  ESMC_Test((weight_gen_nearest(pl_par, pl_quad, weights, 6, 4)), 
             name, failMsg, &result, __FILE__, __LINE__, 0);
  
  // clean up
  delete pl_quad;
  delete pl_par;

#else
  rc = ESMF_SUCCESS;
  strcpy(name, "Nearest neighbor weight generation");
  strcpy(failMsg, "Weights were not generated correctly");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);
#endif


  // --------------------------------------------------------------------------
  // Nearest neighbor N point average spherical
  // --------------------------------------------------------------------------

#if defined ESMF_MOAB

  // build a mesh
  // PointList *pl_par_sph;
  pl_par_sph = create_pointlist_par_sph(rc);
  
  // build a pointlist
  // PointList *pl_quad_sph;
  pl_quad_sph = create_pointlist_sph_for_quad_parallel(rc);
  
  // expected result
  std::fill(weights.begin(), weights.end(), UNINITVAL2);

  //----------------------------------------------------------------------------
  //NEX_Multi_Proc_Only_UTest
  strcpy(name, "Spherical nearest neighbor weight generation");
  strcpy(failMsg, "Weights were not generated correctly");
  ESMC_Test((weight_gen_nearest(pl_par_sph, pl_quad_sph, weights, 6, 4)), 
             name, failMsg, &result, __FILE__, __LINE__, 0);
  
  // clean up
  delete pl_quad_sph;
  delete pl_par_sph;

#else
  rc = ESMF_SUCCESS;
  strcpy(name, "Spherical nearest neighbor weight generation");
  strcpy(failMsg, "Weights were not generated correctly");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);
#endif

  //----------------------------------------------------------------------------
  ESMC_TestEnd(__FILE__, __LINE__, 0);

  return 0;
}


