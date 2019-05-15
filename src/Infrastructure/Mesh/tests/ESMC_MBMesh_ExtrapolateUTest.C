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
#include "ESMCI_MBMesh_Extrapolation.h"
#include "ESMCI_WMat.h"

#include <iostream>
#include <iterator>
#include <vector>
#include <cstring>


#if !defined (M_PI)
// for Windows...
#define M_PI 3.14159265358979323846
#endif

static double UNINITVAL3 = -42;

bool weights_correct_extrapolate(WMat &wts, std::vector<double> weights) {
  bool correct = true;
  if (weights[0] != UNINITVAL3) {
    int ind = 0;
    WMat::WeightMap::iterator mb = wts.begin_row(), me = wts.end_row();
    for(; mb != me; ++mb) {
      std::vector<WMat::Entry> row = mb->second;
      std::vector<WMat::Entry>::iterator vb = row.begin(), ve = row.end();
      for(; vb != ve; ++vb) {
        WMat::Entry rv = *vb;
        if (rv.value /= weights[ind]) correct = false;
        ++ind;
      }
    }
    if (weights.size() != ind) correct = false;
  }
  
  return correct;
}

#if defined ESMF_MOAB
bool extrapolate(PointList *pl1, PointList *pl, 
                 std::vector<double> weights,
                 int method = 1, int num_src_pnts = 0, double dist_exponent = 2,
                 int num_levels = 0, int num_input_levels = 0) {
  bool correct = false;
  int localrc;

  // early exit for ESMF_MOAB=OFF
  if (pl1 == NULL || pl == NULL)
    return true;
  
  // extrapolate with pointlists
  WMat wt, dst_status;
  WMat &wts = wt;
  WMat &ds = dst_status;
  
  MBMesh_Extrapolate(NULL, pl1, NULL, pl, wts,
                     NULL, // pole_constraint_id only valid with source mesh
                     &method, &num_src_pnts, &dist_exponent,
                     &num_levels, &num_input_levels,
                     true, ds, &localrc);

  // verify results
  if (weights_correct_extrapolate(wts, weights)) correct = true;

  // output weight matrix for debugging purposes
// #define OUTPUT_WEIGHTS
#ifdef OUTPUT_WEIGHTS
  std::cout << std::endl << "Extrapolation Weight Matrix" << std::endl;
  // print out weights
  WMat::WeightMap::iterator mb = wts.begin_row(), me = wts.end_row();
  for(; mb != me; ++mb) {
    WMat::Entry col = mb->first;
    std::vector<WMat::Entry> row = mb->second;

    std::cout << "[" << col.id << "," << col.idx << "," << col.value << ","
         << col.src_id << "] - ";

    std::vector<WMat::Entry>::iterator vb = row.begin(), ve = row.end();
    for(; vb != ve; ++vb) {
      WMat::Entry rv = *vb;
      std::cout << "[" << rv.id << "," << rv.idx << "," << rv.value << ","
           << rv.src_id << "] ";
    }
    std::cout << std::endl;
  }
  std::cout << std::endl;
#endif
  return correct;
}
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
  std::fill(weights.begin(), weights.end(), UNINITVAL3);
  
  //----------------------------------------------------------------------------
  //NEX_Multi_Proc_Only_UTest

  strcpy(name, "Extrapolation");
  strcpy(failMsg, "Weights were not generated correctly");
  ESMC_Test((extrapolate(pl_par, pl_quad, weights)), name, failMsg, &result, __FILE__, __LINE__, 0);
  

  // clean up
  delete pl_quad;
  delete pl_par;

#else
  rc = ESMF_SUCCESS;
  strcpy(name, "Extrapolation");
  strcpy(failMsg, "Weights were not generated correctly");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);
#endif

  #if defined ESMF_MOAB

  // --------------------------------------------------------------------------
  // Nearest neighbor spherical
  // --------------------------------------------------------------------------

  // build a mesh
  PointList *pl_par_sph;
  pl_par_sph = create_pointlist_par_sph(rc);
  
  // build a pointlist
  PointList *pl_quad_sph;
  pl_quad_sph = create_pointlist_sph_for_quad_parallel(rc);
  
  // expected result
  std::fill(weights.begin(), weights.end(), UNINITVAL3);

  //----------------------------------------------------------------------------
  //NEX_Multi_Proc_Only_UTest
  strcpy(name, "Spherical extrapolation");
  strcpy(failMsg, "Weights were not generated correctly");
  ESMC_Test((extrapolate(pl_par_sph, pl_quad_sph, weights)), name, failMsg, &result, __FILE__, __LINE__, 0);
  
  // clean up
  delete pl_quad_sph;
  delete pl_par_sph;

#else
  rc = ESMF_SUCCESS;
  strcpy(name, "Extrapolation");
  strcpy(failMsg, "Weights were not generated correctly");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);
#endif

  //----------------------------------------------------------------------------
  ESMC_TestEnd(__FILE__, __LINE__, 0);

  return 0;
}


