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

#if defined ESMF_MOAB
typedef std::map<WMat::Entry, std::vector<WMat::Entry> > WeightMap;
WeightMap weights;
WeightMap::iterator begin_row() { return weights.begin(); }
WeightMap::iterator end_row() { return weights.end(); }

bool weight_gen(MBMesh *mesh, PointList *pl) {
  int rc = ESMF_RC_NOT_IMPL;
  char name[80];
  char failMsg[80];
  int result = 0;

  // do bilinear regridding between mesh and pointlist
  IWeights wt;
  IWeights &wts = wt;
  calc_bilinear_regrid_wgts(mesh, pl, wts);

  cout << endl << "Bilinear Weight Matrix" << endl;
  // print out weights
  WeightMap::iterator mb = wts.begin_row(), me = wts.end_row();
  for(; mb != me; ++mb) {
    WMat::Entry col = mb->first;
    vector<WMat::Entry> row = mb->second;

    cout << "[" << col.id << "," << col.idx << "," << col.value << ","
         << col.src_id << "] - ";

    vector<WMat::Entry>::iterator vb = row.begin(), ve = row.end();
    for(; vb != ve; ++vb) {
      WMat::Entry rv = *vb;
      cout << "[" << rv.id << "," << rv.idx << "," << rv.value << ","
           << rv.src_id << "] ";
    }
    cout << endl;
  }
  cout << endl;

  rc = ESMF_SUCCESS;

  if (rc == ESMF_SUCCESS) return true;
  else return false;

}
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

  #if defined ESMF_MOAB
  // build a mesh
  MBMesh *mesh_quad;
  mesh_quad = create_mesh_quad(rc);
  
  // build a pointlist
  PointList *pl_quad;
  pl_quad = create_pointlist_for_quad(&cv, rc);
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Quadrilateral bilinear weight generation");
  strcpy(failMsg, "Weights were not generated correctly");
  ESMC_Test((weight_gen(mesh_quad, pl_quad)), name, failMsg, &result, __FILE__, __LINE__, 0);
  
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
  ESMC_Test((weight_gen(mesh_quad_sph, pl_quad_sph)), name, failMsg, &result, __FILE__, __LINE__, 0);
  
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
  ESMC_Test((weight_gen(mesh_tri, pl_tri)), name, failMsg, &result, __FILE__, __LINE__, 0);

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
  ESMC_Test((weight_gen(mesh_tri_sph, pl_tri_sph)), name, failMsg, &result, __FILE__, __LINE__, 0);
  
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


