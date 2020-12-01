// $Id$
//==============================================================================
//
// Earth System Modeling Framework
// Copyright 2002-2020, University Corporation for Atmospheric Research,
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

#include "ESMC_MBMeshTestGenMBMesh.C"
#include "ESMC_MBMeshTestGenPL.C"

// other headers
#include "ESMCI_MBMesh.h"
#include "ESMCI_MBMesh_Glue.h"
#include "ESMCI_MBMesh_Util.h"

#include "MBTagConventions.hpp"
#include "moab/Core.hpp"
#endif

#include <iostream>
#include <iterator>
#include <vector>
#include <cstring>

#if !defined (M_PI)
// for Windows...
#define M_PI 3.14159265358979323846
#endif

#if defined ESMF_MOAB

int mesh_print(MBMesh *mesh) {
  int rc;
  int localPet, petCount;
  ESMC_VM vm;

  rc = ESMF_RC_NOT_IMPL;


  // Get parallel information
  vm=ESMC_VMGetGlobal(&rc);
  if (rc != ESMF_SUCCESS) return 0;

  rc=ESMC_VMGet(vm, &localPet, &petCount, (int *)NULL, (MPI_Comm *)NULL,
                (int *)NULL, (int *)NULL);
  if (rc != ESMF_SUCCESS) return 0;

  //Get MOAB Mesh
  Interface *interface=mesh->mesh;

  // Get regions, by dimension, so we stay generic to entity type
  Range elems;
  int rval = interface->get_entities_by_dimension(0, 2, elems);

  printf("MESH PET %d - size = %d\n", localPet, elems.size());

  for (Range::iterator it = elems.begin(); it != elems.end(); ++it) {
    printf(" * ");
    EntityHandle elem = *it;

    int elem_id = interface->id_from_handle(elem);
    //print
    MBMesh_get_gid(mesh, elem, &elem_id);
    std::cout << "PET " << localPet << " - Element " << elem_id
              //<< ": coords = [" << coords[0] << ", " << coords[1] << "]"
              << std::endl;
  }

  return 0;
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

  // common vector for pointlist verification
  std::vector<double*> cv;

#if defined ESMF_MOAB

  // build a mesh
  MBMesh *mesh;
  mesh = create_mesh_quad_10_parallel(ESMC_COORDSYS_CART, rc);

  int num_elem_gids;
  int elem_gids[10];

  int num_node_gids;
  int node_gids[16];

  // total redist case
  if (localPet == 0) {
    num_elem_gids = 1;
    elem_gids[0] = 10;

    num_node_gids = 1;
    node_gids[0] = 160;
  } else if (localPet == 1) {
    num_elem_gids = 2;
    elem_gids[0] = 8;
    elem_gids[1] = 9;

    num_node_gids = 3;
    node_gids[0] = 130;
    node_gids[1] = 140;
    node_gids[2] = 150;
  } else if (localPet == 2) {
    num_elem_gids = 2;
    elem_gids[0] = 4;
    elem_gids[1] = 7;

    num_node_gids = 3;
    node_gids[0] = 40;
    node_gids[1] = 80;
    node_gids[2] = 120;
  } else if (localPet == 3) {
    num_elem_gids = 5;
    elem_gids[0] = 1;
    elem_gids[1] = 2;
    elem_gids[2] = 3;
    elem_gids[3] = 5;
    elem_gids[4] = 6;

    num_node_gids = 9;
    node_gids[0] = 10;
    node_gids[1] = 20;
    node_gids[2] = 30;
    node_gids[3] = 50;
    node_gids[4] = 60;
    node_gids[5] = 70;
    node_gids[6] = 90;
    node_gids[7] = 100;
    node_gids[8] = 110;
  }

  // noop case
  // if (localPet == 3) {
  //   num_elem_gids = 1;
  //   elem_gids[0] = 10;
  // } else if (localPet == 2) {
  //   num_elem_gids = 2;
  //   elem_gids[0] = 8;
  //   elem_gids[1] = 9;
  // } else if (localPet == 1) {
  //   num_elem_gids = 2;
  //   elem_gids[0] = 4;
  //   elem_gids[1] = 7;
  // } else if (localPet == 0) {
  //   num_elem_gids = 5;
  //   elem_gids[0] = 1;
  //   elem_gids[1] = 2;
  //   elem_gids[2] = 3;
  //   elem_gids[3] = 5;
  //   elem_gids[4] = 6;
  // }

  MBMesh *out_mesh;

  //----------------------------------------------------------------------------
  //NEX_disable_UTest_Multi_Proc_Only
  // call redist algorithm
  MBMesh_createredistnodes(&mesh, &num_node_gids, node_gids, &out_mesh, &rc);
  // MBMesh_createredistelems(&mesh, &num_elem_gids, elem_gids, &out_mesh, &rc);
#else
  rc = ESMF_SUCCESS;
#endif
  strcpy(name, "Redist of a Mesh");
  strcpy(failMsg, "MeshRedist failed");
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);



#if defined ESMF_MOAB
  // clean up
  delete mesh;
  delete out_mesh;
#endif

  //----------------------------------------------------------------------------
  ESMC_TestEnd(__FILE__, __LINE__, 0);

  return 0;
}


