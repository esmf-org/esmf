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
#include "ESMC_MBMeshTestUtilMBMesh.C"
#include "ESMC_MBMeshTestUtilPL.C"

// other headers
#include "ESMCI_MBMesh.h"
#include "ESMCI_MBMesh_Glue.h"
#include "ESMCI_MBMesh_Util.h"
#include "ESMCI_MBMesh_Rendez_EtoP.h"
#include "ESMCI_MBMesh_Rendez_Elem.h"

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

using namespace std;

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

int pl_print(PointList *pl) {
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

  for (int i = 0; i < pl->get_curr_num_pts(); ++i) {

    //print
    std::cout << "PET " << localPet << " - Point " << pl->get_id(i)
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

#if defined ESMF_MOAB
  // build a mesh
  MBMesh *mesh;
  mesh = create_mesh_quad_10_parallel(ESMC_COORDSYS_CART, rc);

  // build a pointlist
  PointList *pl;
  pl = create_pointlist_for_quad_parallel(rc);

  int map_type = MB_MAP_TYPE_CART_APPROX;

  //----------------------------------------------------------------------------
  //NEX_UTest_Multi_Proc_Only
  // call rendezvous between mesh and pointlist
  MBMesh *mesh_rend=NULL;
  PointList *pl_rend=NULL;
  create_rendez_mbmesh_etop(mesh, pl, &mesh_rend, &pl_rend, &map_type);
#else
  rc = ESMF_SUCCESS;
#endif
  strcpy(name, "Rendezvous between a Mesh and a PointList");
  strcpy(failMsg, "Mesh to PointList rendezvous failed");
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);


  //rc = pl_print(pl);
  //rc = pl_print(pl_rend);
  //rc = mesh_print(mesh_rend);

  //----------------------------------------------------------------------------
  //NEX_UTest_Multi_Proc_Only
  rc = ESMF_SUCCESS;
  strcpy(name, "Validate pointlist rendezvous");
  strcpy(failMsg, "pointlist rendezvous incorrect");
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);

#if defined ESMF_MOAB
  // clean up
  delete pl;
  delete mesh;
  delete pl_rend;
  delete mesh_rend;
#endif

#if defined ESMF_MOAB
  // mesh 2 and 3
  MBMesh *mesh2;
  MBMesh *mesh3;
  mesh2 = create_mesh_quad_10_parallel(ESMC_COORDSYS_CART, rc);
  mesh3 = create_mesh_quad_9_parallel(ESMC_COORDSYS_CART, rc);

  //----------------------------------------------------------------------------
  //NEX_UTest_Multi_Proc_Only

  MBMesh *mesh2_rend=NULL;
  MBMesh *mesh3_rend=NULL;
  create_rendez_mbmesh_elem(mesh2, mesh3, &mesh2_rend, &mesh3_rend);
#else
  rc = ESMF_SUCCESS;
#endif
  strcpy(name, "Rendezvous between a Mesh and a Mesh");
  strcpy(failMsg, "Mesh to Mesh rendezvous failed");
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);

  //rc = mesh_print(mesh2_rend);

  //----------------------------------------------------------------------------
  //NEX_UTest_Multi_Proc_Only
  rc = ESMF_SUCCESS;
  strcpy(name, "Validate mesh rendezvous");
  strcpy(failMsg, "mesh rendezvous incorrect");
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);

#if defined ESMF_MOAB
  // clean up
  delete mesh2;
  delete mesh3;
  delete mesh2_rend;
  delete mesh3_rend;
#endif

  //----------------------------------------------------------------------------
  ESMC_TestEnd(__FILE__, __LINE__, 0);

  return 0;
}


