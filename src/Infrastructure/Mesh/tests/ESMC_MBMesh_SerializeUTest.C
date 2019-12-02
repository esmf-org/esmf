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
#include "ESMCI_MBMesh_Util.h"
#include "ESMCI_Util.h"


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

int mesh_print_void(void *mesh) {
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

  MBMesh *mbmesh = static_cast<MBMesh*> (mesh); 

  //Get MOAB Mesh
  Interface *interface=mbmesh->mesh;

  // Get regions, by dimension, so we stay generic to entity type
  Range elems;
  int rval = interface->get_entities_by_dimension(0, 2, elems);

  printf("MESH PET %d - size = %d\n", localPet, elems.size());

  for (Range::iterator it = elems.begin(); it != elems.end(); ++it) {
    printf(" * ");
    EntityHandle elem = *it;

    int elem_id = interface->id_from_handle(elem);
    //print
    MBMesh_get_gid(mbmesh, elem, &elem_id);
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

#if defined ESMF_MOAB

  // build a mesh
  MBMesh *mesh;
  mesh = create_mesh_quad(rc);
  
  void *meshp = static_cast<void *> (mesh);
  void *meshp2;

  int len = 4*sizeof(int);
  char *buffer = new char[len];
  int length = len; 
  int offset = 0;
  // ESMF_INQUIREONLY=ESMF_TRUE,
  // ESMF_NOINQUIRE=ESMF_FALSE
  ESMC_InquireFlag inquireflag = ESMF_NOINQUIRE;
  ESMCI_FortranStrLenArg buffer_l = len;


  //----------------------------------------------------------------------------
  //NEX_UTest
  MBMesh_serialize(&meshp, buffer, &length, &offset, &inquireflag, &rc, buffer_l);
#else
  rc = ESMF_SUCCESS;
#endif
  strcpy(name, "Serialization of an MBMesh");
  strcpy(failMsg, "Serialization of an MBMesh failed");
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);

#if defined ESMF_MOAB

  rc = mesh_print_void(meshp);

  offset = 0;
  //----------------------------------------------------------------------------
  //NEX_UTest
  MBMesh_deserialize(&meshp2, buffer, &offset, &rc, buffer_l);
#else
  rc = ESMF_SUCCESS;
#endif
  strcpy(name, "Deserialization of an MBMesh");
  strcpy(failMsg, "Deserialization of an MBMesh failed");
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);

#if defined ESMF_MOAB

  rc = mesh_print_void(meshp2);

  // clean up
  delete buffer;
  delete mesh;
  // cast back to MBMesh for proper deallocation of void*
  MBMesh *meshp2mb = static_cast<MBMesh *> (meshp2);
  delete meshp2mb;
#endif

  //----------------------------------------------------------------------------
  ESMC_TestEnd(__FILE__, __LINE__, 0);

  return 0;
}


