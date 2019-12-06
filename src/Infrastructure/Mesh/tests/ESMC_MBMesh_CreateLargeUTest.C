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

// other headers
#include "ESMCI_MBMesh.h"
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
MBMesh* create_mesh_large(int &rc) {
  //
  // create a mesh large enough to stress the 2,147,483,647 limit of a 
  // 32 byte integer
  //

  int merr, localrc;

  rc = ESMF_RC_NOT_IMPL;

  // ESMC_CoordSys_Flag local_coordSys=ESMC_COORDSYS_CART;
  // 
  // MBMesh *mesh = new MBMesh();
  // Interface *moab_mesh = mesh->mesh;
  // void *meshp = static_cast<void *> (mesh);  
  // 
  // MBMesh_create(&meshp, &pdim, &sdim, &local_coordSys, &rc);
  // if (rc != ESMF_SUCCESS) return NULL;
  
  // New Mesh
  MBMesh *mbmp = new MBMesh();

  // Create MOAB Mesh
  Interface *moab_mesh=new Core();

  // Default value
  long long int long_def_val = 0;
  double dbl_def_val[3] = {0.0, 0.0, 0.0};

  // Setup global id tag
  long_def_val=0;
  merr=moab_mesh->tag_get_handle("global_id_long", 8, MB_TYPE_OPAQUE, mbmp->gid_tag, MB_TAG_BYTES, &long_def_val);
  if (merr != MB_SUCCESS) return NULL;
  
  // Setup owner tag
  int_def_val=-1;
  merr=moab_mesh->tag_get_handle("owner", 1, MB_TYPE_INTEGER, mbmp->owner_tag, MB_TAG_EXCL|MB_TAG_DENSE, &int_def_val);
  if (merr != MB_SUCCESS) return NULL;
  
  // Set Moab Mesh
  mbmp->mesh=moab_mesh;

  // Set dimensions
  mbmp->pdim=2;
  mbmp->sdim=2;

  ESMC_I8 num_verts = 2147483648;
  EntityHandle *vert;
  double cart_coords[3];

  // Create new nodes
  for (int n = 0; n < num_verts; ++n) {
    vert = new EntityHandle;
    
    cart_coords[0]=n*0.01; cart_coords[1]=n*0.01; cart_coords[2]=n*0.01;

    // Add vertex
    merr=moab_mesh->create_vertex(cart_coords, *vert);
    if (merr != MB_SUCCESS) return NULL;

    // Set Ids
    merr=moab_mesh->tag_set_data(mbmp->gid_tag, vert, 1, &n);
    if (merr != MB_SUCCESS) return NULL;
      
    // TODO: segfault here??
    // Set Owners
    // merr=moab_mesh->tag_set_data(mbmp->owner_tag, vert, 1, 0);
    if (merr != MB_SUCCESS) return NULL;
    
    if (n % 1000000 == 0) printf("%d\n", n);
  }
  

  if (rc != ESMF_SUCCESS) return NULL;

  rc = ESMF_SUCCESS;
  return mbmp;
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

  // --------------------------------------------------------------------------
  // large mesh stressing the 32 bit integer limit
  // --------------------------------------------------------------------------
#if defined ESMF_MOAB

  //----------------------------------------------------------------------------
  //NEX_UTest
  MBMesh *mesh_large;
  mesh_large = create_mesh_large(rc);
  if (!mesh_large) rc = ESMC_RC_PTR_NULL;
  
  // clean up
  delete mesh_large;
#else
  rc = ESMF_SUCCESS;
#endif
  strcpy(name, "Large mesh creation");
  strcpy(failMsg, "Mesh creation failed");
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);


  //----------------------------------------------------------------------------
  ESMC_TestEnd(__FILE__, __LINE__, 0);

  return 0;
}


