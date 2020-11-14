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
#include "ESMC_MBMeshTestGen.C"

#include "ESMCI_MBMesh.h"
// #include "ESMCI_MBMesh_Glue.h"
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
  int rc, localrc;
  int localPet, petCount;
  ESMC_VM vm;

  bool do_redist = true;

  //----------------------------------------------------------------------------
  ESMC_TestStart(__FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  rc=ESMC_LogSet(true);

  // Get parallel information
  vm=ESMC_VMGetGlobal(&rc);
  rc=ESMC_VMGet(vm, &localPet, &petCount, (int *)NULL, (MPI_Comm *)NULL,
                (int *)NULL, (int *)NULL);

  if ((petCount == 1) && (do_redist == true)) {
    printf("Can't redist on only one core, resetting redist to false");
    do_redist = false;
}

  // --------------------------------------------------------------------------
  // quad mesh Cartesian
  // --------------------------------------------------------------------------
#if defined ESMF_MOAB
  rc = ESMF_FAILURE;
  // set up a MBMeshTest object
  MBMeshTest *mbt_quad = mbmesh_gen_quad_2d_cart(localrc, do_redist);
  // create the MBMesh from Test object
  if (localrc == ESMF_SUCCESS) localrc = mbt_quad->build_mbmesh();
  // call into get test routine
  if (localrc == ESMF_SUCCESS) rc = mbt_quad->test_get_info();
#else
  rc = ESMF_SUCCESS;
#endif
  //NEX_UTest
  strcpy(name, "MBMeshGet - Cartesian Quadrilateral");
  strcpy(failMsg, "FAIL");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);

  // now do the same for a redist version of the same mesh
#if defined ESMF_MOAB
  rc = ESMF_FAILURE;
  MBMeshTest *mbt_quad_redist = new MBMeshTest();
  if (do_redist) {
    if (localrc == ESMF_SUCCESS) localrc = mbt_quad->mbtRedist(mbt_quad_redist);
    if (localrc == ESMF_SUCCESS) localrc = mbt_quad_redist->build_mbmesh_redist();
    if (localrc == ESMF_SUCCESS) rc = mbt_quad_redist->test_get_info();
  }
#else
  rc = ESMF_SUCCESS;
#endif
  //NEX_UTest
  strcpy(name, "MBMeshGet - Cartesian Quadrilateral Redist");
  strcpy(failMsg, "FAIL");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);

#if defined ESMF_MOAB
  delete mbt_quad;
  delete mbt_quad_redist;
#endif

  // --------------------------------------------------------------------------
  // quad mesh spherical
  // --------------------------------------------------------------------------
#if defined ESMF_MOAB
  rc = ESMF_FAILURE;
  MBMeshTest *mbt_quad_sph = mbmesh_gen_quad_2d_sph(localrc, do_redist);
  if (localrc == ESMF_SUCCESS) localrc = mbt_quad_sph->build_mbmesh();
  if (localrc == ESMF_SUCCESS) rc = mbt_quad_sph->test_get_info();
#else
  rc = ESMF_SUCCESS;
#endif
  //NEX_UTest
  strcpy(name, "MBMeshGet - Spherical Quadrilateral");
  strcpy(failMsg, "FAIL");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);

#if defined ESMF_MOAB
  rc = ESMF_FAILURE;
  MBMeshTest *mbt_quad_sph_redist = new MBMeshTest();
  if (do_redist) {
    if (localrc == ESMF_SUCCESS) localrc = mbt_quad_sph->mbtRedist(mbt_quad_sph_redist);
    if (localrc == ESMF_SUCCESS) localrc = mbt_quad_sph_redist->build_mbmesh_redist();
    if (localrc == ESMF_SUCCESS) rc = mbt_quad_sph_redist->test_get_info(3);
  }
#else
  rc = ESMF_SUCCESS;
#endif
  //NEX_UTest
  strcpy(name, "MBMeshGet - Spherical Quadrilateral Redist");
  strcpy(failMsg, "FAIL");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);

#if defined ESMF_MOAB
  delete mbt_quad_sph;
  delete mbt_quad_sph_redist;
#endif

  // --------------------------------------------------------------------------
  // hexahedral mesh (3d Cartesian)
  // --------------------------------------------------------------------------
#if defined ESMF_MOAB
  rc = ESMF_FAILURE;
  MBMeshTest *mbt_hex = mbmesh_gen_hex_3d_cart(localrc, do_redist);
  if (localrc == ESMF_SUCCESS) localrc = mbt_hex->build_mbmesh();
  if (localrc == ESMF_SUCCESS) rc = mbt_hex->test_get_info();
#else
  rc = ESMF_SUCCESS;
#endif
  //NEX_UTest
  strcpy(name, "MBMeshGet - Cartesian Hexahedral (3D)");
  strcpy(failMsg, "FAIL");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);

#if defined ESMF_MOAB
  rc = ESMF_FAILURE;
  MBMeshTest *mbt_hex_redist = new MBMeshTest();
  if (do_redist) {
    if (localrc == ESMF_SUCCESS) localrc = mbt_hex->mbtRedist(mbt_hex_redist);
    if (localrc == ESMF_SUCCESS) localrc = mbt_hex_redist->build_mbmesh_redist();
    if (localrc == ESMF_SUCCESS) rc = mbt_hex_redist->test_get_info();
  }
#else
  rc = ESMF_SUCCESS;
#endif
  //NEX_UTest
  strcpy(name, "MBMeshGet - Cartesian Hexahedral (3D) Redist");
  strcpy(failMsg, "FAIL");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);

#if defined ESMF_MOAB
  delete mbt_hex;
  delete mbt_hex_redist;
#endif

  // --------------------------------------------------------------------------
  // hexahedral mesh (3d spherical)
  // --------------------------------------------------------------------------
#if defined ESMF_MOAB
  rc = ESMF_FAILURE;
  MBMeshTest *mbt_hex_sph = mbmesh_gen_hex_3d_sph(localrc, do_redist);
  if (localrc == ESMF_SUCCESS) localrc = mbt_hex_sph->build_mbmesh();
  if (localrc == ESMF_SUCCESS) rc = mbt_hex_sph->test_get_info();
#else
  rc = ESMF_SUCCESS;
#endif
  //NEX_UTest
  strcpy(name, "MBMeshGet - Spherical Hexahedral (3D)");
  strcpy(failMsg, "FAIL");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);

#if defined ESMF_MOAB
  rc = ESMF_FAILURE;
  MBMeshTest *mbt_hex_sph_redist = new MBMeshTest();
  if (do_redist) {
    if (localrc == ESMF_SUCCESS) localrc = mbt_hex_sph->mbtRedist(mbt_hex_sph_redist);
    if (localrc == ESMF_SUCCESS) localrc = mbt_hex_sph_redist->build_mbmesh_redist();
    if (localrc == ESMF_SUCCESS) rc = mbt_hex_sph_redist->test_get_info(3);
  }
#else
  rc = ESMF_SUCCESS;
#endif
  //NEX_UTest
  strcpy(name, "MBMeshGet - Spherical Hexahedral (3D) Redist");
  strcpy(failMsg, "FAIL");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);


#if defined ESMF_MOAB
  delete mbt_hex_sph;
  delete mbt_hex_sph_redist;
#endif

// TODO: ngons and mixed are hanging in redist, split element issue?
do_redist = false;

  // --------------------------------------------------------------------------
  // mbmesh_gen_ngon_2d_cart
  // --------------------------------------------------------------------------
#if defined ESMF_MOAB
  rc = ESMF_FAILURE;
  MBMeshTest *mbt_ngon = mbmesh_gen_ngon_2d_cart(localrc, do_redist);
  if (localrc == ESMF_SUCCESS) localrc = mbt_ngon->build_mbmesh();
  if (localrc == ESMF_SUCCESS) rc = mbt_ngon->test_get_info();
#else
  rc = ESMF_SUCCESS;
#endif
  // //cannot yet get connectivity information for ngon meshes
  // //NEX_disable_UTest
  // strcpy(name, "MBMeshGet - Cartesian N-gons");
  // strcpy(failMsg, "FAIL");
  // ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);

#if defined ESMF_MOAB
  rc = ESMF_FAILURE;
  MBMeshTest *mbt_ngon_redist = new MBMeshTest();
  if (do_redist) {
    if (localrc == ESMF_SUCCESS) localrc = mbt_ngon->mbtRedist(mbt_ngon_redist);
    if (localrc == ESMF_SUCCESS) localrc = mbt_ngon_redist->build_mbmesh_redist();
    if (localrc == ESMF_SUCCESS) rc = mbt_ngon_redist->test_get_info();
  }
#else
  rc = ESMF_SUCCESS;
#endif
  // //cannot yet get connectivity information for ngon meshes
  // //NEX_disable_UTest
  // strcpy(name, "MBMeshGet - Cartesian N-gons Redist");
  // strcpy(failMsg, "FAIL");
  // ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);

#if defined ESMF_MOAB
  delete mbt_ngon;
  delete mbt_ngon_redist;
#endif

  // --------------------------------------------------------------------------
  // mbmesh_gen_ngon_2d_sph
  // --------------------------------------------------------------------------
#if defined ESMF_MOAB
  rc = ESMF_FAILURE;
  MBMeshTest *mbt_ngon_sph = mbmesh_gen_ngon_2d_sph(localrc, do_redist);
  if (localrc == ESMF_SUCCESS) localrc = mbt_ngon_sph->build_mbmesh();
  if (localrc == ESMF_SUCCESS) rc = mbt_ngon_sph->test_get_info();
#else
  rc = ESMF_SUCCESS;
#endif
  // //cannot yet get connectivity information for ngon meshes
  // //NEX_disable_UTest
  // strcpy(name, "MBMeshGet - Spherical N-gons");
  // strcpy(failMsg, "FAIL");
  // ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);

#if defined ESMF_MOAB
  rc = ESMF_FAILURE;
  MBMeshTest *mbt_ngon_sph_redist = new MBMeshTest();
  if (do_redist) {
    if (localrc == ESMF_SUCCESS) localrc = mbt_ngon_sph->mbtRedist(mbt_ngon_sph_redist);
    if (localrc == ESMF_SUCCESS) localrc = mbt_ngon_sph_redist->build_mbmesh_redist();
    if (localrc == ESMF_SUCCESS) rc = mbt_ngon_sph_redist->test_get_info();
  }
#else
  rc = ESMF_SUCCESS;
#endif
  //cannot yet get connectivity information for ngon meshes
  //NEX_disable_UTest
  // strcpy(name, "MBMeshGet - Spherical N-gons Redist");
  // strcpy(failMsg, "FAIL");
  // ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);

#if defined ESMF_MOAB
  delete mbt_ngon_sph;
  delete mbt_ngon_sph_redist;
#endif

  // --------------------------------------------------------------------------
  // mixed Cartesian
  // --------------------------------------------------------------------------
#if defined ESMF_MOAB
  rc = ESMF_FAILURE;
  MBMeshTest *mbt_mix = mbmesh_gen_mix_2d_cart(localrc, do_redist);
  if (localrc == ESMF_SUCCESS) localrc = mbt_mix->build_mbmesh();
  if (localrc == ESMF_SUCCESS) rc = mbt_mix->test_get_info();
#else
  rc = ESMF_SUCCESS;
#endif
  //NEX_UTest
  strcpy(name, "MBMeshGet - Cartesian Mixed Topology");
  strcpy(failMsg, "FAIL");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);

#if defined ESMF_MOAB
  rc = ESMF_FAILURE;
  MBMeshTest *mbt_mix_redist = new MBMeshTest();
  if (do_redist) {
    if (localrc == ESMF_SUCCESS) localrc = mbt_mix->mbtRedist(mbt_mix_redist);
    if (localrc == ESMF_SUCCESS) localrc = mbt_mix_redist->build_mbmesh_redist();
    if (localrc == ESMF_SUCCESS) rc = mbt_mix_redist->test_get_info();
  }
#else
  rc = ESMF_SUCCESS;
#endif
  //NEX_UTest
  strcpy(name, "MBMeshGet - Cartesian Mixed Topology Redist");
  strcpy(failMsg, "FAIL");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);

#if defined ESMF_MOAB
  delete mbt_mix;
  delete mbt_mix_redist;
#endif

  // --------------------------------------------------------------------------
  //  mixed spherical
  // --------------------------------------------------------------------------
#if defined ESMF_MOAB
  rc = ESMF_FAILURE;
  MBMeshTest *mbt_mix_sph = mbmesh_gen_mix_2d_sph(localrc, do_redist);
  if (localrc == ESMF_SUCCESS) localrc = mbt_mix_sph->build_mbmesh();
  if (localrc == ESMF_SUCCESS) rc = mbt_mix_sph->test_get_info();
#else
  rc = ESMF_SUCCESS;
#endif
  //NEX_UTest
  strcpy(name, "MBMeshGet - Spherical Mixed Topology");
  strcpy(failMsg, "FAIL");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);

#if defined ESMF_MOAB
  rc = ESMF_FAILURE;
  MBMeshTest *mbt_mix_sph_redist = new MBMeshTest();
  if (do_redist) {
    if (localrc == ESMF_SUCCESS) localrc = mbt_mix_sph->mbtRedist(mbt_mix_sph_redist);
    if (localrc == ESMF_SUCCESS) localrc = mbt_mix_sph_redist->build_mbmesh_redist();
    if (localrc == ESMF_SUCCESS) rc = mbt_mix_sph_redist->test_get_info();
  }
#else
  rc = ESMF_SUCCESS;
#endif
  //NEX_UTest
  strcpy(name, "MBMeshGet - Spherical Mixed Topology Redist");
  strcpy(failMsg, "FAIL");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);

#if defined ESMF_MOAB
  delete mbt_mix_sph;
  delete mbt_mix_sph_redist;
#endif


  //----------------------------------------------------------------------------
  ESMC_TestEnd(__FILE__, __LINE__, 0);

  return 0;
}



