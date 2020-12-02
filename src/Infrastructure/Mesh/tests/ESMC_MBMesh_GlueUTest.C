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
#include "ESMCI_MBMesh_Glue.h"
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

  int writevtk = false;
  int globalpass;
  ESMC_TypeKind_Flag i4 = ESMC_TYPEKIND_I4;
  ESMC_Reduce_Flag sum = ESMC_REDUCE_SUM;

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
    printf("Can't redist on only one core, resetting redist to false.\n");
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
  //NEX_disable_UTest
  strcpy(name, "MBMeshGet - Cartesian Quadrilateral");
  strcpy(failMsg, "FAIL");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);

  // write output to vtk
#if defined ESMF_MOAB
  localrc = ESMC_VMReduce(vm, &rc, &globalpass, 1, &i4, &sum, 0);
  localrc = ESMC_VMBroadcast(vm, &globalpass, 1, &i4, 0);
  if ((localrc == ESMF_SUCCESS) && (globalpass == 0) && writevtk)
    localrc = mbt_quad->print_vtk("mbt_quad");
#endif


  // element redist
#if defined ESMF_MOAB
  rc = ESMF_SUCCESS;
  MBMeshTest *mbt_quad_elem_redist;
  if (do_redist) {
    rc = ESMF_FAILURE;
    mbt_quad_elem_redist = new MBMeshTest();
    if (localrc == ESMF_SUCCESS) localrc = mbt_quad->mbtRedist(mbt_quad_elem_redist);
    if (localrc == ESMF_SUCCESS) localrc = mbt_quad_elem_redist->build_mbmesh_redist(true, false);
    if (localrc == ESMF_SUCCESS) rc = mbt_quad_elem_redist->test_redist_info(true, false);
  }
#else
  rc = ESMF_SUCCESS;
#endif
  //NEX_disable_UTest
  strcpy(name, "MBMeshGet - Cartesian Quadrilateral Element Redist");
  strcpy(failMsg, "FAIL");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);

  // write output to vtk
#if defined ESMF_MOAB
  localrc = ESMC_VMReduce(vm, &rc, &globalpass, 1, &i4, &sum, 0);
  localrc = ESMC_VMBroadcast(vm, &globalpass, 1, &i4, 0);
  if ((localrc == ESMF_SUCCESS) && (globalpass == 0) && writevtk)
    localrc = mbt_quad_elem_redist->print_vtk("mbt_quad_elem_redist");
#endif

  // node redist
#if defined ESMF_MOAB
  rc = ESMF_SUCCESS;
  MBMeshTest *mbt_quad_node_redist;
  if (do_redist) {
    rc = ESMF_FAILURE;
    mbt_quad_node_redist = new MBMeshTest();
    if (localrc == ESMF_SUCCESS) localrc = mbt_quad->mbtRedist(mbt_quad_node_redist);
    if (localrc == ESMF_SUCCESS) localrc = mbt_quad_node_redist->build_mbmesh_redist(false, true);
    if (localrc == ESMF_SUCCESS) rc = mbt_quad_node_redist->test_redist_info(false, true);
  }
#else
  rc = ESMF_SUCCESS;
#endif
  //NEX_disable_UTest
  strcpy(name, "MBMeshGet - Cartesian Quadrilateral Node Redist");
  strcpy(failMsg, "FAIL");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);

  // write output to vtk
#if defined ESMF_MOAB
  localrc = ESMC_VMReduce(vm, &rc, &globalpass, 1, &i4, &sum, 0);
  localrc = ESMC_VMBroadcast(vm, &globalpass, 1, &i4, 0);
  if ((localrc == ESMF_SUCCESS) && (globalpass == 0) && writevtk)
    localrc = mbt_quad_node_redist->print_vtk("mbt_quad_node_redist");
#endif

  // element and node redist
#if defined ESMF_MOAB
  rc = ESMF_SUCCESS;
  MBMeshTest *mbt_quad_elno_redist;
  if (do_redist) {
    rc = ESMF_FAILURE;
    mbt_quad_elno_redist = new MBMeshTest();
    if (localrc == ESMF_SUCCESS) localrc = mbt_quad->mbtRedist(mbt_quad_elno_redist);
    if (localrc == ESMF_SUCCESS) localrc = mbt_quad_elno_redist->build_mbmesh_redist(true, true);
    if (localrc == ESMF_SUCCESS) rc = mbt_quad_elno_redist->test_redist_info(true, true);
  }
#else
  rc = ESMF_SUCCESS;
#endif
  //NEX_disable_UTest
  strcpy(name, "MBMeshGet - Cartesian Quadrilateral Element and Node Redist");
  strcpy(failMsg, "FAIL");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);

  // write output to vtk
#if defined ESMF_MOAB
  localrc = ESMC_VMReduce(vm, &rc, &globalpass, 1, &i4, &sum, 0);
  localrc = ESMC_VMBroadcast(vm, &globalpass, 1, &i4, 0);
  if ((localrc == ESMF_SUCCESS) && (globalpass == 0) && writevtk)
    localrc = mbt_quad_elno_redist->print_vtk("mbt_quad_elno_redist");
#endif

#if defined ESMF_MOAB
  delete mbt_quad;
  if (do_redist) {
    delete mbt_quad_elem_redist;
    delete mbt_quad_node_redist;
    delete mbt_quad_elno_redist;
  }
#endif

  // --------------------------------------------------------------------------
  // quad mesh spherical
  // --------------------------------------------------------------------------
#if defined ESMF_MOAB
  rc = ESMF_FAILURE;
  // set up a MBMeshTest object
  MBMeshTest *mbt_quad_sph = mbmesh_gen_quad_2d_sph(localrc, do_redist);
  if (localrc == ESMF_SUCCESS) localrc = mbt_quad_sph->build_mbmesh();
  if (localrc == ESMF_SUCCESS) rc = mbt_quad_sph->test_get_info();
#else
  rc = ESMF_SUCCESS;
#endif
  //NEX_disable_UTest
  strcpy(name, "MBMeshGet - Spherical Quadrilateral");
  strcpy(failMsg, "FAIL");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);

  // write output to vtk
#if defined ESMF_MOAB
  localrc = ESMC_VMReduce(vm, &rc, &globalpass, 1, &i4, &sum, 0);
  localrc = ESMC_VMBroadcast(vm, &globalpass, 1, &i4, 0);
  if ((localrc == ESMF_SUCCESS) && (globalpass == 0) && writevtk)
    localrc = mbt_quad_sph->print_vtk("mbt_quad_sph");
#endif


  // element redist
#if defined ESMF_MOAB
  rc = ESMF_SUCCESS;
  MBMeshTest *mbt_quad_sph_elem_redist;
  if (do_redist) {
    rc = ESMF_FAILURE;
    mbt_quad_sph_elem_redist = new MBMeshTest();
    if (localrc == ESMF_SUCCESS) localrc = mbt_quad_sph->mbtRedist(mbt_quad_sph_elem_redist);
    if (localrc == ESMF_SUCCESS) localrc = mbt_quad_sph_elem_redist->build_mbmesh_redist(true, false);
    if (localrc == ESMF_SUCCESS) rc = mbt_quad_sph_elem_redist->test_redist_info(true, false);
  }
#else
  rc = ESMF_SUCCESS;
#endif
  //NEX_disable_UTest
  strcpy(name, "MBMeshGet - Spherical Quadrilateral Element Redist");
  strcpy(failMsg, "FAIL");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);

  // write output to vtk
#if defined ESMF_MOAB
  localrc = ESMC_VMReduce(vm, &rc, &globalpass, 1, &i4, &sum, 0);
  localrc = ESMC_VMBroadcast(vm, &globalpass, 1, &i4, 0);
  if ((localrc == ESMF_SUCCESS) && (globalpass == 0) && writevtk)
    localrc = mbt_quad_sph_elem_redist->print_vtk("mbt_quad_sph_elem_redist");
#endif

  // node redist
#if defined ESMF_MOAB
  rc = ESMF_SUCCESS;
  MBMeshTest *mbt_quad_sph_node_redist;
  if (do_redist) {
    rc = ESMF_FAILURE;
    mbt_quad_sph_node_redist = new MBMeshTest();
    if (localrc == ESMF_SUCCESS) localrc = mbt_quad_sph->mbtRedist(mbt_quad_sph_node_redist);
    if (localrc == ESMF_SUCCESS) localrc = mbt_quad_sph_node_redist->build_mbmesh_redist(false, true);
    if (localrc == ESMF_SUCCESS) rc = mbt_quad_sph_node_redist->test_redist_info(false, true);
  }
#else
  rc = ESMF_SUCCESS;
#endif
  //NEX_disable_UTest
  strcpy(name, "MBMeshGet - Spherical Quadrilateral Node Redist");
  strcpy(failMsg, "FAIL");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);

  // write output to vtk
#if defined ESMF_MOAB
  localrc = ESMC_VMReduce(vm, &rc, &globalpass, 1, &i4, &sum, 0);
  localrc = ESMC_VMBroadcast(vm, &globalpass, 1, &i4, 0);
  if ((localrc == ESMF_SUCCESS) && (globalpass == 0) && writevtk)
    localrc = mbt_quad_sph_node_redist->print_vtk("mbt_quad_sph_node_redist");
#endif

  // element and node redist
#if defined ESMF_MOAB
  rc = ESMF_SUCCESS;
  MBMeshTest *mbt_quad_sph_elno_redist;
  if (do_redist) {
    rc = ESMF_FAILURE;
    mbt_quad_sph_elno_redist = new MBMeshTest();
    if (localrc == ESMF_SUCCESS) localrc = mbt_quad_sph->mbtRedist(mbt_quad_sph_elno_redist);
    if (localrc == ESMF_SUCCESS) localrc = mbt_quad_sph_elno_redist->build_mbmesh_redist(true, true);
    if (localrc == ESMF_SUCCESS) rc = mbt_quad_sph_elno_redist->test_redist_info(true, true);
  }
#else
  rc = ESMF_SUCCESS;
#endif
  //NEX_disable_UTest
  strcpy(name, "MBMeshGet - Spherical Quadrilateral Element and Node Redist");
  strcpy(failMsg, "FAIL");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);

  // write output to vtk
#if defined ESMF_MOAB
  localrc = ESMC_VMReduce(vm, &rc, &globalpass, 1, &i4, &sum, 0);
  localrc = ESMC_VMBroadcast(vm, &globalpass, 1, &i4, 0);
  if ((localrc == ESMF_SUCCESS) && (globalpass == 0) && writevtk)
    localrc = mbt_quad_sph_elno_redist->print_vtk("mbt_quad_sph_elno_redist");
#endif

#if defined ESMF_MOAB
  delete mbt_quad_sph;
  if (do_redist) {
    delete mbt_quad_sph_elem_redist;
    delete mbt_quad_sph_node_redist;
    delete mbt_quad_sph_elno_redist;
  }
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
  //NEX_disable_UTest
  strcpy(name, "MBMeshGet - Cartesian Hexahedral (3D)");
  strcpy(failMsg, "FAIL");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);

  // write output to vtk
#if defined ESMF_MOAB
  localrc = ESMC_VMReduce(vm, &rc, &globalpass, 1, &i4, &sum, 0);
  localrc = ESMC_VMBroadcast(vm, &globalpass, 1, &i4, 0);
  if ((localrc == ESMF_SUCCESS) && (globalpass == 0) && writevtk)
    localrc = mbt_hex->print_vtk("mbt_hex");
#endif


// redist
#if defined ESMF_MOAB
  rc = ESMF_SUCCESS;
  MBMeshTest *mbt_hex_elem_redist;
  if (do_redist) {
    rc = ESMF_FAILURE;
    mbt_hex_elem_redist = new MBMeshTest();
    if (localrc == ESMF_SUCCESS) localrc = mbt_hex->mbtRedist(mbt_hex_elem_redist);
    if (localrc == ESMF_SUCCESS) localrc = mbt_hex_elem_redist->build_mbmesh_redist(true, false);
    if (localrc == ESMF_SUCCESS) rc = mbt_hex_elem_redist->test_redist_info(true, false);
  }
#else
  rc = ESMF_SUCCESS;
#endif
  //NEX_disable_UTest
  strcpy(name, "MBMeshGet - Cartesian Hexahedral (3D) Element Redist");
  strcpy(failMsg, "FAIL");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);

  // write output to vtk
#if defined ESMF_MOAB
  localrc = ESMC_VMReduce(vm, &rc, &globalpass, 1, &i4, &sum, 0);
  localrc = ESMC_VMBroadcast(vm, &globalpass, 1, &i4, 0);
  if ((localrc == ESMF_SUCCESS) && (globalpass == 0) && writevtk)
    localrc = mbt_hex_elem_redist->print_vtk("mbt_hex_elem_redist");
#endif

#if defined ESMF_MOAB
  rc = ESMF_SUCCESS;
  MBMeshTest *mbt_hex_node_redist;
  if (do_redist) {
    rc = ESMF_FAILURE;
    mbt_hex_node_redist = new MBMeshTest();
    if (localrc == ESMF_SUCCESS) localrc = mbt_hex->mbtRedist(mbt_hex_node_redist);
    if (localrc == ESMF_SUCCESS) localrc = mbt_hex_node_redist->build_mbmesh_redist(false, true);
    if (localrc == ESMF_SUCCESS) rc = mbt_hex_node_redist->test_redist_info(false, true);
  }
#else
  rc = ESMF_SUCCESS;
#endif
  //NEX_disable_UTest
  strcpy(name, "MBMeshGet - Cartesian Hexahedral (3D) Node Redist");
  strcpy(failMsg, "FAIL");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);

  // write output to vtk
#if defined ESMF_MOAB
  localrc = ESMC_VMReduce(vm, &rc, &globalpass, 1, &i4, &sum, 0);
  localrc = ESMC_VMBroadcast(vm, &globalpass, 1, &i4, 0);
  if ((localrc == ESMF_SUCCESS) && (globalpass == 0) && writevtk)
    localrc = mbt_hex_node_redist->print_vtk("mbt_hex_node_redist");
#endif

#if defined ESMF_MOAB
  rc = ESMF_SUCCESS;
  MBMeshTest *mbt_hex_elno_redist;
  if (do_redist) {
    rc = ESMF_FAILURE;
    mbt_hex_elno_redist = new MBMeshTest();
    if (localrc == ESMF_SUCCESS) localrc = mbt_hex->mbtRedist(mbt_hex_elno_redist);
    if (localrc == ESMF_SUCCESS) localrc = mbt_hex_elno_redist->build_mbmesh_redist(true, true);
    if (localrc == ESMF_SUCCESS) rc = mbt_hex_elno_redist->test_redist_info(true, true);
  }
#else
  rc = ESMF_SUCCESS;
#endif
  //NEX_disable_UTest
  strcpy(name, "MBMeshGet - Cartesian Hexahedral (3D) Element and Node Redist");
  strcpy(failMsg, "FAIL");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);

  // write output to vtk
#if defined ESMF_MOAB
  localrc = ESMC_VMReduce(vm, &rc, &globalpass, 1, &i4, &sum, 0);
  localrc = ESMC_VMBroadcast(vm, &globalpass, 1, &i4, 0);
  if ((localrc == ESMF_SUCCESS) && (globalpass == 0) && writevtk)
    localrc = mbt_hex_elno_redist->print_vtk("mbt_hex_elno_redist");
#endif

#if defined ESMF_MOAB
  delete mbt_hex;
  if (do_redist) {
    delete mbt_hex_elem_redist;
    delete mbt_hex_node_redist;
    delete mbt_hex_elno_redist;
  }
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
  //NEX_disable_UTest
  strcpy(name, "MBMeshGet - Spherical Hexahedral (3D)");
  strcpy(failMsg, "FAIL");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);

#if defined ESMF_MOAB
  // write output to vtk
  localrc = ESMC_VMReduce(vm, &rc, &globalpass, 1, &i4, &sum, 0);
  localrc = ESMC_VMBroadcast(vm, &globalpass, 1, &i4, 0);
  if ((localrc == ESMF_SUCCESS) && (globalpass == 0) && writevtk)
    localrc = mbt_hex_sph->print_vtk("mbt_hex_sph");
#endif

// redist
#if defined ESMF_MOAB
  rc = ESMF_SUCCESS;
  MBMeshTest *mbt_hex_sph_elem_redist;
  if (do_redist) {
    rc = ESMF_FAILURE;
    mbt_hex_sph_elem_redist = new MBMeshTest();
    if (localrc == ESMF_SUCCESS) localrc = mbt_hex_sph->mbtRedist(mbt_hex_sph_elem_redist);
    if (localrc == ESMF_SUCCESS) localrc = mbt_hex_sph_elem_redist->build_mbmesh_redist(true, false);
    if (localrc == ESMF_SUCCESS) rc = mbt_hex_sph_elem_redist->test_redist_info(true, false);
  }
#else
  rc = ESMF_SUCCESS;
#endif
  //NEX_disable_UTest
  strcpy(name, "MBMeshGet - Spherical Hexahedral (3D) Element Redist");
  strcpy(failMsg, "FAIL");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);

  // write output to vtk
#if defined ESMF_MOAB
  localrc = ESMC_VMReduce(vm, &rc, &globalpass, 1, &i4, &sum, 0);
  localrc = ESMC_VMBroadcast(vm, &globalpass, 1, &i4, 0);
  if ((localrc == ESMF_SUCCESS) && (globalpass == 0) && writevtk)
    localrc = mbt_hex_sph_elem_redist->print_vtk("mbt_hex_sph_elem_redist");
#endif

#if defined ESMF_MOAB
  rc = ESMF_SUCCESS;
  MBMeshTest *mbt_hex_sph_node_redist;
  if (do_redist) {
    rc = ESMF_FAILURE;
    mbt_hex_sph_node_redist = new MBMeshTest();
    if (localrc == ESMF_SUCCESS) localrc = mbt_hex_sph->mbtRedist(mbt_hex_sph_node_redist);
    if (localrc == ESMF_SUCCESS) localrc = mbt_hex_sph_node_redist->build_mbmesh_redist(false, true);
    if (localrc == ESMF_SUCCESS) rc = mbt_hex_sph_node_redist->test_redist_info(false, true);
  }
#else
  rc = ESMF_SUCCESS;
#endif
  //NEX_disable_UTest
  strcpy(name, "MBMeshGet - Spherical Hexahedral (3D) Node Redist");
  strcpy(failMsg, "FAIL");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);

  // write output to vtk
#if defined ESMF_MOAB
  localrc = ESMC_VMReduce(vm, &rc, &globalpass, 1, &i4, &sum, 0);
  localrc = ESMC_VMBroadcast(vm, &globalpass, 1, &i4, 0);
  if ((localrc == ESMF_SUCCESS) && (globalpass == 0) && writevtk)
    localrc = mbt_hex_sph_node_redist->print_vtk("mbt_hex_sph_node_redist");
#endif

#if defined ESMF_MOAB
  rc = ESMF_SUCCESS;
  MBMeshTest *mbt_hex_sph_elno_redist;
  if (do_redist) {
    rc = ESMF_FAILURE;
    mbt_hex_sph_elno_redist = new MBMeshTest();
    if (localrc == ESMF_SUCCESS) localrc = mbt_hex_sph->mbtRedist(mbt_hex_sph_elno_redist);
    if (localrc == ESMF_SUCCESS) localrc = mbt_hex_sph_elno_redist->build_mbmesh_redist(true, true);
    if (localrc == ESMF_SUCCESS) rc = mbt_hex_sph_elno_redist->test_redist_info(true, true);
  }
#else
  rc = ESMF_SUCCESS;
#endif
  //NEX_disable_UTest
  strcpy(name, "MBMeshGet - Spherical Hexahedral (3D) Element and Node Redist");
  strcpy(failMsg, "FAIL");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);

  // write output to vtk
#if defined ESMF_MOAB
  localrc = ESMC_VMReduce(vm, &rc, &globalpass, 1, &i4, &sum, 0);
  localrc = ESMC_VMBroadcast(vm, &globalpass, 1, &i4, 0);
  if ((localrc == ESMF_SUCCESS) && (globalpass == 0) && writevtk)
    localrc = mbt_hex_sph_elno_redist->print_vtk("mbt_hex_sph_elno_redist");
#endif


#if defined ESMF_MOAB
  delete mbt_hex_sph;
  if (do_redist) {
    delete mbt_hex_sph_elem_redist;
    delete mbt_hex_sph_node_redist;
    delete mbt_hex_sph_elno_redist;
  }
#endif

// RLO: due to some hefty memory leaks cannot run all tests right now..
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
  //NEX_disable_UTest
  strcpy(name, "MBMeshGet - Cartesian Mixed Topology");
  strcpy(failMsg, "FAIL");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);

  // write output to vtk
#if defined ESMF_MOAB
  localrc = ESMC_VMReduce(vm, &rc, &globalpass, 1, &i4, &sum, 0);
  localrc = ESMC_VMBroadcast(vm, &globalpass, 1, &i4, 0);
  if ((localrc == ESMF_SUCCESS) && (globalpass == 0) && writevtk)
    localrc = mbt_mix->print_vtk("mbt_mix");
#endif

// elem redist 
#if defined ESMF_MOAB
  rc = ESMF_SUCCESS;
  MBMeshTest *mbt_mix_elem_redist;
  if (do_redist) {
    rc = ESMF_FAILURE;
    mbt_mix_elem_redist = new MBMeshTest();
    if (localrc == ESMF_SUCCESS) localrc = mbt_mix->mbtRedist(mbt_mix_elem_redist);
    if (localrc == ESMF_SUCCESS) localrc = mbt_mix_elem_redist->build_mbmesh_redist(true, false);
    if (localrc == ESMF_SUCCESS) rc = mbt_mix_elem_redist->test_redist_info(true, false);
  }
#else
  rc = ESMF_SUCCESS;
#endif
  //NEX_disable_UTest
  strcpy(name, "MBMeshGet - Cartesian Mixed Topology Element Redist");
  strcpy(failMsg, "FAIL");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);

  // write output to vtk
#if defined ESMF_MOAB
  localrc = ESMC_VMReduce(vm, &rc, &globalpass, 1, &i4, &sum, 0);
  localrc = ESMC_VMBroadcast(vm, &globalpass, 1, &i4, 0);
  if ((localrc == ESMF_SUCCESS) && (globalpass == 0) && writevtk)
    localrc = mbt_mix_elem_redist->print_vtk("mbt_mix_elem_redist");
#endif

// node redist 
#if defined ESMF_MOAB
  rc = ESMF_SUCCESS;
  MBMeshTest *mbt_mix_node_redist;
  if (do_redist) {
    rc = ESMF_FAILURE;
    mbt_mix_node_redist = new MBMeshTest();
    if (localrc == ESMF_SUCCESS) localrc = mbt_mix->mbtRedist(mbt_mix_node_redist);
    if (localrc == ESMF_SUCCESS) localrc = mbt_mix_node_redist->build_mbmesh_redist(false, true);
    if (localrc == ESMF_SUCCESS) rc = mbt_mix_node_redist->test_redist_info(false, true);
  }
#else
  rc = ESMF_SUCCESS;
#endif
  //NEX_disable_UTest
  strcpy(name, "MBMeshGet - Cartesian Mixed Topology Node Redist");
  strcpy(failMsg, "FAIL");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);

  // write output to vtk
#if defined ESMF_MOAB
  localrc = ESMC_VMReduce(vm, &rc, &globalpass, 1, &i4, &sum, 0);
  localrc = ESMC_VMBroadcast(vm, &globalpass, 1, &i4, 0);
  if ((localrc == ESMF_SUCCESS) && (globalpass == 0) && writevtk)
    localrc = mbt_mix_node_redist->print_vtk("mbt_mix_node_redist");
#endif

// element and node redist 
#if defined ESMF_MOAB
  rc = ESMF_SUCCESS;
  MBMeshTest *mbt_mix_elno_redist;
  if (do_redist) {
    rc = ESMF_FAILURE;
    mbt_mix_elno_redist = new MBMeshTest();
    if (localrc == ESMF_SUCCESS) localrc = mbt_mix->mbtRedist(mbt_mix_elno_redist);
    if (localrc == ESMF_SUCCESS) localrc = mbt_mix_elno_redist->build_mbmesh_redist(true, true);
    if (localrc == ESMF_SUCCESS) rc = mbt_mix_elno_redist->test_redist_info(true, true);
  }
#else
  rc = ESMF_SUCCESS;
#endif
  //NEX_disable_UTest
  strcpy(name, "MBMeshGet - Cartesian Mixed Topology Element and Node Redist");
  strcpy(failMsg, "FAIL");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);

  // write output to vtk
#if defined ESMF_MOAB
  localrc = ESMC_VMReduce(vm, &rc, &globalpass, 1, &i4, &sum, 0);
  localrc = ESMC_VMBroadcast(vm, &globalpass, 1, &i4, 0);
  if ((localrc == ESMF_SUCCESS) && (globalpass == 0) && writevtk)
    localrc = mbt_mix_elno_redist->print_vtk("mbt_mix_elno_redist");
#endif

#if defined ESMF_MOAB
  delete mbt_mix;
  if (do_redist) {
    delete mbt_mix_elem_redist;
    delete mbt_mix_node_redist;
    delete mbt_mix_elno_redist;
  }
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
  //NEX_disable_UTest
  strcpy(name, "MBMeshGet - Spherical Mixed Topology");
  strcpy(failMsg, "FAIL");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);

  // write output to vtk
#if defined ESMF_MOAB
  localrc = ESMC_VMReduce(vm, &rc, &globalpass, 1, &i4, &sum, 0);
  localrc = ESMC_VMBroadcast(vm, &globalpass, 1, &i4, 0);
  if ((localrc == ESMF_SUCCESS) && (globalpass == 0) && writevtk)
    localrc = mbt_mix_sph->print_vtk("mbt_mix_sph");
#endif

// elem redist
#if defined ESMF_MOAB
  rc = ESMF_SUCCESS;
  MBMeshTest *mbt_mix_sph_elem_redist;
  if (do_redist) {
    rc = ESMF_FAILURE;
    mbt_mix_sph_elem_redist = new MBMeshTest();
    if (localrc == ESMF_SUCCESS) localrc = mbt_mix_sph->mbtRedist(mbt_mix_sph_elem_redist);
    if (localrc == ESMF_SUCCESS) localrc = mbt_mix_sph_elem_redist->build_mbmesh_redist(true, false);
    if (localrc == ESMF_SUCCESS) rc = mbt_mix_sph_elem_redist->test_redist_info(true, false);
  }
#else
  rc = ESMF_SUCCESS;
#endif
  //NEX_disable_UTest
  strcpy(name, "MBMeshGet - Spherical Mixed Topology Element Redist");
  strcpy(failMsg, "FAIL");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);

  // write output to vtk
#if defined ESMF_MOAB
  localrc = ESMC_VMReduce(vm, &rc, &globalpass, 1, &i4, &sum, 0);
  localrc = ESMC_VMBroadcast(vm, &globalpass, 1, &i4, 0);
  if ((localrc == ESMF_SUCCESS) && (globalpass == 0) && writevtk)
    localrc = mbt_mix_sph_elem_redist->print_vtk("mbt_mix_sph_elem_redist");
#endif

// node redist
#if defined ESMF_MOAB
  rc = ESMF_SUCCESS;
  MBMeshTest *mbt_mix_sph_node_redist;
  if (do_redist) {
    rc = ESMF_FAILURE;
    mbt_mix_sph_node_redist = new MBMeshTest();
    if (localrc == ESMF_SUCCESS) localrc = mbt_mix_sph->mbtRedist(mbt_mix_sph_node_redist);
    if (localrc == ESMF_SUCCESS) localrc = mbt_mix_sph_node_redist->build_mbmesh_redist(false, true);
    if (localrc == ESMF_SUCCESS) rc = mbt_mix_sph_node_redist->test_redist_info(false, true);
  }
#else
  rc = ESMF_SUCCESS;
#endif
  //NEX_disable_UTest
  strcpy(name, "MBMeshGet - Spherical Mixed Topology Node Redist");
  strcpy(failMsg, "FAIL");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);

  // write output to vtk
#if defined ESMF_MOAB
  localrc = ESMC_VMReduce(vm, &rc, &globalpass, 1, &i4, &sum, 0);
  localrc = ESMC_VMBroadcast(vm, &globalpass, 1, &i4, 0);
  if ((localrc == ESMF_SUCCESS) && (globalpass == 0) && writevtk)
    localrc = mbt_mix_sph_node_redist->print_vtk("mbt_mix_sph_node_redist");
#endif

// elem and node redist
#if defined ESMF_MOAB
  rc = ESMF_SUCCESS;
  MBMeshTest *mbt_mix_sph_elno_redist;
  if (do_redist) {
    rc = ESMF_FAILURE;
    mbt_mix_sph_elno_redist = new MBMeshTest();
    if (localrc == ESMF_SUCCESS) localrc = mbt_mix_sph->mbtRedist(mbt_mix_sph_elno_redist);
    if (localrc == ESMF_SUCCESS) localrc = mbt_mix_sph_elno_redist->build_mbmesh_redist(true, true);
    if (localrc == ESMF_SUCCESS) rc = mbt_mix_sph_elno_redist->test_redist_info(true, true);
  }
#else
  rc = ESMF_SUCCESS;
#endif
  //NEX_disable_UTest
  strcpy(name, "MBMeshGet - Spherical Mixed Topology Element and Node Redist");
  strcpy(failMsg, "FAIL");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);

  // write output to vtk
#if defined ESMF_MOAB
  localrc = ESMC_VMReduce(vm, &rc, &globalpass, 1, &i4, &sum, 0);
  localrc = ESMC_VMBroadcast(vm, &globalpass, 1, &i4, 0);
  if ((localrc == ESMF_SUCCESS) && (globalpass == 0) && writevtk)
    localrc = mbt_mix_sph_elno_redist->print_vtk("mbt_mix_sph_elno_redist");
#endif

#if defined ESMF_MOAB
  delete mbt_mix_sph;
  if (do_redist) {
    delete mbt_mix_sph_elem_redist;
    delete mbt_mix_sph_node_redist;
    delete mbt_mix_sph_elno_redist;
  }
#endif

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
  //cannot yet get connectivity information for ngon meshes
  //NEX_disable_UTest
  strcpy(name, "MBMeshGet - Cartesian N-gons");
  strcpy(failMsg, "FAIL");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);

  // write output to vtk
#if defined ESMF_MOAB
  localrc = ESMC_VMReduce(vm, &rc, &globalpass, 1, &i4, &sum, 0);
  localrc = ESMC_VMBroadcast(vm, &globalpass, 1, &i4, 0);
  if ((localrc == ESMF_SUCCESS) && (globalpass == 0) && writevtk)
    localrc = mbt_ngon->print_vtk("mbt_ngon");
#endif

// redist
#if defined ESMF_MOAB
  rc = ESMF_SUCCESS;
  MBMeshTest *mbt_ngon_elem_redist;
  if (do_redist) {
    rc = ESMF_FAILURE;
    mbt_ngon_elem_redist = new MBMeshTest();
    if (localrc == ESMF_SUCCESS) localrc = mbt_ngon->mbtRedist(mbt_ngon_elem_redist);
    if (localrc == ESMF_SUCCESS) localrc = mbt_ngon_elem_redist->build_mbmesh_redist(true, false);
    if (localrc == ESMF_SUCCESS) rc = mbt_ngon_elem_redist->test_redist_info(true, false);
  }
#else
  rc = ESMF_SUCCESS;
#endif
  //cannot yet get connectivity information for ngon meshes
  //NEX_disable_UTest
  strcpy(name, "MBMeshGet - Cartesian N-gons Element Redist");
  strcpy(failMsg, "FAIL");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);

  // write output to vtk
#if defined ESMF_MOAB
  localrc = ESMC_VMReduce(vm, &rc, &globalpass, 1, &i4, &sum, 0);
  localrc = ESMC_VMBroadcast(vm, &globalpass, 1, &i4, 0);
  if ((localrc == ESMF_SUCCESS) && (globalpass == 0) && writevtk)
    localrc = mbt_ngon_elem_redist->print_vtk("mbt_ngon_elem_redist");
#endif

#if defined ESMF_MOAB
  rc = ESMF_SUCCESS;
  MBMeshTest *mbt_ngon_node_redist;
  if (do_redist) {
    rc = ESMF_FAILURE;
    mbt_ngon_node_redist = new MBMeshTest();
    if (localrc == ESMF_SUCCESS) localrc = mbt_ngon->mbtRedist(mbt_ngon_node_redist);
    if (localrc == ESMF_SUCCESS) localrc = mbt_ngon_node_redist->build_mbmesh_redist(false, true);
    if (localrc == ESMF_SUCCESS) rc = mbt_ngon_node_redist->test_redist_info(false, true);
  }
#else
  rc = ESMF_SUCCESS;
#endif
  //cannot yet get connectivity information for ngon meshes
  //NEX_disable_UTest
  strcpy(name, "MBMeshGet - Cartesian N-gons Node Redist");
  strcpy(failMsg, "FAIL");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);

  // write output to vtk
#if defined ESMF_MOAB
  localrc = ESMC_VMReduce(vm, &rc, &globalpass, 1, &i4, &sum, 0);
  localrc = ESMC_VMBroadcast(vm, &globalpass, 1, &i4, 0);
  if ((localrc == ESMF_SUCCESS) && (globalpass == 0) && writevtk)
    localrc = mbt_ngon_node_redist->print_vtk("mbt_ngon_node_redist");
#endif

#if defined ESMF_MOAB
  rc = ESMF_SUCCESS;
  MBMeshTest *mbt_ngon_elno_redist;
  if (do_redist) {
    rc = ESMF_FAILURE;
    mbt_ngon_elno_redist = new MBMeshTest();
    if (localrc == ESMF_SUCCESS) localrc = mbt_ngon->mbtRedist(mbt_ngon_elno_redist);
    if (localrc == ESMF_SUCCESS) localrc = mbt_ngon_elno_redist->build_mbmesh_redist(true, true);
    if (localrc == ESMF_SUCCESS) rc = mbt_ngon_elno_redist->test_redist_info(true, true);
  }
#else
  rc = ESMF_SUCCESS;
#endif
  //cannot yet get connectivity information for ngon meshes
  //NEX_disable_UTest
  strcpy(name, "MBMeshGet - Cartesian N-gons Element and Node Redist");
  strcpy(failMsg, "FAIL");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);

  // write output to vtk
#if defined ESMF_MOAB
  localrc = ESMC_VMReduce(vm, &rc, &globalpass, 1, &i4, &sum, 0);
  localrc = ESMC_VMBroadcast(vm, &globalpass, 1, &i4, 0);
  if ((localrc == ESMF_SUCCESS) && (globalpass == 0) && writevtk)
    localrc = mbt_ngon_elno_redist->print_vtk("mbt_ngon_elno_redist");
#endif

#if defined ESMF_MOAB
  delete mbt_ngon;
  if (do_redist) {
    delete mbt_ngon_elem_redist;
    delete mbt_ngon_node_redist;
    delete mbt_ngon_elno_redist;
  }
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
  //cannot yet get connectivity information for ngon meshes
  //NEX_disable_UTest
  strcpy(name, "MBMeshGet - Spherical N-gons");
  strcpy(failMsg, "FAIL");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);

  // write output to vtk
#if defined ESMF_MOAB
  localrc = ESMC_VMReduce(vm, &rc, &globalpass, 1, &i4, &sum, 0);
  localrc = ESMC_VMBroadcast(vm, &globalpass, 1, &i4, 0);
  if ((localrc == ESMF_SUCCESS) && (globalpass == 0) && writevtk)
    localrc = mbt_ngon_sph->print_vtk("mbt_ngon_sph");
#endif

// redist
#if defined ESMF_MOAB
  rc = ESMF_SUCCESS;
  MBMeshTest *mbt_ngon_sph_elem_redist;
  if (do_redist) {
    rc = ESMF_FAILURE;
    mbt_ngon_sph_elem_redist = new MBMeshTest();
    if (localrc == ESMF_SUCCESS) localrc = mbt_ngon_sph->mbtRedist(mbt_ngon_sph_elem_redist);
    if (localrc == ESMF_SUCCESS) localrc = mbt_ngon_sph_elem_redist->build_mbmesh_redist(true, false);
    if (localrc == ESMF_SUCCESS) rc = mbt_ngon_sph_elem_redist->test_redist_info(true, false);
  }
#else
  rc = ESMF_SUCCESS;
#endif
  //cannot yet get connectivity information for ngon meshes
  //NEX_disable_UTest
  strcpy(name, "MBMeshGet - Spherical N-gons Element Redist");
  strcpy(failMsg, "FAIL");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);

  // write output to vtk
#if defined ESMF_MOAB
  localrc = ESMC_VMReduce(vm, &rc, &globalpass, 1, &i4, &sum, 0);
  localrc = ESMC_VMBroadcast(vm, &globalpass, 1, &i4, 0);
  if ((localrc == ESMF_SUCCESS) && (globalpass == 0) && writevtk)
    localrc = mbt_ngon_sph_elem_redist->print_vtk("mbt_ngon_sph_elem_redist");
#endif

#if defined ESMF_MOAB
  rc = ESMF_SUCCESS;
  MBMeshTest *mbt_ngon_sph_node_redist;
  if (do_redist) {
    rc = ESMF_FAILURE;
    mbt_ngon_sph_node_redist = new MBMeshTest();
    if (localrc == ESMF_SUCCESS) localrc = mbt_ngon_sph->mbtRedist(mbt_ngon_sph_node_redist);
    if (localrc == ESMF_SUCCESS) localrc = mbt_ngon_sph_node_redist->build_mbmesh_redist(false, true);
    if (localrc == ESMF_SUCCESS) rc = mbt_ngon_sph_node_redist->test_redist_info(false, true);
  }
#else
  rc = ESMF_SUCCESS;
#endif
  //cannot yet get connectivity information for ngon meshes
  //NEX_disable_UTest
  strcpy(name, "MBMeshGet - Spherical N-gons Node Redist");
  strcpy(failMsg, "FAIL");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);

  // write output to vtk
#if defined ESMF_MOAB
  localrc = ESMC_VMReduce(vm, &rc, &globalpass, 1, &i4, &sum, 0);
  localrc = ESMC_VMBroadcast(vm, &globalpass, 1, &i4, 0);
  if ((localrc == ESMF_SUCCESS) && (globalpass == 0) && writevtk)
    localrc = mbt_ngon_sph_node_redist->print_vtk("mbt_ngon_sph_node_redist");
#endif

#if defined ESMF_MOAB
  rc = ESMF_SUCCESS;
  MBMeshTest *mbt_ngon_sph_elno_redist;
  if (do_redist) {
    rc = ESMF_FAILURE;
    mbt_ngon_sph_elno_redist = new MBMeshTest();
    if (localrc == ESMF_SUCCESS) localrc = mbt_ngon_sph->mbtRedist(mbt_ngon_sph_elno_redist);
    if (localrc == ESMF_SUCCESS) localrc = mbt_ngon_sph_elno_redist->build_mbmesh_redist(true, true);
    if (localrc == ESMF_SUCCESS) rc = mbt_ngon_sph_elno_redist->test_redist_info(true, true);
  }
#else
  rc = ESMF_SUCCESS;
#endif
  //cannot yet get connectivity information for ngon meshes
  //NEX_disable_UTest
  strcpy(name, "MBMeshGet - Spherical N-gons Redist");
  strcpy(failMsg, "FAIL");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);

  // write output to vtk
#if defined ESMF_MOAB
  localrc = ESMC_VMReduce(vm, &rc, &globalpass, 1, &i4, &sum, 0);
  localrc = ESMC_VMBroadcast(vm, &globalpass, 1, &i4, 0);
  if ((localrc == ESMF_SUCCESS) && (globalpass == 0) && writevtk)
    localrc = mbt_ngon_sph_elno_redist->print_vtk("mbt_ngon_sph_elno_redist");
#endif

#if defined ESMF_MOAB
  delete mbt_ngon_sph;
  if (do_redist) {
    delete mbt_ngon_sph_elem_redist;
    delete mbt_ngon_sph_node_redist;
    delete mbt_ngon_sph_elno_redist;
  }
#endif


  //----------------------------------------------------------------------------
  ESMC_TestEnd(__FILE__, __LINE__, 0);

  return 0;
}



