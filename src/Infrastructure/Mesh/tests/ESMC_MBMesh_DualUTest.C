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
#include "ESMC_MBMeshTestUtilPL.C"

// other headers
#include "ESMCI_MBMesh.h"
#include "ESMCI_MBMesh_Dual.h"

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

int main(int argc, char *argv[]) {

  char name[80];
  char failMsg[80];
  int result = 0;
  int rc;
  int merr;
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
  //NEX_disable_UTest
  MBMesh *mesh_quad = NULL;
  mesh_quad = create_mesh_quad_9_parallel_dual(ESMC_COORDSYS_CART, rc);
  if (!mesh_quad) rc = ESMC_RC_PTR_NULL;
  
  MBMesh *mesh_quad_dual = NULL;
  MBMeshDual(mesh_quad, &mesh_quad_dual, &rc);
  if (!mesh_quad_dual) rc = ESMC_RC_PTR_NULL;

  Range range_quad;
  merr=mesh_quad_dual->mesh->get_entities_by_dimension(0,
    mesh_quad_dual->pdim,range_quad);
  if (merr != MB_SUCCESS) rc = ESMF_FAILURE;
  
  // clean up
  delete mesh_quad;
  delete mesh_quad_dual;
#else
  rc = ESMF_SUCCESS;
#endif
  strcpy(name, "Quadrilaterals dual mesh creation");
  strcpy(failMsg, "Mesh creation failed");
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);

  // --------------------------------------------------------------------------
  // get entities from meshes created with spherical quadrilaterals
  // --------------------------------------------------------------------------
#if defined ESMF_MOAB

  //----------------------------------------------------------------------------
  //NEX_disable_UTest
  MBMesh *mesh_quad_sph = NULL;
  mesh_quad_sph = create_mesh_quad_sph(rc);
  if (!mesh_quad_sph) rc = ESMC_RC_PTR_NULL;

  MBMesh *mesh_quad_sph_dual = NULL;
  MBMeshDual(mesh_quad_sph, &mesh_quad_sph_dual, &rc);
  if (!mesh_quad_sph_dual) rc = ESMC_RC_PTR_NULL;

  Range range_quad_sph;
  merr = mesh_quad_sph_dual->mesh->get_entities_by_dimension(0,
    mesh_quad_sph_dual->pdim,range_quad_sph);
  if (merr != MB_SUCCESS) rc = ESMF_FAILURE;

  // clean up
  delete mesh_quad_sph;
  delete mesh_quad_sph_dual;

#else
  rc = ESMF_SUCCESS;
#endif
  strcpy(name, "Spherical quadrilaterals mesh creation");
  strcpy(failMsg, "Mesh creation failed");
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);

  // --------------------------------------------------------------------------
  // get entities from meshes created with triangles
  // --------------------------------------------------------------------------
// #if defined ESMF_MOAB
// 
//   //----------------------------------------------------------------------------
//   //NEX_disable_UTest
//   MBMesh *mesh_tri;
//   mesh_tri = create_mesh_tri(rc);
//   if (!mesh_tri) rc = ESMC_RC_PTR_NULL;
// 
//   MBMesh *mesh_tri_dual = NULL;
//   MBMeshDual(mesh_tri, &mesh_tri_dual, &rc);
//   if (!mesh_tri_dual) rc = ESMC_RC_PTR_NULL;
// 
//   Range range_tri;
//   merr=mesh_tri_dual->mesh->get_entities_by_dimension(0,mesh_tri_dual->pdim,range_tri);
//   if (merr != MB_SUCCESS) rc = ESMF_FAILURE;
// 
//   void *mbptr = (void *) mesh_tri;
//   int len = 12; char fname[len];
//   sprintf(fname, "mesh_tri_%d", localPet);
//   MBMesh_write(&mbptr, fname, &rc, len);
// 
//   void *mbptrd = (void *) mesh_tri_dual;
//   int lend = 17; char fnamed[lend];
//   sprintf(fnamed, "mesh_tri_dual_%d", localPet);
//   MBMesh_write(&mbptrd, fnamed, &rc, lend);
// 
// #else
//   rc = ESMF_SUCCESS;
// #endif
//   strcpy(name, "Triangles mesh creation");
//   strcpy(failMsg, "Mesh creation failed");
//   ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);

//   // --------------------------------------------------------------------------
//   // get entities from meshes created with spherical triangles
//   // --------------------------------------------------------------------------
// #if defined ESMF_MOAB
// 
//   //----------------------------------------------------------------------------
//   //NEX_disable_UTest
//   MBMesh *mesh_tri_sph;
//   mesh_tri_sph = create_mesh_tri_sph(rc);
//   if (!mesh_tri_sph) rc = ESMC_RC_PTR_NULL;
// #else
//   rc = ESMF_SUCCESS;
// #endif
//   strcpy(name, "Spherical triangles mesh creation");
//   strcpy(failMsg, "Mesh creation failed");
//   ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
// 
//   //----------------------------------------------------------------------------
//   //NEX_disable_UTest
// 
// #if defined ESMF_MOAB
//   Range range_tri_sph;
//   Interface *mb_mesh_tri_sph=mesh_tri_sph->mesh;
//   int merr_tri_sph=mb_mesh_tri_sph->get_entities_by_dimension(0,mesh_tri_sph->pdim,range_tri_sph);
//   if (merr_tri_sph != MB_SUCCESS) rc = ESMF_FAILURE;
// 
//   // clean up
//   delete mesh_tri_sph;
// #else
//   rc = ESMF_SUCCESS;
// #endif
// 
//   strcpy(name, "get_entities");
//   strcpy(failMsg, "Cannot get entities");
//   ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);
// 
//   // --------------------------------------------------------------------------
//   // get entities from meshes created with tetrahedrons
//   // --------------------------------------------------------------------------
// #if defined ESMF_MOAB
// 
//   //----------------------------------------------------------------------------
//   //NEX_disable_UTest
//   MBMesh *mesh_tet;
//   mesh_tet = create_mesh_tet(rc);
//   if (!mesh_tet) rc = ESMC_RC_PTR_NULL;
// #else
//   rc = ESMF_SUCCESS;
// #endif
//   strcpy(name, "Tetrahedrons mesh creation");
//   strcpy(failMsg, "Mesh creation failed");
//   ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
// 
//   //----------------------------------------------------------------------------
//   //NEX_disable_UTest
// #if defined ESMF_MOAB
//   Range range_tet;
//   Interface *mb_mesh_tet=mesh_tet->mesh;
//   int merr_tet=mb_mesh_tet->get_entities_by_dimension(0,mesh_tet->pdim,range_tet);
//   if (merr_tet != MB_SUCCESS) rc = ESMF_FAILURE;
// 
//   // clean up
//   delete mesh_tet;
// #else
//   rc = ESMF_SUCCESS;
// #endif
// 
//   strcpy(name, "get_entities");
//   strcpy(failMsg, "Cannot get entities");
//   ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);
// 
//   // --------------------------------------------------------------------------
//   // get entities from meshes created with hexahedrons
//   // --------------------------------------------------------------------------
// #if defined ESMF_MOAB
// 
//   //----------------------------------------------------------------------------
//   //NEX_disable_UTest
//   MBMesh *mesh_hex;
//   mesh_hex = create_mesh_hex(rc);
//   if (!mesh_hex) rc = ESMC_RC_PTR_NULL;
// #else
//   rc = ESMF_SUCCESS;
// #endif
//   strcpy(name, "Hexahedrons mesh creation");
//   strcpy(failMsg, "Mesh creation failed");
//   ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
// 
//   //----------------------------------------------------------------------------
//   //NEX_disable_UTest
// 
// #if defined ESMF_MOAB
//   Range range_hex;
//   Interface *mb_mesh_hex=mesh_hex->mesh;
//   int merr_hex=mb_mesh_hex->get_entities_by_dimension(0,mesh_hex->pdim,range_hex);
//   if (merr_hex != MB_SUCCESS) rc = ESMF_FAILURE;
// 
//   // clean up
//   delete mesh_hex;
// #else
//   rc = ESMF_SUCCESS;
// #endif
// 
//   strcpy(name, "get_entities");
//   strcpy(failMsg, "Cannot get entities");
//   ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  ESMC_TestEnd(__FILE__, __LINE__, 0);

  return 0;
}


