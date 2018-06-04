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

// other headers
#include "ESMCI_MBMesh.h"
#include "ESMCI_MBMesh_Glue.h"
#include "ESMCI_MBMesh_Search_EToP.h"
#include "ESMCI_MBMesh_Util.h"

#include "MBTagConventions.hpp"
#include "moab/Core.hpp"
#include "moab/ElemEvaluator.hpp"
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

bool is_inside (const MBMesh *mesh, const double *coords) {
  int rc;
  rc = ESMF_RC_NOT_IMPL;
  //Get MOAB Mesh
  Interface *moab_mesh=mesh->mesh;

  // Get regions, by dimension, so we stay generic to entity type
  Range elems;
  int rval = moab_mesh->get_entities_by_dimension(0, 2, elems);

  int num_inside = 0;
  for (Range::iterator it = elems.begin(); it != elems.end(); ++it) {
    EntityHandle elem = *it;

    // map the parametric coords of the cell
    int is_inside = 0;
    double pcoords[3];
    ElemEvaluator ee = ElemEvaluator(moab_mesh, elem);
    ee.reverse_eval(coords, 1e-8, 1e-6, pcoords, &is_inside);
    // translate pcoords from [-1,1] to [0,1]
    translate(pcoords);

    int elem_id = moab_mesh->id_from_handle(elem);

    if (is_inside) {
      //print
      std::cout << "    " << "Point is INSIDE Element " << elem_id
                << ": coords = [" << coords[0] << ", " << coords[1] << "]"
                << " : pcoords = [" << pcoords[0] << ", " << pcoords[1] << "]"
                << std::endl;
      ++num_inside;
    }
  }

  if (num_inside > 1) rc = ESMF_FAILURE;
  else rc = ESMF_SUCCESS;

  if (rc != ESMF_SUCCESS) return false;
  else return true;
}

MBMesh* create_mesh_simple_triangles(int &rc) {
  //

  //  1.0   3 -------- 4
  //  0.75  |  \    12 |
  //        |    \     |
  //  0.25  |  11   \  |
  //  0.0   1 -------- 2
  //
  //       0.0 .25 .75 1.0
  //
  //      Node Ids at corners
  //      Element Ids in centers
  //
  //
  //      ( Everything owned by PET 0)
  //

  rc = ESMF_RC_NOT_IMPL;

  int pdim = 2;
  int sdim = 2;

  // set Mesh parameters
  int num_elem = 2;
  int num_node = 4;

  int nodeId_s [] ={1,2,3,4};
  double nodeCoord_s [] ={0.0,0.0, 1.0,0.0,
                          0.0,1.0, 1.0,1.0};
  int nodeOwner_s [] ={0,0,0,0};
  int nodeMask_s [] ={1,1,1,1};
  int elemId_s [] ={11,12};
  // ESMF_MESHELEMTYPE_QUAD
  int elemType_s [] ={ESMC_MESHELEMTYPE_TRI,
                      ESMC_MESHELEMTYPE_TRI};
  int elemConn_s [] ={1,2,3,
                      //2,3,1 works!!
                      2,4,3};
  int elemMask_s [] ={1,1};

  ESMC_CoordSys_Flag local_coordSys=ESMC_COORDSYS_CART;

  int orig_sdim = sdim;

  MBMesh *mesh = new MBMesh();
  void *meshp = static_cast<void *> (mesh);

  MBMesh_create(&meshp, &pdim, &sdim, &local_coordSys, &rc);
  if (rc != ESMF_SUCCESS) return NULL;

  InterArray<int> *ii_node = new InterArray<int>(nodeMask_s,4);

  MBMesh_addnodes(&meshp, &num_node, nodeId_s, nodeCoord_s, nodeOwner_s,
                  ii_node, &local_coordSys, &orig_sdim, &rc);
  if (rc != ESMF_SUCCESS) return NULL;

  InterArray<int> *ii_elem = new InterArray<int>(elemMask_s,2);

  int areapresent = 0;
  int coordspresent = 0;
  int numelemconn = 6;
  int regridconserve = 0;
  MBMesh_addelements(&meshp, &num_elem, elemId_s, elemType_s, ii_elem,
                     &areapresent, NULL,
                     &coordspresent, NULL,
                     &numelemconn, elemConn_s,
                     &regridconserve,
                     &local_coordSys, &orig_sdim, &rc);
  if (rc != ESMF_SUCCESS) return NULL;

  delete ii_node;
  delete ii_elem;

  rc = ESMF_SUCCESS;
  return static_cast<MBMesh *>(meshp);
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
  // triangle mesh that shows points in multiple triangles
  // --------------------------------------------------------------------------

#if 0
#if defined ESMF_MOAB
  // this test demonstrates behavior where point appears in multiple triangular
  // elements if the elements are created with a specific node connectivity
  //----------------------------------------------------------------------------
  //NEX_disable_UTest
  MBMesh *mesh_tri_simple;
  mesh_tri_simple = create_mesh_simple_triangles(rc);
  if (!mesh_tri_simple) rc = ESMC_RC_PTR_NULL;
#endif
  strcpy(name, "Triangles mesh creation");
  strcpy(failMsg, "Mesh creation failed");
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);

#if defined ESMF_MOAB
  //----------------------------------------------------------------------------
  //NEX_disable_UTest
  double coords[2];
  coords[0] = 0.75, coords[1] = 0.75;
  strcpy(name, "is_inside");
  strcpy(failMsg, "A node was found in two different cells");
  ESMC_Test((is_inside(mesh_tri_simple, coords)), name, failMsg, &result, __FILE__, __LINE__, 0);

  // clean up
  delete mesh_tri_simple;
#else
  rc = ESMF_SUCCESS;
#endif
  strcpy(name, "is_inside");
  strcpy(failMsg, "A node was found in two different cells");
  ESMC_Test(ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);
#endif
  // --------------------------------------------------------------------------

  // --------------------------------------------------------------------------
  // get entities from meshes created with quadrilaterals
  // --------------------------------------------------------------------------
#if defined ESMF_MOAB

  //----------------------------------------------------------------------------
  //NEX_UTest
  MBMesh *mesh_quad;
  mesh_quad = create_mesh_quad(rc);
  if (!mesh_quad) rc = ESMC_RC_PTR_NULL;
#else
  rc = ESMF_SUCCESS;
#endif
  strcpy(name, "Quadrilaterals mesh creation");
  strcpy(failMsg, "Mesh creation failed");
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest

#if defined ESMF_MOAB
  Range range_quad;
  Interface *mb_mesh_quad=mesh_quad->mesh;
  int merr_quad=mb_mesh_quad->get_entities_by_dimension(0,mesh_quad->pdim,range_quad);
  if (merr_quad != MB_SUCCESS) rc = ESMF_FAILURE;

  // clean up
  delete mesh_quad;
#else
  rc = ESMF_SUCCESS;
#endif

  strcpy(name, "get_entities");
  strcpy(failMsg, "Cannot get entities");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);

  // --------------------------------------------------------------------------
  // get entities from meshes created with spherical quadrilaterals
  // --------------------------------------------------------------------------
#if defined ESMF_MOAB

  //----------------------------------------------------------------------------
  //NEX_UTest
  MBMesh *mesh_quad_sph;
  mesh_quad_sph = create_mesh_quad_sph(rc);
  if (!mesh_quad_sph) rc = ESMC_RC_PTR_NULL;
#else
  rc = ESMF_SUCCESS;
#endif
  strcpy(name, "Spherical quadrilaterals mesh creation");
  strcpy(failMsg, "Mesh creation failed");
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest

#if defined ESMF_MOAB
  Range range_quad_sph;
  Interface *mb_mesh_quad_sph=mesh_quad_sph->mesh;
  int merr_quad_sph=mb_mesh_quad_sph->get_entities_by_dimension(0,mesh_quad_sph->pdim,range_quad_sph);
  if (merr_quad_sph != MB_SUCCESS) rc = ESMF_FAILURE;
  // clean up
  delete mesh_quad_sph;
#else
  rc = ESMF_SUCCESS;
#endif

  strcpy(name, "get_entities");
  strcpy(failMsg, "Cannot get entities");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);

  // --------------------------------------------------------------------------
  // get entities from meshes created with triangles
  // --------------------------------------------------------------------------
#if defined ESMF_MOAB

  //----------------------------------------------------------------------------
  //NEX_UTest
  MBMesh *mesh_tri;
  mesh_tri = create_mesh_tri(rc);
  if (!mesh_tri) rc = ESMC_RC_PTR_NULL;
#else
  rc = ESMF_SUCCESS;
#endif
  strcpy(name, "Triangles mesh creation");
  strcpy(failMsg, "Mesh creation failed");
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest

#if defined ESMF_MOAB
  Range range_tri;
  Interface *mb_mesh_tri=mesh_tri->mesh;
  int merr_tri=mb_mesh_tri->get_entities_by_dimension(0,mesh_tri->pdim,range_tri);
  if (merr_tri != MB_SUCCESS) rc = ESMF_FAILURE;

  // clean up
  delete mesh_tri;
#else
  rc = ESMF_SUCCESS;
#endif

  strcpy(name, "get_entities");
  strcpy(failMsg, "Cannot get entities");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);

  // --------------------------------------------------------------------------
  // get entities from meshes created with spherical triangles
  // --------------------------------------------------------------------------
#if defined ESMF_MOAB

  //----------------------------------------------------------------------------
  //NEX_UTest
  MBMesh *mesh_tri_sph;
  mesh_tri_sph = create_mesh_tri_sph(rc);
  if (!mesh_tri_sph) rc = ESMC_RC_PTR_NULL;
#else
  rc = ESMF_SUCCESS;
#endif
  strcpy(name, "Spherical triangles mesh creation");
  strcpy(failMsg, "Mesh creation failed");
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest

#if defined ESMF_MOAB
  Range range_tri_sph;
  Interface *mb_mesh_tri_sph=mesh_tri_sph->mesh;
  int merr_tri_sph=mb_mesh_tri_sph->get_entities_by_dimension(0,mesh_tri_sph->pdim,range_tri_sph);
  if (merr_tri_sph != MB_SUCCESS) rc = ESMF_FAILURE;

  // clean up
  delete mesh_tri_sph;
#else
  rc = ESMF_SUCCESS;
#endif

  strcpy(name, "get_entities");
  strcpy(failMsg, "Cannot get entities");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);

  // --------------------------------------------------------------------------
  // get entities from meshes created with tetrahedrons
  // --------------------------------------------------------------------------
#if defined ESMF_MOAB

  //----------------------------------------------------------------------------
  //NEX_UTest
  MBMesh *mesh_tet;
  mesh_tet = create_mesh_tet(rc);
  if (!mesh_tet) rc = ESMC_RC_PTR_NULL;
#else
  rc = ESMF_SUCCESS;
#endif
  strcpy(name, "Tetrahedrons mesh creation");
  strcpy(failMsg, "Mesh creation failed");
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest
#if defined ESMF_MOAB
  Range range_tet;
  Interface *mb_mesh_tet=mesh_tet->mesh;
  int merr_tet=mb_mesh_tet->get_entities_by_dimension(0,mesh_tet->pdim,range_tet);
  if (merr_tet != MB_SUCCESS) rc = ESMF_FAILURE;

  // clean up
  delete mesh_tet;
#else
  rc = ESMF_SUCCESS;
#endif

  strcpy(name, "get_entities");
  strcpy(failMsg, "Cannot get entities");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);

  // --------------------------------------------------------------------------
  // get entities from meshes created with hexahedrons
  // --------------------------------------------------------------------------
#if defined ESMF_MOAB

  //----------------------------------------------------------------------------
  //NEX_UTest
  MBMesh *mesh_hex;
  mesh_hex = create_mesh_hex(rc);
  if (!mesh_hex) rc = ESMC_RC_PTR_NULL;
#else
  rc = ESMF_SUCCESS;
#endif
  strcpy(name, "Hexahedrons mesh creation");
  strcpy(failMsg, "Mesh creation failed");
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //NEX_UTest

#if defined ESMF_MOAB
  Range range_hex;
  Interface *mb_mesh_hex=mesh_hex->mesh;
  int merr_hex=mb_mesh_hex->get_entities_by_dimension(0,mesh_hex->pdim,range_hex);
  if (merr_hex != MB_SUCCESS) rc = ESMF_FAILURE;

  // clean up
  delete mesh_hex;
#else
  rc = ESMF_SUCCESS;
#endif

  strcpy(name, "get_entities");
  strcpy(failMsg, "Cannot get entities");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  ESMC_TestEnd(__FILE__, __LINE__, 0);

  return 0;
}


