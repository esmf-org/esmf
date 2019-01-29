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
#include "moab/ParallelComm.hpp"

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

#if defined ESMF_MOAB
MBMesh* create_mesh_quad_single(int &rc, bool cart, bool collapsed = false) {
  //

  //  2.0   4 -------- 3
  //        |          |
  //  1.0   |    11    |
  //        |          |
  //  0.0   1 -------- 2
  //
  //       0.0   1.0   2.0
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
  int num_elem = 1;
  int num_node = 4;

  double pi = 3.14159;
  double * nodeCoord;
  nodeCoord = (double *) malloc (2*num_node * sizeof (double));

  ESMC_CoordSys_Flag coordSys=ESMC_COORDSYS_CART;
  if (cart) {
    nodeCoord[0] = 0.0;
    nodeCoord[1] = 0.0;
    nodeCoord[2] = 2.0;
    nodeCoord[3] = 0.0;
    nodeCoord[4] = 2.0;
    nodeCoord[5] = 2.0;
    nodeCoord[6] = 0.0;
    nodeCoord[7] = 2.0;
    if (collapsed)
      nodeCoord[6] = 2.0;

  } else {
    coordSys = ESMC_COORDSYS_SPH_RAD;
//    sdim = 3;

    nodeCoord[0] = 0.0;
    nodeCoord[1] = 0.0;
    nodeCoord[2] = pi/4;
    nodeCoord[3] = 0.0;
    nodeCoord[4] = pi/4;
    nodeCoord[5] = pi/4;
    nodeCoord[6] = 0.0;
    nodeCoord[7] = pi/4;
    if (collapsed)
      nodeCoord[6] = pi/4;
  }

  int nodeId_s [] ={1,2,3,4};
  int nodeOwner_s [] ={0,0,0,0};
  int nodeMask_s [] ={0,0,0,0};
  int elemId_s [] ={11};
  int elemType_s [] ={ESMC_MESHELEMTYPE_QUAD};
  int elemConn_s [] ={1,2,3,4};
  int elemMask_s [] ={1};

  MBMesh *mesh = new MBMesh();
  void *meshp = static_cast<void *> (mesh);

  MBMesh_create(&meshp, &pdim, &sdim, &coordSys, &rc);
  if (rc != ESMF_SUCCESS) return NULL;

  InterArray<int> *ii_node = new InterArray<int>(nodeMask_s,4);

  MBMesh_addnodes(&meshp, &num_node, nodeId_s, nodeCoord, nodeOwner_s,
                  ii_node, &coordSys, &sdim, &rc);
  if (rc != ESMF_SUCCESS) return NULL;


  InterArray<int> *ii_elem = new InterArray<int>(elemMask_s,1);

  int areapresent = 0;
  int coordspresent = 0;
  int numelemconn = 4;
  int regridconserve = 0;
  MBMesh_addelements(&meshp, &num_elem, elemId_s, elemType_s, ii_elem,
                     &areapresent, NULL,
                     &coordspresent, NULL,
                     &numelemconn, elemConn_s,
                     &regridconserve,
                     &coordSys, &sdim, &rc);
  if (rc != ESMF_SUCCESS) return NULL;

MBMesh *mbmesh = static_cast<MBMesh *>(meshp);

#ifdef DEBUG_MASK
  EntityHandle *verts=new EntityHandle[num_node];
  mbmesh->verts=verts;
  printf("~~~~~~~~~~~~~~ DEBUG - ESMCI_MBMESH_BILINEAR_TEST ~~~~~~~~~~~~~~~\n");
  printf("ESMCI_MBMESH_BILINEAR_TEST - has_node_mask == %s\n", mbmesh->has_node_mask ? "true" : "false");
  
  int testmask[num_node];
  int merr;
  // this fails with a moab error, invalid tag ( try passing elements instead of verts?)
  Range nodes;
  merr=mbmesh->mesh->get_entities_by_dimension(0, 0, nodes);
  if (merr != MB_SUCCESS) throw (ESMC_RC_MOAB_ERROR);

  merr=mbmesh->mesh->tag_get_data(mbmesh->node_mask_val_tag,  nodes, testmask);
  if (merr != MB_SUCCESS) throw (ESMC_RC_MOAB_ERROR);

  printf("ESMCI_MBMESH_BILINEAR_TEST - tag_get_data test mask is = [");
  for (int i = 0; i < num_node; ++i)
    printf("%d, ", testmask[i]);
  printf("]\n");
  printf("~~~~~~~~~~~~~~ DEBUG - ESMCI_MBMESH_BILINEAR_TEST ~~~~~~~~~~~~~~~\n");
#endif


  delete ii_node;
  delete ii_elem;

  rc = ESMF_SUCCESS;
  return mbmesh;
}

MBMesh* create_mesh_tri_single(int &rc, bool cart) {
  //

  //  1+sqrt(2)  3
  //            /  \
  //  1.0     /  11  \
  //         /        \
  //  0.0   1 -------- 2
  //
  //       0.0   1.0   2.0
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
  int num_elem = 1;
  int num_node = 3;

  double pi = 3.14159;
  double * nodeCoord;
  nodeCoord = (double *) malloc (2*num_node * sizeof (double));

  ESMC_CoordSys_Flag coordSys=ESMC_COORDSYS_CART;
  if (cart) {
    nodeCoord[0] = 0.0;
    nodeCoord[1] = 0.0;
    nodeCoord[2] = 2.0;
    nodeCoord[3] = 0.0;
    nodeCoord[4] = 1.0;
    nodeCoord[5] = 1.0+sqrt(2.0);

  } else {
    coordSys = ESMC_COORDSYS_SPH_RAD;
//    sdim = 3;

    nodeCoord[0] = 0.0;
    nodeCoord[1] = 0.0;
    nodeCoord[2] = pi/4;
    nodeCoord[3] = 0.0;
    nodeCoord[4] = pi/8;
    nodeCoord[5] = pi/8+sqrt(2.0)*pi/8;

  }

  int nodeId_s [] ={1,2,3};
  int nodeOwner_s [] ={0,0,0};
  int nodeMask_s [] ={0,0,0};
  int elemId_s [] ={11};
  int elemType_s [] ={ESMC_MESHELEMTYPE_TRI};
  int elemConn_s [] ={1,2,3};
  int elemMask_s [] ={1};

  MBMesh *mesh = new MBMesh();
  void *meshp = static_cast<void *> (mesh);

  MBMesh_create(&meshp, &pdim, &sdim, &coordSys, &rc);
  if (rc != ESMF_SUCCESS) return NULL;

  InterArray<int> *ii_node = new InterArray<int>(nodeMask_s,3);

  MBMesh_addnodes(&meshp, &num_node, nodeId_s, nodeCoord, nodeOwner_s,
                  ii_node, &coordSys, &sdim, &rc);
  if (rc != ESMF_SUCCESS) return NULL;

  InterArray<int> *ii_elem = new InterArray<int>(elemMask_s,1);

  int areapresent = 0;
  int coordspresent = 0;
  int numelemconn = 3;
  int regridconserve = 0;
  MBMesh_addelements(&meshp, &num_elem, elemId_s, elemType_s, ii_elem,
                     &areapresent, NULL,
                     &coordspresent, NULL,
                     &numelemconn, elemConn_s,
                     &regridconserve,
                     &coordSys, &sdim, &rc);
  if (rc != ESMF_SUCCESS) return NULL;

  delete ii_node;
  delete ii_elem;

  rc = ESMF_SUCCESS;
  return static_cast<MBMesh *>(meshp);
}

PointList* create_pointlist_on_edge(int &rc, bool cart) {
  //
  //

  //  0.0(0)   x
  //
  //              1.0
  //             pi/8
  //

  rc = ESMF_RC_NOT_IMPL;

  int np = 1;
  int dim;

  double pi = 3.14159;

  double x[np];
  double y[np];
  double z[np];

  if (cart) {
    x[0] = 1.0;
    y[0] = 0.0;
    dim = 2;
  } else {
    double phi = 0;
    double theta = pi/8;
    double r = 1.0;

    x[0] = r * cos(phi) * cos(theta);
    y[0] = r * cos(phi) * sin(theta);
    z[0] = r * sin(phi);
    dim = 3;
  }

  PointList *pl = new PointList(np, dim);

  for (int i = 0; i < np; ++i) {
    point p = point();
    p.id = i+1;
    p.coords[0] = x[i];
    p.coords[1] = y[i];
    if (!cart)
      p.coords[2] = z[i];
    pl->add(p.id, p.coords);
  }

  rc = ESMF_SUCCESS;
  return pl;
}

PointList* create_pointlist_on_node(int &rc, bool cart) {
  //
  //

  //  0.0(0)   x
  //
  //              0.0
  //               0
  //

  rc = ESMF_RC_NOT_IMPL;

  int np = 1;
  int dim;

  double pi = 3.14159;

  double x[np];
  double y[np];
  double z[np];

  if (cart) {
    x[0] = 0.0;
    y[0] = 0.0;
    dim = 2;
  } else {
    double phi = 0;
    double theta = 0;
    double r = 1.0;

    x[0] = r * cos(phi) * cos(theta);
    y[0] = r * cos(phi) * sin(theta);
    z[0] = r * sin(phi);
    dim = 3;
  }

  PointList *pl = new PointList(np, dim);

  for (int i = 0; i < np; ++i) {
    point p = point();
    p.id = i+1;
    p.coords[0] = x[i];
    p.coords[1] = y[i];
    if (!cart)
      p.coords[2] = z[i];
    pl->add(p.id, p.coords);
  }

  rc = ESMF_SUCCESS;
  return pl;
}

PointList* create_pointlist_for_quad_single(int &rc, bool cart) {
  //
  //

  //  1.0(pi/8)   x
  //
  //              1.0
  //             pi/8
  //

  rc = ESMF_RC_NOT_IMPL;

  int np = 1;
  int dim;

  double pi = 3.14159;

  double x[np];
  double y[np];
  double z[np];

  if (cart) {
    x[0] = 1.0;
    y[0] = 1.0;
    dim = 2;
  } else {
    double phi = pi/8;
    double theta = pi/8;
    double r = 1.0;

    x[0] = r * cos(phi) * cos(theta);
    y[0] = r * cos(phi) * sin(theta);
    z[0] = r * sin(phi);
    dim = 3;
  }

  PointList *pl = new PointList(np, dim);

  for (int i = 0; i < np; ++i) {
    point p = point();
    p.id = i+1;
    p.coords[0] = x[i];
    p.coords[1] = y[i];
    if (!cart)
      p.coords[2] = z[i];
    pl->add(p.id, p.coords);
  }

  rc = ESMF_SUCCESS;
  return pl;
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

  // Get parallel information
  vm=ESMC_VMGetGlobal(&rc);
  if (rc != ESMF_SUCCESS) return 0;

  rc=ESMC_VMGet(vm, &localPet, &petCount, (int *)NULL, (MPI_Comm *)NULL,
                (int *)NULL, (int *)NULL);
  if (rc != ESMF_SUCCESS) return 0;

#if defined ESMF_MOAB

  MBMesh *mesh_quad_single = NULL;
  MBMesh *mesh_tri_single = NULL;
  PointList *pl_on_edge = NULL;
  PointList *pl_on_node = NULL;
  PointList *pl = NULL;
  bool cart =  false;
  bool collapsed = false;

  std::vector<double> weights;
  weights.resize(4);

#else

  void *mesh_quad_single = NULL;
  void *mesh_tri_single = NULL;
  void *pl_on_edge = NULL;
  void *pl_on_node = NULL;
  void *pl = NULL;
  bool cart =  false;
  bool collapsed = false;

#endif

#if defined ESMF_MOAB

  // --------------------------------------------------------------------------
  // quad mesh bilinear cartesian with pointlist point on edge
  // --------------------------------------------------------------------------

  cart = true;

  // build a mesh
  mesh_quad_single = create_mesh_quad_single(rc, cart);

  // build a pointlist
  pl_on_edge = create_pointlist_on_edge(rc, cart);
  
  // expected result
  std::fill(weights.begin(), weights.end(), UNINITVAL);

  //----------------------------------------------------------------------------
  //NEX_disable_UTest
  strcpy(name, "Quadrilateral Cartesian bilinear weight generation with pointlist point on edge");
  strcpy(failMsg, "Weights were not generated correctly");
  ESMC_Test((weight_gen(mesh_quad_single, pl_on_edge, weights, cart)), name, failMsg, &result, __FILE__, __LINE__, 0);

  // clean up
  delete pl_on_edge;
  delete mesh_quad_single;
#else
  strcpy(name, "Quadrilateral Cartesian bilinear weight generation with pointlist point on edge");
  strcpy(failMsg, "Weights were not generated correctly");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);
#endif

  // --------------------------------------------------------------------------
  // quad mesh bilinear spherical with pointlist point on edge
  // --------------------------------------------------------------------------

#if defined ESMF_MOAB

  cart = false;

  // build a mesh
  mesh_quad_single = create_mesh_quad_single(rc, cart);

  // build a pointlist
  pl_on_edge = create_pointlist_on_edge(rc, cart);

  //----------------------------------------------------------------------------
  //NEX_disable_UTest
  strcpy(name, "Quadrilateral spherical bilinear weight generation with pointlist point on edge");
  strcpy(failMsg, "Weights were not generated correctly");
  ESMC_Test((weight_gen(mesh_quad_single, pl_on_edge, weights, cart)), name, failMsg, &result, __FILE__, __LINE__, 0);

  // clean up
  delete pl_on_edge;
  delete mesh_quad_single;
#else
  strcpy(name, "Quadrilateral spherical bilinear weight generation with pointlist point on edge");
  strcpy(failMsg, "Weights were not generated correctly");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);
#endif

  // --------------------------------------------------------------------------
  // quad mesh bilinear cartesian with pointlist point on node
  // --------------------------------------------------------------------------

#if defined ESMF_MOAB

  cart = true;

  // build a mesh
  mesh_quad_single = create_mesh_quad_single(rc, cart);

  // build a pointlist
  pl_on_node = create_pointlist_on_node(rc, cart);

  //----------------------------------------------------------------------------
  //NEX_disable_UTest
  strcpy(name, "Quadrilateral Cartesian bilinear weight generation with pointlist point on node");
  strcpy(failMsg, "Weights were not generated correctly");
  ESMC_Test((weight_gen(mesh_quad_single, pl_on_node, weights, cart)), name, failMsg, &result, __FILE__, __LINE__, 0);

  // clean up
  delete pl_on_node;
  delete mesh_quad_single;
#else
  strcpy(name, "Quadrilateral Cartesian bilinear weight generation with pointlist point on node");
  strcpy(failMsg, "Weights were not generated correctly");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);
#endif

  // --------------------------------------------------------------------------
  // quad mesh bilinear spherical with pointlist point on node
  // --------------------------------------------------------------------------

#if defined ESMF_MOAB

  cart = false;

  // build a mesh
  mesh_quad_single = create_mesh_quad_single(rc, cart);

  // build a pointlist
  pl_on_node = create_pointlist_on_node(rc, cart);

  //----------------------------------------------------------------------------
  //NEX_disable_UTest
  strcpy(name, "Quadrilateral spherical bilinear weight generation with pointlist point on node");
  strcpy(failMsg, "Weights were not generated correctly");
  ESMC_Test((weight_gen(mesh_quad_single, pl_on_node, weights, cart)), name, failMsg, &result, __FILE__, __LINE__, 0);

  // clean up
  delete pl_on_node;
  delete mesh_quad_single;
#else
  strcpy(name, "Quadrilateral spherical bilinear weight generation with pointlist point on node");
  strcpy(failMsg, "Weights were not generated correctly");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);
#endif

  // --------------------------------------------------------------------------
  // tri mesh bilinear cartesian with pointlist point on edge
  // --------------------------------------------------------------------------

#if defined ESMF_MOAB

  cart = true;

  // build a mesh
  mesh_tri_single = create_mesh_tri_single(rc, cart);

  // build a pointlist
  pl_on_edge = create_pointlist_on_edge(rc, cart);

  //----------------------------------------------------------------------------
  //NEX_disable_UTest
  strcpy(name, "Triangle Cartesian bilinear weight generation with pointlist point on edge");
  strcpy(failMsg, "Weights were not generated correctly");
  ESMC_Test((weight_gen(mesh_tri_single, pl_on_edge, weights, cart)), name, failMsg, &result, __FILE__, __LINE__, 0);

  // clean up
  delete pl_on_edge;
  delete mesh_tri_single;
#else
  strcpy(name, "Triangle Cartesian bilinear weight generation with pointlist point on edge");
  strcpy(failMsg, "Weights were not generated correctly");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);
#endif

  // --------------------------------------------------------------------------
  // tri mesh bilinear spherical with pointlist point on edge
  // --------------------------------------------------------------------------

#if defined ESMF_MOAB

  cart = false;

  // build a mesh
  mesh_tri_single = create_mesh_tri_single(rc, cart);

  // build a pointlist
  pl_on_edge = create_pointlist_on_edge(rc, cart);

  //----------------------------------------------------------------------------
  //NEX_disable_UTest
  strcpy(name, "Triangle spherical bilinear weight generation with pointlist point on edge");
  strcpy(failMsg, "Weights were not generated correctly");
  ESMC_Test((weight_gen(mesh_tri_single, pl_on_edge, weights, cart)), name, failMsg, &result, __FILE__, __LINE__, 0);

  // clean up
  delete pl_on_edge;
  delete mesh_tri_single;
#else
  strcpy(name, "Triangle spherical bilinear weight generation with pointlist point on edge");
  strcpy(failMsg, "Weights were not generated correctly");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);
#endif

  // --------------------------------------------------------------------------
  // tri mesh bilinear cartesian with pointlist point on node
  // --------------------------------------------------------------------------

#if defined ESMF_MOAB

  cart = true;

  // build a mesh
  mesh_tri_single = create_mesh_tri_single(rc, cart);

  // build a pointlist
  pl_on_node = create_pointlist_on_node(rc, cart);

  //----------------------------------------------------------------------------
  //NEX_disable_UTest
  strcpy(name, "Triangle Cartesian bilinear weight generation with pointlist point on node");
  strcpy(failMsg, "Weights were not generated correctly");
  ESMC_Test((weight_gen(mesh_tri_single, pl_on_node, weights, cart)), name, failMsg, &result, __FILE__, __LINE__, 0);

  // clean up
  delete pl_on_node;
  delete mesh_tri_single;
#else
  strcpy(name, "Triangle Cartesian bilinear weight generation with pointlist point on node");
  strcpy(failMsg, "Weights were not generated correctly");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);
#endif

  // --------------------------------------------------------------------------
  // tri mesh bilinear spherical with pointlist point on node
  // --------------------------------------------------------------------------

#if defined ESMF_MOAB

  cart = false;

  // build a mesh
  mesh_tri_single = create_mesh_tri_single(rc, cart);

  // build a pointlist
  pl_on_node = create_pointlist_on_node(rc, cart);

  //----------------------------------------------------------------------------
  //NEX_disable_UTest
  strcpy(name, "Triangle spherical bilinear weight generation with pointlist point on node");
  strcpy(failMsg, "Weights were not generated correctly");
  ESMC_Test((weight_gen(mesh_tri_single, pl_on_node, weights, cart)), name, failMsg, &result, __FILE__, __LINE__, 0);

  // clean up
  delete pl_on_node;
  delete mesh_tri_single;
#else
  strcpy(name, "Triangle spherical bilinear weight generation with pointlist point on node");
  strcpy(failMsg, "Weights were not generated correctly");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);
#endif

  // --------------------------------------------------------------------------
  // collapsed quad to tri bilinear cartesian
  // --------------------------------------------------------------------------

#if defined ESMF_MOAB

  cart = true;
  collapsed = true;

  // build a mesh
  mesh_quad_single = create_mesh_quad_single(rc, cart, collapsed);

  // build a pointlist
  pl = create_pointlist_for_quad_single(rc, cart);

  //----------------------------------------------------------------------------
  //NEX_disable_UTest
  strcpy(name, "Collapsed quadrilateral to triangle Cartesian bilinear weight generation");
  strcpy(failMsg, "Weights were not generated correctly");
  ESMC_Test((weight_gen(mesh_quad_single, pl, weights, cart)), name, failMsg, &result, __FILE__, __LINE__, 0);

  // clean up
  delete pl;
  delete mesh_quad_single;
#else
  strcpy(name, "Collapsed quadrilateral to triangle Cartesian bilinear weight generation");
  strcpy(failMsg, "Weights were not generated correctly");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);
#endif

  // --------------------------------------------------------------------------
  // collapsed quad to tri bilinear cartesian
  // --------------------------------------------------------------------------

#if defined ESMF_MOAB

  cart = false;
  collapsed = true;

  // build a mesh
  mesh_quad_single = create_mesh_quad_single(rc, cart, collapsed);

  // build a pointlist
  pl = create_pointlist_for_quad_single(rc, cart);

  //----------------------------------------------------------------------------
  //NEX_disable_UTest
  strcpy(name, "Collapsed quadrilateral to triangle spherical bilinear weight generation");
  strcpy(failMsg, "Weights were not generated correctly");
  ESMC_Test((weight_gen(mesh_quad_single, pl, weights, cart)), name, failMsg, &result, __FILE__, __LINE__, 0);

  // clean up
  delete pl;
  delete mesh_quad_single;
#else
  strcpy(name, "Collapsed quadrilateral to triangle spherical bilinear weight generation");
  strcpy(failMsg, "Weights were not generated correctly");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);
#endif

  // --------------------------------------------------------------------------
  //----------------------------------------------------------------------------
  ESMC_TestEnd(__FILE__, __LINE__, 0);

  return 0;
}


