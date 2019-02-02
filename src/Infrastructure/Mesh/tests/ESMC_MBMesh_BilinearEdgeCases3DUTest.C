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
#include "ESMCI_MBMesh_Mapping.h"
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

#if defined ESMF_MOAB

MBMesh* create_mesh_quad_single(int &rc, bool cart, bool collapsed = false) {
  //           8----------7
  //          /         / |
  //  2.0   5 -------- 6  |
  //        |  |       |  |
  //  2.0   |  4 ------|- 3
  //        | /        | /
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

  int pdim = 3;
  int sdim = 3;

  // set Mesh parameters
  int num_elem = 1;
  int num_node = 8;

  double pi = 3.14159;
  double * nodeCoord;
  nodeCoord = (double *) malloc (pdim*num_node * sizeof (double));

  ESMC_CoordSys_Flag coordSys=ESMC_COORDSYS_CART;
  if (cart) {
    nodeCoord[0] = 0.0;
    nodeCoord[1] = 0.0;
    nodeCoord[2] = 0.0;
    nodeCoord[3] = 2.0;
    nodeCoord[4] = 0.0;
    nodeCoord[5] = 0.0;
    nodeCoord[6] = 2.0;
    nodeCoord[7] = 2.0;
    nodeCoord[8] = 0.0;
    nodeCoord[9] = 0.0;
    nodeCoord[10] = 2.0;
    nodeCoord[11] = 0.0;
    
    nodeCoord[12] = 0.0;
    nodeCoord[13] = 0.0;
    nodeCoord[14] = 2.0;
    nodeCoord[15] = 2.0;
    nodeCoord[16] = 0.0;
    nodeCoord[17] = 2.0;
    nodeCoord[18] = 2.0;
    nodeCoord[19] = 2.0;
    nodeCoord[20] = 2.0;
    nodeCoord[21] = 0.0;
    nodeCoord[22] = 2.0;
    nodeCoord[23] = 2.0;
  } else {
    coordSys = ESMC_COORDSYS_SPH_RAD;
    nodeCoord[0] = 0.0;
    nodeCoord[1] = 0.0;
    nodeCoord[2] = 0.0;
    nodeCoord[3] = pi/4;
    nodeCoord[4] = 0.0;
    nodeCoord[5] = 0.0;
    nodeCoord[6] = pi/4;
    nodeCoord[7] = pi/4;
    nodeCoord[8] = 0.0;
    nodeCoord[9] = 0.0;
    nodeCoord[10] = pi/4;
    nodeCoord[11] = 0.0;
    
    nodeCoord[12] = 0.0;
    nodeCoord[13] = 0.0;
    nodeCoord[14] = pi/4;
    nodeCoord[15] = pi/4;
    nodeCoord[16] = 0.0;
    nodeCoord[17] = pi/4;
    nodeCoord[18] = pi/4;
    nodeCoord[19] = pi/4;
    nodeCoord[20] = pi/4;
    nodeCoord[21] = 0.0;
    nodeCoord[22] = pi/4;
    nodeCoord[23] = pi/4;
  }

  int nodeId_s [] ={1,2,3,4,5,6,7,8};
  int nodeOwner_s [] ={0,0,0,0,0,0,0,0};
  int nodeMask_s [] ={1,1,1,1,1,1,1,1};
  int elemId_s [] ={11};
  int elemType_s [] ={ESMC_MESHELEMTYPE_HEX};
  int elemConn_s [] ={1,2,4,3,5,6,8,7};
  int elemMask_s [] ={1};

  MBMesh *mesh = new MBMesh();
  void *meshp = static_cast<void *> (mesh);

  MBMesh_create(&meshp, &pdim, &sdim, &coordSys, &rc);
  if (rc != ESMF_SUCCESS) return NULL;

  InterArray<int> *ii_node = new InterArray<int>(nodeMask_s,8);

  MBMesh_addnodes(&meshp, &num_node, nodeId_s, nodeCoord, nodeOwner_s,
                  ii_node, &coordSys, &sdim, &rc);
  if (rc != ESMF_SUCCESS) return NULL;

  InterArray<int> *ii_elem = new InterArray<int>(elemMask_s,1);

  int areapresent = 0;
  int coordspresent = 0;
  int numelemconn = 8;
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
    z[0] = 0.0;
    dim = 3;
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
    z[0] = 0.0;
    dim = 3;
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

PointList* create_pointlist_on_face(int &rc, bool cart) {
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
    z[0] = 0.0;
    dim = 3;
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
    z[0] = 1.0;
    dim = 3;
  } else {
    // double phi = pi/8;
    // double theta = pi/8;
    // double r = 1.0;
    // 
    // x[0] = r * cos(phi) * cos(theta);
    // y[0] = r * cos(phi) * sin(theta);
    // z[0] = r * sin(phi);
    
    x[0] = pi/8;
    y[0] = pi/8;
    z[0] = pi/8;

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

  char name[100];
  char failMsg[100];
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
  PointList *pl_on_edge = NULL;
  PointList *pl_on_node = NULL;
  PointList *pl_on_face = NULL;
  PointList *pl = NULL;
  bool cart =  false;
  bool collapsed = false;
  std::vector<double> weights;
  weights.resize(4);

#else

  void *mesh_quad_single = NULL;
  void *pl_on_edge = NULL;
  void *pl_on_node = NULL;
  void *pl_on_face = NULL;
  void *pl = NULL;
  bool cart =  false;
  bool collapsed = false;

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
  
  // expected result
  std::fill(weights.begin(), weights.end(), UNINITVAL);

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

#if defined ESMF_MOAB

  // --------------------------------------------------------------------------
  // quad mesh bilinear cartesian with pointlist point on edge
  // --------------------------------------------------------------------------

  cart = true;

  // build a mesh
  mesh_quad_single = create_mesh_quad_single(rc, cart);

  // build a pointlist
  pl_on_edge = create_pointlist_on_edge(rc, cart);

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
  // quad mesh bilinear cartesian with pointlist point on face
  // --------------------------------------------------------------------------

#if defined ESMF_MOAB

  cart = true;
  
  // build a mesh
  mesh_quad_single = create_mesh_quad_single(rc, cart);
  
  // build a pointlist
  pl_on_face = create_pointlist_on_face(rc, cart);
  
  //----------------------------------------------------------------------------
  //NEX_disable_UTest
  strcpy(name, "Quadrilateral Cartesian bilinear weight generation with pointlist point on face");
  strcpy(failMsg, "Weights were not generated correctly");
  ESMC_Test((weight_gen(mesh_quad_single, pl_on_face, weights, cart)), name, failMsg, &result, __FILE__, __LINE__, 0);
  
  // clean up
  delete pl_on_face;
  delete mesh_quad_single;
#else
  strcpy(name, "Quadrilateral Cartesian bilinear weight generation with pointlist point on face");
  strcpy(failMsg, "Weights were not generated correctly");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);
#endif
  
  // --------------------------------------------------------------------------
  // quad mesh bilinear spherical with pointlist point on face
  // --------------------------------------------------------------------------
  
#if defined ESMF_MOAB
  
  cart = false;
  
  // build a mesh
  mesh_quad_single = create_mesh_quad_single(rc, cart);
  
  // build a pointlist
  pl_on_face = create_pointlist_on_face(rc, cart);
  
  //----------------------------------------------------------------------------
  //NEX_disable_UTest
  strcpy(name, "Quadrilateral spherical bilinear weight generation with pointlist point on face");
  strcpy(failMsg, "Weights were not generated correctly");
  ESMC_Test((weight_gen(mesh_quad_single, pl_on_face, weights, cart)), name, failMsg, &result, __FILE__, __LINE__, 0);
  
  // clean up
  delete pl_on_face;
  delete mesh_quad_single;
#else
  strcpy(name, "Quadrilateral spherical bilinear weight generation with pointlist point on face");
  strcpy(failMsg, "Weights were not generated correctly");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);
#endif

  // --------------------------------------------------------------------------
  // quad mesh bilinear with pointlist point in interior
  // --------------------------------------------------------------------------

#if defined ESMF_MOAB

  cart = true;

  // build a mesh
  mesh_quad_single = create_mesh_quad_single(rc, cart);

  // build a pointlist
  pl = create_pointlist_for_quad_single(rc, cart);

  //----------------------------------------------------------------------------
  //NEX_disable_UTest
  strcpy(name, "Quadrilateral Cartesian bilinear weight generation with pointlist point in interior");
  strcpy(failMsg, "Weights were not generated correctly");
  ESMC_Test((weight_gen(mesh_quad_single, pl, weights, cart)), name, failMsg, &result, __FILE__, __LINE__, 0);

  // clean up
  delete pl;
  delete mesh_quad_single;
#else
  strcpy(name, "Quadrilateral Cartesian bilinear weight generation with pointlist point in interior");
  strcpy(failMsg, "Weights were not generated correctly");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);
#endif

  // --------------------------------------------------------------------------
  // quad mesh spherical bilinear with pointlist point in interior
  // --------------------------------------------------------------------------

#if defined ESMF_MOAB

  cart = false;

  // build a mesh
  mesh_quad_single = create_mesh_quad_single(rc, cart);

  // build a pointlist
  pl = create_pointlist_for_quad_single(rc, cart);

  //----------------------------------------------------------------------------
  //NEX_disable_UTest
  strcpy(name, "Quadrilateral spherical bilinear weight generation with pointlist point in interior");
  strcpy(failMsg, "Weights were not generated correctly");
  ESMC_Test((weight_gen(mesh_quad_single, pl, weights, cart)), name, failMsg, &result, __FILE__, __LINE__, 0);

  // clean up
  delete pl;
  delete mesh_quad_single;
#else
  strcpy(name, "Quadrilateral spherical bilinear weight generation with pointlist point in interior");
  strcpy(failMsg, "Weights were not generated correctly");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);
#endif
  // --------------------------------------------------------------------------
  //----------------------------------------------------------------------------
  ESMC_TestEnd(__FILE__, __LINE__, 0);

  return 0;
}


