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

#include "Mesh/include/ESMCI_Mesh_Glue.h"

#if defined ESMF_MOAB
#include "ESMC_MBMeshTestUtilMesh.C"
#include "ESMC_MBMeshTestUtilMBMesh.C"
#include "ESMC_MBMeshTestUtilPL.C"

// other headers
#include "ESMCI_MBMesh.h"
#include "ESMCI_MBMesh_Glue.h"
#include "ESMCI_MBMesh_Bilinear.h"
#include "ESMCI_MBMesh_Util.h"

#include "ESMCI_MathUtil.h"

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

using namespace std;

#if defined ESMF_MOAB
MBMesh* create_mesh_pent_single(int &rc, bool cart) {
  //
  //
  //  1.0         4
  //           /      \
  //         /          \ 
  //  0.0  5      11      3
  //        \            /
  //         \          /
  // -1.0     1 ------ 2
  //
  //    -1.0 -0.5  0  0.5  1.0
  //   -pi/8 -pi/4 0  pi/4 pi/8
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
  int num_node = 5;

  double pi = 3.14159;
  double * nodeCoord;
  nodeCoord = (double *) malloc (2*num_node * sizeof (double));

  ESMC_CoordSys_Flag coordSys=ESMC_COORDSYS_CART;
  if (cart) {
    nodeCoord[0] = -0.5;
    nodeCoord[1] = -1.0;
    nodeCoord[2] = 0.5;
    nodeCoord[3] = -1.0;
    nodeCoord[4] = 1.0;
    nodeCoord[5] = 0.0;
    nodeCoord[6] = 0.0;
    nodeCoord[7] = 1.0;
    nodeCoord[8] = -0.5;
    nodeCoord[9] = 0.0;

  } else {
    coordSys = ESMC_COORDSYS_SPH_RAD;
//    sdim = 3;

    nodeCoord[0] = -1*pi/4;
    nodeCoord[1] = -1*pi/8;
    nodeCoord[2] = pi/4;
    nodeCoord[3] = -1*pi/8;
    nodeCoord[4] = pi/8;
    nodeCoord[5] = 0;
    nodeCoord[6] = 0;
    nodeCoord[7] = pi/8;
    nodeCoord[8] = -1*pi/4;
    nodeCoord[9] = 0;
  }

  int nodeId_s [] ={1,2,3,4,5};
  int nodeOwner_s [] ={0,0,0,0,0};
  int nodeMask_s [] ={0,0,0,0,0};
  int elemId_s [] ={11};
  int elemType_s [] ={5};
  int elemConn_s [] ={1,2,3,4,5};
  int elemMask_s [] ={1};

  MBMesh *mesh = new MBMesh();
  void *meshp = static_cast<void *> (mesh);

  MBMesh_create(&meshp, &pdim, &sdim, &coordSys, &rc);
  if (rc != ESMF_SUCCESS) return NULL;

  InterArray<int> *ii_node = new InterArray<int>(nodeMask_s,num_node);

  MBMesh_addnodes(&meshp, &num_node, nodeId_s, nodeCoord, nodeOwner_s,
                  ii_node, &coordSys, &sdim, &rc);
  if (rc != ESMF_SUCCESS) return NULL;

  InterArray<int> *ii_elem = new InterArray<int>(elemMask_s,1);

  int areapresent = 0;
  int coordspresent = 0;
  int numelemconn = num_node;
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

Mesh* create_mesh_pent_legacy(int &rc, bool cart) {
  //
  //
  //  1.0         4
  //           /      \
  //         /          \ 
  //  0.0  5      11      3
  //        \            /
  //         \          /
  // -1.0     1 ------ 2
  //
  //    -1.0 -0.5  0  0.5  1.0
  //   -pi/8 -pi/4 0  pi/4 pi/8
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
  int num_node = 5;

  double pi = 3.14159;
  double * nodeCoord;
  nodeCoord = (double *) malloc (2*num_node * sizeof (double));

  ESMC_CoordSys_Flag coordSys=ESMC_COORDSYS_CART;
  if (cart) {
    nodeCoord[0] = -0.5;
    nodeCoord[1] = -1.0;
    nodeCoord[2] = 0.5;
    nodeCoord[3] = -1.0;
    nodeCoord[4] = 1.0;
    nodeCoord[5] = 0.0;
    nodeCoord[6] = 0.0;
    nodeCoord[7] = 1.0;
    nodeCoord[8] = -0.5;
    nodeCoord[9] = 0.0;

  } else {
    coordSys = ESMC_COORDSYS_SPH_RAD;
//    sdim = 3;

    nodeCoord[0] = -1*pi/4;
    nodeCoord[1] = -1*pi/8;
    nodeCoord[2] = pi/4;
    nodeCoord[3] = -1*pi/8;
    nodeCoord[4] = pi/8;
    nodeCoord[5] = 0;
    nodeCoord[6] = 0;
    nodeCoord[7] = pi/8;
    nodeCoord[8] = -1*pi/4;
    nodeCoord[9] = 0;
  }

  int nodeId_s [] ={1,2,3,4,5};
  int nodeOwner_s [] ={0,0,0,0,0};
  int nodeMask_s [] ={0,0,0,0,0};
  int elemId_s [] ={11};
  int elemType_s [] ={5};
  int elemConn_s [] ={1,2,3,4,5};
  int elemMask_s [] ={1};

  Mesh *mesh = new Mesh();

  ESMCI_meshcreate(&mesh, &pdim, &sdim, &coordSys, &rc);
  if (rc != ESMF_SUCCESS) return NULL;

  InterArray<int> *ii_node = new InterArray<int>(nodeMask_s,num_node);

  ESMCI_meshaddnodes(&mesh, &num_node, nodeId_s, nodeCoord, nodeOwner_s,
                  ii_node, &coordSys, &sdim, &rc);
  if (rc != ESMF_SUCCESS) return NULL;

  InterArray<int> *ii_elem = new InterArray<int>(elemMask_s,1);

  int areapresent = 0;
  int coordspresent = 0;
  int numelemconn = num_node;
  int regridconserve = 0;
  ESMCI_meshaddelements(&mesh, &num_elem, elemId_s, elemType_s, ii_elem,
                     &areapresent, NULL,
                     &coordspresent, NULL,
                     &numelemconn, elemConn_s,
                     &regridconserve,
                     &coordSys, &sdim, &rc);
  if (rc != ESMF_SUCCESS) return NULL;

  delete ii_node;
  delete ii_elem;

  rc = ESMF_SUCCESS;
  return mesh;
}

MBMesh* create_mesh_quad_single(int &rc, bool cart) {
  //
  //  1.0   4 -------- 3  -pi/8
  //        |          |
  //  0.0   |    11    |   0
  //        |          |
  // -1.0   1 -------- 2  -pi/8
  //
  //      -1.0   0.0   1.0
  //      -pi/8   0    pi/8
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
    nodeCoord[0] = -1.0;
    nodeCoord[1] = -1.0;
    nodeCoord[2] = 1.0;
    nodeCoord[3] = -1.0;
    nodeCoord[4] = 1.0;
    nodeCoord[5] = 1.0;
    nodeCoord[6] = -1.0;
    nodeCoord[7] = 1.0;

  } else {
    coordSys = ESMC_COORDSYS_SPH_RAD;
//    sdim = 3;

    nodeCoord[0] = -1*pi/8;
    nodeCoord[1] = -1*pi/8;
    nodeCoord[2] = pi/8;
    nodeCoord[3] = -1*pi/8;
    nodeCoord[4] = pi/8;
    nodeCoord[5] = pi/8;
    nodeCoord[6] = -1*pi/8;
    nodeCoord[7] = pi/8;
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

  delete ii_node;
  delete ii_elem;

  rc = ESMF_SUCCESS;
  return static_cast<MBMesh *>(meshp);
}

MBMesh* create_mesh_tri_single(int &rc, bool cart) {
  //
  //  cart y               sph y
  //
  //  y2         3          y4
  //            /  \
  //  0.0     /  11  \       0
  //         /        \
  //  y1   1 -------- 2     y3
  //
  // cartx x1    0.0  x2
  // sph x x3     0   x4
  //
  // x1 = -1*cos(30*pi/180)
  // x2 = cos(30*pi/180)
  // x3 = pi/4*(-1*cos(30*pi/180))
  // x4 = pi/4*(cos(30*pi/180))
  //
  // y1 = -1*cos(60*pi/180)
  // y2 = 1
  // y3 = pi/4*(-1*cos(60*pi/180))
  // y4 = pi/4*(1)
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

    nodeCoord[0] = -1*cos(30*pi/180);
    nodeCoord[1] = -1*cos(60*pi/180);
    nodeCoord[2] = cos(30*pi/180);
    nodeCoord[3] = -1*cos(60*pi/180);
    nodeCoord[4] = 0;
    nodeCoord[5] = 1;

  } else {
    coordSys = ESMC_COORDSYS_SPH_RAD;
//    sdim = 3;

    nodeCoord[0] = pi/4*(-1*cos(30*pi/180));
    nodeCoord[1] = pi/4*(-1*cos(60*pi/180));
    nodeCoord[2] = pi/4*(cos(30*pi/180));
    nodeCoord[3] = pi/4*(-1*cos(60*pi/180));
    nodeCoord[4] = pi/4*(0);
    nodeCoord[5] = pi/4*(1);

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

PointList* create_pointlist_for_quad_single(int &rc, bool cart) {
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
  bool cart;

  vector<double> weights;
  weights.resize(4);
#endif

  // Get parallel information
  vm=ESMC_VMGetGlobal(&rc);
  if (rc != ESMF_SUCCESS) return 0;

  rc=ESMC_VMGet(vm, &localPet, &petCount, (int *)NULL, (MPI_Comm *)NULL,
                (int *)NULL, (int *)NULL);
  if (rc != ESMF_SUCCESS) return 0;

#ifdef verify_triangulation
  // --------------------------------------------------------------------------
  // pent mesh MOAB
  // --------------------------------------------------------------------------

  cart = true;

  // build a mesh
  MBMesh *mesh_pent_single = create_mesh_pent_single(rc, cart);

  // Call into MOAB
  Interface *mbmesh=mesh_pent_single->mesh;
  char filename[20] = "pentagon.vtk";
  int merr=mbmesh->write_file(filename,NULL,NULL);
  ESMC_Test(merr==MB_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);

  // --------------------------------------------------------------------------
  // pent mesh LEGACY
  // --------------------------------------------------------------------------

  cart = true;

  // build a mesh
  Mesh *mesh_pent_legacy = create_mesh_pent_legacy(rc, cart);


  // Call into legacy mesh creation
  char filenamel[20] = "pentagon_legacy.vtk";
  WriteMesh(*mesh_pent_legacy, filenamel);
  ESMC_Test(merr==MB_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);
#endif


#if defined ESMF_MOAB
  MBMesh *mesh_pent_single;
  MBMesh *mesh_quad_single;
  MBMesh *mesh_tri_single;
  PointList *pl_quad_single;

  // --------------------------------------------------------------------------
  // pent mesh bilinear cartesian
  // --------------------------------------------------------------------------

  // build a pointlist
  pl_quad_single = create_pointlist_for_quad_single(rc, cart);

  // expected result
  std::fill(weights.begin(), weights.end(), UNINITVAL);

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "pentagon Cartesian bilinear weight generation");
  strcpy(failMsg, "Weights were not generated correctly");
  ESMC_Test((weight_gen(mesh_pent_single, pl_quad_single, weights, cart)), name, failMsg, &result, __FILE__, __LINE__, 0);

  // clean up
  delete pl_quad_single;
  delete mesh_pent_single;
#else
  rc = ESMF_SUCCESS;
  strcpy(name, "pentagon Cartesian bilinear weight generation");
  strcpy(failMsg, "Weights were not generated correctly");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);
#endif

  // --------------------------------------------------------------------------
  // pent mesh bilinear spherical
  // --------------------------------------------------------------------------

#if defined ESMF_MOAB
  cart = false;

  // build a mesh
  mesh_pent_single = create_mesh_pent_single(rc, cart);

  // build a pointlist
  pl_quad_single = create_pointlist_for_quad_single(rc, cart);

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "pentagon spherical bilinear weight generation");
  strcpy(failMsg, "Weights were not generated correctly");
  ESMC_Test((weight_gen(mesh_pent_single, pl_quad_single, weights, cart)), name, failMsg, &result, __FILE__, __LINE__, 0);

  // clean up
  delete pl_quad_single;
  delete mesh_pent_single;
#else
  rc = ESMF_SUCCESS;
  strcpy(name, "pentagon spherical bilinear weight generation");
  strcpy(failMsg, "Weights were not generated correctly");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);
#endif


  // --------------------------------------------------------------------------
  // quad mesh bilinear cartesian
  // --------------------------------------------------------------------------
#if defined ESMF_MOAB
  cart = true;

  // build a mesh
  mesh_quad_single = create_mesh_quad_single(rc, cart);
  
  // build a pointlist
  pl_quad_single = create_pointlist_for_quad_single(rc, cart);

  // expected result
  std::fill(weights.begin(), weights.end(), UNINITVAL);

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Quadrilateral Cartesian bilinear weight generation");
  strcpy(failMsg, "Weights were not generated correctly");
  ESMC_Test((weight_gen(mesh_quad_single, pl_quad_single, weights, cart)), name, failMsg, &result, __FILE__, __LINE__, 0);

  // clean up
  delete pl_quad_single;
  delete mesh_quad_single;
#else
  rc = ESMF_SUCCESS;
  strcpy(name, "Quadrilateral Cartesian bilinear weight generation");
  strcpy(failMsg, "Weights were not generated correctly");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);
#endif

  // --------------------------------------------------------------------------
  // quad mesh bilinear spherical
  // --------------------------------------------------------------------------

#if defined ESMF_MOAB
  cart = false;

  // build a mesh
  mesh_quad_single = create_mesh_quad_single(rc, cart);

  // build a pointlist
  pl_quad_single = create_pointlist_for_quad_single(rc, cart);

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Quadrilateral spherical bilinear weight generation");
  strcpy(failMsg, "Weights were not generated correctly");
  ESMC_Test((weight_gen(mesh_quad_single, pl_quad_single, weights, cart)), name, failMsg, &result, __FILE__, __LINE__, 0);

  // clean up
  delete pl_quad_single;
  delete mesh_quad_single;
#else
  rc = ESMF_SUCCESS;
  strcpy(name, "Quadrilateral spherical bilinear weight generation");
  strcpy(failMsg, "Weights were not generated correctly");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);
#endif

  // --------------------------------------------------------------------------
  // tri mesh bilinear cartesian
  // --------------------------------------------------------------------------

#if defined ESMF_MOAB
  cart = true;

  // build a mesh
  mesh_tri_single = create_mesh_tri_single(rc, cart);

  // build a pointlist
  pl_quad_single = create_pointlist_for_quad_single(rc, cart);

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Triangle Cartesian bilinear weight generation");
  strcpy(failMsg, "Weights were not generated correctly");
  ESMC_Test((weight_gen(mesh_tri_single, pl_quad_single, weights, cart)), name, failMsg, &result, __FILE__, __LINE__, 0);

  // clean up
  delete pl_quad_single;
  delete mesh_tri_single;
#else
  rc = ESMF_SUCCESS;
  strcpy(name, "Triangle Cartesian bilinear weight generation");
  strcpy(failMsg, "Weights were not generated correctly");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);
#endif

  // --------------------------------------------------------------------------
  // tri mesh bilinear spherical
  // --------------------------------------------------------------------------

#if defined ESMF_MOAB
  cart = false;

  // build a mesh
  mesh_tri_single = create_mesh_tri_single(rc, cart);

  // build a pointlist
  pl_quad_single = create_pointlist_for_quad_single(rc, cart);

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Triangle spherical bilinear weight generation");
  strcpy(failMsg, "Weights were not generated correctly");
  ESMC_Test((weight_gen(mesh_tri_single, pl_quad_single, weights, cart)), name, failMsg, &result, __FILE__, __LINE__, 0);

//  double * nodeCoord;
//  nodeCoord = (double *) malloc (9 * sizeof (double));
//  nodeCoord[0] = pi/4*(-1*cos(30*pi/180));
//  nodeCoord[1] = pi/4*(-1*cos(60*pi/180));
//  nodeCoord[2] = 0;
//  nodeCoord[3] = pi/4*(cos(30*pi/180));
//  nodeCoord[4] = pi/4*(-1*cos(60*pi/180));
//  nodeCoord[5] = 0;
//  nodeCoord[6] = pi/4*(0);
//  nodeCoord[7] = pi/4*(1);
//  nodeCoord[8] = 0;
//  write_3D_poly_woid_to_vtk("spherical_tri", 3, nodeCoord);

  // clean up
  delete pl_quad_single;
  delete mesh_tri_single;
#else
  rc = ESMF_SUCCESS;
  strcpy(name, "Triangle spherical bilinear weight generation");
  strcpy(failMsg, "Weights were not generated correctly");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);
#endif
  // --------------------------------------------------------------------------
  //----------------------------------------------------------------------------
  ESMC_TestEnd(__FILE__, __LINE__, 0);

  return 0;
}


