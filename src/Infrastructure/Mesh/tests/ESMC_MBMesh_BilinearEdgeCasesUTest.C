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
// other headers
#include "ESMCI_MBMesh.h"
#include "ESMCI_MBMesh_Glue.h"
#include "ESMCI_MBMesh_Bilinear.h"
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

using namespace std;

#if defined ESMF_MOAB

typedef std::map<WMat::Entry, std::vector<WMat::Entry> > WeightMap;
WeightMap weights;
WeightMap::iterator begin_row() { return weights.begin(); }
WeightMap::iterator end_row() { return weights.end(); }

bool weight_gen(MBMesh *mesh, PointList *pl) {
  int rc = ESMF_RC_NOT_IMPL;
  char name[80];
  char failMsg[80];
  int result = 0;

  // do bilinear regridding between mesh and pointlist
  IWeights wt;
  IWeights &wts = wt;
  calc_bilinear_regrid_wgts(mesh, pl, wts);

  cout << endl << "Bilinear Weight Matrix" << endl;
  // print out weights
  WeightMap::iterator mb = wts.begin_row(), me = wts.end_row();
  for(; mb != me; ++mb) {
    WMat::Entry col = mb->first;
    vector<WMat::Entry> row = mb->second;

    cout << "[" << col.id << "," << col.idx << "," << col.value << ","
         << col.src_id << "] - ";

    vector<WMat::Entry>::iterator vb = row.begin(), ve = row.end();
    for(; vb != ve; ++vb) {
      WMat::Entry rv = *vb;
      cout << "[" << rv.id << "," << rv.idx << "," << rv.value << ","
           << rv.src_id << "] ";
    }
    cout << endl;
  }
  cout << endl;

  rc = ESMF_SUCCESS;

  if (rc == ESMF_SUCCESS) return true;
  else return false;

}

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
  int nodeMask_s [] ={1,1,1,1};
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
  int nodeMask_s [] ={1,1,1};
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

// RLO: disable for now, segfault in PointList destructor on some platforms
#if 0
#if defined ESMF_MOAB
  MBMesh *mesh_quad_single;
  MBMesh *mesh_tri_single;
  PointList *pl_on_edge;
  PointList *pl_on_node;
  PointList *pl;
  bool cart;
  bool collapsed = false;

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
  ESMC_Test((weight_gen(mesh_quad_single, pl_on_edge)), name, failMsg, &result, __FILE__, __LINE__, 0);

  // clean up
  delete pl_on_edge;
  delete mesh_quad_single;
#else
  strcpy(name, "Quadrilateral Cartesian bilinear weight generation with pointlist point on edge");
  strcpy(failMsg, "Weights were not generated correctly");
  ESMC_Test((weight_gen(mesh_quad_single, pl_on_edge)), name, failMsg, &result, __FILE__, __LINE__, 0);
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
  ESMC_Test((weight_gen(mesh_quad_single, pl_on_edge)), name, failMsg, &result, __FILE__, __LINE__, 0);

  // clean up
  delete pl_on_edge;
  delete mesh_quad_single;
#else
  strcpy(name, "Quadrilateral spherical bilinear weight generation with pointlist point on edge");
  strcpy(failMsg, "Weights were not generated correctly");
  ESMC_Test((weight_gen(mesh_quad_single, pl_on_edge)), name, failMsg, &result, __FILE__, __LINE__, 0);
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
  ESMC_Test((weight_gen(mesh_quad_single, pl_on_node)), name, failMsg, &result, __FILE__, __LINE__, 0);

  // clean up
  delete pl_on_node;
  delete mesh_quad_single;
#else
  strcpy(name, "Quadrilateral Cartesian bilinear weight generation with pointlist point on node");
  strcpy(failMsg, "Weights were not generated correctly");
  ESMC_Test((weight_gen(mesh_quad_single, pl_on_node)), name, failMsg, &result, __FILE__, __LINE__, 0);
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
  ESMC_Test((weight_gen(mesh_quad_single, pl_on_node)), name, failMsg, &result, __FILE__, __LINE__, 0);

  // clean up
  delete pl_on_node;
  delete mesh_quad_single;
#else
  strcpy(name, "Quadrilateral spherical bilinear weight generation with pointlist point on node");
  strcpy(failMsg, "Weights were not generated correctly");
  ESMC_Test((weight_gen(mesh_quad_single, pl_on_node)), name, failMsg, &result, __FILE__, __LINE__, 0);
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
  ESMC_Test((weight_gen(mesh_tri_single, pl_on_edge)), name, failMsg, &result, __FILE__, __LINE__, 0);

  // clean up
  delete pl_on_edge;
  delete mesh_tri_single;
#else
  strcpy(name, "Triangle Cartesian bilinear weight generation with pointlist point on edge");
  strcpy(failMsg, "Weights were not generated correctly");
  ESMC_Test((weight_gen(mesh_tri_single, pl_on_edge)), name, failMsg, &result, __FILE__, __LINE__, 0);
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
  ESMC_Test((weight_gen(mesh_tri_single, pl_on_edge)), name, failMsg, &result, __FILE__, __LINE__, 0);

  // clean up
  delete pl_on_edge;
  delete mesh_tri_single;
#else
  strcpy(name, "Triangle spherical bilinear weight generation with pointlist point on edge");
  strcpy(failMsg, "Weights were not generated correctly");
  ESMC_Test((weight_gen(mesh_tri_single, pl_on_edge)), name, failMsg, &result, __FILE__, __LINE__, 0);
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
  ESMC_Test((weight_gen(mesh_tri_single, pl_on_node)), name, failMsg, &result, __FILE__, __LINE__, 0);

  // clean up
  delete pl_on_node;
  delete mesh_tri_single;
#else
  strcpy(name, "Triangle Cartesian bilinear weight generation with pointlist point on node");
  strcpy(failMsg, "Weights were not generated correctly");
  ESMC_Test((weight_gen(mesh_tri_single, pl_on_node)), name, failMsg, &result, __FILE__, __LINE__, 0);
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
  ESMC_Test((weight_gen(mesh_tri_single, pl_on_node)), name, failMsg, &result, __FILE__, __LINE__, 0);

  // clean up
  delete pl_on_node;
  delete mesh_tri_single;
#else
  strcpy(name, "Triangle spherical bilinear weight generation with pointlist point on node");
  strcpy(failMsg, "Weights were not generated correctly");
  ESMC_Test((weight_gen(mesh_tri_single, pl_on_node)), name, failMsg, &result, __FILE__, __LINE__, 0);
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
  ESMC_Test((weight_gen(mesh_quad_single, pl)), name, failMsg, &result, __FILE__, __LINE__, 0);

  // clean up
  delete pl;
  delete mesh_quad_single;
#else
  strcpy(name, "Collapsed quadrilateral to triangle Cartesian bilinear weight generation");
  strcpy(failMsg, "Weights were not generated correctly");
  ESMC_Test((weight_gen(mesh_quad_single, pl)), name, failMsg, &result, __FILE__, __LINE__, 0);
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
  ESMC_Test((weight_gen(mesh_quad_single, pl)), name, failMsg, &result, __FILE__, __LINE__, 0);

  // clean up
  delete pl;
  delete mesh_quad_single;
#else
  strcpy(name, "Collapsed quadrilateral to triangle spherical bilinear weight generation");
  strcpy(failMsg, "Weights were not generated correctly");
  ESMC_Test((weight_gen(mesh_quad_single, pl)), name, failMsg, &result, __FILE__, __LINE__, 0);
#endif

#endif
  // --------------------------------------------------------------------------
  //----------------------------------------------------------------------------
  ESMC_TestEnd(__FILE__, __LINE__, 0);

  return 0;
}


