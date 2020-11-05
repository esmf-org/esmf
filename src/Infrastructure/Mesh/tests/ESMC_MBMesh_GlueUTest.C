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

#include "ESMC_MBMeshTestUtilMBMesh.C"

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

class MBMeshTest {
  public:
    MBMesh *mesh;
    // pointer to MBMesh for calls to glue layer routines that still use void*
    void *meshp;

    // integer values
    int pdim;
    int sdim;
    int orig_sdim;
    int num_elem;
    int num_node;
    int num_elem_conn;
    ESMC_CoordSys_Flag coord_sys;

    // present variables
    int node_mask_present;
    int elem_area_present;
    int elem_coord_present;
    int elem_mask_present;

    // node information
    int *nodeId = NULL;
    int *nodeMask = NULL;
    int *nodeOwner = NULL;
    double *nodeCoord = NULL;

    // element information
    int *elemConn = NULL;
    int *elemId = NULL;
    int *elemMask = NULL;
    int *elemType = NULL;
    double *elemArea = NULL;
    double *elemCoord = NULL;

    MBMeshTest(int _pdim, int _sdim, int _num_elem, int _num_node, ESMC_CoordSys_Flag _coord_sys) {
      pdim = _pdim;
      sdim = _sdim;
      orig_sdim = _sdim;
      num_elem = _num_elem;
      num_node = _num_node;
      coord_sys = _coord_sys;
      
      node_mask_present = false;
      elem_area_present = false;
      elem_coord_present = false;
      elem_mask_present = false;

      // allocate the MBMesh object
      mesh = new MBMesh();
      meshp = static_cast<void *> (mesh);

      int localrc;
      MBMesh_create(&meshp, &pdim, &sdim, &coord_sys, &localrc);
      if (localrc != ESMF_SUCCESS) throw localrc;
    }

    ~MBMeshTest(){
      delete [] nodeId;
      delete [] nodeOwner;
      delete [] nodeCoord;
      if (node_mask_present) delete [] nodeMask;
      // 
      delete [] elemConn;
      delete [] elemId;
      delete [] elemType;
      if (elem_area_present) delete [] elemArea;
      if (elem_coord_present) delete [] elemCoord;
      if (elem_mask_present) delete [] elemMask;
    }

    void add_nodes(int *node_id, int *node_owner, 
                   double *node_coord, int *node_mask=NULL) {
      nodeId = new int[num_node];
      std::copy(node_id, node_id+num_node, nodeId);
      nodeOwner = new int[num_node];
      std::copy(node_owner, node_owner+num_node, nodeOwner);
      nodeCoord = new double[num_node*pdim];
      std::copy(node_coord, node_coord+num_node*pdim, nodeCoord);

      InterArray<int> *iin = NULL;
      if (node_mask != NULL) {
        nodeMask = new int[num_node];
        std::copy(node_mask, node_mask+num_node, nodeMask);
        node_mask_present = true;
        iin = new InterArray<int>(nodeMask,num_node);
      }

      int localrc;
      MBMesh_addnodes(&meshp, &num_node, nodeId, nodeCoord, nodeOwner, iin,
                        &coord_sys, &orig_sdim, &localrc);
      if (localrc != ESMF_SUCCESS) throw localrc;


      if (node_mask_present) delete iin;
    }

    void add_elems(int numelemconn, int *elem_conn, int *elem_id, 
                   int *elem_type, int *elem_mask = NULL,
                   double *elem_area = NULL, double *elem_coord = NULL,
                   int *regridconserve = 0) {
      num_elem_conn = numelemconn;

      elemConn = new int[numelemconn];
      std::copy(elem_conn, elem_conn+num_elem_conn, elemConn);
      elemId = new int[num_elem];
      std::copy(elem_id, elem_id+num_elem, elemId);
      elemType = new int[num_elem];
      std::copy(elem_type, elem_type+num_elem, elemType);
      
      InterArray<int> *iie = NULL;
      if (elem_mask != NULL) {
        elemMask = new int[num_elem];
        std::copy(elem_mask, elem_mask+num_elem, elemMask);
        elem_mask_present = true;
        iie = new InterArray<int>(elemMask,num_elem);
      }

      if (elem_area != NULL) {
        elemArea = new double[num_elem];
        std::copy(elem_area, elem_area+num_elem, elemArea);
        elem_area_present = true;
      }

      if (elem_coord != NULL) {
        elemCoord = new double[num_elem*pdim];
        std::copy(elem_coord, elem_coord+num_elem*pdim, elemCoord);
        elem_coord_present = true;
      }

      int localrc;
      MBMesh_addelements(&meshp, &num_elem, elemId, elemType, iie,
                         &elem_area_present, elemArea,
                         &elem_coord_present, elemCoord,
                         &numelemconn, elemConn,
                         regridconserve,
                         &coord_sys, &orig_sdim, &localrc);
      if (localrc != ESMF_SUCCESS) throw localrc;

      if (elem_mask_present) delete iie;
    }
};


MBMeshTest *mesh_gen_quad(int &rc){
  //
  //
  //  2.0   7 ------- 8 -------- 9
  //        |         |          |
  //        |    3    |    4     |
  //        |         |          |
  //  1.0   4 ------- 5 -------- 6
  //        |         |          |
  //        |    1    |    2     |
  //        |         |          |
  //  0.0   1 ------- 2 -------- 3
  //
  //       0.0       1.0        2.0
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

  int num_elem = 4;
  int num_node = 9;

  ESMC_CoordSys_Flag coord_sys=ESMC_COORDSYS_CART;

  int nodeId_s [] ={1,2,3,4,5,6,7,8,9};
  double nodeCoord_s [] ={0.0,0.0, 1.0,0.0, 2.0,0.0,
               0.0,1.0, 1.0,1.0, 2.0,1.0,
               0.0,2.0, 1.0,2.0, 2.0,2.0};
  int nodeOwner_s [] ={0,0,0,0,0,0,0,0,0};
  int nodeMask_s [] ={1,1,1,1,1,1,1,1,1};

  int elemId_s [] ={1,2,3,4};
  // ESMF_MESHELEMTYPE_QUAD
  int elemType_s [] ={ESMC_MESHELEMTYPE_QUAD,
                      ESMC_MESHELEMTYPE_QUAD,
                      ESMC_MESHELEMTYPE_QUAD,
                      ESMC_MESHELEMTYPE_QUAD};
  int elemMask_s [] ={1,1,1,1};
  double elemArea_s [] ={1.0,2.0,3.0,4.0}; // Wrong area, but just to test
  int elemConn_s [] ={1,2,5,4,
              2,3,6,5,
              4,5,8,7,
              5,6,9,8};
  double elemCoord_s [] ={0.5,0.5,0.5,1.5,1.5,0.5,1.5,1.5};

  MBMeshTest *mbt = new MBMeshTest(pdim, sdim, num_elem, num_node, coord_sys);

  mbt->add_nodes(nodeId_s, nodeOwner_s, nodeCoord_s, nodeMask_s);

  int numelemconn = 16;
  int regridconserve = 0;

  mbt->add_elems(numelemconn, elemConn_s, elemId_s, elemType_s, elemMask_s, elemArea_s, elemCoord_s, &regridconserve);

  rc = ESMF_SUCCESS;
  return mbt;
}

MBMeshTest *mesh_gen_quad_sph(int &rc){
  //
  //
  //  pi/5  7 ------- 8 -------- 9
  //        |         |          |
  //        |    3    |    4     |
  //        |         |          |
  //  pi/10 4 ------- 5 -------- 6
  //        |         |          |
  //        |    1    |    2     |
  //        |         |          |
  //  0.0   1 ------- 2 -------- 3
  //
  //       0.0       pi/10       pi/5
  //
  //      Node Ids at corners
  //      Element Ids in centers
  //
  //
  //      ( Everything owned by PET 0)
  //

  rc = ESMF_RC_NOT_IMPL;

  double pi = 3.14159;

  int pdim = 2;
  int sdim = 3;

  int num_elem = 4;
  int num_node = 9;

  ESMC_CoordSys_Flag coord_sys=ESMC_COORDSYS_SPH_DEG;

  int nodeId_s [] ={1,2,3,4,5,6,7,8,9};
  double nodeCoord_s [] ={0.0,0.0, pi/10,0.0, pi/5,0.0,
               0.0,pi/10, pi/10,pi/10, pi/5,pi/10,
               0.0,pi/5, pi/10,pi/5, pi/5,pi/5};
  int nodeOwner_s [] ={0,0,0,0,0,0,0,0,0};
  int nodeMask_s [] ={1,1,1,1,1,1,1,1,1};

  int elemId_s [] ={1,2,3,4};
  // ESMF_MESHELEMTYPE_QUAD
  int elemType_s [] ={ESMC_MESHELEMTYPE_QUAD,
                      ESMC_MESHELEMTYPE_QUAD,
                      ESMC_MESHELEMTYPE_QUAD,
                      ESMC_MESHELEMTYPE_QUAD};
  int elemMask_s [] ={1,1,1,1};
  double elemArea_s [] ={1.0,2.0,3.0,4.0}; // Wrong area, but just to test
  int elemConn_s [] ={1,2,5,4,
              2,3,6,5,
              4,5,8,7,
              5,6,9,8};
  double elemCoord_s [] ={pi/20,pi/20,pi/20,3*pi/20,3*pi/20,pi/20,3*pi/20,3*pi/20};

  MBMeshTest *mbt = new MBMeshTest(pdim, sdim, num_elem, num_node, coord_sys);

  mbt->add_nodes(nodeId_s, nodeOwner_s, nodeCoord_s, nodeMask_s);

  int numelemconn = 16;
  int regridconserve = 0;

  mbt->add_elems(numelemconn, elemConn_s, elemId_s, elemType_s, elemMask_s, elemArea_s, elemCoord_s, &regridconserve);

  rc = ESMF_SUCCESS;
  return mbt;
}

MBMeshTest* mesh_gen_hex(int &rc) {

  rc = ESMF_RC_NOT_IMPL;

  // Mesh variables
  int pdim=3;
  int sdim=3;

  int num_elem = 4;
  int num_node = 18;

  ESMC_CoordSys_Flag coordsys=ESMC_COORDSYS_CART;

  int nodeId [] = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18};
  double nodeCoord [] = {1.0 , 1.0 , 1.0,
                         10.0, 1.0 , 1.0,
                         20.0, 1.0 , 1.0,
                         1.0 , 10.0, 1.0,
                         10.0, 10.0, 1.0,
                         20.0, 10.0, 1.0,
                         1.0 , 20.0, 1.0,
                         10.0, 20.0, 1.0,
                         20.0, 20.0, 1.0,
                         1.0 , 1.0 , 2.0,
                         10.0, 1.0 , 2.0,
                         20.0, 1.0 , 2.0,
                         1.0 , 10.0, 2.0,
                         10.0, 10.0, 2.0,
                         20.0, 10.0, 2.0,
                         1.0 , 20.0, 2.0,
                         10.0, 20.0, 2.0,
                         20.0, 20.0, 2.0};
  int nodeOwner [] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};

  int elemId [] = {1,2,3,4};
  int elemType [] = {ESMC_MESHELEMTYPE_HEX,
                     ESMC_MESHELEMTYPE_HEX,
                     ESMC_MESHELEMTYPE_HEX,
                     ESMC_MESHELEMTYPE_HEX};
  int elemConn [] = {1,  2,   5,   4,
                     10, 11,  14,  13,
                     2,  3,   6,   5,
                     11, 12,  15,  14,
                     4,  5,   8,   7,
                     13, 14,  17,  16,
                     5,  6,   9,   8,
                     14, 15,  18,  17};

  MBMeshTest *mbt = new MBMeshTest(pdim, sdim, num_elem, num_node, coordsys);

  mbt->add_nodes(nodeId, nodeOwner, nodeCoord, NULL);

  int numelemconn = 8*num_elem;
  int regridconserve = 0;

  mbt->add_elems(numelemconn, elemConn, elemId, elemType, NULL, NULL, NULL, &regridconserve);

  rc = ESMF_SUCCESS;
  return mbt;
}

void test_mbmesh_get_info(const MBMeshTest * const mbt, int *rc){
  int localrc;
  bool correct = true;
  if (rc) *rc = ESMF_FAILURE;

  int elemCount;
  MBMesh_GetElemCount(mbt->meshp, &elemCount, &localrc);
  if (localrc != ESMF_SUCCESS) {
    correct = false;
    return;
  }

  if (elemCount != mbt->num_elem) {
    printf("elemCount = %d (correct = %d)\n", elemCount, mbt->num_elem);
    correct = false;
  }

  int nodeCount;
  MBMesh_GetNodeCount(mbt->meshp, &nodeCount, &localrc);
  if (localrc != ESMF_SUCCESS) {
    correct = false;
    return;
  }

  if (nodeCount != mbt->num_node) {
    printf("nodeCount = %d (correct = %d)\n", nodeCount, mbt->num_node);
    correct = false;
  }

  int elemConnCount;
  MBMesh_GetElemConnCount(mbt->meshp, &elemConnCount, &localrc);
  if (localrc != ESMF_SUCCESS) {
    correct = false;
    return;
  }

  if (elemConnCount != mbt->num_elem_conn) {
    printf("elemConnCount = %d (correct = %d)\n", elemConnCount, mbt->num_elem_conn);
    correct = false;
  }
  
  int elemMaskIsPresent, elemAreaIsPresent, elemCoordIsPresent;
  MBMesh_GetElemInfoPresence(mbt->meshp, &elemMaskIsPresent, &elemAreaIsPresent, &elemCoordIsPresent, &localrc);
  if (localrc != ESMF_SUCCESS) {
    correct = false;
    return;
  }

  if ((elemMaskIsPresent!=mbt->elem_mask_present) || (elemAreaIsPresent!=mbt->elem_area_present) || (elemCoordIsPresent!=mbt->elem_coord_present)) {
    printf("elemAreaIsPresent = %d (correct = %d)\n", elemAreaIsPresent, mbt->elem_area_present);
    printf("elemCoordIsPresent = %d (correct = %d)\n", elemCoordIsPresent, mbt->elem_coord_present);
    printf("elemMaskIsPresent = %d (correct = %d)\n", elemMaskIsPresent, mbt->elem_mask_present);
    correct = false;
  }
  
  int nodeMaskIsPresent;
  MBMesh_GetNodeInfoPresence(mbt->meshp, &nodeMaskIsPresent, &localrc);
  if (localrc != ESMF_SUCCESS) {
    correct = false;
    return;
  }

  if (nodeMaskIsPresent!=mbt->node_mask_present) {
    printf("nodeMaskIsPresent = %d (correct = %d)\n", nodeMaskIsPresent, mbt->node_mask_present);
    correct = false;
  }

  // /////////////////// node create info ////////////////////////////
  
  int pdim = mbt->pdim;
  int num_node = mbt->num_node;

  int nodeIds[num_node];
  InterArray<int> *nii = new InterArray<int>(nodeIds,num_node);
  double nodeCoords[num_node*pdim];
  InterArray<double> *nci = new InterArray<double>(nodeCoords,num_node*pdim);
  int nodeMask[num_node];
  InterArray<int> *nmi = new InterArray<int>(nodeMask,num_node);
  int nodeOwners[num_node];
  InterArray<int> *noi = new InterArray<int>(nodeOwners,num_node);
  
  MBMesh_GetNodeCreateInfo(mbt->meshp, nii, NULL, NULL, NULL, &localrc);
  if (localrc != ESMF_SUCCESS) {
    correct = false;
    return;
  }

  if (nii->extent[0] != num_node) correct = false;
  else {
    bool print = false;
    for (int i=0; i<nii->extent[0]; ++i) {
      if (nii->array[i] != mbt->nodeId[i]) {
        correct = false;
        print = true;
      }
    }
    if (print) {
      for (int i=0; i<nii->extent[0]; ++i)
        printf("node_id[%d] = %d (correct = %d)\n", i, nii->array[i], mbt->nodeId[i]);
    }
  }
  

  MBMesh_GetNodeCreateInfo(mbt->meshp, NULL, nci, NULL, NULL, &localrc);
  if (localrc != ESMF_SUCCESS) {
    correct = false;
    return;
  }

  if (nci->extent[0] != num_node*pdim) correct = false;
  else {
    bool print = false;
    for (int i=0; i<nci->extent[0]; ++i) {
      if (nci->array[i] != mbt->nodeCoord[i]) {
        correct = false;
        print = true;
      }
    }
    if (print) {
      for (int i=0; i<nci->extent[0]; ++i)
        printf("node_coord[%d] = %f (correct = %f)\n", i, nci->array[i], mbt->nodeCoord[i]);
    }
  }
  

  MBMesh_GetNodeCreateInfo(mbt->meshp, NULL, NULL, noi, NULL, &localrc);
  if (localrc != ESMF_SUCCESS) {
    correct = false;
    return;
  }

  if (noi->extent[0] != num_node) correct = false;
  else {
    bool print = false;
    for (int i=0; i<noi->extent[0]; ++i) {
      if (noi->array[i] != mbt->nodeOwner[i]) {
        correct = false;
        print = true;
      }
    }
    if (print) {
      for (int i=0; i<noi->extent[0]; ++i)
        printf("node_owner[%d] = %d (correct = %d)\n", i, noi->array[i], mbt->nodeOwner[i]);
    }
  }
  

  if (mbt->node_mask_present) {
    MBMesh_GetNodeCreateInfo(mbt->meshp, NULL, NULL, NULL, nmi, &localrc);
    if (localrc != ESMF_SUCCESS) {
      correct = false;
      return;
    }
  
    if (nmi->extent[0] != num_node) correct = false;
    else {
      bool print = false;
      for (int i=0; i<nmi->extent[0]; ++i) {
        if (nmi->array[i] != mbt->nodeMask[i]) {
          correct = false;
          print = true;
        }
      }
      if (print) {
        for (int i=0; i<nmi->extent[0]; ++i)
          printf("node_mask[%d] = %d (correct = %d)\n", i, nmi->array[i], mbt->nodeMask[i]);
      }
    }
  }
  

  /////////////////// elem create info ////////////////////////////

  int num_elem = mbt->num_elem;
  int numelemconn = mbt->num_elem_conn;

  int elemIds[num_elem];
  InterArray<int> *eii = new InterArray<int>(elemIds,num_elem);
  int elemTypes[num_elem];
  InterArray<int> *eti = new InterArray<int>(elemTypes,num_elem);
  int elemConn[numelemconn];
  InterArray<int> *ecni = new InterArray<int>(elemConn,numelemconn);
  int elemMask[num_elem];
  InterArray<int> *emi = new InterArray<int>(elemMask,num_elem);
  double elemArea[num_elem];
  InterArray<double> *eai = new InterArray<double>(elemArea,num_elem);
  double elemCoords[num_elem*pdim];
  InterArray<double> *eci = new InterArray<double>(elemCoords,num_elem*pdim);

  MBMesh_GetElemCreateInfo(mbt->meshp, eii, NULL, NULL, NULL, NULL, NULL, &localrc);
  if (localrc != ESMF_SUCCESS) {
    correct = false;
    return;
  }

  if (eii->extent[0] != num_elem) correct = false;
  else {
    bool print = false;
    for (int i=0; i<eii->extent[0]; ++i) {
      if (eii->array[i] != mbt->elemId[i]) {
        correct = false;
        print = true;
      }
    }
    if (print) {
      for (int i=0; i<eii->extent[0]; ++i)
        printf("elem_ids[%d] = %d (correct = %d)\n", i, eii->array[i], mbt->elemId[i]);
    }
  }


  MBMesh_GetElemCreateInfo(mbt->meshp, NULL, eti, NULL, NULL, NULL, NULL, &localrc);
  if (localrc != ESMF_SUCCESS) {
    correct = false;
    return;
  }

  if (eti->extent[0] != num_elem) correct = false;
  else {
    bool print = false;
    for (int i=0; i<eti->extent[0]; ++i) {
      if (eti->array[i] != mbt->elemType[i]) {
        correct = false;
        print = true;
      }
    }
    if (print) {
      for (int i=0; i<eti->extent[0]; ++i)
        printf("elem_type[%d] = %d (correct = %d)\n", i, eti->array[i], mbt->elemType[i]);
    }
  }
  

  MBMesh_GetElemCreateInfo(mbt->meshp, NULL, NULL, ecni, NULL, NULL, NULL, &localrc);
  if (localrc != ESMF_SUCCESS) {
    correct = false;
    return;
  }

  if (ecni->extent[0] != numelemconn) correct = false;
  else {
    bool print = false;
    for (int i=0; i<ecni->extent[0]; ++i) {
      if (ecni->array[i] != mbt->elemConn[i]) {
        correct = false;
        print = true;
      }
    }
    if (print) {
      for (int i=0; i<ecni->extent[0]; ++i)
        printf("elem_connectivity[%d] = %d (correct = %d)\n", i, ecni->array[i], mbt->elemConn[i]);
    }
  }
  

  if (mbt->elem_area_present) {
    MBMesh_GetElemCreateInfo(mbt->meshp, NULL, NULL, NULL, NULL, eai, NULL, &localrc);
    if (localrc != ESMF_SUCCESS) {
      correct = false;
      return;
    }
  
    if (eai->extent[0] != num_elem) correct = false;
    else {
      bool print = false;
      for (int i=0; i<eai->extent[0]; ++i) {
        if (eai->array[i] != mbt->elemArea[i]) {
          correct = false;
          print = true;
        }
      }
      if (print) {
        for (int i=0; i<eai->extent[0]; ++i)
          printf("elem_area[%d] = %f (correct = %f)\n", i, eai->array[i], mbt->elemArea[i]);
      }
    }
  }


  if (mbt->elem_coord_present) {
    MBMesh_GetElemCreateInfo(mbt->meshp, NULL, NULL, NULL, NULL, NULL, eci, &localrc);
    if (localrc != ESMF_SUCCESS) {
      correct = false;
      return;
    }
  
    if (eci->extent[0] != num_elem*pdim) correct = false;
    else {
      bool print = false;
      for (int i=0; i<eci->extent[0]; ++i) {
        if (eci->array[i] != mbt->elemCoord[i]) {
          correct = false;
          print = true;
        }
      }
      if (print) {
        for (int i=0; i<eci->extent[0]; ++i)
          printf("elem_coords[%d] = %f (correct = %f)\n", i, eci->array[i], mbt->elemCoord[i]);
      }
    }
  }


  if (mbt->elem_mask_present) {
    MBMesh_GetElemCreateInfo(mbt->meshp, NULL, NULL, NULL, emi, NULL, NULL, &localrc);
    if (localrc != ESMF_SUCCESS) {
      correct = false;
      return;
    }
  
    if (emi->extent[0] != num_elem) correct = false;
    else {
      bool print = false;
      for (int i=0; i<emi->extent[0]; ++i) {
        if (emi->array[i] != mbt->elemMask[i]) {
          correct = false;
          print = true;
        }
      }
      if (print) {
        for (int i=0; i<emi->extent[0]; ++i)
          printf("elem_mask[%d] = %d (correct = %d)\n", i, emi->array[i], mbt->elemMask[i]);
      }
    }
  }


  // // int nodeId_s [] ={10,20,30,40,50,60,70,80,90};
  // int node_ids[num_node];
  // int *node_ids_p = node_ids;
  // int *node_ids_p;
  // node_ids_p = new int[num_node];
  // mesh->get_node_ids_by_range(node_ids_p, &rc);
  // delete [] node_ids_p;

  delete nii, nci, noi;
  if (mbt->node_mask_present) delete nmi;

  delete eii, eti, ecni;
  if (mbt->elem_area_present) delete eai;
  if (mbt->elem_coord_present) delete eci;
  if (mbt->elem_mask_present) delete emi;

  if(rc && (correct == true)) *rc = ESMF_SUCCESS;

}

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
  rc=ESMC_VMGet(vm, &localPet, &petCount, (int *)NULL, (MPI_Comm *)NULL,
                (int *)NULL, (int *)NULL);

  // --------------------------------------------------------------------------
  // quad mesh Cartesian
  // --------------------------------------------------------------------------
#if defined ESMF_MOAB
  MBMeshTest *mbt_quad = mesh_gen_quad(rc);
  if (rc == ESMF_SUCCESS) test_mbmesh_get_info(mbt_quad, &rc);
#else
  rc = ESMF_SUCCESS;
#endif
  //NEX_UTest
  strcpy(name, "MBMeshGet - Quadrilateral");
  strcpy(failMsg, "FAIL");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);

#if defined ESMF_MOAB
  delete mbt_quad;
#endif

  // --------------------------------------------------------------------------
  // quad mesh spherical
  // --------------------------------------------------------------------------
#if defined ESMF_MOAB
  MBMeshTest *mbt_quad_sph = mesh_gen_quad_sph(rc);
  if (rc == ESMF_SUCCESS) test_mbmesh_get_info(mbt_quad_sph, &rc);
#else
  rc = ESMF_SUCCESS;
#endif
  //NEX_UTest
  strcpy(name, "MBMeshGet - Spherical quadrilateral");
  strcpy(failMsg, "FAIL");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);

#if defined ESMF_MOAB
  delete mbt_quad_sph;
#endif

  // --------------------------------------------------------------------------
  // hexahedral mesh (3d)
  // --------------------------------------------------------------------------
#if defined ESMF_MOAB
  MBMeshTest *mbt_hex = mesh_gen_hex(rc);
  if (rc == ESMF_SUCCESS) test_mbmesh_get_info(mbt_hex, &rc);
#else
  rc = ESMF_SUCCESS;
#endif
  //NEX_UTest
  strcpy(name, "MBMeshGet - Hexahedral");
  strcpy(failMsg, "FAIL");
  ESMC_Test(rc==ESMF_SUCCESS, name, failMsg, &result, __FILE__, __LINE__, 0);

#if defined ESMF_MOAB
  delete mbt_hex;
#endif

  //----------------------------------------------------------------------------
  ESMC_TestEnd(__FILE__, __LINE__, 0);

  return 0;
}


