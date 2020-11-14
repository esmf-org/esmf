// $Id$
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
#include "ESMC_MBMeshTest.C"


#include "ESMCI_MBMesh.h"
#endif

#include <iostream>
#include <iterator>
#include <vector>
#include <algorithm>

#if !defined (M_PI)
// for Windows...
#define M_PI 3.14159265358979323846
#endif

static double UNINITVAL = -42;

#ifdef ESMF_MOAB
MBMeshTest *mbmesh_gen_quad_2d_cart(int &rc, bool redist=false){
#undef ESMC_METHOD
#define ESMC_METHOD "mbmesh_gen_quad_2d_cart"
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
  MBMeshTest *mbt = NULL;

  try {

    // Get parallel information
    int localPet, petCount;
    ESMC_VM vm;
    vm=ESMC_VMGetGlobal(&rc);
    MBMESHTEST_CHECK_RC_THROW(rc)

    rc=ESMC_VMGet(vm, &localPet, &petCount, (int *)NULL, (MPI_Comm *)NULL,
                  (int *)NULL, (int *)NULL);
    MBMESHTEST_CHECK_RC_THROW(rc)

    if (petCount !=1 && petCount != 4)
      MBMESHTEST_THROW_ERROR("Must be run with 1 or 4 cores.")

    int pdim = 2;
    int sdim = 2;
    ESMC_CoordSys_Flag coord_sys=ESMC_COORDSYS_CART;

    int num_node;
    int num_elem;
    int num_elem_conn;
    int redist_num_node;
    int redist_num_elem;
    int redist_num_elem_conn;

    if (petCount == 1) {
      num_elem = 4;
      num_node = 9;
      num_elem_conn = 4*num_elem;

      if (redist)
        MBMESHTEST_THROW_ERROR("Cannot run redist with 1 core.")

    } else if (petCount == 4) {
      if (localPet == 0) {
        num_elem = 1;
        num_node = 4;
        num_elem_conn = 4;
        redist_num_elem = 1;
        redist_num_node = 4;
        redist_num_elem_conn = 4;
      } else if (localPet == 1) {
        num_elem = 1;
        num_node = 4;
        num_elem_conn = 4;
        redist_num_elem = 1;
        redist_num_node = 4;
        redist_num_elem_conn = 4;
      } else if (localPet == 2) {
        num_elem = 1;
        num_node = 4;
        num_elem_conn = 4;
        redist_num_elem = 1;
        redist_num_node = 4;
        redist_num_elem_conn = 4;
      } else if (localPet == 3) {
        num_elem = 1;
        num_node = 4;
        num_elem_conn = 4;
        redist_num_elem = 1;
        redist_num_node = 4;
        redist_num_elem_conn = 4;
      }
    }

    if (!redist)
      mbt = new MBMeshTest(pdim, sdim, coord_sys, num_node, num_elem, num_elem_conn);
    else
      mbt = new MBMeshTest(pdim, sdim, coord_sys, num_node, num_elem, num_elem_conn, redist, redist_num_node, redist_num_elem, redist_num_elem_conn);

    if (petCount == 1) {
      mbt->nodeId = {1,2,3,4,5,6,7,8,9};
      mbt->nodeCoord = {0.0,0.0, 1.0,0.0, 2.0,0.0,
                        0.0,1.0, 1.0,1.0, 2.0,1.0,
                        0.0,2.0, 1.0,2.0, 2.0,2.0};
      mbt->nodeOwner = {0,0,0,0,0,0,0,0,0};
      mbt->nodeMask = {1,1,1,1,1,1,1,1,1};
  
      mbt->elemId = {1,2,3,4};
      mbt->elemType = {ESMC_MESHELEMTYPE_QUAD,
                       ESMC_MESHELEMTYPE_QUAD,
                       ESMC_MESHELEMTYPE_QUAD,
                       ESMC_MESHELEMTYPE_QUAD};
      mbt->elemMask = {1,1,1,1};
      mbt->elemArea = {1.0,2.0,3.0,4.0};
      mbt->elemConn = {1,2,5,4,
                       2,3,6,5,
                       4,5,8,7,
                       5,6,9,8};
      mbt->elemCoord = {0.5,0.5,0.5,1.5,1.5,0.5,1.5,1.5};

    } else if (petCount == 4) {
      if (localPet == 0) {
        mbt->nodeId ={1,2,4,5};
        mbt->nodeCoord = {0.0,0.0, 1.0,0.0, 0.0,1.0, 1.0,1.0};
        mbt->nodeOwner = {0,0,0,0};
        mbt->elemId = {1};
        mbt->elemType = {ESMC_MESHELEMTYPE_QUAD};
        mbt->elemConn = {1,2,4,3};
        mbt->elemCoord = {0.5,0.5};
      } else if (localPet == 1) {
        mbt->nodeId = {2,3,5,6};
        mbt->nodeCoord = {1.0,0.0, 2.0,0.0, 1.0,1.0, 2.0,1.0};
        mbt->nodeOwner = {0,1,0,1};
        mbt->elemId = {2};
        mbt->elemType = {ESMC_MESHELEMTYPE_QUAD};
        mbt->elemConn = {1,2,4,3};
        mbt->elemCoord = {1.5,0.5};
      } else if (localPet == 2) {
        mbt->nodeId = {4,5,7,8};
        mbt->nodeCoord = {0.0,1.0, 1.0,1.0, 0.0,2.0, 1.0,2.0};
        mbt->nodeOwner = {0,0,2,2};
        mbt->elemId = {3};
        mbt->elemType = {ESMC_MESHELEMTYPE_QUAD};
        mbt->elemConn = {1,2,4,3};
        mbt->elemCoord = {0.5,1.5};
      } else if (localPet == 3) {
        mbt->nodeId = {5,6,8,9};
        mbt->nodeCoord = {1.0,1.0, 2.0,1.0, 1.0,2.0, 2.0,2.0};
        mbt->nodeOwner = {0,1,2,3};
        mbt->elemId = {4};
        mbt->elemType = {ESMC_MESHELEMTYPE_QUAD};
        mbt->elemConn = {1,2,4,3};
        mbt->elemCoord = {1.5,1.5};
      }

      if (redist) {
        if (localPet == 0) {
          mbt->redist_nodeId = {5,6,8,9};
          mbt->redist_nodeCoord = {1.0,1.0, 2.0,1.0, 1.0,2.0, 2.0,2.0};
          mbt->redist_nodeOwner = {0,0,0,0};
          mbt->redist_elemId = {4};
          mbt->redist_elemType = {ESMC_MESHELEMTYPE_QUAD};
          mbt->redist_elemConn = {1,2,4,3};
          mbt->redist_elemCoord = {1.5,1.5};
        } else if (localPet == 1) {
          mbt->redist_nodeId = {4,5,7,8};
          mbt->redist_nodeCoord = {0.0,1.0, 1.0,1.0, 0.0,2.0, 1.0,2.0};
          mbt->redist_nodeOwner = {1,0,1,0};
          mbt->redist_elemId = {3};
          mbt->redist_elemType = {ESMC_MESHELEMTYPE_QUAD};
          mbt->redist_elemConn = {1,2,4,3};
          mbt->redist_elemCoord = {0.5,1.5};
        } else if (localPet == 2) {
          mbt->redist_nodeId = {2,3,5,6};
          mbt->redist_nodeCoord = {1.0,0.0, 2.0,0.0, 1.0,1.0, 2.0,1.0};
          mbt->redist_nodeOwner = {2,2,0,0};
          mbt->redist_elemId = {2};
          mbt->redist_elemType = {ESMC_MESHELEMTYPE_QUAD};
          mbt->redist_elemConn = {1,2,4,3};
          mbt->redist_elemCoord = {1.5,0.5};
        } else if (localPet == 3) {
          mbt->redist_nodeId ={1,2,4,5};
          mbt->redist_nodeCoord = {0.0,0.0, 1.0,0.0, 0.0,1.0, 1.0,1.0};
          mbt->redist_nodeOwner = {3,2,1,0};
          mbt->redist_elemId = {1};
          mbt->redist_elemType = {ESMC_MESHELEMTYPE_QUAD};
          mbt->redist_elemConn = {1,2,4,3};
          mbt->redist_elemCoord = {0.5,0.5};
        }
      }
    }

  } MBMESHTEST_CATCH_RETURN_NULL(&rc)

  rc = ESMF_SUCCESS;
  return mbt;
}

MBMeshTest *mbmesh_gen_quad_2d_sph(int &rc, bool redist=false){
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
#undef ESMC_METHOD
#define ESMC_METHOD "mbmesh_gen_quad_2d_sph"

  rc = ESMF_RC_NOT_IMPL;
  MBMeshTest *mbt = NULL;

  try {

    // Get parallel information
    int localPet, petCount;
    ESMC_VM vm;
    vm=ESMC_VMGetGlobal(&rc);
    MBMESHTEST_CHECK_RC_THROW(rc)

    rc=ESMC_VMGet(vm, &localPet, &petCount, (int *)NULL, (MPI_Comm *)NULL,
                  (int *)NULL, (int *)NULL);
    MBMESHTEST_CHECK_RC_THROW(rc)

    if (petCount !=1 && petCount != 4)
      MBMESHTEST_THROW_ERROR("Must be run with 1 or 4 cores.")

    // Cartesian to spherical coordinate transformation - radians (suitable for c:[0:2])
    double pi = 3.14159;
    double c2s = pi/10.;

    int localrc;
    mbt = mbmesh_gen_quad_2d_cart(localrc, redist);
    MBMESHTEST_CHECK_RC_THROW(localrc)

    std::for_each(mbt->nodeCoord.begin(), mbt->nodeCoord.end(), [&c2s](double &d) {d*=c2s;});
    std::for_each(mbt->elemCoord.begin(), mbt->elemCoord.end(), [&c2s](double &d) {d*=c2s;});

    std::for_each(mbt->redist_nodeCoord.begin(), mbt->redist_nodeCoord.end(), [&c2s](double &d) {d*=c2s;});
    std::for_each(mbt->redist_elemCoord.begin(), mbt->redist_elemCoord.end(), [&c2s](double &d) {d*=c2s;});

    mbt->coord_sys=ESMC_COORDSYS_SPH_RAD;

  } MBMESHTEST_CATCH_RETURN_NULL(&rc)

  rc = ESMF_SUCCESS;
  return mbt;
}

MBMeshTest* mbmesh_gen_hex_3d_cart(int &rc, bool redist=false) {
#undef ESMC_METHOD
#define ESMC_METHOD "mbmesh_gen_hex_3d_cart"

  rc = ESMF_RC_NOT_IMPL;
  MBMeshTest *mbt = NULL;

  try {

    // Get parallel information
    int localPet, petCount;
    ESMC_VM vm;
    vm=ESMC_VMGetGlobal(&rc);
    MBMESHTEST_CHECK_RC_THROW(rc)

    rc=ESMC_VMGet(vm, &localPet, &petCount, (int *)NULL, (MPI_Comm *)NULL,
                  (int *)NULL, (int *)NULL);
    MBMESHTEST_CHECK_RC_THROW(rc)

    if (petCount !=1 && petCount != 4)
      MBMESHTEST_THROW_ERROR("Must be run with 1 or 4 cores.")

    int pdim=3;
    int sdim=3;
    ESMC_CoordSys_Flag coord_sys=ESMC_COORDSYS_CART;

    int num_node;
    int num_elem;
    int num_elem_conn;
    int redist_num_node;
    int redist_num_elem;
    int redist_num_elem_conn;

    if (petCount == 1) {
      int num_elem = 4;
      int num_node = 18;
      int numelemconn = 8*num_elem;

      if (redist)
        MBMESHTEST_THROW_ERROR("Cannot run redist with 1 core.")

    } else if (petCount == 4) {
      if (localPet == 0) {
        num_elem = 1;
        num_node = 8;
        num_elem_conn = 8;
        redist_num_elem = 1;
        redist_num_node = 8;
        redist_num_elem_conn = 8;
      } else if (localPet == 1) {
        num_elem = 1;
        num_node = 8;
        num_elem_conn = 8;
        redist_num_elem = 1;
        redist_num_node = 8;
        redist_num_elem_conn = 8;
      } else if (localPet == 2) {
        num_elem = 1;
        num_node = 8;
        num_elem_conn = 8;
        redist_num_elem = 1;
        redist_num_node = 8;
        redist_num_elem_conn = 8;
      } else if (localPet == 3) {
        num_elem = 1;
        num_node = 8;
        num_elem_conn = 8;
        redist_num_elem = 1;
        redist_num_node = 8;
        redist_num_elem_conn = 8;
      }
    }

    if (!redist)
      mbt = new MBMeshTest(pdim, sdim, coord_sys, num_node, num_elem, num_elem_conn);
    else
      mbt = new MBMeshTest(pdim, sdim, coord_sys, num_node, num_elem, num_elem_conn, redist, redist_num_node, redist_num_elem, redist_num_elem_conn);

    if (petCount == 1) {
      mbt->nodeId = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18};
      mbt->nodeCoord = {1.0 , 1.0 , 1.0, // 1
                        10.0, 1.0 , 1.0, // 2
                        20.0, 1.0 , 1.0, // 3
                        1.0 , 10.0, 1.0, // 4
                        10.0, 10.0, 1.0, // 5
                        20.0, 10.0, 1.0, // 6
                        1.0 , 20.0, 1.0, // 7
                        10.0, 20.0, 1.0, // 8
                        20.0, 20.0, 1.0, // 9
                        1.0 , 1.0 , 2.0, // 10
                        10.0, 1.0 , 2.0, // 11
                        20.0, 1.0 , 2.0, // 12
                        1.0 , 10.0, 2.0, // 13
                        10.0, 10.0, 2.0, // 14
                        20.0, 10.0, 2.0, // 15
                        1.0 , 20.0, 2.0, // 16
                        10.0, 20.0, 2.0, // 17
                        20.0, 20.0, 2.0}; // 18
      mbt->nodeOwner = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
      mbt->elemId = {1,2,3,4};
      mbt->elemType = {ESMC_MESHELEMTYPE_HEX,
                       ESMC_MESHELEMTYPE_HEX,
                       ESMC_MESHELEMTYPE_HEX,
                       ESMC_MESHELEMTYPE_HEX};
      mbt->elemConn = {1,  2,   5,   4,
                       10, 11,  14,  13,
                       2,  3,   6,   5,
                       11, 12,  15,  14,
                       4,  5,   8,   7,
                       13, 14,  17,  16,
                       5,  6,   9,   8,
                       14, 15,  18,  17};

    } else if (petCount == 4) {
      if (localPet == 0) {
        mbt->nodeId = {1,2,4,5,10,11,13,14};
        mbt->nodeCoord = {1.0 , 1.0 , 1.0,  //1
                          10.0, 1.0 , 1.0,  //2
                          1.0 , 10.0, 1.0,  //4
                          10.0, 10.0, 1.0,  //5
                          1.0 , 1.0 , 2.0,  //10
                          10.0, 1.0 , 2.0,  //11
                          1.0 , 10.0, 2.0,  //13
                          10.0, 10.0, 2.0}; //14
        mbt->nodeOwner = {0,0,0,0,0,0,0,0};
        mbt->elemId = {1};
        mbt->elemType = {ESMC_MESHELEMTYPE_HEX};
        mbt->elemConn = {1,2,4,3,5,6,8,7};
        mbt->elemCoord = {5,5,0.5};
      } else if (localPet == 1) {
        mbt->nodeId = {2,3,5,6,11,12,14,15};
        mbt->nodeCoord = {10.0, 1.0 , 1.0,  //2
                          20.0, 1.0 , 1.0,  //3
                          10.0, 10.0, 1.0,  //5
                          20.0, 10.0, 1.0,  //6
                          1.0 , 20.0, 1.0,  //7
                          10.0, 20.0, 1.0,  //8
                          20.0, 20.0, 1.0,  //9
                          10.0, 1.0 , 2.0,  //11
                          20.0, 1.0 , 2.0,  //12
                          10.0, 10.0, 2.0,  //14
                          20.0, 10.0, 2.0}; //15
        mbt->nodeOwner = {0,1,0,1,0,1,0,1};
        mbt->elemId = {2};
        mbt->elemType = {ESMC_MESHELEMTYPE_HEX};
        mbt->elemConn = {1,2,4,3,5,6,8,7};
        mbt->elemCoord = {15,5,0.5};
      } else if (localPet == 2) {
        mbt->nodeId = {4,5,7,8,13,14,16,17};
        mbt->nodeCoord = {1.0 , 10.0, 1.0,  //4
                          10.0, 10.0, 1.0,  //5
                          1.0 , 20.0, 1.0,  //7
                          10.0, 20.0, 1.0,  //8
                          1.0 , 10.0, 2.0,  //13
                          10.0, 10.0, 2.0,  //14
                          1.0 , 20.0, 2.0,  //16
                          10.0, 20.0, 2.0}; //17
        mbt->nodeOwner = {0,0,2,2,0,0,2,2};
        mbt->elemId = {3};
        mbt->elemType = {ESMC_MESHELEMTYPE_HEX};
        mbt->elemConn = {1,2,4,3,5,6,8,7};
        mbt->elemCoord = {5,15,0.5};
      } else if (localPet == 3) {
        mbt->nodeId = {5,6,8,9,14,15,17,18};
        mbt->nodeCoord = {10.0, 10.0, 1.0,  //5
                          20.0, 10.0, 1.0,  //6
                          10.0, 20.0, 1.0,  //8
                          20.0, 20.0, 1.0,  //9
                          10.0, 10.0, 2.0,  //14
                          20.0, 10.0, 2.0,  //15
                          10.0, 20.0, 2.0,  //17
                          20.0, 20.0, 2.0}; //18
        mbt->nodeOwner = {0,1,2,3,0,1,2,3};
        mbt->elemId = {4};
        mbt->elemType = {ESMC_MESHELEMTYPE_HEX};
        mbt->elemConn = {1,2,4,3,5,6,8,7};
        mbt->elemCoord = {15,15,0.5};
      }

      if (redist) {
        // total redist case
        if (localPet == 0) {
          mbt->redist_nodeId = {5,6,8,9,14,15,17,18};
          mbt->redist_nodeCoord = {10.0, 10.0, 1.0,  //5
                                   20.0, 10.0, 1.0,  //6
                                   10.0, 20.0, 1.0,  //8
                                   20.0, 20.0, 1.0,  //9
                                   10.0, 10.0, 2.0,  //14
                                   20.0, 10.0, 2.0,  //15
                                   10.0, 20.0, 2.0,  //17
                                   20.0, 20.0, 2.0}; //18
          mbt->redist_nodeOwner = {0,0,0,0,0,0,0,0};
          mbt->redist_elemId = {4};
          mbt->redist_elemType = {ESMC_MESHELEMTYPE_HEX};
          mbt->redist_elemConn = {1,2,4,3,5,6,8,7};
          mbt->redist_elemCoord = {15,15,0.5};
        } else if (localPet == 1) {
          mbt->redist_nodeId = {4,5,7,8,13,14,16,17};
          mbt->redist_nodeCoord = {1.0 , 10.0, 1.0,  //4
                                   10.0, 10.0, 1.0,  //5
                                   1.0 , 20.0, 1.0,  //7
                                   10.0, 20.0, 1.0,  //8
                                   1.0 , 10.0, 2.0,  //13
                                   10.0, 10.0, 2.0,  //14
                                   1.0 , 20.0, 2.0,  //16
                                   10.0, 20.0, 2.0}; //17
          mbt->redist_nodeOwner = {1,0,1,0,1,0,1,0};
          mbt->redist_elemId = {3};
          mbt->redist_elemType = {ESMC_MESHELEMTYPE_HEX};
          mbt->redist_elemConn = {1,2,4,3,5,6,8,7};
          mbt->redist_elemCoord = {5,15,0.5};
        } else if (localPet == 2) {
          mbt->redist_nodeId = {2,3,5,6,11,12,14,15};
          mbt->redist_nodeCoord = {10.0, 1.0 , 1.0,  //2
                                   20.0, 1.0 , 1.0,  //3
                                   10.0, 10.0, 1.0,  //5
                                   20.0, 10.0, 1.0,  //6
                                   1.0 , 20.0, 1.0,  //7
                                   10.0, 20.0, 1.0,  //8
                                   20.0, 20.0, 1.0,  //9
                                   10.0, 1.0 , 2.0,  //11
                                   20.0, 1.0 , 2.0,  //12
                                   10.0, 10.0, 2.0,  //14
                                   20.0, 10.0, 2.0}; //15
          mbt->redist_nodeOwner = {2,2,0,0,2,2,0,0};
          mbt->redist_elemId = {2};
          mbt->redist_elemType = {ESMC_MESHELEMTYPE_HEX};
          mbt->redist_elemConn = {1,2,4,3,5,6,8,7};
          mbt->redist_elemCoord = {15,5,0.5};
        } else if (localPet == 3) {
          mbt->redist_nodeId = {1,2,4,5,10,11,13,14};
          mbt->redist_nodeCoord = {1.0 , 1.0 , 1.0,  //1
                      10.0, 1.0 , 1.0,  //2
                      1.0 , 10.0, 1.0,  //4
                      10.0, 10.0, 1.0,  //5
                      1.0 , 1.0 , 2.0,  //10
                      10.0, 1.0 , 2.0,  //11
                      1.0 , 10.0, 2.0,  //13
                      10.0, 10.0, 2.0}; //14
          mbt->redist_nodeOwner = {3,2,1,0,3,2,1,0};
          mbt->redist_elemId = {1};
          mbt->redist_elemType = {ESMC_MESHELEMTYPE_HEX};
          mbt->redist_elemConn = {1,2,4,3,5,6,8,7};
          mbt->redist_elemCoord = {5,5,0.5};
        }
      }
    }

  } MBMESHTEST_CATCH_RETURN_NULL(&rc)

  rc = ESMF_SUCCESS;
  return mbt;
}

MBMeshTest* mbmesh_gen_hex_3d_sph(int &rc, bool redist=false) {
#undef ESMC_METHOD
#define ESMC_METHOD "mbmesh_gen_hex_3d_sph"

  rc = ESMF_RC_NOT_IMPL;
  MBMeshTest *mbt = NULL;

  try {

    // Get parallel information
    int localPet, petCount;
    ESMC_VM vm;
    vm=ESMC_VMGetGlobal(&rc);
    MBMESHTEST_CHECK_RC_THROW(rc)

    rc=ESMC_VMGet(vm, &localPet, &petCount, (int *)NULL, (MPI_Comm *)NULL,
                  (int *)NULL, (int *)NULL);
    MBMESHTEST_CHECK_RC_THROW(rc)

    if (petCount !=1 && petCount != 4)
      MBMESHTEST_THROW_ERROR("Must be run with 1 or 4 cores.")

    // Cartesian to spherical coordinate transformation - degrees (suitable for c:[0:30])
    double c2s = 10.;

    int localrc;
    mbt = mbmesh_gen_hex_3d_cart(localrc, redist);
    MBMESHTEST_CHECK_RC_THROW(localrc)
    std::for_each(mbt->nodeCoord.begin(), mbt->nodeCoord.end(), 
                  [&c2s](double &d) {d*=c2s;});
    std::for_each(mbt->elemCoord.begin(), mbt->elemCoord.end(), 
                  [&c2s](double &d) {d*=c2s;});

    std::for_each(mbt->redist_nodeCoord.begin(), mbt->redist_nodeCoord.end(), [&c2s](double &d) {d*=c2s;});
    std::for_each(mbt->redist_elemCoord.begin(), mbt->redist_elemCoord.end(), [&c2s](double &d) {d*=c2s;});

    mbt->coord_sys=ESMC_COORDSYS_SPH_DEG;

  } MBMESHTEST_CATCH_RETURN_NULL(&rc)

  rc = ESMF_SUCCESS;
  return mbt;
}

MBMeshTest* mbmesh_gen_ngon_2d_cart(int &rc, bool redist=false) {
  //
  //  3.1                    / -- 15 -- \
  //  3.0    13 ------ 14 --             -- 16
  //         |         |                    |
  //         |         |         5           \
  //        /          \                      \
  //  2.0  9     4      10 -- \          / --- 12
  //  1.9   \          /        -- 11 --       /
  //         |         |    2     /          /
  //         |         |         /     3   /
  //  1.5    5 ------- 6        7         8
  //         |            \     \         \
  //   1     |    1         \   \         |
  //         |                \ |          \
  //  0.1    1 --               3 --------- 4
  //   0          \ -- 2 -----/
  //      0  .1   1  1.5        2  2.2 2.5  3  3.1
  //
  //      Node Ids at corners
  //      Element Ids in centers
  //
  //
  //      ( Everything owned by PET 0)
  //
#undef ESMC_METHOD
#define ESMC_METHOD "mbmesh_gen_ngon_2d_cart"

  rc = ESMF_RC_NOT_IMPL;
  MBMeshTest *mbt = NULL;

  try {

    // Get parallel information
    int localPet, petCount;
    ESMC_VM vm;
    vm=ESMC_VMGetGlobal(&rc);
    MBMESHTEST_CHECK_RC_THROW(rc)

    rc=ESMC_VMGet(vm, &localPet, &petCount, (int *)NULL, (MPI_Comm *)NULL,
                  (int *)NULL, (int *)NULL);
    MBMESHTEST_CHECK_RC_THROW(rc)

    if (petCount !=1 && petCount != 4)
      MBMESHTEST_THROW_ERROR("Must be run with 1 or 4 cores.")

    int pdim = 2;
    int sdim = 2;
    ESMC_CoordSys_Flag coord_sys=ESMC_COORDSYS_CART;

    int num_node;
    int num_elem;
    int num_elem_conn;
    int redist_num_node;
    int redist_num_elem;
    int redist_num_elem_conn;

    if (petCount == 1) {
      int num_elem = 5;
      int num_node = 16;
      int num_elem_conn = 2*5+3*6;

      if (redist)
        MBMESHTEST_THROW_ERROR("Cannot run redist with 1 core.")

    } else if (petCount == 4) {
      if (localPet == 0) {
        num_elem = 1;
        num_node = 5;
        num_elem_conn = 5;
        redist_num_elem = 1;
        redist_num_node = 6;
        redist_num_elem_conn = 6;
      } else if (localPet == 1) {
        num_elem = 2;
        num_node = 8;
        num_elem_conn = 5+6;
        redist_num_elem = 1;
        redist_num_node = 6;
        redist_num_elem_conn = 6;
      } else if (localPet == 2) {
        num_elem = 1;
        num_node = 6;
        num_elem_conn = 6;
        redist_num_elem = 2;
        redist_num_node = 8;
        redist_num_elem_conn = 5+6;
      } else if (localPet == 3) {
        num_elem = 1;
        num_node = 6;
        num_elem_conn = 6;
        redist_num_elem = 1;
        redist_num_node = 5;
        redist_num_elem_conn = 5;
      }
    }

    if (!redist)
      mbt = new MBMeshTest(pdim, sdim, coord_sys, num_node, num_elem, num_elem_conn);
    else
      mbt = new MBMeshTest(pdim, sdim, coord_sys, num_node, num_elem, num_elem_conn, redist, redist_num_node, redist_num_elem, redist_num_elem_conn);

    if (petCount == 1) {
      mbt->nodeId = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16};
      mbt->nodeCoord = {0.1,0.1, // 1
                        1.5,0.0, // 2
                        2.0,0.1, // 3
                        3.0,0.1, // 4
                        0.1,1.5, // 5
                        1.5,1.5, // 6
                        2.0,1.5, // 7
                        2.9,1.5, // 8
                        0.0,2.0, // 9
                        1.6,2.0, // 10
                        2.2,1.9, // 11
                        3.1,2.0, // 12
                        0.1,3.0, // 13
                        1.5,3.0, // 14
                        2.2,3.0, // 15
                        3.0,3.0};// 16
      mbt->nodeOwner = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
      mbt->elemId = {1,2,3,4,5};
      mbt->elemType = {5,5,6,6,6};
      mbt->elemConn = {1,2,3,6,5,
                       3,7,11,10,6,
                       3,4,8,12,11,7,
                       5,6,10,14,13,9,
                       10,11,12,16,15,14};
      mbt->elemCoord = {1.0, 1.0,    // 1
                        1.74,1.76,  // 2
                        2.4, 1.6,    // 3
                        1.01,2.042, // 4
                        2.1, 2.5};   // 5

    } else if (petCount == 4) {
      if (localPet == 0) {
        mbt->nodeId = {1,2,3,5,6};
        mbt->nodeCoord = {0.1,0.1, // 1
                          1.5,0.0, // 2
                          2.0,0.1, // 3
                          0.1,1.5, // 5
                          1.5,1.5};// 6
        mbt->nodeOwner = {0,0,0,0,0};
        mbt->elemId = {1};
        mbt->elemType = {5};
        mbt->elemConn = {1,2,3,5,4};
        mbt->elemCoord = {1.0,1.0};
      } else if (localPet == 1) {
        mbt->nodeId = {3,4,6,7,8,10,11,12};
        mbt->nodeCoord = {2.0,0.1, // 3
                          3.0,0.1, // 4
                          1.5,1.5, // 6
                          2.0,1.5, // 7
                          2.9,1.5, // 8
                          1.6,2.0, // 10
                          2.2,1.9, // 11
                          3.1,2.0};// 12
        mbt->nodeOwner = {0,1,0,1,1,1,1,1};
        mbt->elemId = {2,3};
        mbt->elemType = {5,6};
        mbt->elemConn = {1,4,7,6,3,
                         1,2,5,8,7,4};
        mbt->elemCoord = {1.74,1.76,
                          2.4, 1.6,};
      } else if (localPet == 2) {
        mbt->nodeId = {5,6,9,10,13,14};
        mbt->nodeCoord = {0.1,1.5, // 5
                          1.5,1.5, // 6
                          0.0,2.0, // 9
                          1.6,2.0, // 10
                          0.1,3.0, // 13
                          1.5,3.0};// 14
        mbt->nodeOwner = {0,0,2,1,2,2};
        mbt->elemId = {4};
        mbt->elemType = {6};
        mbt->elemConn = {1,2,4,6,5,3};
        mbt->elemCoord = {1.01,2.042};
      } else if (localPet == 3) {
        mbt->nodeId = {10,11,12,14,15,16};
        mbt->nodeCoord = {1.6,2.0, // 10
                          2.2,1.9, // 11
                          3.1,2.0, // 12
                          1.5,3.0, // 14
                          2.2,3.0, // 15
                          3.0,3.0};// 16
        mbt->nodeOwner = {1,1,1,2,3,3};
        mbt->elemId = {5};
        mbt->elemType = {6};
        mbt->elemConn = {1,2,3,6,5,4};
        mbt->elemCoord = {2.1, 2.5};
      }

      if (redist) {
        if (localPet == 0) {
          mbt->nodeId = {10,11,12,14,15,16};
          mbt->nodeCoord = {1.6,2.0, // 10
                            2.2,1.9, // 11
                            3.1,2.0, // 12
                            1.5,3.0, // 14
                            2.2,3.0, // 15
                            3.0,3.0};// 16
          mbt->nodeOwner = {0,0,0,0,0,0};
          mbt->elemId = {5};
          mbt->elemType = {6};
          mbt->elemConn = {1,2,3,6,5,4};
          mbt->elemCoord = {2.1, 2.5};
        } else if (localPet == 1) {
          mbt->nodeId = {5,6,9,10,13,14};
          mbt->nodeCoord = {0.1,1.5, // 5
                            1.5,1.5, // 6
                            0.0,2.0, // 9
                            1.6,2.0, // 10
                            0.1,3.0, // 13
                            1.5,3.0};// 14
          mbt->nodeOwner = {1,1,1,0,1,0};
          mbt->elemId = {4};
          mbt->elemType = {6};
          mbt->elemConn = {1,2,4,6,5,3};
          mbt->elemCoord = {1.01,2.042};
        } else if (localPet == 2) {
          mbt->nodeId = {3,4,6,7,8,10,11,12};
          mbt->nodeCoord = {2.0,0.1, // 3
                            3.0,0.1, // 4
                            1.5,1.5, // 6
                            2.0,1.5, // 7
                            2.9,1.5, // 8
                            1.6,2.0, // 10
                            2.2,1.9, // 11
                            3.1,2.0};// 12
          mbt->nodeOwner = {2,2,1,2,2,0,0,0};
          mbt->elemId = {2,3};
          mbt->elemType = {5,6};
          mbt->elemConn = {1,4,7,6,3,
                          1,2,5,8,7,4};
          mbt->elemCoord = {1.74,1.76,
                            2.4, 1.6,};
        } else if (localPet == 3) {
          mbt->nodeId = {1,2,3,5,6};
          mbt->nodeCoord = {0.1,0.1, // 1
                            1.5,0.0, // 2
                            2.0,0.1, // 3
                            0.1,1.5, // 5
                            1.5,1.5};// 6
          mbt->nodeOwner = {3,3,2,1,1};
          mbt->elemId = {1};
          mbt->elemType = {5};
          mbt->elemConn = {1,2,3,5,4};
          mbt->elemCoord = {1.0,1.0};
        }
      }
    }

  } MBMESHTEST_CATCH_RETURN_NULL(&rc)

  rc = ESMF_SUCCESS;
  return mbt;
}

MBMeshTest* mbmesh_gen_ngon_2d_sph(int &rc, bool redist=false) {
  //
  //  3.1                    / -- 15 -- \
  //  3.0    13 ------ 14 --             -- 16
  //         |         |                    |
  //         |         |         5           \
  //        /          \                      \
  //  2.0  9     4      10 -- \          / --- 12
  //  1.9   \          /        -- 11 --       /
  //         |         |    2     /          /
  //         |         |         /     3   /
  //  1.5    5 ------- 6        7         8
  //         |            \     \         \
  //   1     |    1         \   \         |
  //         |                \ |          \
  //  0.1    1 --               3 --------- 4
  //   0          \ -- 2 -----/
  //      0  .1   1  1.5        2  2.2 2.5  3  3.1
  //
  //      Node Ids at corners
  //      Element Ids in centers
  //
  //
  //      ( Everything owned by PET 0)
  //
#undef ESMC_METHOD
#define ESMC_METHOD "mbmesh_gen_ngon_2d_sph"

  rc = ESMF_RC_NOT_IMPL;
  MBMeshTest *mbt = NULL;

  try {

    // Get parallel information
    int localPet, petCount;
    ESMC_VM vm;
    vm=ESMC_VMGetGlobal(&rc);
    MBMESHTEST_CHECK_RC_THROW(rc)

    rc=ESMC_VMGet(vm, &localPet, &petCount, (int *)NULL, (MPI_Comm *)NULL,
                  (int *)NULL, (int *)NULL);
    MBMESHTEST_CHECK_RC_THROW(rc)

    if (petCount !=1 && petCount != 4)
      MBMESHTEST_THROW_ERROR("Must be run with 1 or 4 cores.")

    // Cartesian to spherical coordinate transformation - degrees (suitable for c:[0:3])
    double c2s = 100.;

    int localrc;
    mbt = mbmesh_gen_ngon_2d_cart(localrc, redist);
    MBMESHTEST_CHECK_RC_THROW(localrc)
    std::for_each(mbt->nodeCoord.begin(), mbt->nodeCoord.end(), 
                  [&c2s](double &d) {d*=c2s;});
    std::for_each(mbt->elemCoord.begin(), mbt->elemCoord.end(), 
                  [&c2s](double &d) {d*=c2s;});

    std::for_each(mbt->redist_nodeCoord.begin(), mbt->redist_nodeCoord.end(), [&c2s](double &d) {d*=c2s;});
    std::for_each(mbt->redist_elemCoord.begin(), mbt->redist_elemCoord.end(), [&c2s](double &d) {d*=c2s;});

    mbt->coord_sys=ESMC_COORDSYS_SPH_DEG;

  } MBMESHTEST_CATCH_RETURN_NULL(&rc)

  rc = ESMF_SUCCESS;
  return mbt;
}

MBMeshTest* mbmesh_gen_mix_2d_cart(int &rc, bool redist=false) {
  //
  //
  //  3.0   13 ------ 14 ------- 15 ------ 16
  //        |         |          |         |
  //        |    8    |    9     |    10   |
  //        |         |          |         |
  //  2.0   9 ------- 10 ------- 11 ------ 12
  //        |         |          |         |
  //        |    5    |    6     |    7    |
  //        |         |          |         |
  //  1.5   5 ------- 6 -------- 7 ------- 8
  //        |         |  \    3  |         |
  //        |    1    |    \     |    4    |
  //        |         |  2   \   |         |
  //  0.0   1 ------- 2 -------- 3 ------- 4
  //
  //       0.0       1.5        2.0        3.0
  //
  //      Node Ids at corners
  //      Element Ids in centers
  //
  //
  //      ( Everything owned by PET 0)
  //
#undef ESMC_METHOD
#define ESMC_METHOD "mbmesh_gen_mix_2d_cart_par"

  rc = ESMF_RC_NOT_IMPL;
  MBMeshTest *mbt = NULL;

  try {

    // Get parallel information
    int localPet, petCount;
    ESMC_VM vm;
    vm=ESMC_VMGetGlobal(&rc);
    MBMESHTEST_CHECK_RC_THROW(rc)

    rc=ESMC_VMGet(vm, &localPet, &petCount, (int *)NULL, (MPI_Comm *)NULL,
                  (int *)NULL, (int *)NULL);
    MBMESHTEST_CHECK_RC_THROW(rc)

    if (petCount !=1 && petCount != 4)
      MBMESHTEST_THROW_ERROR("Must be run with 1 or 4 cores.")

    int pdim = 2;
    int sdim = 2;
    ESMC_CoordSys_Flag coord_sys=ESMC_COORDSYS_CART;

    int num_node;
    int num_elem;
    int num_elem_conn;
    int redist_num_node;
    int redist_num_elem;
    int redist_num_elem_conn;

    if (petCount == 1) {
      int num_elem = 10;
      int num_node = 16;
      int num_elem_conn = 8*4+2*3;

      if (redist)
        MBMESHTEST_THROW_ERROR("Cannot run redist with 1 core.")

    } else if (petCount == 4) {
      if (localPet == 0){
        num_node = 9;
        num_elem = 5;
        num_elem_conn = 3*4+2*3;
        redist_num_node = 4;
        redist_num_elem = 1;
        redist_num_elem_conn = 1*4;
      } else if (localPet == 1) {
        num_node = 6;
        num_elem = 2;
        num_elem_conn = 2*4;
        redist_num_node = 6;
        redist_num_elem = 2;
        redist_num_elem_conn = 2*4;
      } else if (localPet == 2) {
        num_node = 6;
        num_elem = 2;
        num_elem_conn = 2*4;
        redist_num_node = 6;
        redist_num_elem = 2;
        redist_num_elem_conn = 2*4;
      } else if (localPet == 3) {
        num_node = 4;
        num_elem = 1;
        num_elem_conn = 1*4;
        redist_num_node = 9;
        redist_num_elem = 5;
        redist_num_elem_conn = 3*4+2*3;
      }
    }

    if (!redist)
      mbt = new MBMeshTest(pdim, sdim, coord_sys, num_node, num_elem, num_elem_conn);
    else
      mbt = new MBMeshTest(pdim, sdim, coord_sys, num_node, num_elem, num_elem_conn, redist, redist_num_node, redist_num_elem, redist_num_elem_conn);

    if (petCount == 1) {
      mbt->nodeId ={1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16};
      mbt->nodeCoord ={0.0,0.0, 1.5,0.0, 2.0,0.0, 3.0,0.0,
                       0.0,1.5, 1.5,1.5, 2.0,1.5, 3.0,1.5,
                       0.0,2.0, 1.5,2.0, 2.0,2.0, 3.0,2.0,
                       0.0,3.0, 1.5,3.0, 2.0,3.0, 3.0,3.0};
      mbt->nodeOwner ={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
      mbt->elemId ={1,2,3,4,5,6,7,8,9,10};
      mbt->elemType ={ESMC_MESHELEMTYPE_QUAD,
                      ESMC_MESHELEMTYPE_TRI,
                      ESMC_MESHELEMTYPE_TRI,
                      ESMC_MESHELEMTYPE_QUAD,
                      ESMC_MESHELEMTYPE_QUAD,
                      ESMC_MESHELEMTYPE_QUAD,
                      ESMC_MESHELEMTYPE_QUAD,
                      ESMC_MESHELEMTYPE_QUAD,
                      ESMC_MESHELEMTYPE_QUAD,
                      ESMC_MESHELEMTYPE_QUAD};
      mbt->elemConn ={1,2,6,5,
                      2,3,6,
                      3,7,6,
                      3,4,8,7,
                      5,6,10,9,
                      6,7,11,10,
                      7,8,12,11,
                      9,10,14,13,
                      10,11,15,14,
                      11,12,16,15};
      mbt->elemCoord ={1.0,1.0,  1.6, 0.1,  1.9,1.4, 2.5,1.0,
                       1.0,1.75, 1.75,1.75, 2.5,1.75,
                       1.0,2.5,  1.75,2.5,  2.5,2.5};

    } else if (petCount == 4) {

      //   3.0   13 ------ 14 ------ 15     [15] ----------- 16
      //         |         |         |       |               |
      //         |         |         |       |               |
      //         |    8    |    9    |       |       10      |
      //         |         |         |       |               |
      //         |         |         |       |               |
      //   2.0  [9] ----- [10] ---- [11]    [11] ---------- [12]
      //
      //       1.0       1.5       2.0     2.0             3.0
      //
      //                PET 2                      PET 3
      //
      //
      //   2.0   9 ------- 10 ------ 11     [11] ----------- 12
      //         |         |         |       |               |
      //         |    5    |    6    |       |       7       |
      //         |         |         |       |               |
      //   1.5   5 ------- 6 ------- 7      [7] -----------  8
      //         |         |  \   3  |       |               |
      //         |    1    |    \    |       |       4       |
      //         |         | 2    \  |       |               |
      //   0.0   1 ------- 2 ------- 3      [3] ------------ 4
      //
      //         0.0       1.5       2.0     2.0             3.0
      //
      //                PET 0                      PET 1
      //
      //               Node Id labels at corners
      //              Element Id labels in centers

      if (localPet == 0){
        mbt->nodeId = {1,2,3,5,6,7,9,10,11};
        mbt->nodeCoord = {1.0,1.0,1.5,1.0,2.0,1.0,1.0,1.5,1.5,1.5,2.0,1.5,1.0,2.0,1.5,2.0,2.0,2.0};
        mbt->nodeOwner = {0,0,0,0,0,0,0,0,0};
        mbt->elemType = {4,3,3,4,4};
        mbt->elemId = {1,2,3,5,6};
        mbt->elemConn = {1,2,5,4,2,3,5,3,6,5,4,5,8,7,5,6,9,8};
        mbt->elemCoord ={1.0,1.0,1.6,0.1,1.9,1.4,
                    1.0,1.75,1.75, 1.75};
      } else if (localPet == 1) {
        mbt->nodeId = {3,4,7,8,11,12};
        mbt->nodeCoord = {2.0,1.0,3.0,1.0,2.0,1.5,3.0,1.5,2.0,2.0,3.0,2.0};
        mbt->nodeOwner = {0,1,0,1,0,1};
        mbt->elemId = {4,7};
        mbt->elemType = {4,4};
        mbt->elemConn = {1,2,4,3,3,4,6,5};
        mbt->elemCoord ={2.5,1.0, 2.5, 1.75};
      } else if (localPet == 2) {
        mbt->nodeId = {9,10,11,13,14,15};
        mbt->nodeCoord = {1.0,2.0,1.5,2.0,2.0,2.0,1.0,3.0,1.5,3.0,2.0,3.0};
        mbt->nodeOwner = {0,0,0,2,2,2};
        mbt->elemId = {8,9};
        mbt->elemType = {4,4};
        mbt->elemConn = {1,2,5,4,2,3,6,5};
        mbt->elemCoord ={1.0, 2.5, 1.75, 2.5};
      } else if (localPet == 3) {
        mbt->nodeId = {11,12,15,16};
        mbt->nodeCoord = {2.0,2.0,3.0,2.0,2.0,3.0,3.0,3.0};
        mbt->nodeOwner = {0,1,2,3};
        mbt->elemId = {10};
        mbt->elemType = {4};
        mbt->elemConn = {1,2,4,3};
        mbt->elemCoord ={2.5, 2.5};
      }
      if (redist) {
        if (localPet == 0) {
          mbt->redist_nodeId = {11,12,15,16};
          mbt->redist_nodeCoord = {2.0,2.0,3.0,2.0,2.0,3.0,3.0,3.0};
          mbt->redist_nodeOwner = {0,0,0,0};
          mbt->redist_elemId = {10};
          mbt->redist_elemType = {4};
          mbt->redist_elemConn = {1,2,4,3};
          mbt->redist_elemCoord ={2.5, 2.5};
        } else if (localPet == 1) {
          mbt->redist_nodeId = {9,10,11,13,14,15};
          mbt->redist_nodeCoord = {1.0,2.0,1.5,2.0,2.0,2.0,1.0,3.0,1.5,3.0,2.0,3.0};
          mbt->redist_nodeOwner = {1,1,0,1,1,0};
          mbt->redist_elemId = {8,9};
          mbt->redist_elemType = {4,4};
          mbt->redist_elemConn = {1,2,5,4,2,3,6,5};
          mbt->redist_elemCoord ={1.0, 2.5, 1.75, 2.5};
        } else if (localPet == 2) {
          mbt->redist_nodeId = {3,4,7,8,11,12};
          mbt->redist_nodeCoord = {2.0,1.0,3.0,1.0,2.0,1.5,3.0,1.5,2.0,2.0,3.0,2.0};
          mbt->redist_nodeOwner = {2,2,2,2,0,0};
          mbt->redist_elemId = {4,7};
          mbt->redist_elemType = {4,4};
          mbt->redist_elemConn = {1,2,4,3,3,4,6,5};
          mbt->redist_elemCoord ={2.5,1.0, 2.5, 1.75};
        } else if (localPet == 3) {
          mbt->redist_nodeId = {1,2,3,5,6,7,9,10,11};
          mbt->redist_nodeCoord = {1.0,1.0,1.5,1.0,2.0,1.0,1.0,1.5,1.5,1.5,2.0,1.5,1.0,2.0,1.5,2.0,2.0,2.0};
          mbt->redist_nodeOwner = {3,3,2,3,3,2,1,1,0};
          mbt->redist_elemType = {4,3,3,4,4};
          mbt->redist_elemId = {1,2,3,5,6};
          mbt->redist_elemConn = {1,2,5,4,2,3,5,3,6,5,4,5,8,7,5,6,9,8};
          mbt->redist_elemCoord ={1.0,1.0,1.6,0.1,1.9,1.4,
                                  1.0,1.75,1.75, 1.75};
        }
      }
    }

  } MBMESHTEST_CATCH_RETURN_NULL(&rc)

  rc = ESMF_SUCCESS;
  return mbt;
}

MBMeshTest* mbmesh_gen_mix_2d_sph(int &rc, bool redist=false) {
  //
  //
  //  3.0   13 ------ 14 ------- 15 ------ 16
  //        |         |          |         |
  //        |    8    |    9     |    10   |
  //        |         |          |         |
  //  2.0   9 ------- 10 ------- 11 ------ 12
  //        |         |          |         |
  //        |    5    |    6     |    7    |
  //        |         |          |         |
  //  1.5   5 ------- 6 -------- 7 ------- 8
  //        |         |  \    3  |         |
  //        |    1    |    \     |    4    |
  //        |         |  2   \   |         |
  //  0.0   1 ------- 2 -------- 3 ------- 4
  //
  //       0.0       1.5        2.0        3.0
  //
  //      Node Ids at corners
  //      Element Ids in centers
  //
  //
  //      ( Everything owned by PET 0)
  //
#undef ESMC_METHOD
#define ESMC_METHOD "mbmesh_gen_mix_2d_sph"

  rc = ESMF_RC_NOT_IMPL;
  MBMeshTest *mbt = NULL;

  try {

    // Get parallel information
    int localPet, petCount;
    ESMC_VM vm;
    vm=ESMC_VMGetGlobal(&rc);
    MBMESHTEST_CHECK_RC_THROW(rc)

    rc=ESMC_VMGet(vm, &localPet, &petCount, (int *)NULL, (MPI_Comm *)NULL,
                  (int *)NULL, (int *)NULL);
    MBMESHTEST_CHECK_RC_THROW(rc)

    if (petCount !=1 && petCount != 4)
      MBMESHTEST_THROW_ERROR("Must be run with 1 or 4 cores.")

    // Cartesian to spherical coordinate transformation - degrees (suitable for c:[0:3])
    double c2s = 100.;

    int localrc;
    mbt = mbmesh_gen_mix_2d_cart(localrc, redist);
    MBMESHTEST_CHECK_RC_THROW(localrc)
    std::for_each(mbt->nodeCoord.begin(), mbt->nodeCoord.end(), 
                  [&c2s](double &d) {d*=c2s;});
    std::for_each(mbt->elemCoord.begin(), mbt->elemCoord.end(), 
                  [&c2s](double &d) {d*=c2s;});

    std::for_each(mbt->redist_nodeCoord.begin(), mbt->redist_nodeCoord.end(), [&c2s](double &d) {d*=c2s;});
    std::for_each(mbt->redist_elemCoord.begin(), mbt->redist_elemCoord.end(), [&c2s](double &d) {d*=c2s;});

    mbt->coord_sys=ESMC_COORDSYS_SPH_DEG;

  } MBMESHTEST_CATCH_RETURN_NULL(&rc)

  rc = ESMF_SUCCESS;
  return mbt;
}



#endif
