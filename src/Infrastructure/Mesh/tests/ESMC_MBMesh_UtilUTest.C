// $Id$
//==============================================================================
//
// Earth System Modeling Framework
// Copyright 2002-2021, University Corporation for Atmospheric Research,
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
#include "ESMCI_MBMesh.h"
#include "ESMCI_MBMesh_Glue.h"
#include "MBTagConventions.hpp"
#include "moab/Core.hpp"
#endif

#include <cstring>

#if defined ESMF_MOAB
MBMesh* create_mesh_halfway(int &rc) {
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

  // set Mesh parameters
  int num_elem = 4;
  int num_node = 9;

  int nodeId_s [] ={10,20,30,40,50,60,70,80,90};
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

  ESMC_CoordSys_Flag local_coordSys=ESMC_COORDSYS_CART;

  int orig_sdim = sdim;

  MBMesh *mesh = new MBMesh();

  MBMesh_create(&mesh, &pdim, &sdim, &local_coordSys, &rc);
  if (rc != ESMF_SUCCESS) return NULL;

  // InterArray<int> *iin = new InterArray<int>(nodeMask_s,9);
  // 
  // MBMesh_addnodes(&mesh, &num_node, nodeId_s, nodeCoord_s, nodeOwner_s, iin,
  //                 &local_coordSys, &orig_sdim, &rc);
  // if (rc != ESMF_SUCCESS) return NULL;
  // 
  // int areapresent = 1;
  // int coordspresent = 1;
  // int numelemconn = 16;
  // int regridconserve = 0;
  // InterArray<int> *iie = new InterArray<int>(elemMask_s,4);
  // MBMesh_addelements(&mesh, &num_elem, elemId_s, elemType_s, iie,
  //                    &areapresent, elemArea_s,
  //                    &coordspresent, elemCoord_s,
  //                    &numelemconn, elemConn_s,
  //                    &regridconserve,
  //                    &local_coordSys, &orig_sdim, &rc);
  // if (rc != ESMF_SUCCESS) return NULL;
  // 
  // delete iin;
  // delete iie;

  rc = ESMF_SUCCESS;
  return mesh;
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

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Throw an error from the MBMesh");
  strcpy(failMsg, "Did not return ESMC_RC_MOAB_ERROR");
#if defined ESMF_MOAB
  int localrc;
  MBMesh *mesh = create_mesh_halfway(localrc);
  if (localrc == ESMF_SUCCESS) {
    int elemIds[1];
    InterArray<int> *eii = new InterArray<int>(elemIds,1);
    MBMesh_GetElemCreateInfo(mesh, eii, NULL, NULL, NULL, NULL, NULL, &rc);
    delete eii;
  }
#else
  rc = ESMC_RC_MOAB_ERROR;
#endif
  ESMC_Test((rc==ESMC_RC_MOAB_ERROR), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  ESMC_TestEnd(__FILE__, __LINE__, 0);

  return 0;
}


