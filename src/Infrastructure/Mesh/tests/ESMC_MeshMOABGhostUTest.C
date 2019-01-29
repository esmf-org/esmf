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

// ESMF header
#include "ESMC.h"

// ESMF Test header
#include "ESMC_Test.h"

#if defined ESMF_MOAB

#include "ESMC_MBMeshTestUtilMBMesh.C"

#include "ESMCI_MBMesh_Glue.h"

// other headers
#include "ESMCI_MBMesh.h"
#include "ESMCI_MBMesh_Dual.h"

#include "MBTagConventions.hpp"
#include "moab/Core.hpp"
#include "moab/ParallelComm.hpp"
#endif

#include <iostream>
#include <iterator>
#include <vector>
#include <cstring>


//==============================================================================
//BOP
// !PROGRAM: ESMC_MeshMOABGhostUTest - Check for MOAB ghost element functionality
//
// !DESCRIPTION: 
////
//EOP
//-----------------------------------------------------------------------------

MBMesh* create_mesh_quad_9_2(ESMC_CoordSys_Flag coordsys, int &rc) {

  //
  //   3.0   13 ------ 14 ------ 15  ------------------ 16
  //         |         |         |                      |
  //         |         |         |                      |
  //         |    8    |    9    |              10      |
  //         |         |         |                      |
  //         |         |         |                      |
  //   2.0  [9] ----- [10] ---- [11] -----------------  12
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
  //         |         |         |       |               |
  //         |    1    |    2    |       |       4       |
  //         |         |         |       |               |
  //   1.0   1 ------- 2 ------- 3      [3] ------------ 4
  //
  //         1.0       1.5     2.0      2.0             3.0
  //
  //                PET 0                      PET 1
  //
  //               Node Id labels at corners
  //              Element Id labels in centers


  // Get parallel information
  int localPet, petCount;
  ESMC_VM vm;

  vm=ESMC_VMGetGlobal(&rc);
  if (rc != ESMF_SUCCESS) return 0;

  rc=ESMC_VMGet(vm, &localPet, &petCount, (int *)NULL, (MPI_Comm *)NULL,
                (int *)NULL, (int *)NULL);
  if (rc != ESMF_SUCCESS) return 0;

  if (!(petCount == 1 || petCount == 2)) {
    Throw() << "Test function must be run with either 1 or 2 processors";
    return NULL;
  }
  
  // Mesh variables
  int num_elem = 9;
  int num_node = 16;

  // set Mesh parameters
  int *nodeId;
  double *nodeCoord;
  int *nodeOwner;

  int *elemId;
  int *elemType;
  int *elemConn;
  double *elemCoord;

  int areapresent = 0;
  int coordspresent = 1;
  int numelemconn = 0;
  int regridconserve = 0;

  nodeId    = (int *) malloc (num_node * sizeof (int));
  nodeCoord = (double *) malloc (2*num_node * sizeof (double));
  nodeOwner = (int *) malloc (num_node * sizeof (int));
  elemId   = (int *) malloc (num_elem * sizeof (int));
  elemType = (int *) malloc (num_elem * sizeof (int));
  elemConn = (int *) malloc (4*num_elem * sizeof (int));
  elemCoord = (double *) malloc (2*num_elem * sizeof (double));

  int pdim=2;
  int sdim=2;
  // the MBMesh_create interface resets sdim to 3 in this case
  if (coordsys != ESMC_COORDSYS_CART) sdim = 2;

  MBMesh *mesh = new MBMesh();
  void *meshp = static_cast<void *> (mesh);
  MBMesh_create(&meshp, &pdim, &sdim, &coordsys, &rc);

  if (petCount == 1) {
      num_node = 16;
      num_elem = 9;

      nodeId[0]=1;
      nodeId[1]=2;
      nodeId[2]=3;
      nodeId[3]=4;
      nodeId[4]=5;
      nodeId[5]=6;
      nodeId[6]=7;
      nodeId[7]=8;
      nodeId[8]=9;
      nodeId[9]=10;
      nodeId[10]=11;
      nodeId[11]=12;
      nodeId[12]=13;
      nodeId[13]=14;
      nodeId[14]=15;
      nodeId[15]=16;

      nodeCoord[0]=1.0;nodeCoord[1]=1.0;
      nodeCoord[2]=1.5;nodeCoord[3]=1.0;
      nodeCoord[4]=2.0;nodeCoord[5]=1.0;
      nodeCoord[6]=3.0;nodeCoord[7]=1.0;
      nodeCoord[8]=1.0;nodeCoord[9]=1.5;
      nodeCoord[10]=1.5;nodeCoord[11]=1.5;
      nodeCoord[12]=2.0;nodeCoord[13]=1.5;
      nodeCoord[14]=3.0;nodeCoord[15]=1.5;
      nodeCoord[16]=1.0;nodeCoord[17]=2.0;
      nodeCoord[18]=1.5;nodeCoord[19]=2.0;
      nodeCoord[20]=2.0;nodeCoord[21]=2.0;
      nodeCoord[22]=3.0;nodeCoord[23]=2.0;
      nodeCoord[24]=1.0;nodeCoord[25]=3.0;
      nodeCoord[26]=1.5;nodeCoord[27]=3.0;
      nodeCoord[28]=2.0;nodeCoord[29]=3.0;
      nodeCoord[30]=3.0;nodeCoord[31]=3.0;

      nodeOwner[0]=0;
      nodeOwner[1]=0;
      nodeOwner[2]=0;
      nodeOwner[3]=0;
      nodeOwner[4]=0;
      nodeOwner[5]=0;
      nodeOwner[6]=0;
      nodeOwner[7]=0;
      nodeOwner[8]=0;
      nodeOwner[9]=0;
      nodeOwner[10]=0;
      nodeOwner[11]=0;
      nodeOwner[12]=0;
      nodeOwner[13]=0;
      nodeOwner[14]=0;
      nodeOwner[15]=0;

      elemId[0]=1;
      elemId[1]=2;
      elemId[2]=4;
      elemId[3]=5;
      elemId[4]=6;
      elemId[5]=7;
      elemId[6]=8;
      elemId[7]=9;
      elemId[8]=10;

      elemType[0]=ESMC_MESHELEMTYPE_QUAD;
      elemType[1]=ESMC_MESHELEMTYPE_QUAD;
      elemType[2]=ESMC_MESHELEMTYPE_QUAD;
      elemType[3]=ESMC_MESHELEMTYPE_QUAD;
      elemType[4]=ESMC_MESHELEMTYPE_QUAD;
      elemType[5]=ESMC_MESHELEMTYPE_QUAD;
      elemType[6]=ESMC_MESHELEMTYPE_QUAD;
      elemType[7]=ESMC_MESHELEMTYPE_QUAD;
      elemType[8]=ESMC_MESHELEMTYPE_QUAD;

      elemConn[0]=1;elemConn[1]=2;elemConn[2]=6;elemConn[3]=5;
      elemConn[4]=2;elemConn[5]=3;elemConn[6]=7;elemConn[7]=6;
      elemConn[8]=3;elemConn[9]=4;elemConn[10]=8;elemConn[11]=7;
      elemConn[12]=5;elemConn[13]=6;elemConn[14]=10;elemConn[15]=9;
      elemConn[16]=6;elemConn[17]=7;elemConn[18]=11;elemConn[19]=10;
      elemConn[20]=7;elemConn[21]=8;elemConn[22]=12;elemConn[23]=11;
      elemConn[24]=9;elemConn[25]=10;elemConn[26]=14;elemConn[27]=13;
      elemConn[28]=10;elemConn[29]=11;elemConn[30]=15;elemConn[31]=14;
      elemConn[32]=11;elemConn[33]=12;elemConn[34]=16;elemConn[35]=15;


      elemCoord[0]=1.25;elemCoord[1]=1.25;
      elemCoord[2]=1.75;elemCoord[3]=1.25;
      elemCoord[4]=2.5;elemCoord[5]=1.25;
      elemCoord[6]=1.25;elemCoord[7]=1.75;
      elemCoord[8]=1.75;elemCoord[9]=1.75;
      elemCoord[10]=2.5;elemCoord[11]=1.75;
      elemCoord[12]=1.25;elemCoord[13]=2.5;
      elemCoord[14]=1.75;elemCoord[15]=2.5;
      elemCoord[16]=2.5;elemCoord[17]=2.5;
    
      numelemconn = 4*num_elem;
    
  } else if (petCount == 2) {
    if (localPet == 0){
      num_node = 9;
      num_elem = 4;

      nodeId[0]=1;
      nodeId[1]=2;
      nodeId[2]=3;
      nodeId[3]=5;
      nodeId[4]=6;
      nodeId[5]=7;
      nodeId[6]=9;
      nodeId[7]=10;
      nodeId[8]=11;

      nodeCoord[0]=1.0;nodeCoord[1]=1.0;
      nodeCoord[2]=1.5;nodeCoord[3]=1.0;
      nodeCoord[4]=2.0;nodeCoord[5]=1.0;
      nodeCoord[6]=1.0;nodeCoord[7]=1.5;
      nodeCoord[8]=1.5;nodeCoord[9]=1.5;
      nodeCoord[10]=2.0;nodeCoord[11]=1.5;
      nodeCoord[12]=1.0;nodeCoord[13]=2.0;
      nodeCoord[14]=1.5;nodeCoord[15]=2.0;
      nodeCoord[16]=2.0;nodeCoord[17]=2.0;

      nodeOwner[0]=0;
      nodeOwner[1]=0;
      nodeOwner[2]=0;
      nodeOwner[3]=0;
      nodeOwner[4]=0;
      nodeOwner[5]=0;
      nodeOwner[6]=0;
      nodeOwner[7]=0;
      nodeOwner[8]=0;

      elemId[0]=1;
      elemId[1]=2;
      elemId[2]=5;
      elemId[3]=6;

      elemType[0]=ESMC_MESHELEMTYPE_QUAD;
      elemType[1]=ESMC_MESHELEMTYPE_QUAD;
      elemType[2]=ESMC_MESHELEMTYPE_QUAD;
      elemType[3]=ESMC_MESHELEMTYPE_QUAD;

      elemConn[0]=1;elemConn[1]=2;elemConn[2]=5;elemConn[3]=4;
      elemConn[4]=2;elemConn[5]=3;elemConn[6]=6;elemConn[7]=5;
      elemConn[8]=4;elemConn[9]=5;elemConn[10]=8;elemConn[11]=7;
      elemConn[12]=5;elemConn[13]=6;elemConn[14]=9;elemConn[15]=8;

      elemCoord[0]=1.25;elemCoord[1]=1.25;
      elemCoord[2]=1.75;elemCoord[3]=1.25;
      elemCoord[4]=1.25;elemCoord[5]=1.75;
      elemCoord[6]=1.75;elemCoord[7]=1.75;

      numelemconn = 4*num_elem;
    }
    else if (localPet == 1) {
      num_node = 12;
      num_elem = 5;

      nodeId[0]=3;
      nodeId[1]=4;
      nodeId[2]=7;
      nodeId[3]=8;
      nodeId[4]=9;
      nodeId[5]=10;
      nodeId[6]=11;
      nodeId[7]=12;
      nodeId[8]=13;
      nodeId[9]=14;
      nodeId[10]=15;
      nodeId[11]=16;

      nodeCoord[0]=2.0;nodeCoord[1]=1.0;
      nodeCoord[2]=3.0;nodeCoord[3]=1.0;
      nodeCoord[4]=2.0;nodeCoord[5]=1.5;
      nodeCoord[6]=3.0;nodeCoord[7]=1.5;
      nodeCoord[8]=1.0;nodeCoord[9]=2.0;
      nodeCoord[10]=1.5;nodeCoord[11]=2.0;
      nodeCoord[12]=2.0;nodeCoord[13]=2.0;
      nodeCoord[14]=3.0;nodeCoord[15]=2.0;
      nodeCoord[16]=1.0;nodeCoord[17]=3.0;
      nodeCoord[18]=1.5;nodeCoord[19]=3.0;
      nodeCoord[20]=2.0;nodeCoord[21]=3.0;
      nodeCoord[22]=3.0;nodeCoord[23]=3.0;

      nodeOwner[0]=0;
      nodeOwner[1]=1;
      nodeOwner[2]=0;
      nodeOwner[3]=1;
      nodeOwner[4]=0;
      nodeOwner[5]=0;
      nodeOwner[6]=0;
      nodeOwner[7]=1;
      nodeOwner[8]=1;
      nodeOwner[9]=1;
      nodeOwner[10]=1;
      nodeOwner[11]=1;


      elemId[0]=4;
      elemId[1]=7;
      elemId[2]=8;
      elemId[3]=9;
      elemId[4]=10;

      elemType[0]=ESMC_MESHELEMTYPE_QUAD;
      elemType[1]=ESMC_MESHELEMTYPE_QUAD;
      elemType[2]=ESMC_MESHELEMTYPE_QUAD;
      elemType[3]=ESMC_MESHELEMTYPE_QUAD;
      elemType[4]=ESMC_MESHELEMTYPE_QUAD;

      elemConn[0]=1;elemConn[1]=2;elemConn[2]=4;elemConn[3]=3;
      elemConn[4]=3;elemConn[5]=4;elemConn[6]=8;elemConn[7]=7;
      elemConn[8]=5;elemConn[9]=6;elemConn[10]=10;elemConn[11]=9;
      elemConn[12]=6;elemConn[13]=7;elemConn[14]=11;elemConn[15]=10;
      elemConn[16]=7;elemConn[17]=8;elemConn[18]=12;elemConn[19]=11;

      elemCoord[0]=2.5;elemCoord[1]=1.25;
      elemCoord[2]=2.5;elemCoord[3]=1.75;
      elemCoord[4]=1.25;elemCoord[5]=2.5;
      elemCoord[6]=1.75;elemCoord[7]=2.5;
      elemCoord[8]=2.5;elemCoord[9]=2.5;

      numelemconn = 4*num_elem;
    }
  }

  double pid = 3.14159/16.0;
  double r2d = 180.0/3.14159;
  
  if (coordsys == ESMC_COORDSYS_SPH_RAD) {
    for (int i = 0; i < num_node*pdim; ++i) {
      nodeCoord[i] *= pid;
    }
    for (int i = 0; i < num_elem*pdim; ++i) {
      elemCoord[i] *= pid;
    }
  } else if (coordsys == ESMC_COORDSYS_SPH_DEG) {
    for (int i = 0; i < num_node*pdim; ++i) {
      nodeCoord[i] *= pid*r2d;
    }
    for (int i = 0; i < num_elem*pdim; ++i) {
      elemCoord[i] *= pid*r2d;
    }
  }
    
  MBMesh_addnodes(&meshp, &num_node, nodeId, nodeCoord, nodeOwner, NULL,
                  &coordsys, &sdim, &rc);

  MBMesh_addelements(&meshp, &num_elem, elemId, elemType, NULL,
                     &areapresent, NULL,
                     &coordspresent, elemCoord,
                     &numelemconn, elemConn,
                     &regridconserve,
                     &coordsys, &sdim, &rc);

  free(nodeId);
  free(nodeCoord);
  free(nodeOwner);
  free(elemId);
  free(elemType);
  free(elemConn);
  free(elemCoord);

  rc = ESMF_SUCCESS;
  return static_cast<MBMesh *>(meshp);
}

MBMesh* create_mesh_quad_9_4(ESMC_CoordSys_Flag coordsys, int &rc) {

  //
  //   3.0   13 ------ 14 ------ 15    [15] ----------- 16
  //         |         |         |       |              |
  //         |         |         |       |              |
  //         |    8    |    9    |       |      10      |
  //         |         |         |       |              |
  //         |         |         |       |              |
  //   2.0  [9] ----- [10] ---- [11]   [11] ---------- [12]
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
  //         |         |         |       |               |
  //         |    1    |    2    |       |       4       |
  //         |         |         |       |               |
  //   1.0   1 ------- 2 ------- 3      [3] ------------ 4
  //
  //         1.0       1.5     2.0      2.0             3.0
  //
  //                PET 0                      PET 1
  //
  //               Node Id labels at corners
  //              Element Id labels in centers


  // Get parallel information
  int localPet, petCount;
  ESMC_VM vm;

  vm=ESMC_VMGetGlobal(&rc);
  if (rc != ESMF_SUCCESS) return 0;

  rc=ESMC_VMGet(vm, &localPet, &petCount, (int *)NULL, (MPI_Comm *)NULL,
                (int *)NULL, (int *)NULL);
  if (rc != ESMF_SUCCESS) return 0;

  if (!(petCount == 1 || petCount == 4)) {
    Throw() << "Test function must be run with either 1 or 4 processors";
    return NULL;
  }
  
  // Mesh variables
  int num_elem = 9;
  int num_node = 16;

  // set Mesh parameters
  int *nodeId;
  double *nodeCoord;
  int *nodeOwner;

  int *elemId;
  int *elemType;
  int *elemConn;
  double *elemCoord;

  int areapresent = 0;
  int coordspresent = 1;
  int numelemconn = 0;
  int regridconserve = 0;

  nodeId    = (int *) malloc (num_node * sizeof (int));
  nodeCoord = (double *) malloc (2*num_node * sizeof (double));
  nodeOwner = (int *) malloc (num_node * sizeof (int));
  elemId   = (int *) malloc (num_elem * sizeof (int));
  elemType = (int *) malloc (num_elem * sizeof (int));
  elemConn = (int *) malloc (4*num_elem * sizeof (int));
  elemCoord = (double *) malloc (2*num_elem * sizeof (double));

  int pdim=2;
  int sdim=2;
  // the MBMesh_create interface resets sdim to 3 in this case
  if (coordsys != ESMC_COORDSYS_CART) sdim = 2;

  MBMesh *mesh = new MBMesh();
  void *meshp = static_cast<void *> (mesh);
  MBMesh_create(&meshp, &pdim, &sdim, &coordsys, &rc);

  if (petCount == 1) {
      num_node = 16;
      num_elem = 9;

      nodeId[0]=1;
      nodeId[1]=2;
      nodeId[2]=3;
      nodeId[3]=4;
      nodeId[4]=5;
      nodeId[5]=6;
      nodeId[6]=7;
      nodeId[7]=8;
      nodeId[8]=9;
      nodeId[9]=10;
      nodeId[10]=11;
      nodeId[11]=12;
      nodeId[12]=13;
      nodeId[13]=14;
      nodeId[14]=15;
      nodeId[15]=16;

      nodeCoord[0]=1.0;nodeCoord[1]=1.0;
      nodeCoord[2]=1.5;nodeCoord[3]=1.0;
      nodeCoord[4]=2.0;nodeCoord[5]=1.0;
      nodeCoord[6]=3.0;nodeCoord[7]=1.0;
      nodeCoord[8]=1.0;nodeCoord[9]=1.5;
      nodeCoord[10]=1.5;nodeCoord[11]=1.5;
      nodeCoord[12]=2.0;nodeCoord[13]=1.5;
      nodeCoord[14]=3.0;nodeCoord[15]=1.5;
      nodeCoord[16]=1.0;nodeCoord[17]=2.0;
      nodeCoord[18]=1.5;nodeCoord[19]=2.0;
      nodeCoord[20]=2.0;nodeCoord[21]=2.0;
      nodeCoord[22]=3.0;nodeCoord[23]=2.0;
      nodeCoord[24]=1.0;nodeCoord[25]=3.0;
      nodeCoord[26]=1.5;nodeCoord[27]=3.0;
      nodeCoord[28]=2.0;nodeCoord[29]=3.0;
      nodeCoord[30]=3.0;nodeCoord[31]=3.0;

      nodeOwner[0]=0;
      nodeOwner[1]=0;
      nodeOwner[2]=0;
      nodeOwner[3]=0;
      nodeOwner[4]=0;
      nodeOwner[5]=0;
      nodeOwner[6]=0;
      nodeOwner[7]=0;
      nodeOwner[8]=0;
      nodeOwner[9]=0;
      nodeOwner[10]=0;
      nodeOwner[11]=0;
      nodeOwner[12]=0;
      nodeOwner[13]=0;
      nodeOwner[14]=0;
      nodeOwner[15]=0;

      elemId[0]=1;
      elemId[1]=2;
      elemId[2]=4;
      elemId[3]=5;
      elemId[4]=6;
      elemId[5]=7;
      elemId[6]=8;
      elemId[7]=9;
      elemId[8]=10;

      elemType[0]=ESMC_MESHELEMTYPE_QUAD;
      elemType[1]=ESMC_MESHELEMTYPE_QUAD;
      elemType[2]=ESMC_MESHELEMTYPE_QUAD;
      elemType[3]=ESMC_MESHELEMTYPE_QUAD;
      elemType[4]=ESMC_MESHELEMTYPE_QUAD;
      elemType[5]=ESMC_MESHELEMTYPE_QUAD;
      elemType[6]=ESMC_MESHELEMTYPE_QUAD;
      elemType[7]=ESMC_MESHELEMTYPE_QUAD;
      elemType[8]=ESMC_MESHELEMTYPE_QUAD;

      elemConn[0]=1;elemConn[1]=2;elemConn[2]=6;elemConn[3]=5;
      elemConn[4]=2;elemConn[5]=3;elemConn[6]=7;elemConn[7]=6;
      elemConn[8]=3;elemConn[9]=4;elemConn[10]=8;elemConn[11]=7;
      elemConn[12]=5;elemConn[13]=6;elemConn[14]=10;elemConn[15]=9;
      elemConn[16]=6;elemConn[17]=7;elemConn[18]=11;elemConn[19]=10;
      elemConn[20]=7;elemConn[21]=8;elemConn[22]=12;elemConn[23]=11;
      elemConn[24]=9;elemConn[25]=10;elemConn[26]=14;elemConn[27]=13;
      elemConn[28]=10;elemConn[29]=11;elemConn[30]=15;elemConn[31]=14;
      elemConn[32]=11;elemConn[33]=12;elemConn[34]=16;elemConn[35]=15;

      elemCoord[0]=1.25;elemCoord[1]=1.25;
      elemCoord[2]=1.75;elemCoord[3]=1.25;
      elemCoord[4]=2.5;elemCoord[5]=1.25;
      elemCoord[6]=1.25;elemCoord[7]=1.75;
      elemCoord[8]=1.75;elemCoord[9]=1.75;
      elemCoord[10]=2.5;elemCoord[11]=1.75;
      elemCoord[12]=1.25;elemCoord[13]=2.5;
      elemCoord[14]=1.75;elemCoord[15]=2.5;
      elemCoord[16]=2.5;elemCoord[17]=2.5;

      numelemconn = 4*num_elem;
    
  } else if (petCount == 4) {
    if (localPet == 0){
      num_node = 9;
      num_elem = 4;

      nodeId[0]=1;
      nodeId[1]=2;
      nodeId[2]=3;
      nodeId[3]=5;
      nodeId[4]=6;
      nodeId[5]=7;
      nodeId[6]=9;
      nodeId[7]=10;
      nodeId[8]=11;

      nodeCoord[0]=1.0;nodeCoord[1]=1.0;
      nodeCoord[2]=1.5;nodeCoord[3]=1.0;
      nodeCoord[4]=2.0;nodeCoord[5]=1.0;
      nodeCoord[6]=1.0;nodeCoord[7]=1.5;
      nodeCoord[8]=1.5;nodeCoord[9]=1.5;
      nodeCoord[10]=2.0;nodeCoord[11]=1.5;
      nodeCoord[12]=1.0;nodeCoord[13]=2.0;
      nodeCoord[14]=1.5;nodeCoord[15]=2.0;
      nodeCoord[16]=2.0;nodeCoord[17]=2.0;

      nodeOwner[0]=0;
      nodeOwner[1]=0;
      nodeOwner[2]=0;
      nodeOwner[3]=0;
      nodeOwner[4]=0;
      nodeOwner[5]=0;
      nodeOwner[6]=0;
      nodeOwner[7]=0;
      nodeOwner[8]=0;

      elemId[0]=1;
      elemId[1]=2;
      elemId[2]=5;
      elemId[3]=6;

      elemType[0]=ESMC_MESHELEMTYPE_QUAD;
      elemType[1]=ESMC_MESHELEMTYPE_QUAD;
      elemType[2]=ESMC_MESHELEMTYPE_QUAD;
      elemType[3]=ESMC_MESHELEMTYPE_QUAD;

      elemConn[0]=1;elemConn[1]=2;elemConn[2]=5;elemConn[3]=4;
      elemConn[4]=2;elemConn[5]=3;elemConn[6]=6;elemConn[7]=5;
      elemConn[8]=4;elemConn[9]=5;elemConn[10]=8;elemConn[11]=7;
      elemConn[12]=5;elemConn[13]=6;elemConn[14]=9;elemConn[15]=8;

      elemCoord[0]=1.25;elemCoord[1]=1.25;
      elemCoord[2]=1.75;elemCoord[3]=1.25;
      elemCoord[4]=1.25;elemCoord[5]=1.75;
      elemCoord[6]=1.75;elemCoord[7]=1.75;

      numelemconn = 4*num_elem;
    }
    else if (localPet == 1) {
      num_node = 6;
      num_elem = 2;

      nodeId[0]=3;
      nodeId[1]=4;
      nodeId[2]=7;
      nodeId[3]=8;
      nodeId[4]=11;
      nodeId[5]=12;

      nodeCoord[0]=2.0;nodeCoord[1]=1.0;
      nodeCoord[2]=3.0;nodeCoord[3]=1.0;
      nodeCoord[4]=2.0;nodeCoord[5]=1.5;
      nodeCoord[6]=3.0;nodeCoord[7]=1.5;
      nodeCoord[8]=2.0;nodeCoord[9]=2.0;
      nodeCoord[10]=3.0;nodeCoord[11]=2.0;

      nodeOwner[0]=0;
      nodeOwner[1]=1;
      nodeOwner[2]=0;
      nodeOwner[3]=1;
      nodeOwner[4]=0;
      nodeOwner[5]=1;

      elemId[0]=4;
      elemId[1]=7;

      elemType[0]=ESMC_MESHELEMTYPE_QUAD;
      elemType[1]=ESMC_MESHELEMTYPE_QUAD;

      elemConn[0]=1;elemConn[1]=2;elemConn[2]=4;elemConn[3]=3;
      elemConn[4]=3;elemConn[5]=4;elemConn[6]=6;elemConn[7]=5;

      elemCoord[0]=2.5;elemCoord[1]=1.25;
      elemCoord[2]=2.5;elemCoord[3]=1.75;

      numelemconn = 4*num_elem;
    }
    else if (localPet == 2) {
      num_node = 6;
      num_elem = 2;

      nodeId[0]=9;
      nodeId[1]=10;
      nodeId[2]=11;
      nodeId[3]=13;
      nodeId[4]=14;
      nodeId[5]=15;

      nodeCoord[0]=1.0;nodeCoord[1]=2.0;
      nodeCoord[2]=1.5;nodeCoord[3]=2.0;
      nodeCoord[4]=2.0;nodeCoord[5]=2.0;
      nodeCoord[6]=1.0;nodeCoord[7]=3.0;
      nodeCoord[8]=1.5;nodeCoord[9]=3.0;
      nodeCoord[10]=2.0;nodeCoord[11]=3.0;

      nodeOwner[0]=0;
      nodeOwner[1]=0;
      nodeOwner[2]=0;
      nodeOwner[3]=1;
      nodeOwner[4]=1;
      nodeOwner[5]=1;

      elemId[0]=8;
      elemId[1]=9;

      elemType[0]=ESMC_MESHELEMTYPE_QUAD;
      elemType[1]=ESMC_MESHELEMTYPE_QUAD;

      elemConn[0]=1;elemConn[1]=2;elemConn[2]=5;elemConn[3]=4;
      elemConn[4]=2;elemConn[5]=3;elemConn[6]=6;elemConn[7]=5;

      elemCoord[0]=1.25;elemCoord[1]=2.5;
      elemCoord[2]=1.75;elemCoord[3]=2.5;

      numelemconn = 4*num_elem;
    }
    else if (localPet == 3) {
      num_node = 4;
      num_elem = 1;

      nodeId[0]=11;
      nodeId[1]=12;
      nodeId[2]=15;
      nodeId[3]=16;

      nodeCoord[0]=2.0;nodeCoord[1]=2.0;
      nodeCoord[2]=3.0;nodeCoord[3]=2.0;
      nodeCoord[4]=2.0;nodeCoord[5]=3.0;
      nodeCoord[6]=3.0;nodeCoord[7]=3.0;

      nodeOwner[0]=0;
      nodeOwner[1]=1;
      nodeOwner[2]=2;
      nodeOwner[3]=3;

      elemId[0]=10;

      elemType[0]=ESMC_MESHELEMTYPE_QUAD;

      elemConn[0]=1;elemConn[1]=2;elemConn[2]=4;elemConn[3]=3;

      elemCoord[0]=2.5;elemCoord[1]=2.5;

      numelemconn = 4*num_elem;
    }
  }

  double pid = 3.14159/16.0;
  double r2d = 180.0/3.14159;
  
  if (coordsys == ESMC_COORDSYS_SPH_RAD) {
    for (int i = 0; i < num_node*pdim; ++i) {
      nodeCoord[i] *= pid;
    }
    for (int i = 0; i < num_elem*pdim; ++i) {
      elemCoord[i] *= pid;
    }
  } else if (coordsys == ESMC_COORDSYS_SPH_DEG) {
    for (int i = 0; i < num_node*pdim; ++i)
      nodeCoord[i] *= pid*r2d;
    for (int i = 0; i < num_elem*pdim; ++i)
      elemCoord[i] *= pid*r2d;
  }

  MBMesh_addnodes(&meshp, &num_node, nodeId, nodeCoord, nodeOwner, NULL,
                  &coordsys, &sdim, &rc);

  MBMesh_addelements(&meshp, &num_elem, elemId, elemType, NULL,
                     &areapresent, NULL,
                     &coordspresent, elemCoord,
                     &numelemconn, elemConn,
                     &regridconserve,
                     &coordsys, &sdim, &rc);

  free(nodeId);
  free(nodeCoord);
  free(nodeOwner);
  free(elemId);
  free(elemType);
  free(elemConn);
  free(elemCoord);

  rc = ESMF_SUCCESS;
  return static_cast<MBMesh *>(meshp);
}

MBMesh* create_mesh_ph_4(ESMC_CoordSys_Flag coordsys, int &rc) {

//
//  2.5        8        10 --------11
//          /     \   /            |
//  2.1   7         9              12
//        |         |      5       /
//        |    4    |            /
//        |         |          /
//  1.0   4 ------- 5 ------- 6
//        |         |  \   3  |
//        |    1    |    \    |
//        |         |  2   \  |
// -0.1   1 ------- 2 ------- 3
//
//      -0.1       1.0       2.1   2.5 
// 
//        Node Id labels at corners
//       Element Id labels in centers

  // Get parallel information
  int localPet, petCount;
  ESMC_VM vm;

  vm=ESMC_VMGetGlobal(&rc);
  if (rc != ESMF_SUCCESS) return 0;

  rc=ESMC_VMGet(vm, &localPet, &petCount, (int *)NULL, (MPI_Comm *)NULL,
                (int *)NULL, (int *)NULL);
  if (rc != ESMF_SUCCESS) return 0;

  if (!(petCount == 1 || petCount == 4)) {
    Throw() << "Test function must be run with either 1 or 4 processors";
    return NULL;
  }
  
  // Mesh variables
  int num_elem = 5;
  int num_node = 12;

  // set Mesh parameters
  int *nodeId;
  double *nodeCoord;
  int *nodeOwner;

  int *elemId;
  int *elemType;
  int *elemConn;
  double *elemCoord;

  int areapresent = 0;
  int coordspresent = 1;
  int numelemconn = 0;
  int regridconserve = 0;

  nodeId    = (int *) malloc (num_node * sizeof (int));
  nodeCoord = (double *) malloc (2*num_node * sizeof (double));
  nodeOwner = (int *) malloc (num_node * sizeof (int));
  elemId   = (int *) malloc (num_elem * sizeof (int));
  elemType = (int *) malloc (num_elem * sizeof (int));
  elemConn = (int *) malloc (4*num_elem * sizeof (int));
  elemCoord = (double *) malloc (2*num_elem * sizeof (double));

  int pdim=2;
  int sdim=2;
  // the MBMesh_create interface resets sdim to 3 in this case
  if (coordsys != ESMC_COORDSYS_CART) sdim = 2;

  MBMesh *mesh = new MBMesh();
  void *meshp = static_cast<void *> (mesh);
  MBMesh_create(&meshp, &pdim, &sdim, &coordsys, &rc);

  if (petCount == 1) {
    
      nodeId[0]=1;
      nodeId[1]=2;
      nodeId[2]=3;
      nodeId[3]=4;
      nodeId[4]=5;
      nodeId[5]=6;
      nodeId[6]=7;
      nodeId[7]=8;
      nodeId[8]=9;
      nodeId[9]=10;
      nodeId[10]=11;
      nodeId[11]=12;
      
      nodeCoord[0]=-0.1;nodeCoord[1]=-0.1;
      nodeCoord[2]=1.0;nodeCoord[3]=-0.1;
      nodeCoord[4]=2.1;nodeCoord[5]=-0.1;
      nodeCoord[6]=-0.1;nodeCoord[7]=1.0;
      nodeCoord[8]=1.0;nodeCoord[9]=1.0;
      nodeCoord[10]=2.1;nodeCoord[11]=1.0;
      nodeCoord[12]=-0.1;nodeCoord[13]=2.1;
      nodeCoord[14]=0.5;nodeCoord[15]=2.5;
      nodeCoord[16]=1.0;nodeCoord[17]=2.1;
      nodeCoord[18]=1.5;nodeCoord[19]=2.5;
      nodeCoord[20]=2.5;nodeCoord[21]=2.5;
      nodeCoord[22]=2.5;nodeCoord[23]=2.1;
      
      nodeOwner[0]=0;
      nodeOwner[1]=0;
      nodeOwner[2]=0;
      nodeOwner[3]=0;
      nodeOwner[4]=0;
      nodeOwner[5]=0;
      nodeOwner[6]=0;
      nodeOwner[7]=0;
      nodeOwner[8]=0;
      nodeOwner[9]=0;
      nodeOwner[10]=0;
      nodeOwner[11]=0;

      elemId[0]=1;
      elemId[1]=2;
      elemId[2]=3;
      elemId[3]=4;
      elemId[4]=5;

      elemType[0]=ESMC_MESHELEMTYPE_QUAD;
      elemType[1]=ESMC_MESHELEMTYPE_TRI;
      elemType[2]=ESMC_MESHELEMTYPE_TRI;
      elemType[3]=5;
      elemType[4]=6;

      elemConn[0]=1;elemConn[1]=2;elemConn[2]=5;elemConn[3]=4;
      elemConn[4]=2;elemConn[5]=3;elemConn[6]=5;
      elemConn[7]=3;elemConn[8]=6;elemConn[9]=5;
      elemConn[10]=4;elemConn[11]=5;elemConn[12]=9;elemConn[13]=8;elemConn[14]=7;
      elemConn[15]=5;elemConn[16]=6;elemConn[17]=12;
      elemConn[18]=11;elemConn[19]=10;elemConn[20]=9;

      elemCoord[0]=0.45;elemCoord[1]=0.45;
      elemCoord[2]=1.37;elemCoord[3]=0.27;
      elemCoord[4]=1.73;elemCoord[5]=0.63;
      elemCoord[6]=0.46;elemCoord[7]=1.74;
      elemCoord[8]=1.76;elemCoord[9]=1.87;

      numelemconn = 4+3*2+5+6;

  } else if (petCount == 4) {
    if (localPet == 0){
      num_node = 4;
      num_elem = 1;

      nodeId[0]=1;
      nodeId[1]=2;
      nodeId[2]=4;
      nodeId[3]=5;

      nodeCoord[0]=-0.1;nodeCoord[1]=-0.1;
      nodeCoord[2]=1.0;nodeCoord[3]=-0.1;
      nodeCoord[4]=-0.1;nodeCoord[5]=1.0;
      nodeCoord[6]=1.0;nodeCoord[7]=1.0;

      nodeOwner[0]=0;
      nodeOwner[1]=0;
      nodeOwner[2]=0;
      nodeOwner[3]=0;

      elemId[0]=1;

      elemType[0]=ESMC_MESHELEMTYPE_QUAD;

      elemConn[0]=1;elemConn[1]=2;elemConn[2]=4;elemConn[3]=3;

      elemCoord[0]=0.45;elemCoord[1]=0.45;

      numelemconn = 4*num_elem;
    } else if (localPet == 1) {
      num_node = 4;
      num_elem = 2;

      nodeId[0]=2;
      nodeId[1]=3;
      nodeId[2]=5;
      nodeId[3]=6;

      nodeCoord[0]=1.0;nodeCoord[1]=-0.1;
      nodeCoord[2]=2.1;nodeCoord[3]=-0.1;
      nodeCoord[4]=1.0;nodeCoord[5]=1.0;
      nodeCoord[6]=2.1;nodeCoord[7]=1.0;

      nodeOwner[0]=0;
      nodeOwner[1]=1;
      nodeOwner[2]=0;
      nodeOwner[3]=1;

      elemId[0]=2;
      elemId[1]=3;

      elemType[0]=ESMC_MESHELEMTYPE_TRI;
      elemType[1]=ESMC_MESHELEMTYPE_TRI;

      elemConn[0]=1;elemConn[1]=2;elemConn[2]=3;
      elemConn[3]=2;elemConn[4]=4;elemConn[5]=3;

      elemCoord[0]=1.37;elemCoord[1]=0.27;
      elemCoord[2]=1.73;elemCoord[3]=0.63;

      numelemconn = 3*num_elem;
    } else if (localPet == 2) {
      num_node = 5;
      num_elem = 1;

      nodeId[0]=4;
      nodeId[1]=5;
      nodeId[2]=7;
      nodeId[3]=8;
      nodeId[4]=9;

      nodeCoord[0]=-0.1;nodeCoord[1]=1.0;
      nodeCoord[2]=1.0;nodeCoord[3]=1.0;
      nodeCoord[4]=-0.1;nodeCoord[5]=2.1;
      nodeCoord[6]=0.5;nodeCoord[7]=2.5;
      nodeCoord[8]=1.0;nodeCoord[9]=2.1;

      nodeOwner[0]=0;
      nodeOwner[1]=0;
      nodeOwner[2]=2;
      nodeOwner[3]=2;
      nodeOwner[4]=2;

      elemId[0]=4;

      elemType[0]=5;

      elemConn[0]=1;elemConn[1]=2;elemConn[2]=5;elemConn[3]=4;elemConn[4]=3;

      elemCoord[0]=0.46;elemCoord[1]=1.74;

      numelemconn = 5*num_elem;
    } else if (localPet == 3) {
      num_node = 6;
      num_elem = 1;

      nodeId[0]=5;
      nodeId[1]=6;
      nodeId[2]=9;
      nodeId[3]=10;
      nodeId[4]=11;
      nodeId[5]=12;

      nodeCoord[0]=1.0;nodeCoord[1]=1.0;
      nodeCoord[2]=2.1;nodeCoord[3]=1.0;
      nodeCoord[4]=1.0;nodeCoord[5]=2.1;
      nodeCoord[6]=1.5;nodeCoord[7]=2.5;
      nodeCoord[8]=2.5;nodeCoord[9]=2.5;
      nodeCoord[10]=2.5;nodeCoord[11]=2.1;

      nodeOwner[0]=0;
      nodeOwner[1]=1;
      nodeOwner[2]=2;
      nodeOwner[3]=3;
      nodeOwner[4]=3;
      nodeOwner[5]=3;

      elemId[0]=5;

      elemType[0]=6;

      elemConn[0]=1;elemConn[1]=2;elemConn[2]=6;
      elemConn[3]=5;elemConn[4]=4;elemConn[5]=3;

      elemCoord[0]=1.76;elemCoord[1]=1.87;

      numelemconn = 6*num_elem;
    }
  }

  double pid = 3.14159/16.0;
  double r2d = 180.0/3.14159;
  
  if (coordsys == ESMC_COORDSYS_SPH_RAD) {
    for (int i = 0; i < num_node*pdim; ++i) {
      nodeCoord[i] *= pid;
    }
    for (int i = 0; i < num_elem*pdim; ++i) {
      elemCoord[i] *= pid;
    }
  } else if (coordsys == ESMC_COORDSYS_SPH_DEG) {
    for (int i = 0; i < num_node*pdim; ++i)
      nodeCoord[i] *= pid*r2d;
    for (int i = 0; i < num_elem*pdim; ++i)
      elemCoord[i] *= pid*r2d;
  }

  MBMesh_addnodes(&meshp, &num_node, nodeId, nodeCoord, nodeOwner, NULL,
                  &coordsys, &sdim, &rc);

  MBMesh_addelements(&meshp, &num_elem, elemId, elemType, NULL,
                     &areapresent, NULL,
                     &coordspresent, elemCoord,
                     &numelemconn, elemConn,
                     &regridconserve,
                     &coordsys, &sdim, &rc);

  free(nodeId);
  free(nodeCoord);
  free(nodeOwner);
  free(elemId);
  free(elemType);
  free(elemConn);
  free(elemCoord);

  rc = ESMF_SUCCESS;
  return static_cast<MBMesh *>(meshp);
}



int main(void){

  char name[80];
  char failMsg[80];
  int result = 0;
  int rc;

  int localPet, petCount;
  ESMC_VM vm;

  //----------------------------------------------------------------------------
  ESMC_TestStart(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------


  // Get parallel information
  vm=ESMC_VMGetGlobal(&rc);
  if (rc != ESMF_SUCCESS) return 0;

  MPI_Comm mpi_comm;
  rc=ESMC_VMGet(vm, &localPet, &petCount, (int *)NULL, &mpi_comm, (int *)NULL, (int *)NULL);
  if (rc != ESMF_SUCCESS) return 0;

  rc=ESMC_LogSet(true);

  //----------------------------------------------------------------------------
  //NEX_disable_UTest
  strcpy(name, "Moab Ghost element test");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#if defined ESMF_MOAB
  MBMesh *mbmesh = NULL;
  // mbmesh = create_mesh_quad_9_2(ESMC_COORDSYS_SPH_DEG, rc);
  // mbmesh = create_mesh_quad_9_4(ESMC_COORDSYS_SPH_DEG, rc);
  mbmesh = create_mesh_ph_4(ESMC_COORDSYS_SPH_DEG, rc);
  // mbmesh = create_mesh_quad_sph(rc);
  if (!mbmesh) rc = ESMC_RC_PTR_NULL;

  // void *mbptr = (void *) mbmesh;
  // int len = 12; char fname[len];
  // sprintf(fname, "mesh_%d", localPet);
  // MBMesh_write(&mbptr, fname, &rc, len);

// #define DEBUG_EXCHANGE_TAGS
#ifdef DEBUG_EXCHANGE_TAGS
#undef ESMC_METHOD
#define ESMC_METHOD "ESMC_MeshMOABGhostUTest"

  int merr;
  
  // get the indexed pcomm object from the interface
  // pass index 0, it will the one created inside MBMesh_addelements
  ParallelComm *pcomm = ParallelComm::get_pcomm(mbmesh->mesh, 0);
  
  // this is called in MBMesh_addelements
  // merr = pcomm->resolve_shared_ents(0, elems, mbmesh->pdim, mbmesh->pdim-1);
  // MBMESH_CHECK_ERR(merr, rc);
    
  {
  Range shared_ents;
  // Get entities shared with all other processors
  merr = pcomm->get_shared_entities(-1, shared_ents);
  MBMESH_CHECK_ERR(merr, rc);
  
  // Filter shared entities with not not_owned, which means owned
  Range owned_entities;
  merr = pcomm->filter_pstatus(shared_ents, PSTATUS_NOT_OWNED, PSTATUS_NOT, -1, &owned_entities);
  MBMESH_CHECK_ERR(merr, rc);
    
  unsigned int nums[4] = {0}; // to store the owned entities per dimension
  for (int i = 0; i < 4; i++)
    // nums[i] = (nt)shared_ents.num_of_dimension(i);
    nums[i] = (int)owned_entities.num_of_dimension(i);
    
  std::vector<int> rbuf(petCount*4, 0);
  MPI_Gather(nums, 4, MPI_INT, &rbuf[0], 4, MPI_INT, 0, mpi_comm);
  // Print the stats gathered:
  if (0 == localPet) {
    for (int i = 0; i < petCount; i++)
      std::cout << " Shared, owned entities on proc " << i << ": " << rbuf[4*i] << " verts, " <<
          rbuf[4*i + 1] << " edges, " << rbuf[4*i + 2] << " faces, " << rbuf[4*i + 3] << " elements" << endl;
  }
  }
  
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  merr = pcomm->exchange_ghost_cells(mbmesh->pdim, // int ghost_dim
                                     0, // int bridge_dim
                                     1, // int num_layers
                                     0, // int addl_ents
                                     true);// bool store_remote_handles
  MBMESH_CHECK_ERR(merr, rc);

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  {
  Range shared_ents;
  // Get entities shared with all other processors
  merr = pcomm->get_shared_entities(-1, shared_ents);
  MBMESH_CHECK_ERR(merr, rc);
  
  // Filter shared entities with not not_owned, which means owned
  Range owned_entities;
  merr = pcomm->filter_pstatus(shared_ents, PSTATUS_NOT_OWNED, PSTATUS_NOT, -1, &owned_entities);
  MBMESH_CHECK_ERR(merr, rc);
    
  unsigned int nums[4] = {0}; // to store the owned entities per dimension
  for (int i = 0; i < 4; i++)
    // nums[i] = (nt)shared_ents.num_of_dimension(i);
    nums[i] = (int)owned_entities.num_of_dimension(i);
    
  std::vector<int> rbuf(petCount*4, 0);
  MPI_Gather(nums, 4, MPI_INT, &rbuf[0], 4, MPI_INT, 0, mpi_comm);
  // Print the stats gathered:
  if (0 == localPet) {
    for (int i = 0; i < petCount; i++)
      std::cout << " Shared, owned entities on proc " << i << ": " << rbuf[4*i] << " verts, " <<
          rbuf[4*i + 1] << " edges, " << rbuf[4*i + 2] << " faces, " << rbuf[4*i + 3] << " elements" << endl;
  }
  }

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  std::vector<Tag> node_tags;
  std::vector<Tag> elem_tags;
  
  node_tags.push_back(mbmesh->gid_tag);
  node_tags.push_back(mbmesh->orig_pos_tag);
  node_tags.push_back(mbmesh->owner_tag);
  if (mbmesh->has_node_orig_coords) node_tags.push_back(mbmesh->node_orig_coords_tag);
  if (mbmesh->has_node_mask) {
    node_tags.push_back(mbmesh->node_mask_tag);
    node_tags.push_back(mbmesh->node_mask_val_tag);
  }
  
  elem_tags.push_back(mbmesh->gid_tag);
  elem_tags.push_back(mbmesh->orig_pos_tag);
  elem_tags.push_back(mbmesh->owner_tag);
  if (mbmesh->has_elem_frac) elem_tags.push_back(mbmesh->elem_frac_tag);
  if (mbmesh->has_elem_mask) {
    elem_tags.push_back(mbmesh->elem_mask_tag);
    elem_tags.push_back(mbmesh->elem_mask_val_tag);
  }
  if (mbmesh->has_elem_area) elem_tags.push_back(mbmesh->elem_area_tag);
  if (mbmesh->has_elem_coords) elem_tags.push_back(mbmesh->elem_coords_tag);
  if (mbmesh->has_elem_orig_coords) elem_tags.push_back(mbmesh->elem_orig_coords_tag); 
   
  // pcomm->set_debug_verbosity(4);

  Range nodes;
  merr=mbmesh->mesh->get_entities_by_dimension(0, 0, nodes);
  MBMESH_CHECK_ERR(merr, rc);

  merr = pcomm->exchange_tags(node_tags, node_tags, nodes);
  MBMESH_CHECK_ERR(merr, rc);
  
  Range elems;
  merr=mbmesh->mesh->get_entities_by_dimension(0, mbmesh->pdim, elems);
  MBMESH_CHECK_ERR(merr, rc);

  merr = pcomm->exchange_tags(elem_tags, elem_tags, elems);
  MBMESH_CHECK_ERR(merr, rc);

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  printf("%d# node_tags size %d [", Par::Rank(), node_tags.size());
  for (int i=0; i< node_tags.size(); ++i)
    printf("%d, ", node_tags[i]);
  printf("]\n");

  printf("%d# elem_tags size %d [", Par::Rank(), elem_tags.size());
  for (int i=0; i< elem_tags.size(); ++i)
    printf("%d, ", elem_tags[i]);
  printf("]\n");


  {
  // Get a range containing all nodes
  Range range_node;
  merr=mbmesh->mesh->get_entities_by_dimension(0,0,range_node);
  MBMESH_CHECK_ERR(merr, rc);

  printf("%d# node ids [", Par::Rank());
  for(Range::iterator it=range_node.begin(); it !=range_node.end(); it++) {
    const EntityHandle *node=&(*it);
    
    int nid;
    merr=mbmesh->mesh->tag_get_data(mbmesh->gid_tag, node, 1, &nid);
    MBMESH_CHECK_ERR(merr, rc);

    printf("%d, ", nid);
  }
  printf("]  elem ids [");

  Range range_elem;
  merr=mbmesh->mesh->get_entities_by_dimension(0,mbmesh->pdim,range_elem);
  MBMESH_CHECK_ERR(merr, rc);

  for(Range::iterator it=range_elem.begin(); it !=range_elem.end(); it++) {
    const EntityHandle *elem=&(*it);
    
    // Get element id
    int elem_id;
    merr = mbmesh->mesh->tag_get_data(mbmesh->gid_tag, elem, 1, &elem_id);
    MBMESH_CHECK_ERR(merr, rc);
    
    printf("%d, ", elem_id);
  }
  printf("]\n");
    
  }


  {
  // Get a range containing all nodes
  Range range_node;
  merr=mbmesh->mesh->get_entities_by_dimension(0,0,range_node);
  MBMESH_CHECK_ERR(merr, rc);

  for(Range::iterator it=range_node.begin(); it !=range_node.end(); it++) {
    const EntityHandle *node=&(*it);
    
    // Get number of elems
    int num_node_elems=0;
    // get_num_elems_around_node(&node, &num_node_elems);
    // pdim instead of sdim here, for spherical cases
    Range adjs;
    merr = mbmesh->mesh->get_adjacencies(node, 1, mbmesh->pdim, false, adjs);
    MBMESH_CHECK_ERR(merr, rc);
    num_node_elems = adjs.size();

    int nid;
    merr=mbmesh->mesh->tag_get_data(mbmesh->gid_tag, node, 1, &nid);
    MBMESH_CHECK_ERR(merr, rc);
    
    printf("%d# node id %d, adjacencies %d [", Par::Rank(), nid, num_node_elems);
    for(Range::iterator it=adjs.begin(); it !=adjs.end(); it++) {
      const EntityHandle *elem=&(*it);
      
      // Get element id
      int elem_id;
      merr = mbmesh->mesh->tag_get_data(mbmesh->gid_tag, elem, 1, &elem_id);
      MBMESH_CHECK_ERR(merr, rc);
      
      printf("%d, ", elem_id);
    }
    printf("]\n");
    
  }
  }
  
    std::ostringstream ent_str;
    ent_str << "mesh_ghost." <<pcomm->rank() << ".vtk";
    mbmesh->mesh->write_mesh(ent_str.str().c_str());


#else
  MBMesh *mesh_dual = NULL;
  MBMeshDual(mbmesh, &mesh_dual, &rc);
  if (!mesh_dual) rc = ESMC_RC_PTR_NULL;

  void *mbptr = (void *) mbmesh;
  int len = 12; char fname[len];
  sprintf(fname, "mesh_%d", localPet);
  MBMesh_write(&mbptr, fname, &rc, len);
  
  void *mbptrd = (void *) mesh_dual;
  int lend = 17; char fnamed[lend];
  sprintf(fnamed, "mesh_dual_%d", localPet);
  MBMesh_write(&mbptrd, fnamed, &rc, lend);
#endif

#else
  rc = ESMF_SUCCESS;
#endif
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  ESMC_TestEnd(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  return 0;
}

