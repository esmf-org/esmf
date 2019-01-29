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

// other headers
#include "ESMCI_MeshCXX.h"

#include <iostream>
#include <iterator>
#include <vector>


#if !defined (M_PI)
// for Windows...
#define M_PI 3.14159265358979323846
#endif

using ESMCI::MeshCXX;

MeshCXX* create_nmesh_quad(int &rc) {
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

  MeshCXX *mesh = new MeshCXX();

  mesh->create(pdim, sdim, local_coordSys, &rc);
  if (rc != ESMF_SUCCESS) return NULL;

  rc = mesh->addNodes(num_node, nodeId_s, nodeCoord_s, nodeOwner_s);
  if (rc != ESMF_SUCCESS) return NULL;

  rc = mesh->addElements(num_elem, elemId_s, elemType_s, elemConn_s, 
                         elemMask_s, elemArea_s, elemCoord_s);
  if (rc != ESMF_SUCCESS) return NULL;

  rc = ESMF_SUCCESS;
  return mesh;
}

MeshCXX* create_nmesh_quad_sph(int &rc) {
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

  // set Mesh parameters
  int num_elem = 4;
  int num_node = 9;

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
  double elemCoord_s [] ={0.5,0.5,0.5,1.5,1.5,0.5,1.5,1.5};

  ESMC_CoordSys_Flag local_coordSys=ESMC_COORDSYS_SPH_DEG;

  MeshCXX *mesh = new MeshCXX();

  mesh->create(pdim, sdim, local_coordSys, &rc);
  if (rc != ESMF_SUCCESS) return NULL;

  rc = mesh->addNodes(num_node, nodeId_s, nodeCoord_s, nodeOwner_s);
  if (rc != ESMF_SUCCESS) return NULL;

  rc = mesh->addElements(num_elem, elemId_s, elemType_s, elemConn_s, 
                         elemMask_s, elemArea_s, elemCoord_s);
  if (rc != ESMF_SUCCESS) return NULL;

  rc = ESMF_SUCCESS;
  return mesh;
}

MeshCXX* create_nmesh_tri(int &rc) {
  //
  //  2.0   7 ------- 8 -------- 9
  //        |  \   6  |  7    /  |
  //        |    \    |    /     |
  //        |  5   \  | /     8  |
  //  1.0   4 ------- 5 -------- 6
  //        |  1    / |  \    4  |
  //        |     /   |    \     |
  //        |  /   2  |  3   \   |
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
  int num_elem = 8;
  int num_node = 9;

  int nodeId_s [] ={1,2,3,4,5,6,7,8,9};
  double nodeCoord_s [] ={0.0,0.0, 1.0,0.0, 2.0,0.0,
                          0.0,1.0, 1.0,1.0, 2.0,1.0,
                          0.0,2.0, 1.0,2.0, 2.0,2.0};
  int nodeOwner_s [] ={0,0,0,0,0,0,0,0,0};
  int nodeMask_s [] ={1,1,1,1,1,1,1,1};
  int elemId_s [] ={1,2,3,4,5,6,7,8};
  // ESMF_MESHELEMTYPE_QUAD
  int elemType_s [] ={ESMC_MESHELEMTYPE_TRI,
                      ESMC_MESHELEMTYPE_TRI,
                      ESMC_MESHELEMTYPE_TRI,
                      ESMC_MESHELEMTYPE_TRI,
                      ESMC_MESHELEMTYPE_TRI,
                      ESMC_MESHELEMTYPE_TRI,
                      ESMC_MESHELEMTYPE_TRI,
                      ESMC_MESHELEMTYPE_TRI};
  int elemMask_s [] ={1,1,1,1,1,1,1,1};
  double elemArea_s [] ={1.0,2.0,3.0,4.0,1.0,2.0,3.0,4.0};
  int elemConn_s [] ={1,5,4,
                      1,2,5,
                      3,5,2,
                      3,6,5,
                      5,7,4,
                      5,8,7,
                      9,8,5,
                      9,5,6};
  double elemCoord_s [] ={0.5,0.5,0.5,1.5,1.5,0.5,1.5,1.5};

  ESMC_CoordSys_Flag local_coordSys=ESMC_COORDSYS_CART;

  MeshCXX *mesh = new MeshCXX();
  
  mesh->create(pdim, sdim, local_coordSys, &rc);
  if (rc != ESMF_SUCCESS) return NULL;

  rc = mesh->addNodes(num_node, nodeId_s, nodeCoord_s, nodeOwner_s);
  if (rc != ESMF_SUCCESS) return NULL;

  rc = mesh->addElements(num_elem, elemId_s, elemType_s, elemConn_s, 
                         elemMask_s, elemArea_s, elemCoord_s);
  if (rc != ESMF_SUCCESS) return NULL;

  rc = ESMF_SUCCESS;
  return mesh;
}

MeshCXX* create_nmesh_tri_sph(int &rc) {
  //
  //  pi/5  7 ------- 8 -------- 9
  //        |  \   6  |  7    /  |
  //        |    \    |    /     |
  //        |  5   \  | /     8  |
  //  pi/10 4 ------- 5 -------- 6
  //        |  1    / |  \    4  |
  //        |     /   |    \     |
  //        |  /   2  |  3   \   |
  //  0.0   1 ------- 2 -------- 3
  //
  //       0.0       pi/10      pi/5
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

  // set Mesh parameters
  int num_elem = 8;
  int num_node = 9;

  int nodeId_s [] ={1,2,3,4,5,6,7,8,9};
  double nodeCoord_s [] ={0.0,0.0, pi/10,0.0, pi/5,0.0,
                          0.0,pi/10, pi/10,pi/10, pi/5,pi/10,
                          0.0,pi/5, pi/10,pi/5, pi/5,pi/5};
  int nodeOwner_s [] ={0,0,0,0,0,0,0,0,0};
  int nodeMask_s [] ={1,1,1,1,1,1,1,1};
  int elemId_s [] ={1,2,3,4,5,6,7,8};
  // ESMF_MESHELEMTYPE_QUAD
  int elemType_s [] ={ESMC_MESHELEMTYPE_TRI,
                      ESMC_MESHELEMTYPE_TRI,
                      ESMC_MESHELEMTYPE_TRI,
                      ESMC_MESHELEMTYPE_TRI,
                      ESMC_MESHELEMTYPE_TRI,
                      ESMC_MESHELEMTYPE_TRI,
                      ESMC_MESHELEMTYPE_TRI,
                      ESMC_MESHELEMTYPE_TRI};
  int elemMask_s [] ={1,1,1,1,1,1,1,1};
  double elemArea_s [] ={1.0,2.0,3.0,4.0,1.0,2.0,3.0,4.0};
  int elemConn_s [] ={1,5,4,
                      1,2,5,
                      3,5,2,
                      3,6,5,
                      5,7,4,
                      5,8,7,
                      9,8,5,
                      9,5,6};
  double elemCoord_s [] ={0.5,0.5,0.5,1.5,1.5,0.5,1.5,1.5};

  ESMC_CoordSys_Flag local_coordSys=ESMC_COORDSYS_SPH_DEG;

  MeshCXX *mesh = new MeshCXX();

  mesh->create(pdim, sdim, local_coordSys, &rc);
  if (rc != ESMF_SUCCESS) return NULL;

  rc = mesh->addNodes(num_node, nodeId_s, nodeCoord_s, nodeOwner_s);
  if (rc != ESMF_SUCCESS) return NULL;

  rc = mesh->addElements(num_elem, elemId_s, elemType_s, elemConn_s, 
                         elemMask_s, elemArea_s, elemCoord_s);
  if (rc != ESMF_SUCCESS) return NULL;

  rc = ESMF_SUCCESS;
  return mesh;
}

MeshCXX* create_nmesh_tet(int &rc) {
/*
                                       ,|,
                                     ,7`\,\,
                                   ,7`  `| `\,
                                 ,7`     \,  `\,
                               ,7`       `|    `\,
                             ,7`          \,     `\,
                           ,7`            `|       `\,
                         ,7`               \,        `\,
                       ,7`                 `|        /7`\,
                     ,7`                    \,      AV `|`\,
                   ,7`'TTs.,                `|     /7   \, `\,
                 ,7` \\,  `'TTs.,            \,   AV    `|   `\,
               ,7`   `|       `'TTs.,       `|  /7      \,    `\,
             ,7`      \,            `'TTs.,   \,AV       `|      `\,
           ,7`        `|                 `'TTs`|7         \,       `\,
         ,7`           \,                     ,7\\,        `|         K`
       ,7`             `|                  ,7` `|`\,       \,       AV
     ,7`                \,               ,7`    \, `\,     `|      /7
   ,7`                  `|             ,7`      `|   `\,    \,    AV
  ,T,                    \,          ,7`         \,    `\,  `|   /7
  `'TTs.,                `|        ,7`           `|      `\, \, AV
       `'TTs.,            \,     ,7`              \,       `\||/7
            `'TTs.,       `|   ,7`                `|         `AV
                 `'TTs.,   \,,7`                   \,        /7
                      `'TTs`|                      `|       AV
                           `'TTs.,                  \,     /7
                                `'TTs.,             `|    AV
                                     `'TTV.,         \,  /7
                                          `'TTs.,    `| AV
                                               `'TTs.,\/7
                                                    `'T`
*/
  // Get parallel information
  int localPet, petCount;
  ESMC_VM vm;

  vm=ESMC_VMGetGlobal(&rc);
  if (rc != ESMF_SUCCESS) return 0;

  rc=ESMC_VMGet(vm, &localPet, &petCount, (int *)NULL, (MPI_Comm *)NULL,
                (int *)NULL, (int *)NULL);
  if (rc != ESMF_SUCCESS) return 0;

  // Mesh variables
  int pdim=3;
  int sdim=3;
  int num_elem, num_node;

  // set Mesh parameters
  int *nodeId;
  double *nodeCoord;
  int *nodeOwner;

  int *elemId;
  int *elemType;
  int *elemConn;
  int *elemMask;
  double *elemArea;
  double *elemCoord;

  MeshCXX *mesh = new MeshCXX();
  
  ESMC_CoordSys_Flag coordsys=ESMC_COORDSYS_CART;
  mesh->create(pdim, sdim, coordsys, &rc);

  num_node = 10;
  num_elem = 4;
  nodeId    = (int *) malloc (num_node * sizeof (int));
  nodeCoord = (double *) malloc (3*num_node * sizeof (double));
  nodeOwner = (int *) malloc (num_node * sizeof (int));
  elemId   = (int *) malloc (num_elem * sizeof (int));
  elemType = (int *) malloc (num_elem * sizeof (int));
  elemConn = (int *) malloc (4*num_elem * sizeof (int));
  elemMask = (int *) malloc (num_elem * sizeof (int));
  elemArea = (double *) malloc (num_elem * sizeof (double));
  elemCoord = (double *) malloc (num_elem * sizeof (double));

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

  nodeCoord[0]=0.0; nodeCoord[1]=0.0; nodeCoord[2]=0.0;
  nodeCoord[3]=1.0; nodeCoord[4]=0.0; nodeCoord[5]=0.0;
  nodeCoord[6]=2.0; nodeCoord[7]=0.0; nodeCoord[8]=0.0;
  nodeCoord[9]=0.5; nodeCoord[10]=1.0; nodeCoord[11]=0.0;
  nodeCoord[12]=1.5; nodeCoord[13]=1.0; nodeCoord[14]=0.0;
  nodeCoord[15]=1.0; nodeCoord[16]=2.0; nodeCoord[17]=0.0;
  nodeCoord[18]=0.5; nodeCoord[19]=0.5; nodeCoord[20]=1.0;
  nodeCoord[21]=1.0; nodeCoord[22]=0.5; nodeCoord[23]=1.0;
  nodeCoord[24]=1.5; nodeCoord[25]=0.5; nodeCoord[26]=1.0;
  nodeCoord[27]=1.0; nodeCoord[28]=1.5; nodeCoord[29]=1.0;

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

  elemId[0]=1;
  elemId[1]=2;
  elemId[2]=3;
  elemId[3]=4;

  elemType[0]=ESMC_MESHELEMTYPE_TETRA;
  elemType[1]=ESMC_MESHELEMTYPE_TETRA;
  elemType[2]=ESMC_MESHELEMTYPE_TETRA;
  elemType[3]=ESMC_MESHELEMTYPE_TETRA;

  elemConn[0]=1; elemConn[1]=2; elemConn[2]=7; elemConn[3]=4;
  elemConn[4]=2; elemConn[5]=3; elemConn[6]=9; elemConn[7]=5;
  elemConn[8]=2; elemConn[9]=5; elemConn[10]=8; elemConn[11]=4;
  elemConn[12]=4; elemConn[13]=5; elemConn[14]=10; elemConn[15]=6;

  elemMask[0]=1; elemMask[1]=1; elemMask[2]=1; elemMask[3]=1;

  elemArea[0]=1; elemArea[1]=1; elemArea[2]=1; elemArea[3]=1;

  elemCoord[0]=0; elemCoord[1]=0; elemCoord[2]=0; elemCoord[3]=0;

  rc = mesh->addNodes(num_node, nodeId, nodeCoord, nodeOwner);
  if (rc != ESMF_SUCCESS) return NULL;

  rc = mesh->addElements(num_elem, elemId, elemType, elemConn, 
                         elemMask, elemArea, elemCoord);
  if (rc != ESMF_SUCCESS) return NULL;
  
  free(nodeId);
  free(nodeCoord);
  free(nodeOwner);
  free(elemId);
  free(elemType);
  free(elemConn);
  free(elemMask);
  free(elemArea);
  free(elemCoord);

  rc = ESMF_SUCCESS;
  return mesh;
}

MeshCXX* create_nmesh_hex(int &rc) {

  // Get parallel information
  int localPet, petCount;
  ESMC_VM vm;

  vm=ESMC_VMGetGlobal(&rc);
  if (rc != ESMF_SUCCESS) return 0;

  rc=ESMC_VMGet(vm, &localPet, &petCount, (int *)NULL, (MPI_Comm *)NULL,
                (int *)NULL, (int *)NULL);
  if (rc != ESMF_SUCCESS) return 0;

  // Mesh variables
  int pdim=3;
  int sdim=3;
  int num_elem, num_node;

  // set Mesh parameters
  int *nodeId;
  double *nodeCoord;
  int *nodeOwner;

  int *elemId;
  int *elemType;
  int *elemConn;

  int *elemMask;
  double *elemArea;
  double *elemCoord;

  MeshCXX *mesh = new MeshCXX();
  
  ESMC_CoordSys_Flag coordsys=ESMC_COORDSYS_CART;
  mesh->create(pdim, sdim, coordsys, &rc);

  num_node = 18;
  num_elem = 4;

  nodeId    = (int *) malloc (num_node * sizeof (int));
  nodeCoord = (double *) malloc (3*num_node * sizeof (double));
  nodeOwner = (int *) malloc (num_node * sizeof (int));
  elemId   = (int *) malloc (num_elem * sizeof (int));
  elemType = (int *) malloc (num_elem * sizeof (int));
  elemConn = (int *) malloc (8*num_elem * sizeof (int));
  elemMask = (int *) malloc (num_elem * sizeof (int));
  elemArea = (double *) malloc (num_elem * sizeof (double));
  elemCoord = (double *) malloc (num_elem * sizeof (double));

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
  nodeId[16]=17;
  nodeId[17]=18;

  nodeCoord[0] =1.0 ; nodeCoord[1] =1.0 ; nodeCoord[2] =1.0;
  nodeCoord[3] =10.0; nodeCoord[4] =1.0 ; nodeCoord[5] =1.0;
  nodeCoord[6] =20.0; nodeCoord[7] =1.0 ; nodeCoord[8] =1.0;
  nodeCoord[9] =1.0 ; nodeCoord[10]=10.0; nodeCoord[11]=1.0;
  nodeCoord[12]=10.0; nodeCoord[13]=10.0; nodeCoord[14]=1.0;
  nodeCoord[15]=20.0; nodeCoord[16]=10.0; nodeCoord[17]=1.0;
  nodeCoord[18]=1.0 ; nodeCoord[19]=20.0; nodeCoord[20]=1.0;
  nodeCoord[21]=10.0; nodeCoord[22]=20.0; nodeCoord[23]=1.0;
  nodeCoord[24]=20.0; nodeCoord[25]=20.0; nodeCoord[26]=1.0;
  nodeCoord[27]=1.0 ; nodeCoord[28]=1.0 ; nodeCoord[29]=2.0;
  nodeCoord[30]=10.0; nodeCoord[31]=1.0 ; nodeCoord[32]=2.0;
  nodeCoord[33]=20.0; nodeCoord[34]=1.0 ; nodeCoord[35]=2.0;
  nodeCoord[36]=1.0 ; nodeCoord[37]=10.0; nodeCoord[38]=2.0;
  nodeCoord[39]=10.0; nodeCoord[40]=10.0; nodeCoord[41]=2.0;
  nodeCoord[42]=20.0; nodeCoord[43]=10.0; nodeCoord[44]=2.0;
  nodeCoord[45]=1.0 ; nodeCoord[46]=20.0; nodeCoord[47]=2.0;
  nodeCoord[48]=10.0; nodeCoord[49]=20.0; nodeCoord[50]=2.0;
  nodeCoord[51]=20.0; nodeCoord[52]=20.0; nodeCoord[53]=2.0;

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
  nodeOwner[16]=0;
  nodeOwner[17]=0;

  elemId[0]=1;
  elemId[1]=2;
  elemId[2]=3;
  elemId[3]=4;

  elemType[0]=ESMC_MESHELEMTYPE_HEX;
  elemType[1]=ESMC_MESHELEMTYPE_HEX;
  elemType[2]=ESMC_MESHELEMTYPE_HEX;
  elemType[3]=ESMC_MESHELEMTYPE_HEX;

  elemConn[0]=1; elemConn[1]=2; elemConn[2]=5; elemConn[3]=4;
      elemConn[4]=10; elemConn[5]=11; elemConn[6]=14; elemConn[7]=13;
  elemConn[8]=2; elemConn[9]=3; elemConn[10]=6; elemConn[11]=5;
      elemConn[12]=11; elemConn[13]=12; elemConn[14]=15; elemConn[15]=14;
  elemConn[16]=4; elemConn[17]=5; elemConn[18]=8; elemConn[19]=7;
      elemConn[20]=13; elemConn[21]=14; elemConn[22]=17; elemConn[23]=16;
  elemConn[24]=5; elemConn[25]=6; elemConn[26]=9; elemConn[27]=8;
      elemConn[28]=14; elemConn[29]=15; elemConn[30]=18; elemConn[31]=17;

  elemMask[0]=1; elemMask[1]=1; elemMask[2]=1; elemMask[3]=1;

  elemArea[0]=1; elemArea[1]=1; elemArea[2]=1; elemArea[3]=1;

  elemCoord[0]=0; elemCoord[1]=0; elemCoord[2]=0; elemCoord[3]=0;

  rc = mesh->addNodes(num_node, nodeId, nodeCoord, nodeOwner);
  if (rc != ESMF_SUCCESS) return NULL;

  rc = mesh->addElements(num_elem, elemId, elemType, elemConn,
                         elemMask, elemArea, elemCoord);
  if (rc != ESMF_SUCCESS) return NULL;
  
  free(nodeId);
  free(nodeCoord);
  free(nodeOwner);
  free(elemId);
  free(elemType);
  free(elemConn);
  free(elemMask);
  free(elemArea);
  free(elemCoord);

  rc = ESMF_SUCCESS;
  return mesh;
}

MeshCXX* create_nmesh_quad_10_parallel(ESMC_CoordSys_Flag coordsys, int &rc) {

  //
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
  //   1.0   1 ------- 2 ------- 3      [3] ------------ 4
  //
  //           1.0       1.5     2.0     2.0             3.0
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

  if (petCount != 4) {
    //Throw() << "Test function must be run with 4 processors";
    printf("Test function must be run with 4 processors\n");
    return NULL;
  }

  // Mesh variables
  int pdim=2;
  int sdim=2;
  int num_elem = 10;
  int num_node = 16;

  // set Mesh parameters
  int *nodeId;
  double *nodeCoord;
  int *nodeOwner;

  int *elemId;
  int *elemType;
  int *elemConn;
  int *elemMask;
  double *elemArea;
  double *elemCoord;


  nodeId    = (int *) malloc (num_node * sizeof (int));
  nodeCoord = (double *) malloc (2*num_node * sizeof (double));
  nodeOwner = (int *) malloc (num_node * sizeof (int));
  elemId   = (int *) malloc (num_elem * sizeof (int));
  elemType = (int *) malloc (num_elem * sizeof (int));
  elemConn = (int *) malloc (4*num_elem * sizeof (int));
  elemMask = (int *) malloc (num_elem * sizeof (int));
  elemArea = (double *) malloc (num_elem * sizeof (double));
  elemCoord = (double *) malloc (num_elem * sizeof (double));

  MeshCXX *mesh = new MeshCXX();
    mesh->create(pdim, sdim, coordsys, &rc);

  if (localPet == 0){
    num_node = 9;
    num_elem = 5;

    nodeId[0]=10;
    nodeId[1]=20;
    nodeId[2]=30;
    nodeId[3]=50;
    nodeId[4]=60;
    nodeId[5]=70;
    nodeId[6]=90;
    nodeId[7]=100;
    nodeId[8]=110;

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
    elemId[2]=3;
    elemId[3]=5;
    elemId[4]=6;

    elemType[0]=ESMC_MESHELEMTYPE_QUAD;
    elemType[1]=ESMC_MESHELEMTYPE_TRI;
    elemType[2]=ESMC_MESHELEMTYPE_TRI;
    elemType[3]=ESMC_MESHELEMTYPE_QUAD;
    elemType[4]=ESMC_MESHELEMTYPE_QUAD;

    elemConn[0]=1;elemConn[1]=2;elemConn[2]=5;elemConn[3]=4;
    elemConn[4]=2;elemConn[5]=3;elemConn[6]=5;
    elemConn[7]=3;elemConn[8]=6;elemConn[9]=5;
    elemConn[10]=4;elemConn[11]=5;elemConn[12]=8;elemConn[13]=7;
    elemConn[14]=5;elemConn[15]=6;elemConn[16]=9;elemConn[17]=8;

    elemMask[0]=1; elemMask[1]=1; elemMask[2]=1; elemMask[3]=1; elemMask[4]=1;

    elemArea[0]=1; elemArea[1]=1; elemArea[2]=1; elemArea[3]=1; elemArea[4]=1;

    elemCoord[0]=0; elemCoord[1]=0; elemCoord[2]=0; elemCoord[3]=0; elemCoord[4]=0;
  }
  else if (localPet == 1) {
    num_node = 6;
    num_elem = 2;

    nodeId[0]=30;
    nodeId[1]=40;
    nodeId[2]=70;
    nodeId[3]=80;
    nodeId[4]=110;
    nodeId[5]=120;

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

    elemMask[0]=1; elemMask[1]=1;

    elemArea[0]=1; elemArea[1]=1;

    elemCoord[0]=0; elemCoord[1]=0;
  }
  else if (localPet == 2) {
    num_node = 6;
    num_elem = 2;

    nodeId[0]=90;
    nodeId[1]=100;
    nodeId[2]=110;
    nodeId[3]=130;
    nodeId[4]=140;
    nodeId[5]=150;

    nodeCoord[0]=1.0;nodeCoord[1]=2.0;
    nodeCoord[2]=1.5;nodeCoord[3]=2.0;
    nodeCoord[4]=2.0;nodeCoord[5]=2.0;
    nodeCoord[6]=1.0;nodeCoord[7]=3.0;
    nodeCoord[8]=1.5;nodeCoord[9]=3.0;
    nodeCoord[10]=2.0;nodeCoord[11]=3.0;

    nodeOwner[0]=0;
    nodeOwner[1]=0;
    nodeOwner[2]=0;
    nodeOwner[3]=2;
    nodeOwner[4]=2;
    nodeOwner[5]=2;

    elemId[0]=8;
    elemId[1]=9;

    elemType[0]=ESMC_MESHELEMTYPE_QUAD;
    elemType[1]=ESMC_MESHELEMTYPE_QUAD;

    elemConn[0]=1;elemConn[1]=2;elemConn[2]=5;elemConn[3]=4;
    elemConn[4]=2;elemConn[5]=3;elemConn[6]=6;elemConn[7]=5;

    elemMask[0]=1; elemMask[1]=1;

    elemArea[0]=1; elemArea[1]=1;

    elemCoord[0]=0; elemCoord[1]=0;
  }
  else if (localPet == 3) {
    num_node = 4;
    num_elem = 1;

    nodeId[0]=110;
    nodeId[1]=120;
    nodeId[2]=150;
    nodeId[3]=160;

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

    elemMask[0]=1;

    elemArea[0]=1;

    elemCoord[0]=0;
  }

  rc = mesh->addNodes(num_node, nodeId, nodeCoord, nodeOwner);
  if (rc != ESMF_SUCCESS) return NULL;

  rc = mesh->addElements(num_elem, elemId, elemType, elemConn,
                         elemMask, elemArea, elemCoord);
  if (rc != ESMF_SUCCESS) return NULL;
  
  free(nodeId);
  free(nodeCoord);
  free(nodeOwner);
  free(elemId);
  free(elemType);
  free(elemConn);
  free(elemMask);
  free(elemArea);
  free(elemCoord);

  rc = ESMF_SUCCESS;
  return mesh;
}

MeshCXX* create_nmesh_quad_sph_10_parallel(ESMC_CoordSys_Flag coordsys, int &rc) {

  //
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
  //   1.0   1 ------- 2 ------- 3      [3] ------------ 4
  //
  //           1.0       1.5     2.0     2.0             3.0
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

  if (petCount != 4) {
    //Throw() << "Test function must be run with 4 processors";
    printf("Test function must be run with 4 processors\n");
    return NULL;
  }

  // Mesh variables
  int pdim=2;
  int sdim=2;
  int num_elem = 10;
  int num_node = 16;

  // set Mesh parameters
  int *nodeId;
  double *nodeCoord;
  int *nodeOwner;

  int *elemId;
  int *elemType;
  int *elemConn;
  int *elemMask;
  double *elemArea;
  double *elemCoord;

  int pi2 = 3.14159/2;

  nodeId    = (int *) malloc (num_node * sizeof (int));
  nodeCoord = (double *) malloc (2*num_node * sizeof (double));
  nodeOwner = (int *) malloc (num_node * sizeof (int));
  elemId   = (int *) malloc (num_elem * sizeof (int));
  elemType = (int *) malloc (num_elem * sizeof (int));
  elemConn = (int *) malloc (4*num_elem * sizeof (int));
  elemMask = (int *) malloc (num_elem * sizeof (int));
  elemArea = (double *) malloc (num_elem * sizeof (double));
  elemCoord = (double *) malloc (num_elem * sizeof (double));

  MeshCXX *mesh = new MeshCXX();
    mesh->create(pdim, sdim, coordsys, &rc);

  if (localPet == 0){
    num_node = 9;
    num_elem = 5;

    nodeId[0]=1;
    nodeId[1]=2;
    nodeId[2]=3;
    nodeId[3]=5;
    nodeId[4]=6;
    nodeId[5]=7;
    nodeId[6]=9;
    nodeId[7]=10;
    nodeId[8]=11;

    nodeCoord[0]= 1.0 * pi2;nodeCoord[1]= 1.0 * pi2;
    nodeCoord[2]= 1.5 * pi2;nodeCoord[3]= 1.0 * pi2;
    nodeCoord[4]= 2.0 * pi2;nodeCoord[5]= 1.0 * pi2;
    nodeCoord[6]= 1.0 * pi2;nodeCoord[7]= 1.5 * pi2;
    nodeCoord[8]= 1.5 * pi2;nodeCoord[9]= 1.5 * pi2;
    nodeCoord[10]=2.0 * pi2;nodeCoord[11]=1.5 * pi2;
    nodeCoord[12]=1.0 * pi2;nodeCoord[13]=2.0 * pi2;
    nodeCoord[14]=1.5 * pi2;nodeCoord[15]=2.0 * pi2;
    nodeCoord[16]=2.0 * pi2;nodeCoord[17]=2.0 * pi2;

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
    elemId[2]=3;
    elemId[3]=5;
    elemId[4]=6;

    elemType[0]=ESMC_MESHELEMTYPE_QUAD;
    elemType[1]=ESMC_MESHELEMTYPE_TRI;
    elemType[2]=ESMC_MESHELEMTYPE_TRI;
    elemType[3]=ESMC_MESHELEMTYPE_QUAD;
    elemType[4]=ESMC_MESHELEMTYPE_QUAD;

    elemConn[0]=1;elemConn[1]=2;elemConn[2]=5;elemConn[3]=4;
    elemConn[4]=2;elemConn[5]=3;elemConn[6]=5;
    elemConn[7]=3;elemConn[8]=6;elemConn[9]=5;
    elemConn[10]=4;elemConn[11]=5;elemConn[12]=8;elemConn[13]=7;
    elemConn[14]=5;elemConn[15]=6;elemConn[16]=9;elemConn[17]=8;

    elemMask[0]=1; elemMask[1]=1; elemMask[2]=1; elemMask[3]=1; elemMask[4]=1;

    elemArea[0]=1; elemArea[1]=1; elemArea[2]=1; elemArea[3]=1; elemArea[4]=1;

    elemCoord[0]=0; elemCoord[1]=0; elemCoord[2]=0; elemCoord[3]=0; elemCoord[4]=0;
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

    nodeCoord[0]= 2.0 * pi2;nodeCoord[1]= 1.0 * pi2;
    nodeCoord[2]= 3.0 * pi2;nodeCoord[3]= 1.0 * pi2;
    nodeCoord[4]= 2.0 * pi2;nodeCoord[5]= 1.5 * pi2;
    nodeCoord[6]= 3.0 * pi2;nodeCoord[7]= 1.5 * pi2;
    nodeCoord[8]= 2.0 * pi2;nodeCoord[9]= 2.0 * pi2;
    nodeCoord[10]=3.0 * pi2;nodeCoord[11]=2.0 * pi2;

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

    elemMask[0]=1; elemMask[1]=1;

    elemArea[0]=1; elemArea[1]=1;

    elemCoord[0]=0; elemCoord[1]=0;
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

    nodeCoord[0]= 1.0 * pi2;nodeCoord[1]= 2.0 * pi2;
    nodeCoord[2]= 1.5 * pi2;nodeCoord[3]= 2.0 * pi2;
    nodeCoord[4]= 2.0 * pi2;nodeCoord[5]= 2.0 * pi2;
    nodeCoord[6]= 1.0 * pi2;nodeCoord[7]= 3.0 * pi2;
    nodeCoord[8]= 1.5 * pi2;nodeCoord[9]= 3.0 * pi2;
    nodeCoord[10]=2.0 * pi2;nodeCoord[11]=3.0 * pi2;

    nodeOwner[0]=0;
    nodeOwner[1]=0;
    nodeOwner[2]=0;
    nodeOwner[3]=2;
    nodeOwner[4]=2;
    nodeOwner[5]=2;

    elemId[0]=8;
    elemId[1]=9;

    elemType[0]=ESMC_MESHELEMTYPE_QUAD;
    elemType[1]=ESMC_MESHELEMTYPE_QUAD;

    elemConn[0]=1;elemConn[1]=2;elemConn[2]=5;elemConn[3]=4;
    elemConn[4]=2;elemConn[5]=3;elemConn[6]=6;elemConn[7]=5;

    elemMask[0]=1; elemMask[1]=1;

    elemArea[0]=1; elemArea[1]=1;

    elemCoord[0]=0; elemCoord[1]=0;
  }
  else if (localPet == 3) {
    num_node = 4;
    num_elem = 1;

    nodeId[0]=11;
    nodeId[1]=12;
    nodeId[2]=15;
    nodeId[3]=16;

    nodeCoord[0]=2.0 * pi2;nodeCoord[1]=2.0 * pi2;
    nodeCoord[2]=3.0 * pi2;nodeCoord[3]=2.0 * pi2;
    nodeCoord[4]=2.0 * pi2;nodeCoord[5]=3.0 * pi2;
    nodeCoord[6]=3.0 * pi2;nodeCoord[7]=3.0 * pi2;

    nodeOwner[0]=0;
    nodeOwner[1]=1;
    nodeOwner[2]=2;
    nodeOwner[3]=3;

    elemId[0]=10;

    elemType[0]=ESMC_MESHELEMTYPE_QUAD;

    elemConn[0]=1;elemConn[1]=2;elemConn[2]=4;elemConn[3]=3;

    elemMask[0]=1;

    elemArea[0]=1;

    elemCoord[0]=0;
  }

  rc = mesh->addNodes(num_node, nodeId, nodeCoord, nodeOwner);
  if (rc != ESMF_SUCCESS) return NULL;

  rc = mesh->addElements(num_elem, elemId, elemType, elemConn,
                         elemMask, elemArea, elemCoord);
  if (rc != ESMF_SUCCESS) return NULL;
  
  free(nodeId);
  free(nodeCoord);
  free(nodeOwner);
  free(elemId);
  free(elemType);
  free(elemConn);
  free(elemMask);
  free(elemArea);
  free(elemCoord);

  rc = ESMF_SUCCESS;
  return mesh;
}

MeshCXX* create_nmesh_quad_9_parallel(ESMC_CoordSys_Flag coordsys, int &rc) {

  //
  //      3.0   13 ------ 14                 [14] ----- 15 ------ 16
  //             |         |                   |         |         |
  //    PET 2    |    7    |           PET 3   |    8    |    9    |
  //             |         |                   |         |         |
  //            [9] ----- [10]                [10] ---- [11] ---- [12]

  //       2.0   9 ------- 10 ------ 11 ------ 12
  //             |         |         |         |
  //    PET 1    |    4    |    5    |    6    |
  //             |         |         |         |
  //            [5] ----- [6] ----- [7] ----- [8]

  //       1.5   5 ------- 6 ------- 7 ------- 8
  //             |         |         |         |
  //    PET 0    |    1    |    2    |    3    |
  //             |         |         |         |
  //       1.0   1 ------- 2 ------- 3  ------ 4
  //
  //           1.0       1.5       2.0         3.0
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

  if (petCount != 4) {
    //Throw() << "Test function must be run with 4 processors";
    printf("Test function must be run with 4 processors\n");
    return NULL;
  }

  // Mesh variables
  int pdim=2;
  int sdim=2;
  int num_elem = 9;
  int num_node = 16;

  // set Mesh parameters
  int *nodeId;
  double *nodeCoord;
  int *nodeOwner;

  int *elemId;
  int *elemType;
  int *elemConn;
  int *elemMask;
  double *elemArea;
  double *elemCoord;


  nodeId    = (int *) malloc (num_node * sizeof (int));
  nodeCoord = (double *) malloc (2*num_node * sizeof (double));
  nodeOwner = (int *) malloc (num_node * sizeof (int));
  elemId   = (int *) malloc (num_elem * sizeof (int));
  elemType = (int *) malloc (num_elem * sizeof (int));
  elemConn = (int *) malloc (4*num_elem * sizeof (int));
  elemMask = (int *) malloc (num_elem * sizeof (int));
  elemArea = (double *) malloc (num_elem * sizeof (double));
  elemCoord = (double *) malloc (num_elem * sizeof (double));

  MeshCXX *mesh = new MeshCXX();
    mesh->create(pdim, sdim, coordsys, &rc);

  if (localPet == 0){
    num_node = 8;
    num_elem = 3;

    nodeId[0]=1;
    nodeId[1]=2;
    nodeId[2]=3;
    nodeId[3]=4;
    nodeId[4]=5;
    nodeId[5]=6;
    nodeId[6]=7;
    nodeId[7]=8;

    nodeCoord[0]=1.0;nodeCoord[1]=1.0;
    nodeCoord[2]=1.5;nodeCoord[3]=1.0;
    nodeCoord[4]=2.0;nodeCoord[5]=1.0;
    nodeCoord[6]=3.0;nodeCoord[7]=1.0;
    nodeCoord[8]=1.0;nodeCoord[9]=1.5;
    nodeCoord[10]=1.5;nodeCoord[11]=1.5;
    nodeCoord[12]=2.0;nodeCoord[13]=1.5;
    nodeCoord[14]=3.0;nodeCoord[15]=1.5;

    nodeOwner[0]=0;
    nodeOwner[1]=0;
    nodeOwner[2]=0;
    nodeOwner[3]=0;
    nodeOwner[4]=0;
    nodeOwner[5]=0;
    nodeOwner[6]=0;
    nodeOwner[7]=0;

    elemId[0]=1;
    elemId[1]=2;
    elemId[2]=3;

    elemType[0]=ESMC_MESHELEMTYPE_QUAD;
    elemType[1]=ESMC_MESHELEMTYPE_QUAD;
    elemType[2]=ESMC_MESHELEMTYPE_QUAD;

    elemConn[0]=1;elemConn[1]=2;elemConn[2]=6;elemConn[3]=5;
    elemConn[4]=2;elemConn[5]=3;elemConn[6]=7;elemConn[7]=6;
    elemConn[8]=3;elemConn[9]=4;elemConn[10]=8;elemConn[11]=7;

    elemMask[0]=1; elemMask[1]=1; elemMask[2]=1;

    elemArea[0]=1; elemArea[1]=1; elemArea[2]=1;

    elemCoord[0]=0; elemCoord[1]=0; elemCoord[2]=0;
  }
  else if (localPet == 1) {
    num_node = 8;
    num_elem = 3;

    nodeId[0]=5;
    nodeId[1]=6;
    nodeId[2]=7;
    nodeId[3]=8;
    nodeId[4]=9;
    nodeId[5]=10;
    nodeId[6]=11;
    nodeId[7]=12;

    nodeCoord[0]=1.0;nodeCoord[1]=1.5;
    nodeCoord[2]=1.5;nodeCoord[3]=1.5;
    nodeCoord[4]=2.0;nodeCoord[5]=1.5;
    nodeCoord[6]=3.0;nodeCoord[7]=1.5;
    nodeCoord[8]=1.0;nodeCoord[9]=2.0;
    nodeCoord[10]=1.5;nodeCoord[11]=2.0;
    nodeCoord[12]=2.0;nodeCoord[13]=2.0;
    nodeCoord[14]=3.0;nodeCoord[15]=2.0;

    nodeOwner[0]=0;
    nodeOwner[1]=0;
    nodeOwner[2]=0;
    nodeOwner[3]=0;
    nodeOwner[4]=1;
    nodeOwner[5]=1;
    nodeOwner[6]=1;
    nodeOwner[7]=1;

    elemId[0]=4;
    elemId[1]=5;
    elemId[2]=6;

    elemType[0]=ESMC_MESHELEMTYPE_QUAD;
    elemType[1]=ESMC_MESHELEMTYPE_QUAD;
    elemType[2]=ESMC_MESHELEMTYPE_QUAD;

    elemConn[0]=1;elemConn[1]=2;elemConn[2]=6;elemConn[3]=5;
    elemConn[4]=2;elemConn[5]=3;elemConn[6]=7;elemConn[7]=6;
    elemConn[8]=3;elemConn[9]=4;elemConn[10]=8;elemConn[11]=7;

    elemMask[0]=1; elemMask[1]=1; elemMask[2]=1;

    elemArea[0]=1; elemArea[1]=1; elemArea[2]=1;

    elemCoord[0]=0; elemCoord[1]=0; elemCoord[2]=0;
  }
  else if (localPet == 2) {
    num_node = 4;
    num_elem = 1;

    nodeId[0]=9;
    nodeId[1]=10;
    nodeId[2]=13;
    nodeId[3]=14;

    nodeCoord[0]=1.0;nodeCoord[1]=2.0;
    nodeCoord[2]=1.5;nodeCoord[3]=2.0;
    nodeCoord[4]=1.0;nodeCoord[5]=3.0;
    nodeCoord[6]=1.5;nodeCoord[7]=3.0;

    nodeOwner[0]=1;
    nodeOwner[1]=1;
    nodeOwner[2]=2;
    nodeOwner[3]=2;

    elemId[0]=7;

    elemType[0]=ESMC_MESHELEMTYPE_QUAD;

    elemConn[0]=1;elemConn[1]=2;elemConn[2]=4;elemConn[3]=3;

    elemMask[0]=1;

    elemArea[0]=1;

    elemCoord[0]=0;
  }
  else if (localPet == 3) {
    num_node = 6;
    num_elem = 2;

    nodeId[0]=10;
    nodeId[1]=11;
    nodeId[2]=12;
    nodeId[3]=14;
    nodeId[4]=15;
    nodeId[5]=16;

    nodeCoord[0]=1.5;nodeCoord[1]=2.0;
    nodeCoord[2]=2.0;nodeCoord[3]=2.0;
    nodeCoord[4]=3.0;nodeCoord[5]=2.0;
    nodeCoord[6]=1.5;nodeCoord[7]=3.0;
    nodeCoord[8]=2.0;nodeCoord[9]=3.0;
    nodeCoord[10]=3.0;nodeCoord[11]=3.0;

    nodeOwner[0]=1;
    nodeOwner[1]=1;
    nodeOwner[2]=1;
    nodeOwner[3]=2;
    nodeOwner[4]=3;
    nodeOwner[5]=3;

    elemId[0]=8;
    elemId[1]=9;

    elemType[0]=ESMC_MESHELEMTYPE_QUAD;
    elemType[1]=ESMC_MESHELEMTYPE_QUAD;

    elemConn[0]=1;elemConn[1]=2;elemConn[2]=5;elemConn[3]=4;
    elemConn[4]=2;elemConn[5]=3;elemConn[6]=6;elemConn[7]=5;

    elemMask[0]=1; elemMask[1]=1;

    elemArea[0]=1; elemArea[1]=1;

    elemCoord[0]=0; elemCoord[1]=0;
  }

  rc = mesh->addNodes(num_node, nodeId, nodeCoord, nodeOwner);
  if (rc != ESMF_SUCCESS) return NULL;

  rc = mesh->addElements(num_elem, elemId, elemType, elemConn,
                         elemMask, elemArea, elemCoord);
  if (rc != ESMF_SUCCESS) return NULL;
  
  free(nodeId);
  free(nodeCoord);
  free(nodeOwner);
  free(elemId);
  free(elemType);
  free(elemConn);
  free(elemMask);
  free(elemArea);
  free(elemCoord);

  rc = ESMF_SUCCESS;
  return mesh;
}
