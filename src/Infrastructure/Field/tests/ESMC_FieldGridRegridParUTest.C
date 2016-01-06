// $Id$
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

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

// ESMF header
#include "ESMC.h"

// ESMF Test header
#include "ESMC_Test.h"

#define center

//==============================================================================
//BOP
// !PROGRAM: ESMC_FieldRegridParUTest - Check ESMC_FieldRegrid functionality
//
// !DESCRIPTION:
//
//EOP
//-----------------------------------------------------------------------------

int main(void){

  // Test variables
  char name[80];
  char failMsg[80];
  int result = 0;
  int rc;

  int localPet, petCount;
  ESMC_VM vm;
  
  // Field variables
  ESMC_ArraySpec arrayspec;
  ESMC_RouteHandle routehandle;
  ESMC_Field srcfield, dstfield, exactfield;

  // Grid variables
  ESMC_Grid srcgrid;
  int dimcount = 2;
  int *maxIndex;
  ESMC_InterfaceInt i_maxIndex;

  // Mesh variables
  int pdim=2;
  int sdim=2;
  ESMC_Mesh dstmesh;
  int num_elem, num_node;


  // computation variables
  int p;
  double x, y, exact, tol;
  //----------------------------------------------------------------------------
  ESMC_TestStart(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  // Get parallel information
  vm=ESMC_VMGetGlobal(&rc);
  if (rc != ESMF_SUCCESS) return 0;

  rc=ESMC_VMGet(vm, &localPet, &petCount, (int *)NULL, (MPI_Comm *)NULL,
                (int *)NULL, (int *)NULL);
  if (rc != ESMF_SUCCESS) return 0;

  rc=ESMC_LogSet(true);

  //----------------------------------------------------------------------------
  //----------------------- Grid CREATION --------------------------------------
  //----------------------------------------------------------------------------

#ifdef ESMF_TESTEXHAUSTIVE
  //----------------------------------------------------------------------------
  double ub_x, lb_x, max_x, min_x, cellwidth_x, cellcenter_x;
  double ub_y, lb_y, max_y, min_y, cellwidth_y, cellcenter_y;
  ub_x = 21;
  ub_y = 21;
  lb_x = 0;
  lb_y = 0;
  max_x = 21;
  max_y = 21;
  min_x = 0;
  min_y = 0;
  cellwidth_x = (max_x-min_x)/(ub_x-lb_x);
  cellwidth_y = (max_y-min_y)/(ub_y-lb_y);

  cellcenter_x = cellwidth_x/double(2);
  cellcenter_y = cellwidth_y/double(2);

  maxIndex = (int *)malloc(dimcount*sizeof(int));
  maxIndex[0] = int(ub_x);
  maxIndex[1] = int(ub_y);
  rc = ESMC_InterfaceIntSet(&i_maxIndex, maxIndex, dimcount);

  //----------------------------------------------------------------------------
  //EX_UTest_Multi_Proc_Only
  strcpy(name, "GridCreate");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  ESMC_CoordSys_Flag coordsys = ESMC_COORDSYS_CART;
  ESMC_TypeKind_Flag typekind = ESMC_TYPEKIND_R8;
  ESMC_IndexFlag indexflag = ESMC_INDEX_GLOBAL;
  srcgrid = ESMC_GridCreateNoPeriDim(&i_maxIndex, &coordsys, &typekind, &indexflag, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);

  // free memory
  free(maxIndex);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //  GridAddCoord to srcgrid
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest_Multi_Proc_Only
  strcpy(name, "GridAddCoord");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_GridAddCoord(srcgrid, ESMC_STAGGERLOC_CENTER);
  rc = ESMC_GridAddCoord(srcgrid, ESMC_STAGGERLOC_CORNER);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  // CORNERS

  //----------------------------------------------------------------------------
  //EX_UTest_Multi_Proc_Only
  // get and fill first coord array and computational bounds
  int *exLB_corner = (int *)malloc(dimcount*sizeof(int));
  int *exUB_corner = (int *)malloc(dimcount*sizeof(int));

  strcpy(name, "GridGetCoord - X");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  double *gridXCorner = (double *)ESMC_GridGetCoord(srcgrid, 1,
                                                    ESMC_STAGGERLOC_CORNER,
                                                    exLB_corner, exUB_corner, &rc);

  p = 0;
  for (int i1=exLB_corner[1]; i1<=exUB_corner[1]; ++i1) {
    for (int i0=exLB_corner[0]; i0<=exUB_corner[0]; ++i0) {
      gridXCorner[p]=(double)(i0-1)*cellwidth_x;
      ++p;
    }
  }

  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  printf("exLB_corner = [%d,%d]\n", exLB_corner[0], exLB_corner[1]);
  printf("exUB_corner = [%d,%d]\n", exUB_corner[0], exUB_corner[1]);

  //----------------------------------------------------------------------------
  //EX_UTest_Multi_Proc_Only
  // get and fill second coord array
  strcpy(name, "GridGetCoord - Y");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  double *gridYCorner = (double *)ESMC_GridGetCoord(srcgrid, 2,
                                                    ESMC_STAGGERLOC_CORNER,
                                                    NULL, NULL, &rc);

  p = 0;
  for (int i1=exLB_corner[1]; i1<=exUB_corner[1]; ++i1) {
    for (int i0=exLB_corner[0]; i0<=exUB_corner[0]; ++i0) {
      gridYCorner[p]=(double)(i1-1)*cellwidth_y;
      ++p;
    }
  }

  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  // CENTERS

  //----------------------------------------------------------------------------
  //EX_UTest_Multi_Proc_Only
  // get and fill first coord array and computational bounds
  int *exLB_center = (int *)malloc(dimcount*sizeof(int));
  int *exUB_center = (int *)malloc(dimcount*sizeof(int));

  strcpy(name, "GridGetCoord - X");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  double *gridXCenter = (double *)ESMC_GridGetCoord(srcgrid, 1,
                                                    ESMC_STAGGERLOC_CENTER,
                                                    exLB_center, exUB_center, &rc);

  p = 0;
  for (int i1=exLB_center[1]; i1<=exUB_center[1]; ++i1) {
    for (int i0=exLB_center[0]; i0<=exUB_center[0]; ++i0) {
      gridXCenter[p]=(double)(i0-1)*cellwidth_x + (double)(cellwidth_x/2.0);
      ++p;
    }
  }

  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  printf("exLB_center = [%d,%d]\n", exLB_center[0], exLB_center[1]);
  printf("exUB_center = [%d,%d]\n", exUB_center[0], exUB_center[1]);

  //----------------------------------------------------------------------------
  //EX_UTest_Multi_Proc_Only
  // get and fill second coord array
  strcpy(name, "GridGetCoord - Y");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  double *gridYCenter = (double *)ESMC_GridGetCoord(srcgrid, 2,
                                                    ESMC_STAGGERLOC_CENTER,
                                                    NULL, NULL, &rc);

  p = 0;
  for (int i1=exLB_center[1]; i1<=exUB_center[1]; ++i1) {
    for (int i0=exLB_center[0]; i0<=exUB_center[0]; ++i0) {
      gridYCenter[p]=(double)(i1-1)*cellwidth_y + (double)(cellwidth_y/2.0);
      ++p;
    }
  }

  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------




  //              Destination Mesh
  // 
  //
  //  20.0 3.0   13 ------ 14 ------ 15     [15] ----------- 16    
  //             |         |         |       |               |
  //             |         |         |       |               |
  //             |    8    |    9    |       |       10      |
  //             |         |         |       |               |
  //             |         |         |       |               |
  //  15.0 2.0  [9] ----- [10] ---- [11]    [11] ---------- [12]
  //                                                              
  //           1.0       1.5       2.0     2.0             3.0 
  //           1.0       10.0      15.0    15.0            20.0 
  //          
  //                    PET 2                      PET 3
  //          
  //          
  //  15.0 2.0   9 ------- 10 ------ 11     [11] ----------- 12      
  //             |         |         |       |               |
  //             |    5    |    6    |       |       7       |
  //             |         |         |       |               |
  //  10.0 1.5   5 ------- 6 ------- 7      [7] -----------  8
  //             |         |  \   3  |       |               |
  //             |    1    |    \    |       |       4       |
  //             |         | 2    \  |       |               |
  //  1.0  1.0   1 ------- 2 ------- 3      [3] ------------ 4
  //                                                    
  //           1.0       1.5       2.0     2.0             3.0 
  //           1.0       10.0       15.0    15.0            20.0 
  // 
  //                PET 0                      PET 1
  //
  //               Node Id labels at corners
  //              Element Id labels in centers


  // set Mesh parameters
  int *nodeId;
  double *nodeCoord;
  int *nodeOwner;

  int *elemId;
  int *elemType;
  int *elemConn;


  if (localPet == 0){
    num_node = 9;
    num_elem = 5;

    nodeId    = (int *) malloc (num_node * sizeof (int));
    nodeCoord = (double *) malloc (2*num_node * sizeof (double));
    nodeOwner = (int *) malloc (num_node * sizeof (int));
    elemId   = (int *) malloc (num_elem * sizeof (int));
    elemType = (int *) malloc (num_elem * sizeof (int));
    elemConn = (int *) malloc (4*num_elem * sizeof (int));

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
    nodeCoord[2]=10.0;nodeCoord[3]=1.0;
    nodeCoord[4]=15.0;nodeCoord[5]=1.0;
    nodeCoord[6]=1.0;nodeCoord[7]=10.0;
    nodeCoord[8]=10.0;nodeCoord[9]=10.0;
    nodeCoord[10]=15.0;nodeCoord[11]=10.0;
    nodeCoord[12]=1.0;nodeCoord[13]=15.0;
    nodeCoord[14]=10.0;nodeCoord[15]=15.0;
    nodeCoord[16]=15.0;nodeCoord[17]=15.0;
/*    nodeCoord[0]=1.0;nodeCoord[1]=1.0;
    nodeCoord[2]=1.5;nodeCoord[3]=1.0;
    nodeCoord[4]=2.0;nodeCoord[5]=1.0;
    nodeCoord[6]=1.0;nodeCoord[7]=1.5;
    nodeCoord[8]=1.5;nodeCoord[9]=1.5;
    nodeCoord[10]=2.0;nodeCoord[11]=1.5;
    nodeCoord[12]=1.0;nodeCoord[13]=2.0;
    nodeCoord[14]=1.5;nodeCoord[15]=2.0;
    nodeCoord[16]=2.0;nodeCoord[17]=2.0;*/
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
    // ESMF_MESHELEMTYPE_QUAD=9
    // ESMF_MESHELEMTYPE_TRI=5
    elemConn[0]=1;elemConn[1]=2;elemConn[2]=5;elemConn[3]=4;

    elemConn[4]=2;elemConn[5]=3;elemConn[6]=5;
    elemConn[7]=3;elemConn[8]=6;elemConn[9]=5;
    elemConn[10]=4;elemConn[11]=5;elemConn[12]=8;elemConn[13]=7;
    elemConn[14]=5;elemConn[15]=6;elemConn[16]=9;elemConn[17]=8;
  }
  else if (localPet == 1) {
    num_node = 6;
    num_elem = 2;

    nodeId    = (int *) malloc (num_node * sizeof (int));
    nodeCoord = (double *) malloc (2*num_node * sizeof (double));
    nodeOwner = (int *) malloc (num_node * sizeof (int));
    elemId   = (int *) malloc (num_elem * sizeof (int));
    elemType = (int *) malloc (num_elem * sizeof (int));
    elemConn = (int *) malloc (4*num_elem * sizeof (int));

    nodeId[0]=3;
    nodeId[1]=4;
    nodeId[2]=7;
    nodeId[3]=8;
    nodeId[4]=11;
    nodeId[5]=12;
    nodeCoord[0]=15.0;nodeCoord[1]=1.0;
    nodeCoord[2]=20.0;nodeCoord[3]=1.0;
    nodeCoord[4]=15.0;nodeCoord[5]=10.0;
    nodeCoord[6]=20.0;nodeCoord[7]=10.0;
    nodeCoord[8]=15.0;nodeCoord[9]=15.0;
    nodeCoord[10]=20.0;nodeCoord[11]=15.0;
    /*nodeCoord[0]=2.0;nodeCoord[1]=1.0;
    nodeCoord[2]=3.0;nodeCoord[3]=1.0;
    nodeCoord[4]=2.0;nodeCoord[5]=1.5;
    nodeCoord[6]=3.0;nodeCoord[7]=1.5;
    nodeCoord[8]=2.0;nodeCoord[9]=2.0;
    nodeCoord[10]=3.0;nodeCoord[11]=2.0;*/
    nodeOwner[0]=0;
    nodeOwner[1]=1;
    nodeOwner[2]=0;
    nodeOwner[3]=1;
    nodeOwner[4]=0;
    nodeOwner[5]=1;
    elemId[0]=4;
    elemId[1]=7;
    // ESMF_MESHELEMTYPE_QUAD=9
    // ESMF_MESHELEMTYPE_TRI=5
    elemType[0]=ESMC_MESHELEMTYPE_QUAD;
    elemType[1]=ESMC_MESHELEMTYPE_QUAD;
    elemConn[0]=1;elemConn[1]=2;elemConn[2]=4;elemConn[3]=3;
    elemConn[4]=3;elemConn[5]=4;elemConn[6]=6;elemConn[7]=5;
  }
  else if (localPet == 2) {
    num_node = 6;
    num_elem = 2;

    nodeId    = (int *) malloc (num_node * sizeof (int));
    nodeCoord = (double *) malloc (2*num_node * sizeof (double));
    nodeOwner = (int *) malloc (num_node * sizeof (int));
    elemId   = (int *) malloc (num_elem * sizeof (int));
    elemType = (int *) malloc (num_elem * sizeof (int));
    elemConn = (int *) malloc (4*num_elem * sizeof (int));

    nodeId[0]=9;
    nodeId[1]=10;
    nodeId[2]=11;
    nodeId[3]=13;
    nodeId[4]=14;
    nodeId[5]=15;
    nodeCoord[0]=1.0;nodeCoord[1]=15.0;
    nodeCoord[2]=10.0;nodeCoord[3]=15.0;
    nodeCoord[4]=15.0;nodeCoord[5]=15.0;
    nodeCoord[6]=1.0;nodeCoord[7]=20.0;
    nodeCoord[8]=10.0;nodeCoord[9]=20.0;
    nodeCoord[10]=15.0;nodeCoord[11]=20.0;
    /*nodeCoord[0]=1.0;nodeCoord[1]=2.0;
    nodeCoord[2]=1.5;nodeCoord[3]=2.0;
    nodeCoord[4]=2.0;nodeCoord[5]=2.0;
    nodeCoord[6]=1.0;nodeCoord[7]=3.0;
    nodeCoord[8]=1.5;nodeCoord[9]=3.0;
    nodeCoord[10]=2.0;nodeCoord[11]=3.0;*/
    nodeOwner[0]=0;
    nodeOwner[1]=0;
    nodeOwner[2]=0;
    nodeOwner[3]=2;
    nodeOwner[4]=2;
    nodeOwner[5]=2;
    elemId[0]=8;
    elemId[1]=9;
    // ESMF_MESHELEMTYPE_QUAD=9
    // ESMF_MESHELEMTYPE_TRI=5
    elemType[0]=ESMC_MESHELEMTYPE_QUAD;
    elemType[1]=ESMC_MESHELEMTYPE_QUAD;
    elemConn[0]=1;elemConn[1]=2;elemConn[2]=5;elemConn[3]=4;
    elemConn[4]=2;elemConn[5]=3;elemConn[6]=6;elemConn[7]=5;
  }
  else if (localPet == 3) {
    num_node = 4;
    num_elem = 1;

    nodeId    = (int *) malloc (num_node * sizeof (int));
    nodeCoord = (double *) malloc (2*num_node * sizeof (double));
    nodeOwner = (int *) malloc (num_node * sizeof (int));
    elemId   = (int *) malloc (num_elem * sizeof (int));
    elemType = (int *) malloc (num_elem * sizeof (int));
    elemConn = (int *) malloc (4*num_elem * sizeof (int));

    nodeId[0]=11;
    nodeId[1]=12;
    nodeId[2]=15;
    nodeId[3]=16;
    nodeCoord[0]=15.0;nodeCoord[1]=15.0;
    nodeCoord[2]=20.0;nodeCoord[3]=15.0;
    nodeCoord[4]=15.0;nodeCoord[5]=20.0;
    nodeCoord[6]=20.0;nodeCoord[7]=20.0;
    /*nodeCoord[0]=2.0;nodeCoord[1]=2.0;
    nodeCoord[2]=3.0;nodeCoord[3]=2.0;
    nodeCoord[4]=2.0;nodeCoord[5]=3.0;
    nodeCoord[6]=3.0;nodeCoord[7]=3.0;*/
    nodeOwner[0]=0;
    nodeOwner[1]=1;
    nodeOwner[2]=2;
    nodeOwner[3]=3;
    elemId[0]=10;
    // ESMF_MESHELEMTYPE_QUAD=9
    // ESMF_MESHELEMTYPE_TRI=5
    elemType[0]=ESMC_MESHELEMTYPE_QUAD;
    elemConn[0]=1;elemConn[1]=2;elemConn[2]=4;elemConn[3]=3;
  }

  //----------------------------------------------------------------------------
  //EX_UTest_Multi_Proc_Only
  strcpy(name, "MeshCreate");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  dstmesh = ESMC_MeshCreate(pdim,sdim,&coordsys,&rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest_Multi_Proc_Only
  strcpy(name, "MeshAddNodes");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_MeshAddNodes(dstmesh, num_node, nodeId, nodeCoord, nodeOwner);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest_Multi_Proc_Only
  strcpy(name, "MeshAddElements");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_MeshAddElements(dstmesh, num_elem, elemId, elemType, elemConn, 
                            NULL,NULL, NULL);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
 
  //----------------------------------------------------------------------------
  //EX_UTest_Multi_Proc_Only
  strcpy(name, "MeshGetLocalNodeCount");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  int num_node_out;
  rc = ESMC_MeshGetLocalNodeCount(dstmesh, &num_node_out);
  ESMC_Test((rc==ESMF_SUCCESS) && num_node==num_node_out, 
            name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  printf("num_node = %d\nnum_node_out=%d\n", num_node, num_node_out);

  //----------------------------------------------------------------------------
  //EX_UTest_Multi_Proc_Only
  strcpy(name, "MeshGetLocalElementCount");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  int num_elem_out;
  rc = ESMC_MeshGetLocalElementCount(dstmesh, &num_elem_out);
  ESMC_Test((rc==ESMF_SUCCESS) && num_elem==num_elem_out, 
            name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  printf("num_elem = %d\nnum_elem_out=%d\n", num_elem, num_elem_out);


  //----------------------------------------------------------------------------
  //---------------------- FIELD CREATION --------------------------------------
  //----------------------------------------------------------------------------


  //----------------------------------------------------------------------------
  //EX_UTest_Multi_Proc_Only
  strcpy(name, "Create ESMC_Field object from a Grid via TypeKind");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#ifdef corner
  srcfield = ESMC_FieldCreateGridTypeKind(srcgrid, ESMC_TYPEKIND_R8, 
    ESMC_STAGGERLOC_CORNER, NULL, NULL, NULL, "srcfield", &rc);
#endif
#ifdef center
  srcfield = ESMC_FieldCreateGridTypeKind(srcgrid, ESMC_TYPEKIND_R8, 
    ESMC_STAGGERLOC_CENTER, NULL, NULL, NULL, "srcfield", &rc);
#endif
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest_Multi_Proc_Only
  strcpy(name, "Create ESMC_Field object from a Mesh via TypeKind");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  dstfield = ESMC_FieldCreateMeshTypeKind(dstmesh, 
    ESMC_TYPEKIND_R8, ESMC_MESHLOC_NODE, NULL, NULL, NULL, "dstfield", &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //EX_UTest_Multi_Proc_Only
  strcpy(name, "Create ESMC_Field object from a Mesh via TypeKind");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  exactfield = ESMC_FieldCreateMeshTypeKind(dstmesh, 
    ESMC_TYPEKIND_R8, ESMC_MESHLOC_NODE, NULL, NULL, NULL, "dstfield", &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //-------------------------- REGRIDDING --------------------------------------
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest_Multi_Proc_Only
  strcpy(name, "Get a void * C pointer to data from ESMC_Field object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  double * srcfieldptr = (double *)ESMC_FieldGetPtr(srcfield, 0, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

#ifdef corner
  // define analytic field on source field
  p = 0;
  for (int i1=exLB_corner[1]; i1<=exUB_corner[1]; ++i1) {
    for (int i0=exLB_corner[0]; i0<=exUB_corner[0]; ++i0) {
      x = gridXCorner[p];
      y = gridYCorner[p];
      //srcfieldptr[p] = 20.0;
      srcfieldptr[p] = 20.0+x+y;
      ++p;
    }
  }
#endif
#ifdef center
  // define analytic field on source field
  p = 0;
  for (int i1=exLB_center[1]; i1<=exUB_center[1]; ++i1) {
    for (int i0=exLB_center[0]; i0<=exUB_center[0]; ++i0) {
      x = gridXCenter[p];
      y = gridYCenter[p];
      //srcfieldptr[p] = 20.0;
      srcfieldptr[p] = 20.0+x+y;
      ++p;
    }
  }
#endif
  
  //----------------------------------------------------------------------------
  //EX_UTest_Multi_Proc_Only
  strcpy(name, "Get a void * C pointer to data from ESMC_Field object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  double * dstfieldptr = (double *)ESMC_FieldGetPtr(dstfield, 0, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest_Multi_Proc_Only
  strcpy(name, "Get a void * C pointer to data from ESMC_Field object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  double * exactptr = (double *)ESMC_FieldGetPtr(exactfield, 0, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest_Multi_Proc_Only
  strcpy(name, "Get the ownedNode count from ESMC_Mesh object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  int num_onode;
  rc = ESMC_MeshGetOwnedNodeCount(dstmesh, &num_onode);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  // initialize destination field
  for(int i=0; i<num_onode; ++i)
    dstfieldptr[i] = 0.0;

  // initialize destination field
  p = 0;
  for(int i=0; i<num_node; ++i) {
    if (nodeOwner[i] == localPet) {
      x = nodeCoord[2*i];
      y = nodeCoord[2*i+1];
      exactptr[p] = 20.0 + x + y;
      p++;
    }
  }

  //----------------------------------------------------------------------------
  //EX_UTest_Multi_Proc_Only
  strcpy(name, "Create an ESMC_RouteHandle via ESMC_FieldRegridStore()");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  ESMC_RegridMethod_Flag regridmethod = ESMC_REGRIDMETHOD_BILINEAR;
  ESMC_UnmappedAction_Flag unmappedaction = ESMC_UNMAPPEDACTION_ERROR;
  rc = ESMC_FieldRegridStore(srcfield, dstfield, NULL, NULL, &routehandle, 
                             &regridmethod, NULL, NULL, NULL, NULL, &unmappedaction,
                             NULL, NULL, NULL);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest_Multi_Proc_Only
  strcpy(name, "Execute ESMC_FieldRegrid()");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_FieldRegrid(srcfield, dstfield, routehandle, NULL);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest_Multi_Proc_Only
  strcpy(name, "Release an ESMC_RouteHandle via ESMC_FieldRegridRelease()");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_FieldRegridRelease(&routehandle);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //-------------------------- REGRID VALIDATION -------------------------------
  //----------------------------------------------------------------------------
  //EX_UTest_Multi_Proc_Only
  strcpy(name, "Regridding Validation");
  strcpy(failMsg, "Did not have acceptable accuracy");

  bool correct = true;
  // check destination field against analytic field
  for(int i=0; i<num_onode; ++i) {
    tol = .0001;
    if (ESMC_dabs(dstfieldptr[i]-exactptr[i]) > tol) {
      printf("PET%d: dstfieldptr[%d]:%f /= %f\n", 
             localPet, i, dstfieldptr[i], exactptr[i]);
      correct=false;
    }
  }
  ESMC_Test((correct==true), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  free(nodeId);
  free(nodeCoord);
  free(nodeOwner);
  free(elemId);
  free(elemType);
  free(elemConn);

  //----------------------------------------------------------------------------
  //EX_UTest_Multi_Proc_Only
  strcpy(name, "Destroy ESMC_Field object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_FieldDestroy(&srcfield);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest_Multi_Proc_Only
  strcpy(name, "Destroy ESMC_Field object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_FieldDestroy(&dstfield);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest_Multi_Proc_Only
  strcpy(name, "Destroy ESMC_Grid object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_GridDestroy(&srcgrid);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest_Multi_Proc_Only
  strcpy(name, "Destroy ESMC_Mesh object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_MeshDestroy(&dstmesh);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
#endif

  //----------------------------------------------------------------------------
  ESMC_TestEnd(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  return 0;
}
