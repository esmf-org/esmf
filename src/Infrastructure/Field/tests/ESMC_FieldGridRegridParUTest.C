// $Id: ESMC_FieldGridRegridParUTest.C,v 1.1 2012/03/15 19:24:15 rokuingh Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2012, University Corporation for Atmospheric Research, 
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

//==============================================================================
//BOP
// !PROGRAM: ESMC_FieldRegridUTest - Check ESMC_FieldRegrid functionality
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
  int *gridToFieldMap, *ungriddedLBound, *ungriddedUBound;
  ESMC_InterfaceInt i_gridToFieldMap, i_ungriddedLBound, i_ungriddedUBound;
  int *gridToFieldMap2, *ungriddedLBound2, *ungriddedUBound2;
  ESMC_InterfaceInt i_gridToFieldMap2, i_ungriddedLBound2, i_ungriddedUBound2;
  ESMC_Field srcfield, dstfield;

  // Grid variables
  ESMC_Grid srcgrid;
  int dimcount = 2;
  int *maxIndex;
  ESMC_InterfaceInt i_maxIndex;
  int p;

  // Mesh variables
  int pdim=2;
  int sdim=2;
  ESMC_Mesh dstmesh;
  int num_elem, num_node;

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
  //EX_disable_UTest_Multi_Proc_Only
  // Create a Grid
  double ub_x, lb_x, max_x, min_x, cellwidth_x, cellcenter_x;
  double ub_y, lb_y, max_y, min_y, cellwidth_y, cellcenter_y;
  ub_x = 9;
  ub_y = 9;
  lb_x = 1;
  lb_y = 1;
  max_x = 4;
  max_y = 4;
  min_x = 0;
  min_y = 0;
  cellwidth_x = (max_x-min_x)/(ub_x-lb_x);
  cellwidth_y = (max_y-min_y)/(ub_y-lb_y);

  cellcenter_x = cellwidth_x/double(2);
  cellcenter_y = cellwidth_y/double(2);

  maxIndex = (int *)malloc(dimcount*sizeof(int));
  maxIndex[0] = int(ub_x);
  maxIndex[1] = int(ub_y);
  i_maxIndex = ESMC_InterfaceIntCreate(maxIndex, dimcount, &rc);

  //----------------------------------------------------------------------------
  //EX_disable_UTest_Multi_Proc_Only
  strcpy(name, "GridCreate");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  srcgrid = ESMC_GridCreateNoPeriDim(i_maxIndex, ESMC_COORDSYS_CART,
                                         ESMC_TYPEKIND_R8, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);

  // free memory
  free(maxIndex);
  ESMC_InterfaceIntDestroy(&i_maxIndex);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //  GridAddCoord to srcgrid
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_disable_UTest_Multi_Proc_Only
  strcpy(name, "GridAddCoord");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_GridAddCoord(srcgrid, ESMC_STAGGERLOC_CENTER);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_disable_UTest_Multi_Proc_Only
  // get and fill first coord array and computational bounds
  int *exLBound = (int *)malloc(dimcount*sizeof(int));
  int *exUBound = (int *)malloc(dimcount*sizeof(int));

  strcpy(name, "GridGetCoord - X");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  double *gridXCoord = (double *)ESMC_GridGetCoord(srcgrid, 1,
                                                   ESMC_STAGGERLOC_CENTER,
                                                   exLBound, exUBound, &rc);

  p = 0;
  for (int i1=exLBound[1]; i1<=exUBound[1]; ++i1) {
    for (int i0=exLBound[0]; i0<=exUBound[0]; ++i0) {
      gridXCoord[p]=(double)(i0)*cellwidth_x - cellcenter_x;
      /*printf("PET%d - set gridXCoord[%d] = %f (%f)\n", localPet, p, 
        (double)(i0)*cellwidth_x - cellcenter_x, gridXCoord[p]);*/
      ++p;
    }
  }

  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_disable_UTest_Multi_Proc_Only
  // get and fill second coord array
  strcpy(name, "GridGetCoord - Y");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  double *gridYCoord = (double *)ESMC_GridGetCoord(srcgrid, 2,
                                                   ESMC_STAGGERLOC_CENTER,
                                                   NULL, NULL, &rc);

  p = 0;
  for (int i1=exLBound[1]; i1<=exUBound[1]; ++i1) {
    for (int i0=exLBound[0]; i0<=exUBound[0]; ++i0) {
      gridYCoord[p]=(double)(i1)*cellwidth_y - cellcenter_y;
      /*printf("PET%d - set gridYCoord[%d] = %f (%f)\n", localPet, p, 
        (double)(i1)*cellwidth_y - cellcenter_y, gridYCoord[p]);*/
      ++p;
    }
  }

  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------



  //              Destination Mesh
  // 
  //
  //  3.0   13 ------ 14 ------ 15     [15] ----------- 16    
  //        |         |         |       |               |
  //        |         |         |       |               |
  //        |    8    |    9    |       |       10      |
  //        |         |         |       |               |
  //        |         |         |       |               |
  //  2.0  [9] ----- [10] ---- [11]    [11] ---------- [12]
  //                                                          
  //       1.0       1.5       2.0     2.0             3.0 
  //      
  //                PET 2                      PET 3
  //      
  //      
  //  2.0   9 ------- 10 ------ 11     [11] ----------- 12      
  //        |         |         |       |               |
  //        |    5    |    6    |       |       7       |
  //        |         |         |       |               |
  //  1.5   5 ------- 6 ------- 7      [7] -----------  8
  //        |         |  \   3  |       |               |
  //        |    1    |    \    |       |       4       |
  //        |         | 2    \  |       |               |
  //  1.0   1 ------- 2 ------- 3      [3] ------------ 4
  //                                                
  //       1.0       1.5       2.0     2.0             3.0 
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
    elemType[0]=9;
    elemType[1]=5;
    elemType[2]=5;
    elemType[3]=9;
    elemType[4]=9;
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
    // ESMF_MESHELEMTYPE_QUAD=9
    // ESMF_MESHELEMTYPE_TRI=5
    elemType[0]=9;
    elemType[1]=9;
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
    // ESMF_MESHELEMTYPE_QUAD=9
    // ESMF_MESHELEMTYPE_TRI=5
    elemType[0]=9;
    elemType[1]=9;
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
    nodeCoord[0]=2.0;nodeCoord[1]=2.0;
    nodeCoord[2]=3.0;nodeCoord[3]=2.0;
    nodeCoord[4]=2.0;nodeCoord[5]=3.0;
    nodeCoord[6]=3.0;nodeCoord[7]=3.0;
    nodeOwner[0]=0;
    nodeOwner[1]=1;
    nodeOwner[2]=2;
    nodeOwner[3]=3;
    elemId[0]=10;
    // ESMF_MESHELEMTYPE_QUAD=9
    // ESMF_MESHELEMTYPE_TRI=5
    elemType[0]=9;
    elemConn[0]=1;elemConn[1]=2;elemConn[2]=4;elemConn[3]=3;
  }

  //----------------------------------------------------------------------------
  //EX_disable_UTest_Multi_Proc_Only
  strcpy(name, "MeshCreate");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  dstmesh = ESMC_MeshCreate(pdim,sdim,&rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_disable_UTest_Multi_Proc_Only
  strcpy(name, "MeshAddNodes");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_MeshAddNodes(dstmesh, num_node, nodeId, nodeCoord, nodeOwner);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_disable_UTest_Multi_Proc_Only
  strcpy(name, "MeshAddElements");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_MeshAddElements(dstmesh, num_elem, elemId, elemType, elemConn);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
 
  //----------------------------------------------------------------------------
  //EX_disable_UTest_Multi_Proc_Only
  strcpy(name, "MeshGetLocalNodeCount");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  int num_node_out;
  rc = ESMC_MeshGetLocalNodeCount(dstmesh, &num_node_out);
  ESMC_Test((rc==ESMF_SUCCESS) && num_node==num_node_out, 
            name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  printf("num_node = %d\nnum_node_out=%d\n", num_node, num_node_out);

  //----------------------------------------------------------------------------
  //EX_disable_UTest_Multi_Proc_Only
  strcpy(name, "MeshGetLocalElementCount");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  int num_elem_out;
  rc = ESMC_MeshGetLocalElementCount(dstmesh, &num_elem_out);
  ESMC_Test((rc==ESMF_SUCCESS) && num_elem==num_elem_out, 
            name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  printf("num_elem = %d\nnum_elem_out=%d\n", num_elem, num_elem_out);

/*
  free(nodeId);
  free(nodeCoord);
  free(nodeOwner);
  free(elemId);
  free(elemType);
  free(elemConn);
*/
  //----------------------------------------------------------------------------
  //---------------------- FIELD CREATION --------------------------------------
  //----------------------------------------------------------------------------


  //----------------------------------------------------------------------------
  //EX_disable_UTest_Multi_Proc_Only
  // Set the arrayspec
  strcpy(name, "ArraySpecSet");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_ArraySpecSet(&arrayspec, 2, ESMC_TYPEKIND_R8);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
    
  //----------------------------------------------------------------------------
  //EX_disable_UTest_Multi_Proc_Only
  strcpy(name, "Set up gridToFieldMap");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  gridToFieldMap = (int *)malloc(sizeof(int));
  gridToFieldMap[0] = 1;
  i_gridToFieldMap = ESMC_InterfaceIntCreate(gridToFieldMap, 1, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //EX_disable_UTest_Multi_Proc_Only
  strcpy(name, "Set up ungriddedLBound");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  ungriddedLBound = (int *)malloc(2*sizeof(int));
  ungriddedLBound[0] = 0;
  ungriddedLBound[1] = 0;
  i_ungriddedLBound = ESMC_InterfaceIntCreate(ungriddedLBound, 0, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_disable_UTest_Multi_Proc_Only
  strcpy(name, "Set up ungriddedUBound");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  ungriddedUBound = (int *)malloc(2*sizeof(int));
  ungriddedUBound[0] = 0;
  ungriddedUBound[1] = 0;
  i_ungriddedUBound = ESMC_InterfaceIntCreate(ungriddedUBound, 0, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //EX_disable_UTest_Multi_Proc_Only
  strcpy(name, "Create ESMC_Field object from a Grid via TypeKind");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  srcfield = ESMC_FieldCreateGridTypeKind(srcgrid, ESMC_TYPEKIND_R8, 
    ESMC_STAGGERLOC_CENTER, NULL, NULL, NULL, "srcfield", &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_disable_UTest_Multi_Proc_Only
  strcpy(name, "Create ESMC_Field object from a Mesh via TypeKind");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  dstfield = ESMC_FieldCreateMeshTypeKind(dstmesh, 
    ESMC_TYPEKIND_R8, ESMC_MESHLOC_NODE,
    i_gridToFieldMap, i_ungriddedLBound,
    i_ungriddedUBound, "dstfield", &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  

//  rc = ESMC_FieldPrint(srcfield);
//  rc = ESMC_FieldPrint(dstfield);

  //----------------------------------------------------------------------------
  //-------------------------- REGRIDDING --------------------------------------
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_disable_UTest_Multi_Proc_Only
  strcpy(name, "Get a void * C pointer to data from ESMC_Field object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  double * srcfieldptr = (double *)ESMC_FieldGetPtr(srcfield, 0, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  // define analytic field on source field
  p = 0;
  double x,y;
  for (int i1=exLBound[1]; i1<=exUBound[1]; ++i1) {
    for (int i0=exLBound[0]; i0<=exUBound[0]; ++i0) {
      x = gridXCoord[p];
      y = gridYCoord[p];
      srcfieldptr[p] = 20.0+x+y;
      ++p;
    }
  }

  
  //----------------------------------------------------------------------------
  //EX_disable_UTest_Multi_Proc_Only
  strcpy(name, "Get a void * C pointer to data from ESMC_Field object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  double * dstfieldptr = (double *)ESMC_FieldGetPtr(dstfield, 0, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  // initialize destination field
  {
    int i;
    for(i=0;i<num_node;++i)
      dstfieldptr[i] = 0.0;
  }

  //----------------------------------------------------------------------------
  //EX_disable_UTest_Multi_Proc_Only
  strcpy(name, "Create an ESMC_RouteHandle via ESMC_FieldRegridStore()");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_FieldRegridStore(srcfield, dstfield, &routehandle, 
                        ESMC_REGRIDMETHOD_BILINEAR, ESMC_UNMAPPEDACTION_ERROR);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_disable_UTest_Multi_Proc_Only
  strcpy(name, "Execute ESMC_FieldRegrid()");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_FieldRegrid(srcfield, dstfield, routehandle);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_disable_UTest_Multi_Proc_Only
  strcpy(name, "Execute ESMC_RouteHandlePrint()");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_RouteHandlePrint(routehandle);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_disable_UTest_Multi_Proc_Only
  strcpy(name, "Release an ESMC_RouteHandle via ESMC_FieldRegridRelease()");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_FieldRegridRelease(&routehandle);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  

  //----------------------------------------------------------------------------
  //-------------------------- REGRID VALIDATION -------------------------------
  //----------------------------------------------------------------------------
  //EX_disable_UTest_Multi_Proc_Only
  strcpy(name, "Regridding Validation");
  strcpy(failMsg, "Did not have acceptable accuracy");

  bool correct = true;
  {
    double x,y;
    int i;
    // 2. check destination field against source field
    for(i=0;i<num_node;++i) {
      x=nodeCoord[2*i];
      y=nodeCoord[2*i+1];
      // if error is too big report an error
      if ( abs((long)( dstfieldptr[i]-(x+y+20.0)) ) > 0.0001) {
        printf("dstfieldptr[%d] = %f\n and it should be = %f\n", i, dstfieldptr[i], x+y+20.0);
        correct=false;
      }
    }
  }
  ESMC_Test((correct==true), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------


  //----------------------------------------------------------------------------
  //EX_disable_UTest_Multi_Proc_Only
  strcpy(name, "Destroy ESMC_Field object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  //rc = ESMC_FieldDestroy(&srcfield);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_disable_UTest_Multi_Proc_Only
  strcpy(name, "Destroy ESMC_Field object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  //rc = ESMC_FieldDestroy(&dstfield);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_disable_UTest_Multi_Proc_Only
  strcpy(name, "Destroy ESMC_Grid object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  //rc = ESMC_GridDestroy(&srcgrid);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_disable_UTest_Multi_Proc_Only
  strcpy(name, "Destroy ESMC_Mesh object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  //rc = ESMC_MeshDestroy(&dstmesh);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
/*
  ESMC_InterfaceIntDestroy(&i_gridToFieldMap);
  free(gridToFieldMap);
  ESMC_InterfaceIntDestroy(&i_ungriddedLBound);
  free(ungriddedLBound);
  ESMC_InterfaceIntDestroy(&i_ungriddedUBound);
  free(ungriddedUBound);
*/
#endif

  //----------------------------------------------------------------------------
  ESMC_TestEnd(result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  return 0;
}
