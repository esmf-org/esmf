// $Id: ESMC_FieldGridRegridCsrvUTest.C,v 1.9 2012/04/04 16:58:22 rokuingh Exp $
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
#include <math.h>

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

  // VM variables
  int localPet, petCount;
  ESMC_VM vm;
  
  // Field variables
  ESMC_RouteHandle routehandle;
  ESMC_Field srcfield, dstfield;

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
  double x, y, exact, tol;
  int p;

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
  //EX_UTest
  // Create a Grid
  double ub_x, lb_x, max_x, min_x, cellwidth_x;
  double ub_y, lb_y, max_y, min_y, cellwidth_y;
  ub_x = 4;
  ub_y = 4;
  lb_x = 0;
  lb_y = 0;
  max_x = 4;
  max_y = 4;
  min_x = 0;
  min_y = 0;

  cellwidth_x = (max_x-min_x)/(ub_x-lb_x);
  cellwidth_y = (max_y-min_y)/(ub_y-lb_y);

  maxIndex = (int *)malloc(dimcount*sizeof(int));
  maxIndex[0] = int(ub_x);
  maxIndex[1] = int(ub_y);
  i_maxIndex = ESMC_InterfaceIntCreate(maxIndex, dimcount, &rc);

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
  //EX_UTest
  strcpy(name, "GridAddCoord");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_GridAddCoord(srcgrid, ESMC_STAGGERLOC_CORNER);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "GridAddItem");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#ifdef masking
  rc = ESMC_GridAddCoord(srcgrid, ESMC_STAGGERLOC_CENTER);
  rc = ESMC_GridAddItem(srcgrid, ESMC_GRIDITEM_MASK, ESMC_STAGGERLOC_CORNER);
#endif
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "GridGetItem - mask");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#ifdef masking
  int *mask = (int *)ESMC_GridGetItem(srcgrid, ESMC_GRIDITEM_MASK, 
                                            ESMC_STAGGERLOC_CORNER, &rc);
#endif
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  // get and fill first coord array and computational bounds
  int *exLBound = (int *)malloc(dimcount*sizeof(int));
  int *exUBound = (int *)malloc(dimcount*sizeof(int));

  strcpy(name, "GridGetCoord - X");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  double *gridXCoord = (double *)ESMC_GridGetCoord(srcgrid, 1,
                                                   ESMC_STAGGERLOC_CORNER,
                                                   exLBound, exUBound, &rc);

  p = 0;
  for (int i1=exLBound[1]; i1<=exUBound[1]; ++i1) {
    for (int i0=exLBound[0]; i0<=exUBound[0]; ++i0) {
      if (i0==exLBound[0]) gridXCoord[p] = lb_x;
      gridXCoord[p]=(double)(i0-1)*cellwidth_x;
      ++p;
    }
  }

  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  printf("exLBounds = [%d,%d]\n", exLBound[0], exLBound[1]);
  printf("exUBounds = [%d,%d]\n", exUBound[0], exUBound[1]);

  //----------------------------------------------------------------------------
  //EX_UTest
  // get and fill second coord array
  strcpy(name, "GridGetCoord - Y");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  double *gridYCoord = (double *)ESMC_GridGetCoord(srcgrid, 2,
                                                   ESMC_STAGGERLOC_CORNER,
                                                   NULL, NULL, &rc);

  p = 0;
  for (int i1=exLBound[1]; i1<=exUBound[1]; ++i1) {
    for (int i0=exLBound[0]; i0<=exUBound[0]; ++i0) {
      if (i1==exLBound[1]) gridYCoord[p] = lb_y;
      gridYCoord[p]=(double)(i1-1)*cellwidth_y;
      ++p;
    }
  }

  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

#ifdef masking
  //----------------------------------------------------------------------------
  // get center stagger coordinate bounds
  int *exLBound_center = (int *)malloc(dimcount*sizeof(int));
  int *exUBound_center = (int *)malloc(dimcount*sizeof(int));
  double *dummy = (double *)ESMC_GridGetCoord(srcgrid, 1,
                                              ESMC_STAGGERLOC_CENTER,
                                              exLBound_center, 
                                              exUBound_center, &rc);
  //----------------------------------------------------------------------------

  printf("exLBounds_center = [%d,%d]\n", exLBound_center[0], exLBound_center[1]);
  printf("exUBounds_center = [%d,%d]\n", exUBound_center[0], exUBound_center[1]);

  // set the masking
  p = 0;
/*
  for (int i1=exLBound_center[1]; i1<=exUBound_center[1]; ++i1) {
    for (int i0=exLBound_center[0]; i0<=exUBound_center[0]; ++i0) {
*/
  for (int i1=exLBound[1]; i1<=exUBound[1]; ++i1) {
    for (int i0=exLBound[0]; i0<=exUBound[0]; ++i0) {
      if (i0 == 2 ) {
        mask[p] = 1;
        printf("Masked value at [%f,%f]\n", gridXCoord[p], gridYCoord[p]);
      } else mask[p] = 0;
      ++p;
    }
  }
#endif




  //              Destination Mesh
  // 
  //
  //  3.0   13 -------14 --------15--------16
  //        |         |          |         |
  //        |    7    |    8     |   9     |
  //        |         |          |         |
  //  2.5   9 ------- 10 --------11--------12
  //        |         |          |         |
  //        |    4    |    5     |   6     |
  //        |         |          |         |
  //  1.5   5 ------- 6 -------- 7-------- 8
  //        |         |          |         |
  //        |    1    |    2     |   3     |
  //        |         |          |         |
  //  1.0   1 ------- 2 -------- 3-------- 4
  //      
  //       1.0       1.5        2.5       3.0
  //
  //      Node Ids at corners
  //      Element Ids in centers
  //
  //
  //      ( Everything owned by PET 0) 
  // 


  num_elem = 9;
  num_node = 16;

  int nodeId [] ={1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16};
  double nodeCoord [] ={1.0,1.0, 1.5,1.0, 2.5,1.0, 3.0,1.0,
               1.0,1.5, 1.5,1.5, 2.5,1.5, 3.0,1.5,
               1.0,2.5, 1.5,2.5, 2.5,2.5, 3.0,2.5,
               1.0,3.0, 1.5,3.0, 2.5,3.0, 3.0,3.0};
  int nodeOwner [] ={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
  int elemId [] ={1,2,3,4,5,6,7,8,9};
  // ESMF_MESHELEMTYPE_QUAD=9
  int elemType [] = {9,9,9,9,9,9,9,9,9};
  int elemConn [] ={1,2,6,5,
              2,3,7,6,
              3,4,8,7,
              5,6,10,9,
              6,7,11,10,
              7,8,12,11,
              9,10,14,13,
              10,11,15,14,
              11,12,16,15};

  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "MeshCreate");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  dstmesh = ESMC_MeshCreate(pdim,sdim,&rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "MeshAddNodes");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_MeshAddNodes(dstmesh, num_node, nodeId, nodeCoord, nodeOwner);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "MeshAddElements");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_MeshAddElements(dstmesh, num_elem, elemId, elemType, elemConn, NULL);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
 
  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "MeshGetLocalNodeCount");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  int num_node_out;
  rc = ESMC_MeshGetLocalNodeCount(dstmesh, &num_node_out);
  ESMC_Test((rc==ESMF_SUCCESS) && num_node==num_node_out, 
            name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  printf("num_node = %d\nnum_node_out=%d\n", num_node, num_node_out);

  //----------------------------------------------------------------------------
  //EX_UTest
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
  //EX_UTest
  strcpy(name, "Create ESMC_Field object from a Grid via TypeKind");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  srcfield = ESMC_FieldCreateGridTypeKind(srcgrid, ESMC_TYPEKIND_R8, 
    ESMC_STAGGERLOC_CENTER, NULL, NULL, NULL, "srcfield", &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "Create ESMC_Field object from a Mesh via TypeKind");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  dstfield = ESMC_FieldCreateMeshTypeKind(dstmesh, 
    ESMC_TYPEKIND_R8, ESMC_MESHLOC_ELEMENT, NULL, NULL, NULL, "dstfield", &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  

  //----------------------------------------------------------------------------
  //-------------------------- REGRIDDING --------------------------------------
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "Get a void * C pointer to data from ESMC_Field object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  float *srcfieldptr = (float *)ESMC_FieldGetPtr(srcfield, 0, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  // define analytic field on source field
  p = 0;
  for (int i1=exLBound[1]; i1<=exUBound[1]; ++i1) {
    for (int i0=exLBound[0]; i0<=exUBound[0]; ++i0) {
      x = gridXCoord[p];
      y = gridYCoord[p];
#ifdef masking
      if (mask[p] == 1)
        srcfieldptr[p] = 10000000;
      else
#endif
        srcfieldptr[p] = 20.0+y; // this maps all points
      //srcfieldptr[p] = 20.0+x+y; // this has 8 points that don't map
      //srcfieldptr[p] = 20.0+x;   // this has 8 points that don't map
      //srcfieldptr[p] = 20.0+pow(y,2); // this has 2 points that don't map
      ++p;
    }
  }

  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "Get a void * C pointer to data from ESMC_Field object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  float *dstfieldptr = (float *)ESMC_FieldGetPtr(dstfield, 0, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  // initialize destination field
  for(int i=0; i<num_elem; ++i)
    dstfieldptr[i] = 0.0;

  //----------------------------------------------------------------------------
  //EX_UTest
  int *maskValues = (int *)malloc(sizeof(int));
  maskValues[0] = 1;
  strcpy(name, "Create an InterfaceInt for maskValues in ESMC_FieldRegridStore()");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  ESMC_InterfaceInt i_maskValues = ESMC_InterfaceIntCreate(maskValues, 1, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "Create an ESMC_RouteHandle via ESMC_FieldRegridStore()");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#ifdef masking
  rc = ESMC_FieldRegridStore(srcfield, dstfield, &i_maskValues, NULL, &routehandle, 
#else
  rc = ESMC_FieldRegridStore(srcfield, dstfield, NULL, NULL, &routehandle, 
#endif
                        ESMC_REGRIDMETHOD_CONSERVE, ESMC_UNMAPPEDACTION_ERROR);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "Execute ESMC_FieldRegrid()");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_FieldRegrid(srcfield, dstfield, routehandle);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "Execute ESMC_RouteHandlePrint()");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_RouteHandlePrint(routehandle);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "Release an ESMC_RouteHandle via ESMC_FieldRegridRelease()");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_FieldRegridRelease(&routehandle);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  // Source Grid
  p = 0;
  printf("\nSource Grid coords: \n");
  for (int i1=exLBound[1]; i1<=exUBound[1]; ++i1) {
    for (int i0=exLBound[0]; i0<=exUBound[0]; ++i0) {
      x = gridXCoord[p];
      y = gridYCoord[p];
      printf("[%f,%f]\n", x, y); 
      ++p;
    }
  }
  printf("\n");
  
  // Destination Mesh
  printf("\nDestination Mesh coords: \n");
  for(int i=0; i<num_node; ++i) { 
    x=nodeCoord[2*i];
    y=nodeCoord[2*i+1];
    printf("[%f,%f]\n", x, y); 
  }
  printf("\n");


  //----------------------------------------------------------------------------
  //-------------------------- REGRID VALIDATION -------------------------------
  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "Regridding Validation");
  strcpy(failMsg, "Did not have acceptable accuracy");


  // check destination field against source field
  bool correct = true;
  for(int i=0; i<num_elem; ++i) {
    x=nodeCoord[2*i];
    y=nodeCoord[2*i+1];
    //exact = 20.0+x+y;
    //exact = 20.0+x;
    //exact = 20.0+pow(y,2);
    exact = 20.0+y;
#ifdef masking
    tol = 100;
#else
    tol = .0001;
#endif
    if ( abs((double)( dstfieldptr[i]-exact) ) > tol) {
      printf("dstfieldptr[%d]:\n%f /= %f\n", 
             i, dstfieldptr[i], exact);
      correct=false;
    }
  }
  ESMC_Test((correct==true), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------


  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "Destroy ESMC_Field object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_FieldDestroy(&srcfield);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "Destroy ESMC_Field object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_FieldDestroy(&dstfield);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "Destroy ESMC_Grid object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_GridDestroy(&srcgrid);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "Destroy ESMC_Mesh object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_MeshDestroy(&dstmesh);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

#endif
  //----------------------------------------------------------------------------
  ESMC_TestEnd(result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  return 0;
}
