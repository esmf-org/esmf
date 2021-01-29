// $Id$
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

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

// ESMF header
#include "ESMC.h"

// ESMF Test header
#include "ESMC_Test.h"

// must have gridmasking or meshmasking or grid, mesh, and bothmasking or nomasking
#define gridmasking
#define meshmasking
#define bothmasking
//#define nomasking

//==============================================================================
//BOP
// !PROGRAM: ESMC_FieldGridRegridCsrv2UTest - Check ESMC_FieldRegrid functionality
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
  ESMC_Field srcfield, dstfield, srcAreaField, dstAreaField, 
             srcFracField, dstFracField;

  // Grid variables
  ESMC_Grid grid;
  int dimcount = 2;
  int *maxIndex;
  ESMC_InterArrayInt i_maxIndex;

  // Mesh variables
  int pdim=2;
  int sdim=2;
  ESMC_Mesh mesh;
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
  rc = ESMC_InterArrayIntSet(&i_maxIndex, maxIndex, dimcount);

  strcpy(name, "GridCreate");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  ESMC_CoordSys_Flag coordsys = ESMC_COORDSYS_CART;
  ESMC_TypeKind_Flag typekind = ESMC_TYPEKIND_R8;
  grid = ESMC_GridCreateNoPeriDim(&i_maxIndex, &coordsys, &typekind, NULL, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);

  // free memory
  free(maxIndex);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //  GridAddCoord to grid
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "GridAddCoord");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_GridAddCoord(grid, ESMC_STAGGERLOC_CORNER);
  rc = ESMC_GridAddCoord(grid, ESMC_STAGGERLOC_CENTER);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "GridAddItem");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#ifdef gridmasking
  rc = ESMC_GridAddItem(grid, ESMC_GRIDITEM_MASK, ESMC_STAGGERLOC_CENTER);
#endif
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "GridGetItem - mask");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#ifdef gridmasking
  int *mask = (int *)ESMC_GridGetItem(grid, ESMC_GRIDITEM_MASK, 
                                            ESMC_STAGGERLOC_CENTER, NULL, &rc);
#endif
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  // CORNERS

  //----------------------------------------------------------------------------
  //EX_UTest
  // get and fill first coord array and computational bounds
  int *exLB_corner = (int *)malloc(dimcount*sizeof(int));
  int *exUB_corner = (int *)malloc(dimcount*sizeof(int));

  strcpy(name, "GridGetCoord - X");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  double *gridXCorner = (double *)ESMC_GridGetCoord(grid, 1,
                                                    ESMC_STAGGERLOC_CORNER, NULL,
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
  //EX_UTest
  // get and fill second coord array
  strcpy(name, "GridGetCoord - Y");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  double *gridYCorner = (double *)ESMC_GridGetCoord(grid, 2,
                                                    ESMC_STAGGERLOC_CORNER, NULL,
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
  //EX_UTest
  // get and fill first coord array and computational bounds
  int *exLB_center = (int *)malloc(dimcount*sizeof(int));
  int *exUB_center = (int *)malloc(dimcount*sizeof(int));

  strcpy(name, "GridGetCoord - X");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  double *gridXCenter = (double *)ESMC_GridGetCoord(grid, 1,
                                                    ESMC_STAGGERLOC_CENTER, NULL,
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
  //EX_UTest
  // get and fill second coord array
  strcpy(name, "GridGetCoord - Y");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  double *gridYCenter = (double *)ESMC_GridGetCoord(grid, 2,
                                                    ESMC_STAGGERLOC_CENTER, NULL,
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

#ifdef gridmasking
  // set the masking
  p = 0;
  for (int i1=exLB_center[1]; i1<=exUB_center[1]; ++i1) {
    for (int i0=exLB_center[0]; i0<=exUB_center[0]; ++i0) {
      if (i0 == 2 ) {
        mask[p] = 1;
        printf("Masked value at [%f,%f]\n", gridXCenter[p], gridYCenter[p]);
      } else mask[p] = 0;
      ++p;
    }
  }
#endif




  //              Destination Mesh
  // 
  //
  //  4.0   13 -------14 --------15--------16
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
  //  0.0   1 ------- 2 -------- 3-------- 4
  //      
  //       0.0       1.5        2.5       4.0
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
  double nodeCoord [] ={0.0,0.0, 1.5,0.0, 2.5,0.0, 4.0,0.0,
               0.0,1.5, 1.5,1.5, 2.5,1.5, 4.0,1.5,
               0.0,2.5, 1.5,2.5, 2.5,2.5, 4.0,2.5,
               0.0,4.0, 1.5,4.0, 2.5,4.0, 4.0,4.0};
  int nodeOwner [] ={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
  int elemId [] ={1,2,3,4,5,6,7,8,9};
  // ESMF_MESHELEMTYPE_QUAD=9
  int elemType [] = {ESMC_MESHELEMTYPE_QUAD,
                     ESMC_MESHELEMTYPE_QUAD,
                     ESMC_MESHELEMTYPE_QUAD,
                     ESMC_MESHELEMTYPE_QUAD,
                     ESMC_MESHELEMTYPE_QUAD,
                     ESMC_MESHELEMTYPE_QUAD,
                     ESMC_MESHELEMTYPE_QUAD,
                     ESMC_MESHELEMTYPE_QUAD,
                     ESMC_MESHELEMTYPE_QUAD};
  int elemConn [] ={1,2,6,5,
              2,3,7,6,
              3,4,8,7,
              5,6,10,9,
              6,7,11,10,
              7,8,12,11,
              9,10,14,13,
              10,11,15,14,
              11,12,16,15};
  int elemMask [] = {0,0,0,0,1,0,0,0,0};

  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "MeshCreate");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  mesh = ESMC_MeshCreate(pdim,sdim,&coordsys,&rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "MeshAddNodes");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_MeshAddNodes(mesh, num_node, nodeId, nodeCoord, nodeOwner);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "MeshAddElements");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#ifdef meshmasking
  rc = ESMC_MeshAddElements(mesh, num_elem, elemId, elemType, elemConn, 
                            elemMask, NULL, NULL);
#else
  rc = ESMC_MeshAddElements(mesh, num_elem, elemId, elemType, elemConn, NULL, 
                            NULL, NULL);
#endif
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
 
  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "MeshGetLocalNodeCount");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  int num_node_out;
  rc = ESMC_MeshGetLocalNodeCount(mesh, &num_node_out);
  ESMC_Test((rc==ESMF_SUCCESS) && num_node==num_node_out, 
            name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  printf("num_node = %d\nnum_node_out=%d\n", num_node, num_node_out);

  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "MeshGetLocalElementCount");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  int num_elem_out;
  rc = ESMC_MeshGetLocalElementCount(mesh, &num_elem_out);
  ESMC_Test((rc==ESMF_SUCCESS) && num_elem==num_elem_out, 
            name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  printf("num_elem = %d\nnum_elem_out=%d\n", num_elem, num_elem_out);

  // Mesh coordinates
  printf("\nSource Mesh corner coords: \n");
  for(int i=0; i<num_node; ++i) { 
    x = nodeCoord[2*i];
    y = nodeCoord[2*i+1];
    printf("[%f,%f]\n", x, y); 
  }
  printf("\n");

  // Grid corner coordinates
  p = 0;
  printf("\nDestination Grid corner coords: \n");
  for (int i1=exLB_corner[1]; i1<=exUB_corner[1]; ++i1) {
    for (int i0=exLB_corner[0]; i0<=exUB_corner[0]; ++i0) {
      x = gridXCorner[p];
      y = gridYCorner[p];
      printf("[%f,%f]\n", x, y); 
      ++p;
    }
  }
  printf("\n");
  
  // Grid center coordinates
  p = 0;
  printf("\nDestination Grid center coords: \n");
  for (int i1=exLB_center[1]; i1<=exUB_center[1]; ++i1) {
    for (int i0=exLB_center[0]; i0<=exUB_center[0]; ++i0) {
      x = gridXCenter[p];
      y = gridYCenter[p];
      printf("[%f,%f]\n", x, y); 
      ++p;
    }
  }
  printf("\n");
  
  //----------------------------------------------------------------------------
  //---------------------- FIELD CREATION --------------------------------------
  //----------------------------------------------------------------------------


  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "Create ESMC_Field object from a Grid via TypeKind");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  dstfield = ESMC_FieldCreateGridTypeKind(grid, ESMC_TYPEKIND_R8, 
    ESMC_STAGGERLOC_CENTER, NULL, NULL, NULL, "srcfield", &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "Create ESMC_Field object from a Mesh via TypeKind");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  srcfield = ESMC_FieldCreateMeshTypeKind(mesh, 
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
  double *srcfieldptr = (double *)ESMC_FieldGetPtr(srcfield, 0, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  int offset = 0; 
  // define analytic field on source field
  for(int i=0; i<num_elem; ++i) {
    if (elemType[i] == 5) {
      printf("Cannot compute a non-constant analytic field for a mesh with\
              triangular elements\n");
      ESMC_TestEnd(__FILE__, __LINE__, 0);
    }
    double x1 = nodeCoord[(elemConn[offset]-1)*2];
    double x2 = nodeCoord[(elemConn[offset+1]-1)*2];
    double y1 = nodeCoord[(elemConn[offset+1]-1)*2+1];
    double y2 = nodeCoord[(elemConn[offset+3]-1)*2+1];
    x = (x1+x2)/2.0;
    y = (y1+y2)/2.0;
    srcfieldptr[i] = 20.0 + x + y;
    offset = offset + 4;
  }

  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "Get a void * C pointer to data from ESMC_Field object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  double *dstfieldptr = (double *)ESMC_FieldGetPtr(dstfield, 0, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  // initialize destination field
  p = 0;
  for (int i1=exLB_center[1]; i1<=exUB_center[1]; ++i1) {
    for (int i0=exLB_center[0]; i0<=exUB_center[0]; ++i0) {
      x = gridXCenter[p];
      y = gridYCenter[p];
#ifdef gridmasking
      if (mask[p] == 1)
        dstfieldptr[p] = 10000000;
      else
#endif
      dstfieldptr[p] = 0.0; // this has 8 points that don't map
      ++p;
    }
  }

  //----------------------------------------------------------------------------
  //EX_UTest
  int *maskValues = (int *)malloc(sizeof(int));
  maskValues[0] = 1;
  strcpy(name, "Create an InterArray for maskValues in ESMC_FieldRegridStore()");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  ESMC_InterArrayInt i_maskValues;
  rc = ESMC_InterArrayIntSet(&i_maskValues, maskValues, 1);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "Create an ESMC_RouteHandle via ESMC_FieldRegridStore()");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");

  srcFracField = ESMC_FieldCreateMeshTypeKind(mesh, ESMC_TYPEKIND_R8,
    ESMC_MESHLOC_ELEMENT, NULL, NULL, NULL, "srcFracField", &rc);
  dstFracField = ESMC_FieldCreateGridTypeKind(grid, ESMC_TYPEKIND_R8, 
    ESMC_STAGGERLOC_CENTER, NULL, NULL, NULL, "dstFracField", &rc);

  ESMC_RegridMethod_Flag regridmethod = ESMC_REGRIDMETHOD_CONSERVE;
#ifdef meshmasking
  rc = ESMC_FieldRegridStore(srcfield, dstfield, &i_maskValues, NULL, &routehandle, 
                             &regridmethod, NULL, NULL, NULL, NULL, NULL, NULL,
                             NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
                             &srcFracField, &dstFracField);
#endif
#ifdef gridmasking
  rc = ESMC_FieldRegridStore(srcfield, dstfield, NULL, &i_maskValues, &routehandle, 
                             &regridmethod, NULL, NULL, NULL, NULL, NULL, NULL,
                             NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
                             &srcFracField, &dstFracField);
#endif
#ifdef bothmasking
  rc = ESMC_FieldRegridStore(srcfield, dstfield, &i_maskValues, &i_maskValues, &routehandle, 
                             &regridmethod, NULL, NULL, NULL, NULL, NULL, NULL,
                             NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
                             &srcFracField, &dstFracField);
#endif
#ifdef nomasking
  rc = ESMC_FieldRegridStore(srcfield, dstfield, NULL, NULL, &routehandle, 
                             &regridmethod, NULL, NULL, NULL, NULL, NULL, NULL,
                             NULL, NULL, NULL, NULL, NULL, NULL, NULL, 
                             &srcFracField, &dstFracField);
#endif
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "Execute ESMC_FieldRegrid()");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_FieldRegrid(srcfield, dstfield, routehandle, NULL);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "Release an ESMC_RouteHandle via ESMC_FieldRegridRelease()");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_FieldRegridRelease(&routehandle);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "Execute ESMC_FieldRegridGetArea() - source");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");

  srcAreaField = ESMC_FieldCreateMeshTypeKind(mesh, ESMC_TYPEKIND_R8,
    ESMC_MESHLOC_ELEMENT, NULL, NULL, NULL, "srcAreaField", &rc);

  rc = ESMC_FieldRegridGetArea(srcAreaField);

  //printf("Source Area Field pointer\n");
  double *srcAreaFieldPtr = (double *)ESMC_FieldGetPtr(srcAreaField, 0, &rc);
  bool pass = true;
  for(int i=0; i<num_elem; ++i) {
    //printf("%f\n",srcAreaFieldPtr[p]);
    if (srcAreaFieldPtr[i] <= 0.0) pass = false;
  }
  //printf("\n");

  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "Execute ESMC_FieldRegridGetArea() - destination");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");

  dstAreaField = ESMC_FieldCreateGridTypeKind(grid, ESMC_TYPEKIND_R8,
    ESMC_STAGGERLOC_CENTER, NULL, NULL, NULL, "dstAreaField", &rc);

  rc = ESMC_FieldRegridGetArea(dstAreaField);

  //printf("Destination Area Field pointer\n");
  double * dstAreaFieldPtr = (double *)ESMC_FieldGetPtr(dstAreaField, 0, &rc);
  pass = true;
  p = 0;
  for (int i1=exLB_center[1]; i1<=exUB_center[1]; ++i1) {
    for (int i0=exLB_center[0]; i0<=exUB_center[0]; ++i0) {
    //printf("%f\n",dstAreaFieldPtr[i]);
    if (dstAreaFieldPtr[p] <= 0.0) pass = false;
      ++p;
    }
  }
  //printf("\n");

  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //-------------------------- REGRID VALIDATION -------------------------------
  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "Regridding Validation");
  strcpy(failMsg, "Did not have acceptable accuracy");

  // get the fraction fields
  double * srcFracFieldPtr = (double *)ESMC_FieldGetPtr(srcFracField, 0, &rc);
  double * dstFracFieldPtr = (double *)ESMC_FieldGetPtr(dstFracField, 0, &rc);

  double srcmass = 0;
  for (int i=0; i<num_elem; ++i)
    srcmass += srcfieldptr[i]*srcAreaFieldPtr[i]*srcFracFieldPtr[i];

  // check destination field against analytic field
  bool correct = true;
  double dstmass = 0;
  p = 0;
  for (int i1=exLB_center[1]; i1<=exUB_center[1]; ++i1) {
    for (int i0=exLB_center[0]; i0<=exUB_center[0]; ++i0) {
      // compute the mass
      dstmass += dstfieldptr[p]*dstAreaFieldPtr[p];
      //printf("%f - %f\n", dstfieldptr[p], dstAreaFieldPtr[p]);
      x = gridXCenter[p];
      y = gridYCenter[p];
      exact = 20.0 + x + y;
      tol = 100;
#ifdef nomasking
      tol = .0001;
#endif
      if (ESMC_dabs(dstfieldptr[p]-exact) > tol) {
        printf("dstfield [%f,%f]:\n%f /= %f\n", 
               x, y , dstfieldptr[p], exact);
        correct=false;
      }
      ++p;
    }
  }
  // check that the mass is conserved
  if (ESMC_dabs(srcmass - dstmass) > .0001) correct = false;
  //printf("srcmass = %f, dstmass = %f\n", srcmass, dstmass);

  ESMC_Test((correct==true), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  free(exLB_center);
  free(exUB_center);
  free(exLB_corner);
  free(exUB_corner);
  free(maskValues);

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
  rc = ESMC_GridDestroy(&grid);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "Destroy ESMC_Mesh object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_MeshDestroy(&mesh);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

#endif
  //----------------------------------------------------------------------------
  ESMC_TestEnd(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  return 0;
}

