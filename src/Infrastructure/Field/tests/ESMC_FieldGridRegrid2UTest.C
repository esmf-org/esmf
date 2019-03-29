// $Id$
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

//==============================================================================
//BOP
// !PROGRAM: ESMC_FieldGridRegrid2UTest - Check ESMC_FieldRegrid functionality
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
  //EX_UTest
  // Create a Grid
  double ub_x, lb_x, max_x, min_x, cellwidth_x, cellcenter_x, cellcorner_x;
  double ub_y, lb_y, max_y, min_y, cellwidth_y, cellcenter_y, cellcorner_y;
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
  cellcenter_x = cellwidth_x/double(2);
  cellcenter_y = cellwidth_y/double(2);
  cellcorner_x = cellwidth_x;
  cellcorner_y = cellwidth_y;

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

  //----------------------------------------------------------------------------
  //----------------------------------------------------------------------------
  //EX_UTest
  // get and fill first coord array and computational bounds
  int *exLBound = (int *)malloc(dimcount*sizeof(int));
  int *exUBound = (int *)malloc(dimcount*sizeof(int));

  strcpy(name, "GridGetCoord - X");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  double *gridXCoord = (double *)ESMC_GridGetCoord(grid, 1,
                                                   ESMC_STAGGERLOC_CENTER, NULL,
                                                   exLBound, exUBound, &rc);

  //printf("exLBounds = [%d,%d]\n", exLBound[0], exLBound[1]);
  //printf("exUBounds = [%d,%d]\n", exUBound[0], exUBound[1]);

  p = 0;
  for (int i1=exLBound[1]; i1<=exUBound[1]; ++i1) {
    for (int i0=exLBound[0]; i0<=exUBound[0]; ++i0) {
      gridXCoord[p]=(double)(i0)*cellwidth_x - cellcenter_x;
      ++p;
    }
  }

  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  // get and fill second coord array
  strcpy(name, "GridGetCoord - Y");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  double *gridYCoord = (double *)ESMC_GridGetCoord(grid, 2,
                                                   ESMC_STAGGERLOC_CENTER, NULL,
                                                   NULL, NULL, &rc);

  p = 0;
  for (int i1=exLBound[1]; i1<=exUBound[1]; ++i1) {
    for (int i0=exLBound[0]; i0<=exUBound[0]; ++i0) {
      gridYCoord[p]=(double)(i1)*cellwidth_y - cellcenter_y;
      ++p;
    }
  }

  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

#ifdef gridmasking
  // set the masking
  p = 0;
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

  printf("\nDestination Grid coords: \n");
  // define analytic field on source field
  p = 0;
  for (int i1=exLBound[1]; i1<=exUBound[1]; ++i1) {
    for (int i0=exLBound[0]; i0<=exUBound[0]; ++i0) {
      x = gridXCoord[p];
      y = gridYCoord[p];
      printf("[%f,%f]\n", x, y);
      ++p;
    }
  }
  printf("\n");


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
  double nodeCoord [] ={0.0,0.0, 1.5,0.0, 2.5, 0.0, 4.0,0.0,
               0.0,1.5, 1.5,1.5, 2.5,1.5, 4.0,1.5,
               0.0,2.5, 1.5,2.5, 2.5,2.5, 4.0,2.5,
               0.0,4.0, 1.5,4.0, 2.5,4.0, 4.0,4.0};
  int nodeOwner [] ={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
  int elemId [] ={1,2,3,4,5,6,7,8,9};
  // ESMF_MESHELEMTYPE_QUAD
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

  printf("\nSource Mesh coords: \n");
  for(int i=0; i<num_node; ++i) {
    x=nodeCoord[2*i];
    y=nodeCoord[2*i+1];
    printf("[%f,%f]\n", x, y);
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
    ESMC_TYPEKIND_R8, ESMC_MESHLOC_NODE, NULL, NULL, NULL, "dstfield", &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------


  //----------------------------------------------------------------------------
  //-------------------------- REGRIDDING --------------------------------------
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "Get a void * C pointer to data from ESMC_Field object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  double * srcfieldptr = (double *)ESMC_FieldGetPtr(srcfield, 0, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  // initialize source field
  for(int i=0; i<num_node; ++i) {
    x=nodeCoord[2*i];
    y=nodeCoord[2*i+1];
    srcfieldptr[i] = 20.0 + x + y;
  }

  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "Get a void * C pointer to data from ESMC_Field object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  double * dstfieldptr = (double *)ESMC_FieldGetPtr(dstfield, 0, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  // initialize destination field
  p = 0;
  for (int i1=exLBound[1]; i1<=exUBound[1]; ++i1) {
    for (int i0=exLBound[0]; i0<=exUBound[0]; ++i0) {
      dstfieldptr[p] = 0.0;
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
  ESMC_UnmappedAction_Flag unmappedaction = ESMC_UNMAPPEDACTION_IGNORE;
#ifdef gridmasking
  rc = ESMC_FieldRegridStore(srcfield, dstfield, &i_maskValues, NULL, &routehandle,
                             NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, &unmappedaction,
                             NULL, NULL, NULL, NULL, NULL, NULL);
#endif
#ifdef meshmasking
  rc = ESMC_FieldRegridStore(srcfield, dstfield, NULL, &i_maskValues, &routehandle,
                             NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, &unmappedaction,
                             NULL, NULL, NULL, NULL, NULL, NULL);
#endif
#ifdef bothmasking
  rc = ESMC_FieldRegridStore(srcfield, dstfield, &i_maskValues, &i_maskValues, &routehandle,
                             NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, &unmappedaction,
                             NULL, NULL, NULL, NULL, NULL, NULL);
#endif
#ifdef nomasking
  rc = ESMC_FieldRegridStore(srcfield, dstfield, NULL, NULL, &routehandle,
                             NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, &unmappedaction,
                             NULL, NULL, NULL, NULL, NULL, NULL);
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
  //-------------------------- REGRID VALIDATION -------------------------------
  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "Regridding Validation");
  strcpy(failMsg, "Did not have acceptable accuracy");

  // check destination field against source field
  bool correct = true;
  p = 0;
  for (int i1=exLBound[1]; i1<=exUBound[1]; ++i1) {
    for (int i0=exLBound[0]; i0<=exUBound[0]; ++i0) {
      x=gridXCoord[p];
      y=gridYCoord[p];
      exact = 20.0 + x + y;
      // set tolerance differently for masking
      // we can get away with this because in this case we are just testing that
      // the masking actually worked, if it didn't the remapped values would be
      // many order of magnitude larger than this.  However, in the masking case
      // the regridding accuracy is not checked very well.
      // ifdef gridmasking, meshmasking or both
      tol = 100;
#ifdef nomasking
      tol = .0001;
#endif
      if (ESMC_dabs(dstfieldptr[p]-exact) > tol) {
        printf("dstfieldptr [%f,%f]:\n%f /= %f\n",
               x, y, dstfieldptr[p], exact);
        correct=false;
      }
    ++p;
    }
  }
  ESMC_Test((correct==true), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  free(exLBound);
  free(exUBound);
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
