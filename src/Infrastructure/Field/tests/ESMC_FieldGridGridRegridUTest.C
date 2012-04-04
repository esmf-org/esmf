// $Id: ESMC_FieldGridGridRegridUTest.C,v 1.1 2012/04/04 16:58:22 rokuingh Exp $
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
// !PROGRAM: ESMC_FieldGridGridRegridUTest - Check ESMC_FieldRegrid functionality
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
  ESMC_RouteHandle routehandle;
  ESMC_Field srcfield, dstfield;

  // Grid variables
  ESMC_Grid srcgrid, dstgrid;
  int dimcount = 2;
  int *maxIndex, *maxIndex_d;
  ESMC_InterfaceInt i_maxIndex, i_maxIndex_d;
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
/*
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
*/

  double src_min_x, src_max_x, src_min_y, src_max_y, src_nx, src_ny;
  double dst_min_x, dst_max_x, dst_min_y, dst_max_y, dst_nx, dst_ny;
  src_nx = 4;
  src_ny = 4;
  src_min_x = 0.0;
  src_min_y = 0.0;
  src_max_x = 10.0;
  src_max_y = 10.0;

  dst_nx = 3;
  dst_ny = 3;
  dst_min_x = 0.0;
  dst_min_y = 0.0;
  dst_max_x = 10.0;
  dst_max_y = 10.0;

  maxIndex = (int *)malloc(dimcount*sizeof(int));
  maxIndex[0] = int(src_nx);
  maxIndex[1] = int(src_ny);
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

  printf("exLBounds = [%d,%d]\n", exLBound[0], exLBound[1]);
  printf("exUBounds = [%d,%d]\n", exUBound[0], exUBound[1]);

  p = 0;
  for (int i1=exLBound[1]; i1<=exUBound[1]; ++i1) {
    for (int i0=exLBound[0]; i0<=exUBound[0]; ++i0) {
//      if (i0==exLBound[0]) gridXCoord[p] = lb_x;
//      gridXCoord[p]=(double)(i0-1)*cellwidth_x;
      gridXCoord[p] = (src_max_x-src_min_x)*(double)(i0-1)/(double)(src_nx-1)+src_min_x;
#ifdef masking
      if (i0 == 2 ) mask[p] = 1;
      else mask[p] = 0;
#endif
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
  double *gridYCoord = (double *)ESMC_GridGetCoord(srcgrid, 2,
                                                   ESMC_STAGGERLOC_CORNER,
                                                   NULL, NULL, &rc);

  p = 0;
  for (int i1=exLBound[1]; i1<=exUBound[1]; ++i1) {
    for (int i0=exLBound[0]; i0<=exUBound[0]; ++i0) {
//      if (i1==exLBound[1]) gridYCoord[p] = lb_y;
//      gridYCoord[p]=(double)(i1-1)*cellwidth_y;
      gridYCoord[p] = (src_max_y-src_min_y)*(double)(i1-1)/(double)(src_ny-1)+src_min_y;
      ++p;
    }
  }

  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //----------------------- Grid CREATION --------------------------------------
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  // Create a Grid
/*
  ub_x = 3;
  ub_y = 3;
  lb_x = 1;
  lb_y = 1;
  max_x = 3;
  max_y = 3;
  min_x = 1;
  min_y = 1;
  cellwidth_x = (max_x-min_x)/(ub_x-lb_x);
  cellwidth_y = (max_y-min_y)/(ub_y-lb_y);
*/

  maxIndex_d = (int *)malloc(dimcount*sizeof(int));
  maxIndex_d[0] = int(dst_nx);
  maxIndex_d[1] = int(dst_ny);
  i_maxIndex_d = ESMC_InterfaceIntCreate(maxIndex_d, dimcount, &rc);

  strcpy(name, "GridCreate");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  dstgrid = ESMC_GridCreateNoPeriDim(i_maxIndex_d, ESMC_COORDSYS_CART,
                                         ESMC_TYPEKIND_R8, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);

  // free memory
  free(maxIndex_d);
  ESMC_InterfaceIntDestroy(&i_maxIndex_d);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //  GridAddCoord to dstgrid
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "GridAddCoord");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_GridAddCoord(dstgrid, ESMC_STAGGERLOC_CORNER);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  // get and fill first coord array and computational bounds
  int *exLBound_d = (int *)malloc(dimcount*sizeof(int));
  int *exUBound_d = (int *)malloc(dimcount*sizeof(int));

  strcpy(name, "GridGetCoord - X");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  double *gridXCoord_d = (double *)ESMC_GridGetCoord(dstgrid, 1,
                                                   ESMC_STAGGERLOC_CORNER,
                                                   exLBound_d, exUBound_d, &rc);

  printf("exLBounds = [%d,%d]\n", exLBound_d[0], exLBound_d[1]);
  printf("exUBounds = [%d,%d]\n", exUBound_d[0], exUBound_d[1]);

  p = 0;
  for (int i1=exLBound_d[1]; i1<=exUBound_d[1]; ++i1) {
    for (int i0=exLBound_d[0]; i0<=exUBound_d[0]; ++i0) {
//      if (i0==exLBound_d[0]) gridXCoord_d[p] = lb_x;
//      gridXCoord_d[p]=((double)(i0-1)*cellwidth_x)+lb_x;
      gridXCoord_d[p] = (dst_max_x-dst_min_x)*(double)(i0-1)/(double)(dst_nx-1)+dst_min_x;
#ifdef destination_masking
      if (i0 == 2 ) mask_d[p] = 1;
      else mask_d[p] = 0;
#endif
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
  double *gridYCoord_d = (double *)ESMC_GridGetCoord(dstgrid, 2,
                                                   ESMC_STAGGERLOC_CORNER,
                                                   NULL, NULL, &rc);

  p = 0;
  for (int i1=exLBound_d[1]; i1<=exUBound_d[1]; ++i1) {
    for (int i0=exLBound_d[0]; i0<=exUBound_d[0]; ++i0) {
//      if (i1==exLBound_d[1]) gridYCoord_d[p] = lb_y;
//      gridYCoord_d[p]=((double)(i1-1)*cellwidth_y)+lb_y;
      gridYCoord_d[p] = (dst_max_y-dst_min_y)*(double)(i1-1)/(double)(dst_ny-1)+dst_min_y;
      ++p;
    }
  }

  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------



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
  strcpy(name, "Create ESMC_Field object from a Grid via TypeKind");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  dstfield = ESMC_FieldCreateGridTypeKind(dstgrid, ESMC_TYPEKIND_R8, 
    ESMC_STAGGERLOC_CENTER, NULL, NULL, NULL, "dstfield", &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  

  //----------------------------------------------------------------------------
  //-------------------------- REGRIDDING --------------------------------------
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "Get a void * C pointer to data from ESMC_Field object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  float * srcfieldptr = (float *)ESMC_FieldGetPtr(srcfield, 0, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  printf("\nSource Grid coords: \n");
  // define analytic field on source field
  p = 0;
  double x,y;
  for (int i1=exLBound[1]; i1<=exUBound[1]; ++i1) {
    for (int i0=exLBound[0]; i0<=exUBound[0]; ++i0) {
      x = gridXCoord[p];
      y = gridYCoord[p];
      printf("[%f,%f]\n", x, y); 
      //srcfieldptr[p] = 20.0+x+y; // this has 8 points that don't map
      //srcfieldptr[p] = 20.0+x;   // this has 8 points that don't map
      //srcfieldptr[p] = 20.0+pow(y,2); // this has 2 points that don't map
      srcfieldptr[p] = 20.0+y; // this maps all points
      ++p;
    }
  }
  printf("\n");
  
  printf("\nDestination Grid coords: \n");
  // define analytic field on source field
  p = 0;
  for (int i1=exLBound_d[1]; i1<=exUBound_d[1]; ++i1) {
    for (int i0=exLBound_d[0]; i0<=exUBound_d[0]; ++i0) {
      x = gridXCoord_d[p];
      y = gridYCoord_d[p];
      printf("[%f,%f]\n", x, y); 
      ++p;
    }
  }
  printf("\n");
  
  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "Get a void * C pointer to data from ESMC_Field object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  float * dstfieldptr = (float *)ESMC_FieldGetPtr(dstfield, 0, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  // define analytic field on source field
  p = 0;
  for (int i1=exLBound_d[1]; i1<=exUBound_d[1]; ++i1) {
    for (int i0=exLBound_d[0]; i0<=exUBound_d[0]; ++i0) {
      //dstfieldptr[p] = 20.0+x+y; // this has 8 points that don't map
      //dstfieldptr[p] = 20.0+x;   // this has 8 points that don't map
      //dstfieldptr[p] = 20.0+pow(y,2); // this has 2 points that don't map
      dstfieldptr[p] = 0.0; // this maps all points
      ++p;
    }
  }
  
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
  

  //----------------------------------------------------------------------------
  //-------------------------- REGRID VALIDATION -------------------------------
  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "Regridding Validation");
  strcpy(failMsg, "Did not have acceptable accuracy");

  // define analytic field on source field
  double exact;
  bool correct = true;
  p = 0;
  for (int i1=exLBound_d[1]; i1<=exUBound_d[1]; ++i1) {
    for (int i0=exLBound_d[0]; i0<=exUBound_d[0]; ++i0) {
      x = gridXCoord_d[p];
      y = gridYCoord_d[p];
      //exact = 20.0+x+y; // this has 8 points that don't map
      //exact = 20.0+x;   // this has 8 points that don't map
      //exact = 20.0+pow(y,2); // this has 2 points that don't map
      exact = 20.0+y; // this maps all points
#ifdef masking 
      if ( abs((double)( dstfieldptr[p]-exact) ) > 100) {
#else
      if ( abs((double)( dstfieldptr[p]-exact) ) > 0.0001) {
#endif
        printf("dstfieldptr[%d] = %f\n and it should be = %f\n", p, dstfieldptr[p], exact);
        correct=false;
      }
      ++p;
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
  rc = ESMC_GridDestroy(&dstgrid);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

#endif
  //----------------------------------------------------------------------------
  ESMC_TestEnd(result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  return 0;
}
