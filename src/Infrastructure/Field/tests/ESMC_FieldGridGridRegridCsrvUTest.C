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

//#define masking

//==============================================================================
//BOP
// !PROGRAM: ESMC_FieldGridGridRegridCsrvUTest - Check ESMC_FieldRegrid functionality
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
  ESMC_Grid srcgrid, dstgrid;
  int dimcount = 2;
  int *maxIndex, *maxIndex_d;
  ESMC_InterArrayInt i_maxIndex, i_maxIndex_d;

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

  double ub_x, lb_x, max_x, min_x, cellwidth_x;
  double ub_y, lb_y, max_y, min_y, cellwidth_y;
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

  maxIndex = (int *)malloc(dimcount*sizeof(int));
  maxIndex[0] = int(ub_x);
  maxIndex[1] = int(ub_y);
  rc = ESMC_InterArrayIntSet(&i_maxIndex, maxIndex, dimcount);

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
  //EX_UTest
  strcpy(name, "GridAddCoord");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_GridAddCoord(srcgrid, ESMC_STAGGERLOC_CORNER);
  rc = ESMC_GridAddCoord(srcgrid, ESMC_STAGGERLOC_CENTER);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "GridAddItem");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#ifdef masking
  rc = ESMC_GridAddItem(srcgrid, ESMC_GRIDITEM_MASK, ESMC_STAGGERLOC_CENTER);
#endif
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "GridGetItem - mask");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#ifdef masking
  int *mask = (int *)ESMC_GridGetItem(srcgrid, ESMC_GRIDITEM_MASK, 
                                      ESMC_STAGGERLOC_CENTER, &rc);
#endif
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  // CORNERS

  //----------------------------------------------------------------------------
  //EX_UTest
  // get and fill first coord array and computational bounds
  int *exLBound_corner = (int *)malloc(dimcount*sizeof(int));
  int *exUBound_corner = (int *)malloc(dimcount*sizeof(int));

  strcpy(name, "GridGetCoord - X - corner");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  double *gridXCorner = (double *)ESMC_GridGetCoord(srcgrid, 1,
                                                    ESMC_STAGGERLOC_CORNER, NULL,
                                                    exLBound_corner, 
                                                    exUBound_corner, &rc);

  //printf("exLBound_corner = [%d,%d]\n", exLBound_corner[0], exLBound_corner[1]);
  //printf("exUBound_corner = [%d,%d]\n", exUBound_corner[0], exUBound_corner[1]);

  p = 0;
  for (int i1=exLBound_corner[1]; i1<=exUBound_corner[1]; ++i1) {
    for (int i0=exLBound_corner[0]; i0<=exUBound_corner[0]; ++i0) {
        gridXCorner[p]=((double)(i0-1)*cellwidth_x) + lb_x;
      ++p;
    }
  }

  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  // get and fill second coord array
  strcpy(name, "GridGetCoord - Y - corner");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  double *gridYCorner = (double *)ESMC_GridGetCoord(srcgrid, 2,
                                                    ESMC_STAGGERLOC_CORNER, NULL,
                                                    NULL, NULL, &rc);

  p = 0;
  for (int i1=exLBound_corner[1]; i1<=exUBound_corner[1]; ++i1) {
    for (int i0=exLBound_corner[0]; i0<=exUBound_corner[0]; ++i0) {
      gridYCorner[p]=((double)(i1-1)*cellwidth_y) + lb_y;
      ++p;
    }
  }

  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  // CENTERS

  //----------------------------------------------------------------------------
  //EX_UTest
  // get and fill first coord array and computational bounds
  int *exLBound_center = (int *)malloc(dimcount*sizeof(int));
  int *exUBound_center = (int *)malloc(dimcount*sizeof(int));

  strcpy(name, "GridGetCoord - X - center");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  double *gridXCenter = (double *)ESMC_GridGetCoord(srcgrid, 1,
                                                    ESMC_STAGGERLOC_CENTER, NULL,
                                                    exLBound_center, 
                                                    exUBound_center, &rc);

  //printf("exLBound_center = [%d,%d]\n", exLBound_center[0], exLBound_center[1]);
  //printf("exUBound_center = [%d,%d]\n", exUBound_center[0], exUBound_center[1]);

  p = 0;
  for (int i1=exLBound_center[1]; i1<=exUBound_center[1]; ++i1) {
    for (int i0=exLBound_center[0]; i0<=exUBound_center[0]; ++i0) {
        gridXCenter[p]=((double)(i0-1)*cellwidth_x) + lb_x 
                       + ((double)cellwidth_x/2.0);
      ++p;
    }
  }

  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  // get and fill second coord array
  strcpy(name, "GridGetCoord - Y - center");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  double *gridYCenter = (double *)ESMC_GridGetCoord(srcgrid, 2,
                                                    ESMC_STAGGERLOC_CENTER, NULL,
                                                    NULL, NULL, &rc);

  p = 0;
  for (int i1=exLBound_center[1]; i1<=exUBound_center[1]; ++i1) {
    for (int i0=exLBound_center[0]; i0<=exUBound_center[0]; ++i0) {
      gridYCenter[p]=((double)(i1-1)*cellwidth_y) + lb_y
                     + ((double)cellwidth_y/2.0);
      ++p;
    }
  }

  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

#ifdef masking
  // set the masking
  p = 0;
  for (int i1=exLBound_center[1]; i1<=exUBound_center[1]; ++i1) {
    for (int i0=exLBound_center[0]; i0<=exUBound_center[0]; ++i0) {
      if (i0 == 2) {
        mask[p] = 1;
        //printf("Masked value at [%f,%f]\n", gridXCenter[p], gridYCenter[p]);
      } else mask[p] = 0;
      ++p;
    }
  }
#endif


  //----------------------------------------------------------------------------
  //----------------------- Grid CREATION --------------------------------------
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  // Create a Grid

  ub_x = 19;
  ub_y = 19;
  lb_x = 1;
  lb_y = 1;
  max_x = 19;
  max_y = 19;
  min_x = 1;
  min_y = 1;
  cellwidth_x = (max_x-min_x)/(ub_x-lb_x);
  cellwidth_y = (max_y-min_y)/(ub_y-lb_y);


  maxIndex_d = (int *)malloc(dimcount*sizeof(int));
  maxIndex_d[0] = int(ub_x);
  maxIndex_d[1] = int(ub_y);
  rc = ESMC_InterArrayIntSet(&i_maxIndex_d, maxIndex_d, dimcount);

  strcpy(name, "GridCreate");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  dstgrid = ESMC_GridCreateNoPeriDim(&i_maxIndex_d, &coordsys, &typekind, &indexflag, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);

  // free memory
  free(maxIndex_d);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //  GridAddCoord to dstgrid
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "GridAddCoord");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_GridAddCoord(dstgrid, ESMC_STAGGERLOC_CORNER);
  rc = ESMC_GridAddCoord(dstgrid, ESMC_STAGGERLOC_CENTER);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  // CORNERS

  //----------------------------------------------------------------------------
  //EX_UTest
  // get and fill first coord array and computational bounds
  int *exLBound_dcorner = (int *)malloc(dimcount*sizeof(int));
  int *exUBound_dcorner = (int *)malloc(dimcount*sizeof(int));

  strcpy(name, "GridGetCoord - X - corner");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  double *gridXCorner_d = (double *)ESMC_GridGetCoord(dstgrid, 1,
                                                     ESMC_STAGGERLOC_CORNER, NULL,
                                                     exLBound_dcorner, 
                                                     exUBound_dcorner, &rc);

  //printf("exLBound_dcorner = [%d,%d]\n", exLBound_dcorner[0], exLBound_dcorner[1]);
  //printf("exUBound_dcorner = [%d,%d]\n", exUBound_dcorner[0], exUBound_dcorner[1]);

  p = 0;
  for (int i1=exLBound_dcorner[1]; i1<=exUBound_dcorner[1]; ++i1) {
    for (int i0=exLBound_dcorner[0]; i0<=exUBound_dcorner[0]; ++i0) {
      gridXCorner_d[p]=((double)(i0-1) * cellwidth_x) + lb_x;
      ++p;
    }
  }

  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  // get and fill second coord array
  strcpy(name, "GridGetCoord - Y - corner");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  double *gridYCorner_d = (double *)ESMC_GridGetCoord(dstgrid, 2,
                                                     ESMC_STAGGERLOC_CORNER, NULL,
                                                     NULL, NULL, &rc);

  p = 0;
  for (int i1=exLBound_dcorner[1]; i1<=exUBound_dcorner[1]; ++i1) {
    for (int i0=exLBound_dcorner[0]; i0<=exUBound_dcorner[0]; ++i0) {
      gridYCorner_d[p]=((double)(i1-1) * cellwidth_y) + lb_y;
      ++p;
    }
  }

  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  // CENTER

  //----------------------------------------------------------------------------
  //EX_UTest
  // get and fill first coord array and computational bounds
  int *exLBound_dcenter = (int *)malloc(dimcount*sizeof(int));
  int *exUBound_dcenter = (int *)malloc(dimcount*sizeof(int));

  strcpy(name, "GridGetCoord - X - center");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  double *gridXCenter_d = (double *)ESMC_GridGetCoord(dstgrid, 1,
                                                     ESMC_STAGGERLOC_CENTER, NULL,
                                                     exLBound_dcenter, 
                                                     exUBound_dcenter, &rc);

  //printf("exLBound_dcenter = [%d,%d]\n", exLBound_dcenter[0], exLBound_dcenter[1]);
  //printf("exUBound_dcenter = [%d,%d]\n", exUBound_dcenter[0], exUBound_dcenter[1]);

  p = 0;
  for (int i1=exLBound_dcenter[1]; i1<=exUBound_dcenter[1]; ++i1) {
    for (int i0=exLBound_dcenter[0]; i0<=exUBound_dcenter[0]; ++i0) {
      gridXCenter_d[p]=((double)(i0-1)*cellwidth_x) + lb_x
                       + ((double)cellwidth_x / 2.0);
      ++p;
    }
  }

  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  // get and fill second coord array
  strcpy(name, "GridGetCoord - Y - center");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  double *gridYCenter_d = (double *)ESMC_GridGetCoord(dstgrid, 2,
                                                      ESMC_STAGGERLOC_CENTER, NULL,
                                                      NULL, NULL, &rc);

  p = 0;
  for (int i1=exLBound_dcenter[1]; i1<=exUBound_dcenter[1]; ++i1) {
    for (int i0=exLBound_dcenter[0]; i0<=exUBound_dcenter[0]; ++i0) {
      gridYCenter_d[p]=((double)(i1-1)*cellwidth_y) + lb_y
                       + ((double)cellwidth_y / 2.0);
      ++p;
    }
  }

  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

#if 0
  printf("\nSource Grid corner coords: \n");
  p = 0;
  for (int i1=exLBound_corner[1]; i1<=exUBound_corner[1]; ++i1) {
    for (int i0=exLBound_corner[0]; i0<=exUBound_corner[0]; ++i0) {
      x = gridXCorner[p];
      y = gridYCorner[p];
      printf("[%f,%f]\n", x, y); 
      ++p;
    }
  }
  printf("\n");
  
  printf("\nSource Grid center coords: \n");
  p = 0;
  for (int i1=exLBound_center[1]; i1<=exUBound_center[1]; ++i1) {
    for (int i0=exLBound_center[0]; i0<=exUBound_center[0]; ++i0) {
      x = gridXCenter[p];
      y = gridYCenter[p];
      printf("[%f,%f]\n", x, y); 
      ++p;
    }
  }
  printf("\n");
  
  printf("\nDestination Grid corner coords: \n");
  p = 0;
  for (int i1=exLBound_dcorner[1]; i1<=exUBound_dcorner[1]; ++i1) {
    for (int i0=exLBound_dcorner[0]; i0<=exUBound_dcorner[0]; ++i0) {
      x = gridXCorner_d[p];
      y = gridYCorner_d[p];
      printf("[%f,%f]\n", x, y); 
      ++p;
    }
  }
  printf("\n");

  printf("\nDestination Grid center coords: \n");
  p = 0;
  for (int i1=exLBound_dcenter[1]; i1<=exUBound_dcenter[1]; ++i1) {
    for (int i0=exLBound_dcenter[0]; i0<=exUBound_dcenter[0]; ++i0) {
      x = gridXCenter_d[p];
      y = gridYCenter_d[p];
      printf("[%f,%f]\n", x, y); 
      ++p;
    }
  }
  printf("\n");
#endif

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
  double * srcfieldptr = (double *)ESMC_FieldGetPtr(srcfield, 0, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  // define analytic field on source field
  p = 0;
  for (int i1=exLBound_center[1]; i1<=exUBound_center[1]; ++i1) {
    for (int i0=exLBound_center[0]; i0<=exUBound_center[0]; ++i0) {
      x = gridXCenter[p];
      y = gridYCenter[p];
      //srcfieldptr[p] = 20.0;
      srcfieldptr[p] = 20.0 + x + y;
      ++p;
    }
  }
  
  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "Get a void * C pointer to data from ESMC_Field object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  double * dstfieldptr = (double *)ESMC_FieldGetPtr(dstfield, 0, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  // zero out the destination field
  p = 0;
  for (int i1=exLBound_dcenter[1]; i1<=exUBound_dcenter[1]; ++i1) {
    for (int i0=exLBound_dcenter[0]; i0<=exUBound_dcenter[0]; ++i0) {
      dstfieldptr[p] = 0.0;
      ++p;
    }
  }
  
  //----------------------------------------------------------------------------
  //EX_UTest
  int *maskValues = (int *)malloc(sizeof(int));
  maskValues[0] = 1;
  strcpy(name, "Set an InterArray for maskValues in ESMC_FieldRegridStore()");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  ESMC_InterArrayInt i_maskValues;
  rc = ESMC_InterArrayIntSet(&i_maskValues, maskValues, 1);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "Create an ESMC_RouteHandle via ESMC_FieldRegridStore()");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");

  srcFracField = ESMC_FieldCreateGridTypeKind(srcgrid, ESMC_TYPEKIND_R8, 
    ESMC_STAGGERLOC_CENTER, NULL, NULL, NULL, "srcFracField", &rc);
  dstFracField = ESMC_FieldCreateGridTypeKind(dstgrid, ESMC_TYPEKIND_R8,
    ESMC_STAGGERLOC_CENTER, NULL, NULL, NULL, "dstFracField", &rc);

  ESMC_RegridMethod_Flag regridmethod = ESMC_REGRIDMETHOD_CONSERVE;
  ESMC_UnmappedAction_Flag unmappedaction = ESMC_UNMAPPEDACTION_IGNORE;
#ifdef masking
  rc = ESMC_FieldRegridStore(srcfield, dstfield, &i_maskValues, NULL, &routehandle, 
                             &regridmethod, NULL, NULL, NULL, NULL, 
                             NULL, NULL, NULL, NULL, &unmappedaction, NULL, NULL, NULL,
                             NULL, &srcFracField, &dstFracField);
#else
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

  srcAreaField = ESMC_FieldCreateGridTypeKind(srcgrid, ESMC_TYPEKIND_R8,
    ESMC_STAGGERLOC_CENTER, NULL, NULL, NULL, "srcAreaField", &rc);

  rc = ESMC_FieldRegridGetArea(srcAreaField);

  //printf("Source Area Field pointer\n");
  double * srcAreaFieldPtr = (double *)ESMC_FieldGetPtr(srcAreaField, 0, &rc);
  bool pass = true;
  p = 0;
  for (int i1=exLBound_center[1]; i1<=exUBound_center[1]; ++i1) {
    for (int i0=exLBound_center[0]; i0<=exUBound_center[0]; ++i0) {
    //printf("%f\n",srcAreaFieldPtr[i]);
    if (srcAreaFieldPtr[p] <= 0.0) pass = false;
      ++p;
    }
  }
  //printf("\n");

  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "Execute ESMC_FieldRegridGetArea() - destination");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");

  dstAreaField = ESMC_FieldCreateGridTypeKind(dstgrid, ESMC_TYPEKIND_R8,
    ESMC_STAGGERLOC_CENTER, NULL, NULL, NULL, "dstAreaField", &rc);

  rc = ESMC_FieldRegridGetArea(dstAreaField);

  //printf("Destination Area Field pointer\n");
  double * dstAreaFieldPtr = (double *)ESMC_FieldGetPtr(dstAreaField, 0, &rc);
  pass = true;
  p = 0;
  for (int i1=exLBound_dcenter[1]; i1<=exUBound_dcenter[1]; ++i1) {
    for (int i0=exLBound_dcenter[0]; i0<=exUBound_dcenter[0]; ++i0) {
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
  p = 0;
  for (int i1=exLBound_center[1]; i1<=exUBound_center[1]; ++i1) {
    for (int i0=exLBound_center[0]; i0<=exUBound_center[0]; ++i0) {
      srcmass += srcfieldptr[p]*srcAreaFieldPtr[p]*srcFracFieldPtr[p];
      //printf("%f - %f - %f\n", dstfieldptr[p], dstAreaFieldPtr[p], dstFracFieldPtr[p]);
      ++p;
    }
  }

  // validate against analytic values on destination field
  bool correct = true;
  double dstmass = 0;
  p = 0;
  for (int i1=exLBound_dcenter[1]; i1<=exUBound_dcenter[1]; ++i1) {
    for (int i0=exLBound_dcenter[0]; i0<=exUBound_dcenter[0]; ++i0) {
      // compute the mass
      dstmass += dstfieldptr[p]*dstAreaFieldPtr[p];
      //printf("%f - %f\n", dstfieldptr[p], dstAreaFieldPtr[p]);
      x = gridXCenter_d[p];
      y = gridYCenter_d[p];
      //exact = 20.0;
      exact = 20.0 + x + y;
      // set tolerance differently for masking
      // we can get away with this because in this case we are just testing that
      // the masking actually worked, if it didn't the remapped values would be
      // many order of magnitude larger than this.  However, in the masking case
      // the regridding accuracy is not checked very well.
      tol = .0001;
#ifdef masking
      tol = 100;
#endif
      if (ESMC_dabs(dstfieldptr[p]-exact) > tol) {
        printf("dstfieldptr [%f,%f]:\n%f /= %f\n", 
               x, y, dstfieldptr[p], exact);
        correct=false;
      }
      ++p;
    }
  }
  // check that the mass is conserved
  if (ESMC_dabs(srcmass - dstmass) > .0001) correct = false;
  //printf("srcmass = %f, dstmass = %f, srcmass-dstmass = %f\n", srcmass, dstmass, srcmass-dstmass);

  ESMC_Test((correct==true), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  free(exLBound_corner);
  free(exUBound_corner);
  free(exLBound_center);
  free(exUBound_center);
  free(exLBound_dcorner);
  free(exUBound_dcorner);
  free(exLBound_dcenter);
  free(exUBound_dcenter);
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
  ESMC_TestEnd(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  return 0;
}
