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

#include "stdlib.h"
#include "stdio.h"
#include "string.h"

// ESMF header
#include "ESMC.h"

 // ESMF Test header
#include "ESMC_Test.h"

//==============================================================================
//BOP
// !PROGRAM: ESMC_FieldSMMFromFileUTest - Check ESMC_FieldSMMFromFile
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
   
  //----------------------------------------------------------------------------
  ESMC_TestStart(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  // VM variables
  int localPet, petCount;
  ESMC_VM vm;

  ESMC_RouteHandle routehandle;
  ESMC_Field srcfield, dstfield;
  ESMC_Grid srcgrid, dstgrid;
  ESMC_RouteHandle rh, rh2;

  int dimcount = 2;
  int *maxIndex, *maxIndex_d;
  ESMC_InterArrayInt i_maxIndex, i_maxIndex_d;

  int p;

  bool correct = true;

  // Get parallel information
  vm=ESMC_VMGetGlobal(&rc);
  if (rc != ESMF_SUCCESS) return 0;

  rc=ESMC_VMGet(vm, &localPet, &petCount, (int *)NULL, (MPI_Comm *)NULL,
                (int *)NULL, (int *)NULL);
  if (rc != ESMF_SUCCESS) return ESMF_FAILURE;

  //----------------------------------------------------------------------------
  //----------------------- Grid CREATION --------------------------------------
  //----------------------------------------------------------------------------

  double ub_x, lb_x, max_x, min_x, cellwidth_x;
  double ub_y, lb_y, max_y, min_y, cellwidth_y;
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

  maxIndex = (int *)malloc(dimcount*sizeof(int));
  maxIndex[0] = int(ub_x);
  maxIndex[1] = int(ub_y);
  rc = ESMC_InterArrayIntSet(&i_maxIndex, maxIndex, dimcount);

  ESMC_CoordSys_Flag coordsys = ESMC_COORDSYS_CART;
  ESMC_TypeKind_Flag typekind = ESMC_TYPEKIND_R8;
  srcgrid = ESMC_GridCreateNoPeriDim(&i_maxIndex, &coordsys, &typekind, NULL, &rc);

  free(maxIndex);

  //  GridAddCoord to srcgrid
  rc = ESMC_GridAddCoord(srcgrid, ESMC_STAGGERLOC_CENTER);

  // get and fill first coord array and computational bounds
  int *exLBound_s = (int *)malloc(dimcount*sizeof(int));
  int *exUBound_s = (int *)malloc(dimcount*sizeof(int));

  double *gridXCoord = (double *)ESMC_GridGetCoord(srcgrid, 1,
                                                   ESMC_STAGGERLOC_CENTER, NULL,
                                                   exLBound_s, exUBound_s, &rc);

  double *gridYCoord = (double *)ESMC_GridGetCoord(srcgrid, 2,
                                                   ESMC_STAGGERLOC_CENTER, NULL,
                                                   NULL, NULL, &rc);


  p = 0;
  for (int i1=exLBound_s[1]; i1<=exUBound_s[1]; ++i1) {
    for (int i0=exLBound_s[0]; i0<=exUBound_s[0]; ++i0) {
      gridXCoord[p]=((double)(i0-1)*cellwidth_x) + lb_x + (double)(cellwidth_x/2.0);
      gridYCoord[p]=((double)(i1-1)*cellwidth_y) + lb_y + (double)(cellwidth_y/2.0);

      ++p;
    }
  }

  // Create a Grid

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


  maxIndex_d = (int *)malloc(dimcount*sizeof(int));
  maxIndex_d[0] = int(ub_x);
  maxIndex_d[1] = int(ub_y);
  rc = ESMC_InterArrayIntSet(&i_maxIndex_d, maxIndex_d, dimcount);

  dstgrid = ESMC_GridCreateNoPeriDim(&i_maxIndex_d, &coordsys, &typekind, NULL, &rc);

  // free memory
  free(maxIndex_d);

  //  GridAddCoord to dstgrid
  rc = ESMC_GridAddCoord(dstgrid, ESMC_STAGGERLOC_CENTER);

  // get and fill first coord array and computational bounds
  int *exLBound_d = (int *)malloc(dimcount*sizeof(int));
  int *exUBound_d = (int *)malloc(dimcount*sizeof(int));

  double *gridXCoord_d = (double *)ESMC_GridGetCoord(dstgrid, 1,
                                                     ESMC_STAGGERLOC_CENTER, NULL,
                                                     exLBound_d, exUBound_d, &rc);

  double *gridYCoord_d = (double *)ESMC_GridGetCoord(dstgrid, 2,
                                                     ESMC_STAGGERLOC_CENTER, NULL,
                                                     NULL, NULL, &rc);

  p = 0;
  for (int i1=exLBound_d[1]; i1<=exUBound_d[1]; ++i1) {
    for (int i0=exLBound_d[0]; i0<=exUBound_d[0]; ++i0) {
      gridXCoord_d[p]=((double)(i0-1)*cellwidth_x) + lb_x + (double)(cellwidth_x/2.0) ;
      gridYCoord_d[p]=((double)(i1-1)*cellwidth_y) + lb_y + (double)(cellwidth_y/2.0);

      ++p;
    }
  }
  //----------------------------------------------------------------------------

#if 0
  //----------------------------------------------------------------------------
  printf("\nSource Grid coords: \n");
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
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  printf("\nDestination Grid coords: \n");
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
#endif


  //----------------------------------------------------------------------------
  //---------------------- FIELD CREATION --------------------------------------
  //----------------------------------------------------------------------------

  srcfield = ESMC_FieldCreateGridTypeKind(srcgrid, ESMC_TYPEKIND_R8,
    ESMC_STAGGERLOC_CENTER, NULL, NULL, NULL, "srcfield", &rc);
  if (rc != ESMF_SUCCESS) return ESMF_FAILURE;

  dstfield = ESMC_FieldCreateGridTypeKind(dstgrid, ESMC_TYPEKIND_R8,
    ESMC_STAGGERLOC_CENTER, NULL, NULL, NULL, "dstfield", &rc);
  if (rc != ESMF_SUCCESS) return ESMF_FAILURE;

  // get and fill first coord array and computational bounds
  int *exLBound = (int *)malloc(dimcount*sizeof(int));
  int *exUBound = (int *)malloc(dimcount*sizeof(int));

  rc = ESMC_FieldGetBounds(srcfield, 0, exLBound, exUBound, dimcount);
  if (rc != ESMF_SUCCESS) return ESMF_FAILURE;

  double * srcfieldptr = (double *)ESMC_FieldGetPtr(srcfield, 0, &rc);
  if (rc != ESMF_SUCCESS) return ESMF_FAILURE;

  p = 0;
  for (int i1=exLBound[1]; i1<=exUBound[1]; ++i1) {
    for (int i0=exLBound[0]; i0<=exUBound[0]; ++i0) {
      srcfieldptr[p] = 42.0;
      p++;
    }
  }

  // get and fill first coord array and computational bounds
  int *exLBound2 = (int *)malloc(dimcount*sizeof(int));
  int *exUBound2 = (int *)malloc(dimcount*sizeof(int));

  rc = ESMC_FieldGetBounds(dstfield, 0, exLBound2, exUBound2, dimcount);
  if (rc != ESMF_SUCCESS) return ESMF_FAILURE;

  double * dstfieldptr = (double *)ESMC_FieldGetPtr(dstfield, 0, &rc);
  if (rc != ESMF_SUCCESS) return ESMF_FAILURE;

  // initialize destination field
  p = 0;
  for (int i1=exLBound2[1]; i1<=exUBound2[1]; ++i1) {
    for (int i0=exLBound2[0]; i0<=exUBound2[0]; ++i0) {
      dstfieldptr[p] = 0.0;
      p++;
    }
  }

  //----------------------------------------------------------------------------
  //-------------------------- REGRIDDING --------------------------------------
  //----------------------------------------------------------------------------

  ESMC_UnmappedAction_Flag unmappedaction = ESMC_UNMAPPEDACTION_IGNORE;
  rc = ESMC_FieldRegridStore(srcfield, dstfield, NULL, NULL, &rh, 
                             NULL, NULL, NULL, NULL, NULL,
                             NULL, NULL, NULL, NULL, &unmappedaction,
                             NULL, NULL, NULL, NULL, NULL, NULL);
  if (rc != ESMF_SUCCESS) return ESMF_FAILURE;

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "ESMC_RouteHandleWrite test");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_RouteHandleWrite(rh, "rh_file.RH");
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "ESMC_RouteHandleCreateFromFile test");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rh2 = ESMC_RouteHandleCreateFromFile("rh_file.RH", &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  rc = ESMC_FieldRegrid(srcfield, dstfield, rh, NULL);
  if (rc != ESMF_SUCCESS) return ESMF_FAILURE;
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "ESMC_FieldRegrid validation of RouteHandleWrite");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");

  p = 0;
  for (int i1=exLBound[1]; i1<=exUBound[1]; ++i1) {
    for (int i0=exLBound[0]; i0<=exUBound[0]; ++i0) {
      if ((dstfieldptr[p] - 42.) > .01) {
        correct = false;
        // printf("source value = %f\n", srcfieldptr[p]);
      }
      p++;
    }
  }

  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  rc = ESMC_FieldRegrid(srcfield, dstfield, rh2, NULL);
  if (rc != ESMF_SUCCESS) return ESMF_FAILURE;
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "ESMC_FieldRegrid validation of RouteHandleCreateFromFile");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");

  p = 0;
  for (int i1=exLBound[1]; i1<=exUBound[1]; ++i1) {
    for (int i0=exLBound[0]; i0<=exUBound[0]; ++i0) {
      if ((dstfieldptr[p] - 42.) > .01) {
        correct = false;
        // printf("source value = %f\n", srcfieldptr[p]);
      }
      p++;
    }
  }

  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  rc = ESMC_FieldRegridRelease(&rh);
  if (rc != ESMF_SUCCESS) return ESMF_FAILURE;
  rc = ESMC_FieldRegridRelease(&rh2);
  if (rc != ESMF_SUCCESS) return ESMF_FAILURE;
  //----------------------------------------------------------------------------

  free(exLBound);
  free(exUBound);
  free(exLBound2);
  free(exUBound2);
  free(exLBound_s);
  free(exUBound_s);
  free(exLBound_d);
  free(exUBound_d);

  rc = ESMC_FieldDestroy(&srcfield);
  rc = ESMC_FieldDestroy(&dstfield);
  rc = ESMC_GridDestroy(&srcgrid);
  rc = ESMC_GridDestroy(&dstgrid);

  //----------------------------------------------------------------------------
  ESMC_TestEnd(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  return 0;
}
