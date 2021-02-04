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

#define masking

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

  // VM variables
  int localPet, petCount;
  ESMC_VM vm;
  
  // Field variables
  ESMC_RouteHandle routehandle;
  ESMC_Field srcfield, dstfield;

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

  strcpy(name, "GridCreate");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  ESMC_CoordSys_Flag coordsys = ESMC_COORDSYS_CART;
  ESMC_TypeKind_Flag typekind = ESMC_TYPEKIND_R8;
  srcgrid = ESMC_GridCreateNoPeriDim(&i_maxIndex, &coordsys, &typekind, NULL, &rc);
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
                                      ESMC_STAGGERLOC_CENTER, NULL, &rc);
#endif
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  // get and fill first coord array and computational bounds
  int *exLBound = (int *)malloc(dimcount*sizeof(int));
  int *exUBound = (int *)malloc(dimcount*sizeof(int));

  strcpy(name, "Grid Get and Set Coords");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  double *gridXCoord = (double *)ESMC_GridGetCoord(srcgrid, 1,
                                                   ESMC_STAGGERLOC_CENTER, NULL,
                                                   exLBound, exUBound, &rc);

  double *gridYCoord = (double *)ESMC_GridGetCoord(srcgrid, 2,
                                                   ESMC_STAGGERLOC_CENTER, NULL,
                                                   NULL, NULL, &rc);

  printf("exLBounds = [%d,%d]\n", exLBound[0], exLBound[1]);
  printf("exUBounds = [%d,%d]\n", exUBound[0], exUBound[1]);

  p = 0;
  for (int i1=exLBound[1]; i1<=exUBound[1]; ++i1) {
    for (int i0=exLBound[0]; i0<=exUBound[0]; ++i0) {
      gridXCoord[p]=((double)(i0-1)*cellwidth_x) + lb_x + (double)(cellwidth_x/2.0);
      gridYCoord[p]=((double)(i1-1)*cellwidth_y) + lb_y + (double)(cellwidth_y/2.0);
#ifdef masking
      if (i0 == 2) mask[p] = 1;
      else mask[p] = 0;
#endif
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

  strcpy(name, "GridCreate");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  dstgrid = ESMC_GridCreateNoPeriDim(&i_maxIndex_d, &coordsys, &typekind, NULL, &rc);
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
  rc = ESMC_GridAddCoord(dstgrid, ESMC_STAGGERLOC_CENTER);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "GridAddItem");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#ifdef masking
  rc = ESMC_GridAddItem(dstgrid, ESMC_GRIDITEM_MASK, ESMC_STAGGERLOC_CENTER);
#endif
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "GridGetItem - mask");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#ifdef masking
  int *dstmask = (int *)ESMC_GridGetItem(dstgrid, ESMC_GRIDITEM_MASK, 
                                      ESMC_STAGGERLOC_CENTER, NULL, &rc);
#endif
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  // get and fill first coord array and computational bounds
  int *exLBound_d = (int *)malloc(dimcount*sizeof(int));
  int *exUBound_d = (int *)malloc(dimcount*sizeof(int));

  strcpy(name, "Grid Get and Set Coords - DST");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  double *gridXCoord_d = (double *)ESMC_GridGetCoord(dstgrid, 1,
                                                     ESMC_STAGGERLOC_CENTER, NULL,
                                                     exLBound_d, exUBound_d, &rc);

  double *gridYCoord_d = (double *)ESMC_GridGetCoord(dstgrid, 2,
                                                     ESMC_STAGGERLOC_CENTER, NULL,
                                                     NULL, NULL, &rc);

  printf("exLBounds = [%d,%d]\n", exLBound_d[0], exLBound_d[1]);
  printf("exUBounds = [%d,%d]\n", exUBound_d[0], exUBound_d[1]);

  p = 0;
  for (int i1=exLBound_d[1]; i1<=exUBound_d[1]; ++i1) {
    for (int i0=exLBound_d[0]; i0<=exUBound_d[0]; ++i0) {
      gridXCoord_d[p]=((double)(i0-1)*cellwidth_x) + lb_x + (double)(cellwidth_x/2.0) ;
      gridYCoord_d[p]=((double)(i1-1)*cellwidth_y) + lb_y + (double)(cellwidth_y/2.0);
#ifdef masking
      if (i1 == 2) dstmask[p] = 1;
      else dstmask[p] = 0;
#endif
      ++p;
    }
  }

  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
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

  // define analytic field on destination field
  p = 0;
  for (int i1=exLBound[1]; i1<=exUBound[1]; ++i1) {
    for (int i0=exLBound[0]; i0<=exUBound[0]; ++i0) {
      x = gridXCoord[p];
      y = gridYCoord[p];
      srcfieldptr[p] = 20.0 + x + y;
      //srcfieldptr[p] = 20.0;
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

  // define analytic field on destination field
  p = 0;
  for (int i1=exLBound_d[1]; i1<=exUBound_d[1]; ++i1) {
    for (int i0=exLBound_d[0]; i0<=exUBound_d[0]; ++i0) {
      dstfieldptr[p] = -1.0;
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
  ESMC_UnmappedAction_Flag unmappedaction = ESMC_UNMAPPEDACTION_IGNORE;
  ESMC_ExtrapMethod_Flag extrapmethod = ESMC_EXTRAPMETHOD_NEAREST_IDAVG;
#ifdef masking
  rc = ESMC_FieldRegridStore(srcfield, dstfield, &i_maskValues, &i_maskValues, 
                             &routehandle, 
                             NULL, NULL, NULL, NULL, NULL, 
                             &extrapmethod, NULL, NULL, NULL, &unmappedaction,
                             NULL, NULL, NULL, NULL, NULL, NULL);
#else
  rc = ESMC_FieldRegridStore(srcfield, dstfield, NULL, NULL, &routehandle, 
                             NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
                             NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
#endif
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "Execute ESMC_FieldRegrid()");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#ifdef masking
  // we are using the ESMC_REGION_SELECT option here because the destination
  // field was initialized to -1, so the values that are modified by the 
  // SMM operation will need to be initialized to 0 before having the new
  // value added to them.
  ESMC_Region_Flag zeroregion = ESMC_REGION_SELECT;
  rc = ESMC_FieldRegrid(srcfield, dstfield, routehandle, &zeroregion);
#else
  rc = ESMC_FieldRegrid(srcfield, dstfield, routehandle, NULL);
#endif
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

  // validate against analytic values on destination field
  bool correct = true;
  p = 0;
  for (int i1=exLBound_d[1]; i1<=exUBound_d[1]; ++i1) {
    for (int i0=exLBound_d[0]; i0<=exUBound_d[0]; ++i0) {
      x = gridXCoord_d[p];
      y = gridYCoord_d[p];
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
#ifdef masking
      if (dstmask[p] == 1) {
        if (dstfieldptr[p] >= 0) {
          printf("dstfieldptr [%f,%f] = %f!!\n", x, y, dstfieldptr[p]);
          correct=false;
        }
      }
#endif
      ++p;
    }
  }

  ESMC_Test((correct==true), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  free(exLBound);
  free(exUBound);
  free(exLBound_d);
  free(exUBound_d);
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
