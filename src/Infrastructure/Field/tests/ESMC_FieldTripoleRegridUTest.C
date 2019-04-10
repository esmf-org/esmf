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

//==============================================================================
//BOP
// !PROGRAM: ESMC_FieldTripoleRegridUTest - Check ESMC_FieldRegrid functionality
//
// !DESCRIPTION:
//
//EOP
//-----------------------------------------------------------------------------


void create_grid(ESMC_Grid &grid, double max_x_in, double max_y_in)
{

  // Create a Grid
  double max_x, max_y, dx, dy, DEG2RAD;
  max_x = max_x_in;
  max_y = max_y_in;
  dx = 360.0/max_x;
  dy = 180.0/max_y;

  DEG2RAD = 3.141592653589793/180.0;

  int *maxIndex;
  ESMC_InterArrayInt i_maxIndex;
  int dimcount = 2;
  maxIndex = (int *)malloc(dimcount*sizeof(int));
  maxIndex[0] = int(max_x);
  maxIndex[1] = int(max_y);
  ESMC_InterArrayIntSet(&i_maxIndex, maxIndex, dimcount);

  ESMC_CoordSys_Flag coordsys = ESMC_COORDSYS_CART;
  ESMC_TypeKind_Flag typekind = ESMC_TYPEKIND_R8;
  ESMC_PoleKind_Flag polekind[2];
  polekind[0] = ESMC_POLEKIND_MONOPOLE;
  polekind[1] = ESMC_POLEKIND_BIPOLE;
  ESMC_PoleKind_Flag *pkptr = polekind;

  grid = ESMC_GridCreate1PeriDim(&i_maxIndex, NULL, NULL, &coordsys, &typekind,
                                 pkptr, NULL, NULL);

  // free memory
  free(maxIndex);
  //----------------------------------------------------------------------------

  // add coordinates to center
  int rc = ESMC_GridAddCoord(grid, ESMC_STAGGERLOC_CENTER);

#ifdef masking
  rc = ESMC_GridAddItem(grid, ESMC_GRIDITEM_MASK, ESMC_STAGGERLOC_CENTER);

  int *mask = (int *)ESMC_GridGetItem(grid, ESMC_GRIDITEM_MASK,
                                            ESMC_STAGGERLOC_CENTER, NULL);
#endif

  // get and fill first coord array and computational bounds
  int *exLBound_c = (int *)malloc(dimcount*sizeof(int));
  int *exUBound_c = (int *)malloc(dimcount*sizeof(int));

  double *gridXCoord_c = (double *)ESMC_GridGetCoord(grid, 1,
                                                   ESMC_STAGGERLOC_CORNER, NULL,
                                                   exLBound_c, exUBound_c, NULL);
  double *gridYCoord_c = (double *)ESMC_GridGetCoord(grid, 2,
                                                   ESMC_STAGGERLOC_CORNER, NULL,
                                                   NULL, NULL, NULL);

  printf("1\n");
  int p = 0;
  for (int i1=exLBound_c[1]; i1<=exUBound_c[1]; ++i1) {
    for (int i0=exLBound_c[0]; i0<=exUBound_c[0]; ++i0) {
      gridXCoord_c[p]=(double)(i0)*dx;
      gridYCoord_c[p]=(double)(i1)*dy - 90.0;
      ++p;
    }
  }

  printf("2\n");
  // get and fill first coord array and computational bounds
  int *exLBound = (int *)malloc(dimcount*sizeof(int));
  int *exUBound = (int *)malloc(dimcount*sizeof(int));

  double *gridXCoord = (double *)ESMC_GridGetCoord(grid, 1,
                                                   ESMC_STAGGERLOC_CENTER, NULL,
                                                   exLBound, exUBound, NULL);
  double *gridYCoord = (double *)ESMC_GridGetCoord(grid, 2,
                                                   ESMC_STAGGERLOC_CENTER, NULL,
                                                   NULL, NULL, NULL);

  p = 0;
  for (int i1=exLBound[1]; i1<=exUBound[1]; ++i1) {
    for (int i0=exLBound[0]; i0<=exUBound[0]; ++i0) {
      gridXCoord[p]=(double)(i0)*dx + 0.5*dx;
      double y = (double)(i1)*dy - 90.0;
      double yp1 = (double)(i1+1)*dy - 90.0;
      gridYCoord[p]=y+yp1/2.0;
      ++p;
    }
  }

#ifdef masking
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
}

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
  create_grid(srcgrid, 18, 10);
  create_grid(dstgrid, 10, 8);

  //----------------------------------------------------------------------------
  //----------------------- Field CREATION --------------------------------------
  //----------------------------------------------------------------------------

  srcfield = ESMC_FieldCreateGridTypeKind(srcgrid, ESMC_TYPEKIND_R8,
    ESMC_STAGGERLOC_CENTER, NULL, NULL, NULL, "srcfield", &rc);
  dstfield = ESMC_FieldCreateGridTypeKind(dstgrid, ESMC_TYPEKIND_R8,
    ESMC_STAGGERLOC_CENTER, NULL, NULL, NULL, "dstfield", &rc);

  //----------------------------------------------------------------------------
  //-------------------------- REGRIDDING --------------------------------------
  //----------------------------------------------------------------------------

  // get grid bounds and field pointers
  int *exLBound_s = (int *)malloc(dimcount*sizeof(int));
  int *exUBound_s = (int *)malloc(dimcount*sizeof(int));

  double *gridXCoord_s = (double *)ESMC_GridGetCoord(srcgrid, 1,
                                                   ESMC_STAGGERLOC_CENTER, NULL,
                                                   exLBound_s, exUBound_s, NULL);
  double *gridYCoord_s = (double *)ESMC_GridGetCoord(srcgrid, 2,
                                                   ESMC_STAGGERLOC_CENTER, NULL,
                                                   NULL, NULL, NULL);

  //----------------------------------------------------------------------------
  double * srcfieldptr = (double *)ESMC_FieldGetPtr(srcfield, 0, &rc);
  //----------------------------------------------------------------------------

  // define analytic field on source field
  p = 0;
  for (int i1=exLBound_s[1]; i1<=exUBound_s[1]; ++i1) {
    for (int i0=exLBound_s[0]; i0<=exUBound_s[0]; ++i0) {
      x = gridXCoord_s[p];
      y = gridYCoord_s[p];
#ifdef masking
      if (mask[p] == 1)
        srcfieldptr[p] = 10000000;
      else
#endif
        srcfieldptr[p] = 20.0+x+y;
      ++p;
    }
  }


  // get grid bounds and field pointers
  int *exLBound_d = (int *)malloc(dimcount*sizeof(int));
  int *exUBound_d = (int *)malloc(dimcount*sizeof(int));

  double *gridXCoord_d = (double *)ESMC_GridGetCoord(dstgrid, 1,
                                                   ESMC_STAGGERLOC_CENTER, NULL,
                                                   exLBound_d, exUBound_d, &rc);
  double *gridYCoord_d = (double *)ESMC_GridGetCoord(dstgrid, 2,
                                                   ESMC_STAGGERLOC_CENTER, NULL,
                                                   NULL, NULL, NULL);

  //----------------------------------------------------------------------------
  double * dstfieldptr = (double *)ESMC_FieldGetPtr(dstfield, 0, &rc);
  //----------------------------------------------------------------------------

  // initialize destination field
  p = 0;
  for (int i1=exLBound_d[1]; i1<=exUBound_d[1]; ++i1) {
    for (int i0=exLBound_d[0]; i0<=exUBound_d[0]; ++i0) {
      dstfieldptr[p] = 0.0;
      ++p;
    }
  }

  //----------------------------------------------------------------------------
  int *maskValues = (int *)malloc(sizeof(int));
  maskValues[0] = 1;
  ESMC_InterArrayInt i_maskValues;
  rc = ESMC_InterArrayIntSet(&i_maskValues, maskValues, 1);

  //----------------------------------------------------------------------------
  ESMC_UnmappedAction_Flag unmappedaction = ESMC_UNMAPPEDACTION_IGNORE;
  ESMC_RegridMethod_Flag regridmethod = ESMC_REGRIDMETHOD_CONSERVE;
#ifdef masking
  rc = ESMC_FieldRegridStore(srcfield, dstfield, &i_maskValues, NULL, &routehandle,
                             NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, &unmappedaction,
                             NULL, NULL, NULL, NULL, NULL, NULL);
#else
  rc = ESMC_FieldRegridStore(srcfield, dstfield, NULL, NULL, &routehandle,
                             NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
                             NULL, NULL, NULL, NULL, NULL, NULL);
#endif
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  rc = ESMC_FieldRegrid(srcfield, dstfield, routehandle, NULL);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  rc = ESMC_FieldRegridRelease(&routehandle);
  //----------------------------------------------------------------------------

#ifdef ESMF_TESTEXHAUSTIVE
  //----------------------------------------------------------------------------
  //-------------------------- REGRID VALIDATION -------------------------------
  //----------------------------------------------------------------------------
  //EX_disable_UTest
  strcpy(name, "Regridding Validation");
  strcpy(failMsg, "Did not have acceptable accuracy");

  bool correct = true;
  // check destination field against source field
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
#ifdef masking
      tol = 100;
#else
      tol = .0001;
#endif
      if (ESMC_dabs(dstfieldptr[p]-exact) > tol) {
        printf("dstfieldptr[%d] (%f,%f):\n%f /= %f\n",
               p, x, y, dstfieldptr[p], exact);
        correct=false;
      }
      ++p;
    }
  }
  ESMC_Test((correct==true), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
#endif

  //----------------------------------------------------------------------------
  free(exLBound_s);
  free(exUBound_s);
  free(exLBound_d);
  free(exUBound_d);
  free(maskValues);
  rc = ESMC_FieldDestroy(&srcfield);
  rc = ESMC_FieldDestroy(&dstfield);
  rc = ESMC_GridDestroy(&srcgrid);
  rc = ESMC_GridDestroy(&dstgrid);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  ESMC_TestEnd(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  return 0;
}
