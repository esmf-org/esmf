// $Id: ESMC_GridUTest.C,v 1.1 2012/03/07 16:47:18 rokuingh Exp $
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
// !PROGRAM: ESMC_GridUTest - Check ESMC_Grid functionality
//
// !DESCRIPTION:
//
//EOP
//-----------------------------------------------------------------------------

int main(void){

  char name[80];
  char failMsg[80];
  int result = 0;
  int localPet, petCount;
  int rc;
  bool correct;

  ESMC_Grid grid_np, grid_1p;
  ESMC_VM vm;

  int dimcount = 2;
  int *maxIndex;
  ESMC_InterfaceInt i_maxIndex;
  int p;
  bool pass;

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

#ifdef ESMF_TESTEXHAUSTIVE
  //----------------------------------------------------------------------------
  //  GridCreateNoPeriDim
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  // Create a Grid
  maxIndex = (int *)malloc(dimcount*sizeof(int));
  maxIndex[0] = 20;
  maxIndex[1] = 20;
  i_maxIndex = ESMC_InterfaceIntCreate(maxIndex, dimcount, &rc);

  strcpy(name, "GridCreate");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  grid_np = ESMC_GridCreateNoPeriDim(i_maxIndex, ESMC_COORDSYS_CART, 
                                         ESMC_TYPEKIND_R8, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);

  // free memory
  free(maxIndex);
  ESMC_InterfaceIntDestroy(&i_maxIndex);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //  GridAddCoord to grid_np
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "GridAddCoord");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_GridAddCoord(grid_np, ESMC_STAGGERLOC_CENTER);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //  GridCreate1PeriDim
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  // Create a Grid
  maxIndex = (int *)malloc(dimcount*sizeof(int));
  maxIndex[0] = 12;
  maxIndex[1] = 20;
  i_maxIndex = ESMC_InterfaceIntCreate(maxIndex, dimcount, &rc);

  strcpy(name, "GridCreate");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  grid_1p = ESMC_GridCreate1PeriDim(i_maxIndex, ESMC_COORDSYS_CART,
                                    ESMC_TYPEKIND_R8, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  // free memory
  free(maxIndex);
  ESMC_InterfaceIntDestroy(&i_maxIndex);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //  GridAddCoord and GetCoord on grid_1p
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "GridAddCoord");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_GridAddCoord(grid_1p, ESMC_STAGGERLOC_CENTER);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  // get and fill first coord array and computational bounds
  int *exLBound = (int *)malloc(dimcount*sizeof(int));
  int *exUBound = (int *)malloc(dimcount*sizeof(int));

  strcpy(name, "GridGetCoord - X");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  double *gridXCoord = (double *)ESMC_GridGetCoord(grid_1p, 1,
                                                   ESMC_STAGGERLOC_CENTER,
                                                   exLBound, exUBound, &rc);

  p = 0;
  for (int i1=exLBound[1]; i1<=exUBound[1]; ++i1) {
    for (int i0=exLBound[0]; i0<=exUBound[0]; ++i0) {
      gridXCoord[p]=(double)(i0);
      //printf("PET%d - set gridXCoord[%d] = %f (%f)\n", localPet, p, 
      //  (double)(i0), gridXCoord[p]);
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
  double *gridYCoord = (double *)ESMC_GridGetCoord(grid_1p, 2, 
                                                   ESMC_STAGGERLOC_CENTER,
                                                   NULL, NULL, &rc);

  p = 0;
  for (int i1=exLBound[1]; i1<=exUBound[1]; ++i1) {
    for (int i0=exLBound[0]; i0<=exUBound[0]; ++i0) {
      gridYCoord[p]=(double)(i1);
      //printf("PET%d - set gridYCoord[%d] = %f (%f)\n", localPet, p, 
      //  (double)(i1), gridYCoord[p]);
      ++p;
    }
  }

  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "Validate Grid coordinates - X");
  strcpy(failMsg, "Grid X coordinates not set correctly");
  double *gridXCoord_check = (double *)ESMC_GridGetCoord(grid_1p, 1,
                                                         ESMC_STAGGERLOC_CENTER, 
                                                         NULL, NULL, &rc);
  pass = true;
  p = 0;
  for (int i1=exLBound[1]; i1<=exUBound[1]; ++i1) {
    for (int i0=exLBound[0]; i0<=exUBound[0]; ++i0) {
      if (gridXCoord_check[p] != (double)(i0)) {
        printf("FAIL - PET%d - gridXCoord[%d] = %f\n",localPet, p, gridXCoord_check[p]);
        pass = false;
      }
      ++p;
    }
  }

  ESMC_Test((pass==true), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "Validate Grid coordinates - Y");
  strcpy(failMsg, "Grid Y coordinates not set correctly");
  double *gridYCoord_check = (double *)ESMC_GridGetCoord(grid_1p, 2, 
                                                         ESMC_STAGGERLOC_CENTER, 
                                                         NULL, NULL, &rc);
  pass = true;
  p = 0;
  for (int i1=exLBound[1]; i1<=exUBound[1]; ++i1) {
    for (int i0=exLBound[0]; i0<=exUBound[0]; ++i0) {
      if (gridYCoord_check[p] != (double)(i1)) {
        printf("FAIL - PET%d - gridYCoord[%d] = %f\n",localPet, p, gridYCoord_check[p]);
        pass = false;
      }
      ++p;
    }
  }

  ESMC_Test((pass==true), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //  GridDestroy
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  // Destroy a Grid
  strcpy(name, "GridDestroy");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_GridDestroy(&grid_np);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  // Destroy a Grid
  strcpy(name, "GridDestroy");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_GridDestroy(&grid_1p);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
#endif

  //----------------------------------------------------------------------------
  ESMC_TestEnd(result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  return 0;
}

