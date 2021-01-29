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
  int *maxIndex;
  ESMC_InterArrayInt i_maxIndex;

  bool correct = true;

  // Get parallel information
  vm=ESMC_VMGetGlobal(&rc);
  if (rc != ESMF_SUCCESS) return 0;

  rc=ESMC_VMGet(vm, &localPet, &petCount, (int *)NULL, (MPI_Comm *)NULL,
                (int *)NULL, (int *)NULL);
  if (rc != ESMF_SUCCESS) return 0;

  //----------------------------------------------------------------------------
  //----------------------- GRID CREATION --------------------------------------
  //----------------------------------------------------------------------------

  int dimcount = 2;
  maxIndex = (int *)malloc(dimcount*sizeof(int));
  maxIndex[0] = 8;
  maxIndex[1] = 8;
  rc = ESMC_InterArrayIntSet(&i_maxIndex, maxIndex, dimcount);

  ESMC_IndexFlag indexflag = ESMC_INDEX_GLOBAL;
  srcgrid = ESMC_GridCreateNoPeriDim(&i_maxIndex, NULL, NULL, &indexflag, &rc);
  if (rc != ESMF_SUCCESS) return 0;

  dstgrid = ESMC_GridCreateNoPeriDim(&i_maxIndex, NULL, NULL, &indexflag, &rc);
  if (rc != ESMF_SUCCESS) return 0;

  int *exLBound = NULL;
  int *exUBound = NULL;
  int p = 0;

  ESMC_GridAddCoord(srcgrid, ESMC_STAGGERLOC_CENTER);

  exLBound = (int *)malloc(dimcount*sizeof(int));
  exUBound = (int *)malloc(dimcount*sizeof(int));

  double *gridXCoord = (double *)ESMC_GridGetCoord(srcgrid, 1,
                                                   ESMC_STAGGERLOC_CENTER, NULL,
                                                   exLBound, exUBound, &rc);

  double *gridYCoord = (double *)ESMC_GridGetCoord(srcgrid, 2,
                                                   ESMC_STAGGERLOC_CENTER, NULL,
                                                   NULL, NULL, &rc);

  //printf("exLBounds = [%d,%d]\n", exLBound[0], exLBound[1]);
  //printf("exUBounds = [%d,%d]\n", exUBound[0], exUBound[1]);

  p = 0;
  for (int i1=exLBound[1]; i1<=exUBound[1]; ++i1) {
    for (int i0=exLBound[0]; i0<=exUBound[0]; ++i0) {
      gridXCoord[p]=i0;
      gridYCoord[p]=i1;
      ++p;
    }
  }

  ESMC_GridAddCoord(dstgrid, ESMC_STAGGERLOC_CENTER);

  exLBound = (int *)malloc(dimcount*sizeof(int));
  exUBound = (int *)malloc(dimcount*sizeof(int));

  gridXCoord = (double *)ESMC_GridGetCoord(dstgrid, 1,
                                                   ESMC_STAGGERLOC_CENTER, NULL,
                                                   exLBound, exUBound, &rc);

  gridYCoord = (double *)ESMC_GridGetCoord(dstgrid, 2,
                                                   ESMC_STAGGERLOC_CENTER, NULL,
                                                   NULL, NULL, &rc);

  // printf("exLBounds = [%d,%d]\n", exLBound[0], exLBound[1]);
  // printf("exUBounds = [%d,%d]\n", exUBound[0], exUBound[1]);

  p = 0;
  for (int i1=exLBound[1]; i1<=exUBound[1]; ++i1) {
    for (int i0=exLBound[0]; i0<=exUBound[0]; ++i0) {
      gridXCoord[p]=i0;
      gridYCoord[p]=i1;
      ++p;
    }
  }

  //----------------------------------------------------------------------------
  //---------------------- FIELD CREATION --------------------------------------
  //----------------------------------------------------------------------------

  srcfield = ESMC_FieldCreateGridTypeKind(srcgrid, ESMC_TYPEKIND_R8,
    ESMC_STAGGERLOC_CENTER, NULL, NULL, NULL, "srcfield", &rc);
  if (rc != ESMF_SUCCESS) return 0;

  dstfield = ESMC_FieldCreateGridTypeKind(dstgrid, ESMC_TYPEKIND_R8,
    ESMC_STAGGERLOC_CENTER, NULL, NULL, NULL, "dstfield", &rc);
  if (rc != ESMF_SUCCESS) return 0;

  // get and fill first coord array and computational bounds
  exLBound = (int *)malloc(dimcount*sizeof(int));
  exUBound = (int *)malloc(dimcount*sizeof(int));

  rc = ESMC_FieldGetBounds(srcfield, 0, exLBound, exUBound, dimcount);
  if (rc != ESMF_SUCCESS) return 0;

  double * srcfieldptr = (double *)ESMC_FieldGetPtr(srcfield, 0, &rc);
  if (rc != ESMF_SUCCESS) return 0;

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
  if (rc != ESMF_SUCCESS) return 0;

  double * dstfieldptr = (double *)ESMC_FieldGetPtr(dstfield, 0, &rc);
  if (rc != ESMF_SUCCESS) return 0;

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

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "ESMC_FieldRegridStoreFile test");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  ESMC_Logical create_rh = ESMF_TRUE;
  rc = ESMC_FieldRegridStoreFile(srcfield, dstfield, 
                                 "data/weights_esmc_smmsff.nc", NULL, NULL,
                                 &routehandle, NULL, NULL, NULL, NULL, NULL,
                                 NULL, NULL, &create_rh, NULL, NULL, NULL,
                                 NULL, NULL, NULL, NULL, NULL);
#if (defined ESMF_PIO && ( defined ESMF_NETCDF || defined ESMF_PNETCDF))
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
#else
  strcpy(failMsg, "Did not return ESMF_RC_LIB_NOT_PRESENT");
  ESMC_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, &result, __FILE__, __LINE__, 0);
#endif
  //----------------------------------------------------------------------------

#if 0
  printf("srcfield before smmstore = [\n");
  p = 0;
  for (int i1=exLBound[1]; i1<=exUBound[1]; ++i1) {
    for (int i0=exLBound[0]; i0<=exUBound[0]; ++i0) {
      printf("%f, ", srcfieldptr[p]);
      p++;
    }
  }
  printf("]\n");
#endif

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "ESMC_FieldSMMStore from File test");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  // skip this routine if weight file was not written because of library not present
  if (rc == ESMF_RC_LIB_NOT_PRESENT) {
    ESMC_Test((ESMF_SUCCESS==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  } else {
    rc = ESMC_FieldSMMStore(srcfield, dstfield, "data/weights_esmc_smmsff.nc", 
                            &routehandle, NULL, NULL, NULL);
#if (defined ESMF_PIO && ( defined ESMF_NETCDF || defined ESMF_PNETCDF))
    ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
#else
    strcpy(failMsg, "Did not return ESMF_RC_LIB_NOT_PRESENT");
    ESMC_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, &result, __FILE__, __LINE__, 0);
#endif
  }
  //----------------------------------------------------------------------------


  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "ESMC_FieldSMMStore From File validation");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  // skip this routine if weight file was not written because of library not present
  if (rc == ESMF_RC_LIB_NOT_PRESENT) {
    ESMC_Test((ESMF_SUCCESS==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  } else {
    p = 0;
    for (int i1=exLBound[1]; i1<=exUBound[1]; ++i1) {
      for (int i0=exLBound[0]; i0<=exUBound[0]; ++i0) {
        if ((srcfieldptr[p] - 42.) > .01) {
          correct = false;
          // printf("source value = %f\n", srcfieldptr[p]);
        }
        p++;
      }
    }
#if (defined ESMF_PIO && ( defined ESMF_NETCDF || defined ESMF_PNETCDF))
    ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
#else
    strcpy(failMsg, "Did not return ESMF_RC_LIB_NOT_PRESENT");
    ESMC_Test((rc==ESMF_RC_LIB_NOT_PRESENT), name, failMsg, &result, __FILE__, __LINE__, 0);
#endif
  }
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "ESMC_FieldRegridRelease");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#if (defined ESMF_PIO && ( defined ESMF_NETCDF || defined ESMF_PNETCDF))
  rc = ESMC_FieldRegridRelease(&routehandle);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
#else
  // dummy test
  ESMC_Test((true), name, failMsg, &result, __FILE__, __LINE__, 0);
#endif
  //----------------------------------------------------------------------------

  free(exLBound);
  free(exUBound);
  free(exLBound2);
  free(exUBound2);

  rc = ESMC_FieldDestroy(&srcfield);
  rc = ESMC_FieldDestroy(&dstfield);
  rc = ESMC_GridDestroy(&srcgrid);
  rc = ESMC_GridDestroy(&dstgrid);

  //----------------------------------------------------------------------------
  ESMC_TestEnd(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  return 0;
}
