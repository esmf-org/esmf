// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2016, University Corporation for Atmospheric Research,
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

  ESMC_Grid grid_np, grid_1p, grid_1p_pdim1, grid_1p_pdim2, grid_tripole, grid_from_file;
  ESMC_VM vm;

  int dimcount = 2;
  int *maxIndex;
  ESMC_InterfaceInt i_maxIndex;
  int p;
  bool pass;
  int elbnd[dimcount],eubnd[dimcount];


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
  //  GridCreate1PeriDim (no periodicDim or poleDim specified)
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Create a Grid
  ESMC_CoordSys_Flag coordsys = ESMC_COORDSYS_CART;
  ESMC_TypeKind_Flag typekind = ESMC_TYPEKIND_R8;
  maxIndex = (int *)malloc(dimcount*sizeof(int));
  maxIndex[0] = 12;
  maxIndex[1] = 20;
  rc = ESMC_InterfaceIntSet(&i_maxIndex, maxIndex, dimcount);

  strcpy(name, "GridCreate");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  grid_1p = ESMC_GridCreate1PeriDim(&i_maxIndex, NULL, NULL, &coordsys, &typekind, NULL, NULL, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //  GridCreate1PeriDim (periodicDim = 1, no poleDim specified)
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Create a Grid
  strcpy(name, "GridCreate_periodicDim_1");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  int periodicDim = 1;
  grid_1p_pdim1 = ESMC_GridCreate1PeriDim(&i_maxIndex, &periodicDim, NULL, 
                                          &coordsys, &typekind, NULL, NULL, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  if (rc == ESMF_SUCCESS) {
    rc = ESMC_GridDestroy(&grid_1p_pdim1);
  }
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //  GridCreate1PeriDim (periodicDim = 2, poleDim = 1)
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Create a Grid
  strcpy(name, "GridCreate_periodicDim_2");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  // The periodicDim cannot be the same as poledim, so need to change
  // both parameters.
  periodicDim = 2;
  int poleDim = 1;
  grid_1p_pdim2 = ESMC_GridCreate1PeriDim(&i_maxIndex, &periodicDim, &poleDim,
                                          &coordsys, &typekind, NULL, NULL, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  if (rc == ESMF_SUCCESS) {
    rc = ESMC_GridDestroy(&grid_1p_pdim2);
  }
  free(maxIndex);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Create grid object from SCRIP file with both regDecomp and decompflag
  // set to NULL.
  strcpy(name, "GridCreateFromFile_SCRIP");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#ifdef ESMF_NETCDF
  grid_from_file = ESMC_GridCreateFromFile("data/T42_grid.nc",
					   ESMC_FILEFORMAT_SCRIP,
					   NULL, NULL, NULL, NULL, NULL, NULL,
					   NULL, NULL, NULL, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  if (rc == ESMF_SUCCESS) {
    rc = ESMC_GridDestroy(&grid_from_file);
  }
#else
  // No NetCDF, so just PASS this test.
  ESMC_Test(1, name, failMsg, &result, __FILE__, __LINE__, 0);
#endif
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Create grid object from SCRIP file with regDecomp specified and
  // decompflag = {BALANCED, BALANCED}
  strcpy(name, "GridCreateFromFile_SCRIP_decomp_BALANCED_BALANCED");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#ifdef ESMF_NETCDF
  int regDecomp[2] = {petCount,1};
  int decompflag[2] = {ESMC_DECOMP_BALANCED, ESMC_DECOMP_BALANCED};
  grid_from_file = ESMC_GridCreateFromFile("data/T42_grid.nc", 
					   ESMC_FILEFORMAT_SCRIP,
					   regDecomp, decompflag,
					   NULL, NULL, NULL, NULL, NULL, NULL, NULL,
					   &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  if (rc == ESMF_SUCCESS) {
    rc = ESMC_GridDestroy(&grid_from_file);
  }
#else
  // No NetCDF, so just PASS this test.
  ESMC_Test(1, name, failMsg, &result, __FILE__, __LINE__, 0);
#endif
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Create grid object from SCRIP file with regDecomp specified and
  // decompflag = {BALANCED, RESTFIRST}
  strcpy(name, "GridCreateFromFile_SCRIP_decomp_BALANCED_RESTFIRST");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#ifdef ESMF_NETCDF
  decompflag[1] = ESMC_DECOMP_RESTFIRST;
  grid_from_file = ESMC_GridCreateFromFile("data/T42_grid.nc", 
					   ESMC_FILEFORMAT_SCRIP,
					   regDecomp, decompflag, 
					   NULL, NULL, NULL, NULL, NULL, NULL, NULL,
					   &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  if (rc == ESMF_SUCCESS) {
    rc = ESMC_GridDestroy(&grid_from_file);
  }
#else
  // No NetCDF, so just PASS this test.
  ESMC_Test(1, name, failMsg, &result, __FILE__, __LINE__, 0);
#endif
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Create grid object from SCRIP file with regDecomp specified and
  // decompflag = {BALANCED, RESTLAST}
  strcpy(name, "GridCreateFromFile_SCRIP_decomp_BALANCED_RESTLAST");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#ifdef ESMF_NETCDF
  decompflag[1] = ESMC_DECOMP_RESTLAST;
  grid_from_file = ESMC_GridCreateFromFile("data/T42_grid.nc", 
					   ESMC_FILEFORMAT_SCRIP,
					   regDecomp, decompflag, 
					   NULL, NULL, NULL, NULL, NULL, NULL, NULL,
					   &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  if (rc == ESMF_SUCCESS) {
    rc = ESMC_GridDestroy(&grid_from_file);
  }
#else
  // No NetCDF, so just PASS this test.
  ESMC_Test(1, name, failMsg, &result, __FILE__, __LINE__, 0);
#endif
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Create grid object from SCRIP file with regDecomp specified and
  // decompflag = {BALANCED, CYCLIC}.  Since CYCLIC is currently not 
  // support, this test passes if ESMC_GridCreateFromFile fails with
  // rc=ESMF_RC_ARG_OUTOFRANGE.
  strcpy(name, "GridCreateFromFile_SCRIP_decomp_BALANCED_CYCLIC");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#ifdef ESMF_NETCDF
  decompflag[1] = ESMC_DECOMP_CYCLIC;
  grid_from_file = ESMC_GridCreateFromFile("data/T42_grid.nc", 
					   ESMC_FILEFORMAT_SCRIP,
					   regDecomp, decompflag,
					   NULL, NULL, NULL, NULL, NULL, NULL, NULL,
					   &rc);
  ESMC_Test((rc==ESMF_RC_ARG_OUTOFRANGE), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  if (rc == ESMF_SUCCESS) {
    rc = ESMC_GridDestroy(&grid_from_file);
  }
#else
  // No NetCDF, so just PASS this test.
  ESMC_Test(1, name, failMsg, &result, __FILE__, __LINE__, 0);
#endif
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Create grid object from SCRIP file with regDecomp specified and
  // decompflag = {RESTFIRST, BALANCED}
  strcpy(name, "GridCreateFromFile_SCRIP_decomp_RESTFIRST_BALANCED");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#ifdef ESMF_NETCDF
  decompflag[0] = ESMC_DECOMP_RESTFIRST;
  decompflag[1] = ESMC_DECOMP_BALANCED;
  grid_from_file = ESMC_GridCreateFromFile("data/T42_grid.nc", 
					   ESMC_FILEFORMAT_SCRIP,
					   regDecomp, decompflag, 
					   NULL, NULL, NULL, NULL, NULL, NULL, NULL,
					   &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  if (rc == ESMF_SUCCESS) {
    rc = ESMC_GridDestroy(&grid_from_file);
  }
#else
  // No NetCDF, so just PASS this test.
  ESMC_Test(1, name, failMsg, &result, __FILE__, __LINE__, 0);
#endif
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Create grid object from SCRIP file with regDecomp specified and
  // decompflag = {RESTFIRST, RESTFIRST}
  strcpy(name, "GridCreateFromFile_SCRIP_decomp_RESTFIRST_RESTFIRST");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#ifdef ESMF_NETCDF
  decompflag[1] = ESMC_DECOMP_RESTFIRST;
  grid_from_file = ESMC_GridCreateFromFile("data/T42_grid.nc", 
					   ESMC_FILEFORMAT_SCRIP,
					   regDecomp, decompflag, 
					   NULL, NULL, NULL, NULL, NULL, NULL, NULL,
					   &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  if (rc == ESMF_SUCCESS) {
    rc = ESMC_GridDestroy(&grid_from_file);
  }
#else
  // No NetCDF, so just PASS this test.
  ESMC_Test(1, name, failMsg, &result, __FILE__, __LINE__, 0);
#endif
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Create grid object from SCRIP file with regDecomp specified and
  // decompflag = {RESTFIRST, RESTLAST}
  strcpy(name, "GridCreateFromFile_SCRIP_decomp_RESTFIRST_RESTLAST");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#ifdef ESMF_NETCDF
  decompflag[1] = ESMC_DECOMP_RESTLAST;
  grid_from_file = ESMC_GridCreateFromFile("data/T42_grid.nc", 
					   ESMC_FILEFORMAT_SCRIP,
					   regDecomp, decompflag, 
					   NULL, NULL, NULL, NULL, NULL, NULL, NULL,
					   &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  if (rc == ESMF_SUCCESS) {
    rc = ESMC_GridDestroy(&grid_from_file);
  }
#else
  // No NetCDF, so just PASS this test.
  ESMC_Test(1, name, failMsg, &result, __FILE__, __LINE__, 0);
#endif
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Create grid object from SCRIP file with regDecomp specified and
  // decompflag = {RESTFIRST, CYCLIC}.  Since CYCLIC is currently not 
  // support, this test passes if ESMC_GridCreateFromFile fails with
  // rc=ESMF_RC_ARG_OUTOFRANGE.
  strcpy(name, "GridCreateFromFile_SCRIP_decomp_RESTFIRST_CYCLIC");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#ifdef ESMF_NETCDF
  decompflag[1] = ESMC_DECOMP_CYCLIC;
  grid_from_file = ESMC_GridCreateFromFile("data/T42_grid.nc", 
					   ESMC_FILEFORMAT_SCRIP,
					   regDecomp, decompflag,
					   NULL, NULL, NULL, NULL, NULL, NULL, NULL,
					   &rc);
  ESMC_Test((rc==ESMF_RC_ARG_OUTOFRANGE), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  if (rc == ESMF_SUCCESS) {
    rc = ESMC_GridDestroy(&grid_from_file);
  }
#else
  // No NetCDF, so just PASS this test.
  ESMC_Test(1, name, failMsg, &result, __FILE__, __LINE__, 0);
#endif
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Create grid object from SCRIP file with regDecomp specified and
  // decompflag = {RESTLAST, BALANCED}
  strcpy(name, "GridCreateFromFile_SCRIP_decomp_RESTLAST_BALANCED");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#ifdef ESMF_NETCDF
  decompflag[0] = ESMC_DECOMP_RESTLAST;
  decompflag[1] = ESMC_DECOMP_BALANCED;
  grid_from_file = ESMC_GridCreateFromFile("data/T42_grid.nc", 
					   ESMC_FILEFORMAT_SCRIP,
					   regDecomp, decompflag, 
					   NULL, NULL, NULL, NULL, NULL, NULL, NULL,
					   &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  if (rc == ESMF_SUCCESS) {
    rc = ESMC_GridDestroy(&grid_from_file);
  }
#else
  // No NetCDF, so just PASS this test.
  ESMC_Test(1, name, failMsg, &result, __FILE__, __LINE__, 0);
#endif
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Create grid object from SCRIP file with regDecomp specified and
  // decompflag = {RESTLAST, RESTFIRST}
  strcpy(name, "GridCreateFromFile_SCRIP_decomp_RESTLAST_RESTFIRST");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#ifdef ESMF_NETCDF
  decompflag[1] = ESMC_DECOMP_RESTFIRST;
  grid_from_file = ESMC_GridCreateFromFile("data/T42_grid.nc", 
					   ESMC_FILEFORMAT_SCRIP,
					   regDecomp, decompflag, 
					   NULL, NULL, NULL, NULL, NULL, NULL, NULL,
					   &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  if (rc == ESMF_SUCCESS) {
    rc = ESMC_GridDestroy(&grid_from_file);
  }
#else
  // No NetCDF, so just PASS this test.
  ESMC_Test(1, name, failMsg, &result, __FILE__, __LINE__, 0);
#endif
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Create grid object from SCRIP file with regDecomp specified and
  // decompflag = {RESTLAST, RESTLAST}
  strcpy(name, "GridCreateFromFile_SCRIP_decomp_RESTLAST_RESTLAST");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#ifdef ESMF_NETCDF
  decompflag[1] = ESMC_DECOMP_RESTLAST;
  grid_from_file = ESMC_GridCreateFromFile("data/T42_grid.nc", 
					   ESMC_FILEFORMAT_SCRIP,
					   regDecomp, decompflag, 
					   NULL, NULL, NULL, NULL, NULL, NULL, NULL,
					   &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  if (rc == ESMF_SUCCESS) {
    rc = ESMC_GridDestroy(&grid_from_file);
  }
#else
  // No NetCDF, so just PASS this test.
  ESMC_Test(1, name, failMsg, &result, __FILE__, __LINE__, 0);
#endif
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Create grid object from SCRIP file with regDecomp specified and
  // decompflag = {RESTLAST, CYCLIC}.  Since CYCLIC is currently not 
  // support, this test passes if ESMC_GridCreateFromFile fails with
  // rc=ESMF_RC_ARG_OUTOFRANGE.
  strcpy(name, "GridCreateFromFile_SCRIP_decomp_RESTLAST_CYCLIC");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#ifdef ESMF_NETCDF
  decompflag[1] = ESMC_DECOMP_CYCLIC;
  grid_from_file = ESMC_GridCreateFromFile("data/T42_grid.nc", 
					   ESMC_FILEFORMAT_SCRIP,
					   regDecomp, decompflag,
					   NULL, NULL, NULL, NULL, NULL, NULL, NULL,
					   &rc);
  ESMC_Test((rc==ESMF_RC_ARG_OUTOFRANGE), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  if (rc == ESMF_SUCCESS) {
    rc = ESMC_GridDestroy(&grid_from_file);
  }
#else
  // No NetCDF, so just PASS this test.
  ESMC_Test(1, name, failMsg, &result, __FILE__, __LINE__, 0);
#endif
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Create grid object from SCRIP file with regDecomp specified and
  // decompflag = {CYCLIC, BALANCED}.  Since CYCLIC is currently not 
  // support, this test passes if ESMC_GridCreateFromFile fails with
  // rc=ESMF_RC_ARG_OUTOFRANGE.
  strcpy(name, "GridCreateFromFile_SCRIP_decomp_CYCLIC_BALANCED");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#ifdef ESMF_NETCDF
  decompflag[0] = ESMC_DECOMP_CYCLIC;
  decompflag[1] = ESMC_DECOMP_BALANCED;
  grid_from_file = ESMC_GridCreateFromFile("data/T42_grid.nc", 
					   ESMC_FILEFORMAT_SCRIP,
					   regDecomp, decompflag,
					   NULL, NULL, NULL, NULL, NULL, NULL, NULL,
					   &rc);
  ESMC_Test((rc==ESMF_RC_ARG_OUTOFRANGE), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  if (rc == ESMF_SUCCESS) {
    rc = ESMC_GridDestroy(&grid_from_file);
  }
#else
  // No NetCDF, so just PASS this test.
  ESMC_Test(1, name, failMsg, &result, __FILE__, __LINE__, 0);
#endif
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Create grid object from SCRIP file with regDecomp specified and
  // decompflag = {CYCLIC, RESTFIRST}.  Since CYCLIC is currently not 
  // support, this test passes if ESMC_GridCreateFromFile fails with
  // rc=ESMF_RC_ARG_OUTOFRANGE.
  strcpy(name, "GridCreateFromFile_SCRIP_decomp_CYCLIC_RESTFIRST");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#ifdef ESMF_NETCDF
  decompflag[1] = ESMC_DECOMP_RESTFIRST;
  grid_from_file = ESMC_GridCreateFromFile("data/T42_grid.nc", 
					   ESMC_FILEFORMAT_SCRIP,
					   regDecomp, decompflag,
					   NULL, NULL, NULL, NULL, NULL, NULL, NULL,
					   &rc);
  ESMC_Test((rc==ESMF_RC_ARG_OUTOFRANGE), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  if (rc == ESMF_SUCCESS) {
    rc = ESMC_GridDestroy(&grid_from_file);
  }
#else
  // No NetCDF, so just PASS this test.
  ESMC_Test(1, name, failMsg, &result, __FILE__, __LINE__, 0);
#endif
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Create grid object from SCRIP file with regDecomp specified and
  // decompflag = {CYCLIC, RESTLAST}.  Since CYCLIC is currently not 
  // support, this test passes if ESMC_GridCreateFromFile fails with
  // rc=ESMF_RC_ARG_OUTOFRANGE.
  strcpy(name, "GridCreateFromFile_SCRIP_decomp_CYCLIC_RESTLAST");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#ifdef ESMF_NETCDF
  decompflag[1] = ESMC_DECOMP_RESTLAST;
  grid_from_file = ESMC_GridCreateFromFile("data/T42_grid.nc", 
					   ESMC_FILEFORMAT_SCRIP,
					   regDecomp, decompflag,
					   NULL, NULL, NULL, NULL, NULL, NULL, NULL,
					   &rc);
  ESMC_Test((rc==ESMF_RC_ARG_OUTOFRANGE), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  if (rc == ESMF_SUCCESS) {
    rc = ESMC_GridDestroy(&grid_from_file);
  }
#else
  // No NetCDF, so just PASS this test.
  ESMC_Test(1, name, failMsg, &result, __FILE__, __LINE__, 0);
#endif
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Create grid object from SCRIP file with regDecomp specified and
  // decompflag = {CYCLIC, CYCLIC}.  Since CYCLIC is currently not 
  // support, this test passes if ESMC_GridCreateFromFile fails with
  // rc=ESMF_RC_ARG_OUTOFRANGE.
  strcpy(name, "GridCreateFromFile_SCRIP_decomp_CYCLIC_CYCLIC");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#ifdef ESMF_NETCDF
  decompflag[1] = ESMC_DECOMP_CYCLIC;
  grid_from_file = ESMC_GridCreateFromFile("data/T42_grid.nc", 
					   ESMC_FILEFORMAT_SCRIP,
					   regDecomp, decompflag,
					   NULL, NULL, NULL, NULL, NULL, NULL, NULL,
					   &rc);
  ESMC_Test((rc==ESMF_RC_ARG_OUTOFRANGE), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  if (rc == ESMF_SUCCESS) {
    rc = ESMC_GridDestroy(&grid_from_file);
  }
#else
  // No NetCDF, so just PASS this test.
  ESMC_Test(1, name, failMsg, &result, __FILE__, __LINE__, 0);
#endif
  //----------------------------------------------------------------------------

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
  rc = ESMC_InterfaceIntSet(&i_maxIndex, maxIndex, dimcount);

  strcpy(name, "GridCreate");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  grid_np = ESMC_GridCreateNoPeriDim(&i_maxIndex, &coordsys, &typekind, NULL, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);

  // free memory
  free(maxIndex);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //  GridAddCoord to grid_np
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "GridAddCoord");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_GridAddCoord(grid_np, ESMC_STAGGERLOC_CORNER);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------


  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "GridGetCoordBounds");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc=ESMC_GridGetCoordBounds(grid_np, 
                             ESMC_STAGGERLOC_CORNER, 
                             elbnd,
                             eubnd,NULL);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //  GridAddItem to grid_np
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "GridAddItem");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_GridAddItem(grid_np, ESMC_GRIDITEM_MASK, ESMC_STAGGERLOC_CORNER);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //  GridCreate1PeriDim for a tripole grid
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  // Create a Grid
  maxIndex = (int *)malloc(dimcount*sizeof(int));
  maxIndex[0] = 12;
  maxIndex[1] = 20;
  rc = ESMC_InterfaceIntSet(&i_maxIndex, maxIndex, dimcount);

  strcpy(name, "GridCreate");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  ESMC_PoleKind_Flag polekind[2];
  polekind[0] = ESMC_POLEKIND_MONOPOLE;
  polekind[1] = ESMC_POLEKIND_BIPOLE;
  ESMC_PoleKind_Flag *pkptr = polekind;
  grid_tripole = ESMC_GridCreate1PeriDim(&i_maxIndex, NULL, NULL, &coordsys, 
                                         &typekind, pkptr, NULL, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  // free memory
  free(maxIndex);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //  GridAddCoord and GetCoord on grid_tripole
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "GridAddCoord");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_GridAddCoord(grid_tripole, ESMC_STAGGERLOC_CENTER);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  // get and fill first coord array and computational bounds
  int *exLBound_tripole = (int *)malloc(dimcount*sizeof(int));
  int *exUBound_tripole = (int *)malloc(dimcount*sizeof(int));

  strcpy(name, "GridGetCoord - X");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  double *gridXCoord_tripole = (double *)ESMC_GridGetCoord(grid_tripole, 1,
                                                   ESMC_STAGGERLOC_CENTER,
                                                   exLBound_tripole, 
                                                   exUBound_tripole, &rc);

  p = 0;
  for (int i1=exLBound_tripole[1]; i1<=exUBound_tripole[1]; ++i1) {
    for (int i0=exLBound_tripole[0]; i0<=exUBound_tripole[0]; ++i0) {
      gridXCoord_tripole[p]=(double)(i0);
      //printf("PET%d - set gridXCoord_tripole[%d] = %f (%f)\n", localPet, p, 
      //  (double)(i0), gridXCoord_tripole[p]);
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
  double *gridYCoord_tripole = (double *)ESMC_GridGetCoord(grid_tripole, 2, 
                                                   ESMC_STAGGERLOC_CENTER,
                                                   NULL, NULL, &rc);

  p = 0;
  for (int i1=exLBound_tripole[1]; i1<=exUBound_tripole[1]; ++i1) {
    for (int i0=exLBound_tripole[0]; i0<=exUBound_tripole[0]; ++i0) {
      gridYCoord_tripole[p]=(double)(i1);
      //printf("PET%d - set gridYCoord_tripole[%d] = %f (%f)\n", localPet, p, 
      //  (double)(i1), gridYCoord_tripole[p]);
      ++p;
    }
  }

  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
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
  //  GridAddItem mask to grid_1p
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "GridAddItem");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_GridAddItem(grid_1p, ESMC_GRIDITEM_MASK, ESMC_STAGGERLOC_CENTER);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  // get and fill item array
  strcpy(name, "GridGetItem");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  int *gridMask = (int *)ESMC_GridGetItem(grid_1p, ESMC_GRIDITEM_MASK,
                                                   ESMC_STAGGERLOC_CENTER,
                                                   &rc);

  p = 0;
  for (int i1=exLBound[1]; i1<=exUBound[1]; ++i1) {
    for (int i0=exLBound[0]; i0<=exUBound[0]; ++i0) {
      gridMask[p]=i0;
      //printf("PET%d - set gridMask[%d] = %d (%d)\n", localPet, p, 
        //i0, gridMask[p]);
      ++p;
    }
  }

  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //  GridAddItem area to grid_1p
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  strcpy(name, "GridAddItem");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_GridAddItem(grid_1p, ESMC_GRIDITEM_AREA, ESMC_STAGGERLOC_CENTER);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  // get and fill item array
  strcpy(name, "GridGetItem");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  double *gridArea = (double *)ESMC_GridGetItem(grid_1p, ESMC_GRIDITEM_AREA,
                                                   ESMC_STAGGERLOC_CENTER,
                                                   &rc);

  p = 0;
  for (int i1=exLBound[1]; i1<=exUBound[1]; ++i1) {
    for (int i0=exLBound[0]; i0<=exUBound[0]; ++i0) {
      gridArea[p]=1;
      //printf("PET%d - set gridMask[%d] = %d (%d)\n", localPet, p,
        //i0, gridMask[p]);
      ++p;
    }
  }

  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //  GridWrite
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //EX_UTest
  // Write a Grid
  strcpy(name, "GridWrite");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_GridWrite(grid_1p, ESMC_STAGGERLOC_CENTER, "gridfile");
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);

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
  ESMC_TestEnd(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  return 0;
}

