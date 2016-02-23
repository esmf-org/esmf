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

extern "C" {
  void ESMC_ScripInq (const char *, int *, int *, int *);
  void ESMC_GridspecInq (const char *, int *, int *, int *);
}

//==============================================================================
//BOP
// !PROGRAM: ESMC_IO_InqUTest - Check Scrip and Gridspec inquiry functionality
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

  ESMC_Grid grid_np, grid_1p, grid_tripole, grid_from_file;
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

#ifdef ESMF_TESTEXHAUSTIVE
  // Exhaustive tests here
#endif
  //----------------------------------------------------------------------------
  //NEX_UTest
  // Test Scrip inquiry.
  strcpy(name, "ScripInq");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#ifdef ESMF_NETCDF
  int grid_dims[2] = {0,0};
  int grid_rank = 0;
  ESMC_ScripInq("./T42_grid.nc", grid_dims, &grid_rank, &rc);
  printf ("grid_rank=%d\n", grid_rank);
  printf ("grid_dims=[%d,%d]\n", grid_dims[0], grid_dims[1]);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
#else
  // No NetCDF, so just PASS this test.
  ESMC_Test(1, name, failMsg, &result, __FILE__, __LINE__, 0);
#endif
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Test Gridspec inquiry.
  strcpy(name, "GridspecInq");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
#ifdef ESMF_NETCDF
  int gridspec_grid_dims[2] = {0,0};
  int ndims = 0;
  ESMC_GridspecInq("./GRIDSPEC_320x160.nc", &ndims, gridspec_grid_dims, &rc);
  printf ("ndims=%d\n", ndims);
  printf ("grid_dims=[%d,%d]\n", gridspec_grid_dims[0], gridspec_grid_dims[1]);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
#else
  // No NetCDF, so just PASS this test.
  ESMC_Test(1, name, failMsg, &result, __FILE__, __LINE__, 0);
#endif
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  ESMC_TestEnd(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  return 0;
}
