// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2022, University Corporation for Atmospheric Research,
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
// !PROGRAM: ESMC_FieldUTest - Check ESMC_Field functionality
//
// !DESCRIPTION:
//
//EOP
//-----------------------------------------------------------------------------

int main(void){

  char name[80];
  char failMsg[80];
  int result = 0;
  int rc;

  int *gridToFieldMap, *ungriddedLBound, *ungriddedUBound;
  ESMC_InterArrayInt i_gridToFieldMap, i_ungriddedLBound, i_ungriddedUBound;
  ESMC_ArraySpec arrayspec;
  ESMC_Field field;
  ESMC_Grid grid, grid1;
  ESMC_Array array;
  
  bool correct = false;

  //----------------------------------------------------------------------------
  ESMC_TestStart(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  ESMC_LogSet(true);

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Create a mesh
  const char *coords[2];
  coords[0] = "longitude";
  coords[1] = "latitude";
  strcpy(name, "GridCreate1DGridSpec");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  grid = ESMC_GridCreateFromFile("data/gridspec1Dcoords.nc", ESMC_FILEFORMAT_GRIDSPEC, 
                         NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
                         coords, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  int exLB[2], exUB[2];
  rc = ESMC_GridGetCoordBounds(grid, ESMC_STAGGERLOC_CENTER, NULL, exLB, exUB, NULL);

  printf("exlb = [%d, %d]\n", exLB[0], exLB[1]);
  printf("exub = [%d, %d]\n", exUB[0], exUB[1]);

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Set the arrayspec
  strcpy(name, "ArraySpecSet");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_ArraySpecSet(&arrayspec, 3, ESMC_TYPEKIND_R8);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Set up gridToFieldMap");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  gridToFieldMap = (int *)malloc(2*sizeof(int));
  gridToFieldMap[0] = 1;
  gridToFieldMap[1] = 2;
  rc = ESMC_InterArrayIntSet(&i_gridToFieldMap, gridToFieldMap, 2);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Set up ungriddedLBound");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  ungriddedLBound = (int *)malloc(sizeof(int));
  ungriddedLBound[0] = 1;
  rc = ESMC_InterArrayIntSet(&i_ungriddedLBound, ungriddedLBound, 1);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Set up ungriddedUBound");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  ungriddedUBound = (int *)malloc(sizeof(int));
  ungriddedUBound[0] = 366;
  rc = ESMC_InterArrayIntSet(&i_ungriddedUBound, ungriddedUBound, 1);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Create ESMC_Field object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  field = ESMC_FieldCreateGridArraySpec(grid, arrayspec, ESMC_STAGGERLOC_CENTER, 
    &i_gridToFieldMap, &i_ungriddedLBound, &i_ungriddedUBound, "field1", &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------


  //----------------------------------------------------------------------------
  // Field Read and Write
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Read a Field from File");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_FieldRead(field, "data/gridspec1Dcoords.nc", "pr", 366, ESMF_IOFMT_NETCDF);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Write a Field to File");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_FieldWrite(field, "gridspec1Dcoords-out.nc", "pr", true, ESMC_FILESTATUS_UNKNOWN, 366, ESMF_IOFMT_NETCDF);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------


  //----------------------------------------------------------------------------
  // Field object retrieval tests
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Get an ESMC_Array object from ESMC_Field object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  array = ESMC_FieldGetArray(field, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Get a void * C pointer to data from ESMC_Field object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  void * ptr = ESMC_FieldGetPtr(field, 0, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Print an ESMC_Field object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_FieldPrint(field);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Destroy ESMC_Field object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_FieldDestroy(&field);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Destroy ESMC_Grid object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_GridDestroy(&grid);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  free(gridToFieldMap);
  free(ungriddedLBound);
  free(ungriddedUBound);

  //----------------------------------------------------------------------------
  ESMC_TestEnd(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  return 0;
}
