// $Id$
//
// Earth System Modeling Framework
// Copyright (c) 2002-2025, University Corporation for Atmospheric Research, 
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
// !PROGRAM: ESMC_ArrayUTest - Check ESMC_Array functionality
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
  
  ESMC_ArraySpec arrayspec;
  int *minIndexValues, *maxIndexValues;
  ESMC_InterArrayInt minIndex, maxIndex;
  ESMC_DistGrid distgrid;
  ESMC_Array array;

  //----------------------------------------------------------------------------
  ESMC_TestStart(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  // Set the arrayspec
  strcpy(name, "ArraySpecSet");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_ArraySpecSet(&arrayspec, 2, ESMC_TYPEKIND_I4);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
    
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Set up minIndex");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  minIndexValues = (int *)malloc(2*sizeof(int));
  minIndexValues[0] = minIndexValues[1] = 1;
  rc = ESMC_InterArrayIntSet(&minIndex, minIndexValues, 2);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Set up maxIndex");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  maxIndexValues = (int *)malloc(2*sizeof(int));
  maxIndexValues[0] = 5;
  maxIndexValues[1] = 10;
  rc = ESMC_InterArrayIntSet(&maxIndex, maxIndexValues, 2);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Create 5 x 10 ESMC_DistGrid object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  distgrid = ESMC_DistGridCreate(minIndex, maxIndex, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  // okay to free minIndexValues and maxIndexValues variables now
  free(minIndexValues);
  free(maxIndexValues);
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Create ESMC_Array object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  array = ESMC_ArrayCreate(arrayspec, distgrid, "array1", &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Print ESMC_Array object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_ArrayPrint(array);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Destroy ESMC_DistGrid object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_DistGridDestroy(&distgrid);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Destroy ESMC_Array object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_ArrayDestroy(&array);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  ESMC_TestEnd(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  return 0;
}
