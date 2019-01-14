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

// ESMF header
#include "ESMC.h"

// ESMF Test header
#include "ESMC_Test.h"

//==============================================================================
//BOP
// !PROGRAM: ESMC_TestUTest - Check ESMC_Test functionality
//
// !DESCRIPTION:
//
//  The code in this file drives C State unit tests.
 // The companion file ESMC\_State.F90 contains the definitions for the
 // State methods.

//EOP
//-----------------------------------------------------------------------------

int main(void){

  ESMC_State myState;               // ESMC_State object
  const char* arrayName;
  char name[80];
  char failMsg[80];
  int result = 0;
  int rc = 0;
  int localrc;

  ESMC_ArraySpec arrayspec;
  int *minIndexValues, *maxIndexValues;
  ESMC_InterArrayInt minIndex, maxIndex;
  ESMC_DistGrid distgrid;
  ESMC_Array array;
  ESMC_Array retrievedArray;

  //----------------------------------------------------------------------------
  ESMC_TestStart(__FILE__, __LINE__, 0);
  ESMC_VM vm = ESMC_VMGetGlobal (&rc);
  int localPet, petCount;
  rc = ESMC_VMGet (vm, &localPet, &petCount, NULL, NULL, NULL, NULL);
  //----------------------------------------------------------------------------
  

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Create a state object -- cf
  strcpy(name, "StateCreate Unit test");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  myState = ESMC_StateCreate("name",&rc);
  ESMC_Test((rc == ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__,0);
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
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Create ESMC_Array object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  array = ESMC_ArrayCreate(arrayspec, distgrid, "array1", &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
 
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Retrieve the name of an ESMC_Array object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  arrayName = ESMC_ArrayGetName(array,&rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Verify array object name retrieved");
  strcpy(failMsg, "Did not retrieve array name correctly");
  ESMC_Test(!strcmp(arrayName, "array1"), name, failMsg, &result, __FILE__,
    __LINE__, 0);
  //----------------------------------------------------------------------------
 
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Initialize data an ESMC_Array object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  int *arrayData = (int *) ESMC_ArrayGetPtr(array, 0,&rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Print ESMC_Array object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_ArrayPrint(array);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //NEX_UTest
  strcpy(name, "Add an Array to a State object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_StateAddArray(myState,array);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Get an array from a state, based on its name");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_StateGetArray(myState, "array1", &retrievedArray);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Compare array stored into and array retrieved from a state");
  strcpy(failMsg, "Discrepancy on array pointers");
  ESMC_Test((array.ptr==retrievedArray.ptr), name, failMsg, &result, __FILE__, 
            __LINE__, 0);
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
  //NEX_UTest
  // Destroy a state object -- cf
  strcpy(name, "StateDestroy Unit test");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_StateDestroy(&myState);
  ESMC_Test((rc == ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__,0);
  //----------------------------------------------------------------------------
 
  free (minIndexValues);
  free (maxIndexValues);

  //----------------------------------------------------------------------------
  ESMC_TestEnd(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  return 0;
}
