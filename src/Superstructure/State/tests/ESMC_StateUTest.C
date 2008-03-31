// $Id: ESMC_StateUTest.C,v 1.8 2008/03/31 22:25:25 theurich Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================

#include <stdlib.h>
#include <string.h>
#include "ESMC.h"
#include "ESMC_Test.h"
#include "ESMC_Start.h"
#include "ESMC_DistGrid.h"
#include "ESMC_ArraySpec.h"
#include "ESMC_State.h"

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

  ESMC_State st;              // ESMC_State object
  char name[80];
  char failMsg[80];
  int result = 0;
  int rc = 0;
  int localrc;

  ESMC_ArraySpec arrayspec;
  int *minIndexValues, *maxIndexValues;
  ESMC_InterfaceInt minIndex, maxIndex;
  ESMC_DistGrid distgrid;
  ESMC_Array array;
  ESMC_Array retrievedArray;

  //----------------------------------------------------------------------------
  ESMC_TestStart(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Create a state object -- cf
  strcpy(name, "StateCreate Unit test \0");
  strcpy(failMsg, "Did not return ESMF_SUCCESS \0");
  st = ESMC_StateCreate("stateName\0",&rc);
  ESMC_Test((rc == ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__,0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  // Set the arrayspec
  strcpy(name, "ArraySpecSet\0");
  strcpy(failMsg, "Did not return ESMF_SUCCESS \0");
  rc = ESMC_ArraySpecSet(&arrayspec, 2, ESMC_TYPEKIND_I4);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
    
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Set up minIndex\0");
  strcpy(failMsg, "Did not return ESMF_SUCCESS\0");
  minIndexValues = (int *)malloc(2*sizeof(int));
  minIndexValues[0] = minIndexValues[1] = 1;
  minIndex = ESMC_InterfaceIntCreate(minIndexValues, 2, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Set up maxIndex\0");
  strcpy(failMsg, "Did not return ESMF_SUCCESS\0");
  maxIndexValues = (int *)malloc(2*sizeof(int));
  maxIndexValues[0] = 5;
  maxIndexValues[1] = 10;
  maxIndex = ESMC_InterfaceIntCreate(maxIndexValues, 2, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Create 5 x 10 ESMC_DistGrid object\0");
  strcpy(failMsg, "Did not return ESMF_SUCCESS\0");
  distgrid = ESMC_DistGridCreate(minIndex, maxIndex, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Clean up minIndex\0");
  strcpy(failMsg, "Did not return ESMF_SUCCESS\0");
  free(minIndexValues);
  rc = ESMC_InterfaceIntDestroy(&minIndex);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Clean up maxIndex\0");
  strcpy(failMsg, "Did not return ESMF_SUCCESS\0");
  free(maxIndexValues);
  rc = ESMC_InterfaceIntDestroy(&maxIndex);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Create ESMC_Array object\0");
  strcpy(failMsg, "Did not return ESMF_SUCCESS\0");
  array = ESMC_ArrayCreate(arrayspec, distgrid, "array1\0", &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
 
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Print ESMC_Array object\0");
  strcpy(failMsg, "Did not return ESMF_SUCCESS\0");
  rc = ESMC_ArrayPrint(array);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //NEX_UTest
  strcpy(name, "Add an Array to a State object\0");
  strcpy(failMsg, "Did not return ESMF_SUCCESS\0");
  rc = ESMC_StateAddArray(st,array);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

 //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Get an array from a state, based on its name\0");
  strcpy(failMsg, "Did not return ESMF_SUCCESS\0");
  rc = ESMC_StateGetArray(st, "array1\0", &retrievedArray);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Compare array stored into and array retrieved from a state\0");
  strcpy(failMsg, "Discrepancy on array pointers\0");
  ESMC_Test((array.ptr==retrievedArray.ptr), name, failMsg, &result, __FILE__, 
            __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Destroy ESMC_DistGrid object\0");
  strcpy(failMsg, "Did not return ESMF_SUCCESS\0");
  rc = ESMC_DistGridDestroy(&distgrid);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Destroy ESMC_Array object\0");
  strcpy(failMsg, "Did not return ESMF_SUCCESS\0");
  rc = ESMC_ArrayDestroy(&array);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------


  //----------------------------------------------------------------------------
  //NEX_UTest
  // Destroy a state object -- cf
  strcpy(name, "StateDestroy Unit test \0");
  strcpy(failMsg, "Did not return ESMF_SUCCESS \0");
  rc = ESMC_StateDestroy(st);
  ESMC_Test((rc == ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__,0);
  //----------------------------------------------------------------------------
 

  //----------------------------------------------------------------------------
  ESMC_TestEnd(result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  return 0;
}
