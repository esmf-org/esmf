// $Id: ESMC_DistGridUTest.C,v 1.5.4.1 2010/02/05 19:55:27 svasquez Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research, 
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
// !PROGRAM: ESMC_DistGridUTest - Check ESMC_DistGrid functionality
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
  
  int *minIndexValues, *maxIndexValues;
  ESMC_InterfaceInt minIndex, maxIndex;
  ESMC_DistGrid distgrid;

  //----------------------------------------------------------------------------
  ESMC_TestStart(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Set up minIndex");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  minIndexValues = (int *)malloc(2*sizeof(int));
  minIndexValues[0] = minIndexValues[1] = 1;
  minIndex = ESMC_InterfaceIntCreate(minIndexValues, 2, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Set up maxIndex");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  maxIndexValues = (int *)malloc(2*sizeof(int));
  maxIndexValues[0] = 5;
  maxIndexValues[1] = 10;
  maxIndex = ESMC_InterfaceIntCreate(maxIndexValues, 2, &rc);
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
  strcpy(name, "Clean up minIndex");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  free(minIndexValues);
  rc = ESMC_InterfaceIntDestroy(&minIndex);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Clean up maxIndex");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  free(maxIndexValues);
  rc = ESMC_InterfaceIntDestroy(&maxIndex);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Print ESMC_DistGrid object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_DistGridPrint(distgrid);
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
  strcpy(name, "Print destroyed ESMC_DistGrid object");
  strcpy(failMsg, "Did return ESMF_SUCCESS");
  rc = ESMC_DistGridPrint(distgrid);
  ESMC_Test((rc!=ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  ESMC_TestEnd(result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  return 0;
}
