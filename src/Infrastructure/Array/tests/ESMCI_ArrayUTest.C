// $Id$
//
// Earth System Modeling Framework
// Copyright (c) 2002-2023, University Corporation for Atmospheric Research,
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

#include "ESMC.h"
#include "ESMC_Test.h"
#include "ESMCI_Array.h"

using namespace ESMCI;

//==============================================================================
//BOP
// !PROGRAM: ESMCI_ArrayUTest - Check ESMCI_Array functionality
//
// !DESCRIPTION:
//
//EOP
//-----------------------------------------------------------------------------

int main(void){

  char name[80];
  char failMsg[80];
  int result = 0;
  int rc = ESMF_FAILURE;

  ArraySpec arrayspec2d;
  DistGrid *distgrid2d;
  DistGrid *distgrid4d;
  ESMCI::Array *array2d;

  //----------------------------------------------------------------------------
  ESMC_TestStart(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "ArraySpecSet");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = arrayspec2d.set(2, ESMC_TYPEKIND_I4);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Create 2-d DistGrid");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  int minIndex2d[2] = {1,1};
  int maxIndex2d[2] = {5,10};
  InterArray<int> minIndex2dInterface(minIndex2d, 2);
  InterArray<int> maxIndex2dInterface(maxIndex2d, 2);
  distgrid2d = DistGrid::create(&minIndex2dInterface, &maxIndex2dInterface,
      NULL, NULL, 0, NULL, NULL, NULL, NULL, NULL, (DELayout*)NULL, NULL, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Create 2-d Array");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  array2d = Array::create(&arrayspec2d, distgrid2d,
      NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Check number of replicated dims in 2-d Array with 0 replicated dims");
  strcpy(failMsg, "Incorrect number of replicated dims");
  int replicatedDimCount = array2d->getReplicatedDimCount();
  ESMC_Test((replicatedDimCount == 0), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Destroy 2-d Array");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = Array::destroy(&array2d);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Destroy 2-d DistGrid");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = DistGrid::destroy(&distgrid2d);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Create 4-d DistGrid");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  int minIndex4d[4] = {1,1,1,1};
  int maxIndex4d[4] = {5,10,4,8};
  InterArray<int> minIndex4dInterface(minIndex4d, 4);
  InterArray<int> maxIndex4dInterface(maxIndex4d, 4);
  distgrid4d = DistGrid::create(&minIndex4dInterface, &maxIndex4dInterface,
      NULL, NULL, 0, NULL, NULL, NULL, NULL, NULL, (DELayout*)NULL, NULL, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Create 2-d Array with 2 replicated dims");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  int distgridToArrayMap4d[4] = {2,0,1,0};
  InterArray<int> distgridToArrayMap4dInterface(distgridToArrayMap4d, 4);
  array2d = Array::create(&arrayspec2d, distgrid4d,
      &distgridToArrayMap4dInterface, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Check number of replicated dims in 2-d Array with 2 replicated dims");
  strcpy(failMsg, "Incorrect number of replicated dims");
  replicatedDimCount = array2d->getReplicatedDimCount();
  ESMC_Test((replicatedDimCount == 2), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Destroy 2-d Array");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = Array::destroy(&array2d);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Destroy 4-d DistGrid");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = DistGrid::destroy(&distgrid4d);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  ESMC_TestEnd(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  return 0;
}
