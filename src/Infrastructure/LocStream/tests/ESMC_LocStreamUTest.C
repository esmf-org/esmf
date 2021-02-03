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

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

// ESMF header
#include "ESMC.h"

// ESMF Test header
#include "ESMC_Test.h"

#include "ESMC_LocStream.h"
//==============================================================================
//BOP
// !PROGRAM: ESMC_LocStreamUTest - Check ESMC_LocStream functionality
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

  // Field variables
  ESMC_Field srcfield;

  // LocStream variables
  ESMC_LocStream srclocstream;
  ESMC_Array array;
  int ls_size=9;
  const char *keyName="AnyNameHere";
  int cLBound,cUBound;
  double *farray, *farray2, *farray3;

  //----------------------------------------------------------------------------
  ESMC_TestStart(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------


  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "LocStreamCreateLocal");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  srclocstream = ESMC_LocStreamCreateLocal(ls_size, NULL, NULL,&rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "LocStreamAddKeyAlloc");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_LocStreamAddKeyAlloc(srclocstream,keyName,NULL);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "LocStreamGetKeyPtr");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  farray = (double *) ESMC_LocStreamGetKeyPtr(srclocstream,keyName,0,&rc);
  farray[0]=9.9;
  farray[1]=1.1;
  farray[2]=2.2;
  farray[3]=3.3;
  farray[4]=4.4;
  farray[5]=5.5;
  farray[6]=6.6;
  farray[7]=7.7;
  farray[8]=8.8;
  farray2 = (double *) ESMC_LocStreamGetKeyPtr(srclocstream,keyName,0,&rc);
  ESMC_Test((rc==ESMF_SUCCESS) && farray2[8]==farray[8] && farray2[0]==farray[0],
            name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "LocStreamGetKeyPtr");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  array = ESMC_LocStreamGetKeyArray(srclocstream,keyName,&rc);
  farray3 = (double *) ESMC_ArrayGetPtr(array, 0, &rc);
  ESMC_Test((rc==ESMF_SUCCESS) && farray3[8]==farray[8] && farray3[0]==farray[0],
            name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "LocStreamGetBounds");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_LocStreamGetBounds(srclocstream,0,&cLBound,&cUBound);
  ESMC_Test((rc==ESMF_SUCCESS) && cLBound==1 && cUBound==9, name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Create ESMC_Field object on LocStream");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  srcfield = ESMC_FieldCreateLocStreamTypeKind(srclocstream, ESMC_TYPEKIND_R8,
    NULL, NULL, NULL, "srcfield", &rc);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Destroy ESMC_Field object from LocStream");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_FieldDestroy(&srcfield);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Destroy ESMC_LocStream object");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_LocStreamDestroy(&srclocstream);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  ESMC_TestEnd(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  return 0;
}
