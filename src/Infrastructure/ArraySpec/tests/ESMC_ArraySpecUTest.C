// $Id: ESMC_ArraySpecUTest.C,v 1.6.4.1 2010/02/05 19:53:24 svasquez Exp $
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
//  The code in this file drives C ArraySpec unit tests.
 // The companion file ESMF\_ArraySpec.F90 contains the definitions for the
 // ArraySpec methods.

//EOP
//-----------------------------------------------------------------------------

int main(void){

  ESMC_ArraySpec arrayspec;              // ESMC_ArraySpec object
  char name[80];
  char failMsg[80];
  int result = 0;
  int rc = 0;
  int localrc;
  int rank = 2;
  int rank_out=0;
  ESMC_TypeKind typekind; 
  ESMC_TypeKind typekind_out; 
  typekind= ESMC_TYPEKIND_I4;

  //----------------------------------------------------------------------------
  ESMC_TestStart(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  

  //----------------------------------------------------------------------------
  //NEX_UTest
  // Create an arrayspec object -- cf
  strcpy(name, "ArraySpecSet Unit test");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_ArraySpecSet(&arrayspec, rank, typekind);
  ESMC_Test((rc == ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  // Get the data values of an arrayspec object -- cf
  strcpy(name, "ArraySpecGet Unit test");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = ESMC_ArraySpecGet(arrayspec, &rank_out, &typekind_out);
  ESMC_Test((rc == ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

#ifdef ESMF_TESTEXHAUSTIVE
  //EX_UTest
  // Verify output of ESMC_ArraySpecGet: kind
  strcpy(name, "ArraySpecGet: rank Unit test");
  strcpy(failMsg, "Rank returned is wrong");
  ESMC_Test((rank == rank_out), name, failMsg, &result, __FILE__, __LINE__, 0);

  //----------------------------------------------------------------------------

  //EX_UTest
  // Verify output of ESMC_ArraySpecGet: typekind
  strcpy(name, "ArraySpecGet: typekind Unit test");
  strcpy(failMsg, "Typekind returned is wrong");
  ESMC_Test((typekind == typekind_out), name, failMsg, &result, __FILE__, __LINE__, 0);

#endif
  //----------------------------------------------------------------------------
  ESMC_TestEnd(result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  return 0;
}
