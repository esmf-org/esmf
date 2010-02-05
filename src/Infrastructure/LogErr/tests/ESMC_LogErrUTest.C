// $Id: ESMC_LogErrUTest.C,v 1.3.4.1 2010/02/05 19:59:06 svasquez Exp $
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

// ESMF header
#include "ESMC.h"

// ESMF Test header
#include "ESMC_Test.h"

//==============================================================================
//BOP
// !PROGRAM: ESMC_LogErrUTest - Check ESMC_LogErr functionality
//
// !DESCRIPTION:
//
//EOP
//-----------------------------------------------------------------------------

int main(void){

  char name[80];
  char failMsg[80];
  int result = 0;
  int trueFalseRc;

  char *msg = "C LogErr Write Message";
  int msgtype = ESMC_LOG_INFO;

  //----------------------------------------------------------------------------
  ESMC_TestStart(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Write a Log Message"); 
  strcpy(failMsg, "Did not return ESMF_TRUE");
  trueFalseRc = ESMC_LogWrite(msg, msgtype);
  ESMC_Test((trueFalseRc=ESMF_TRUE), name, failMsg, &result, __FILE__, __LINE__,
    0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  ESMC_TestEnd(result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  return 0;
}

