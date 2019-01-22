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
#include <stdio.h>

// ESMF header
#include "ESMC.h"
#include "ESMCI_VM.h"
#include "ESMCI_LogErr.h"

// ESMF Test header
#include "ESMC_Test.h"

//==============================================================================
//BOP
// !PROGRAM: ESMC_LogErrPerfUTest - This unit test file tests LogErr performance
//
// !DESCRIPTION:
//
//EOP
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef ESMC_METHOD
#define ESMC_METHOD "perfFoundError()"
int perfFoundError(int n, double &dt){
  double t0, t1;
  int rc;
  ESMCI::VMK::wtime(&t0);
  for (int i=0; i<n; i++){
    ESMC_LogDefault.FoundError(ESMF_SUCCESS,
      ESMC_CONTEXT, &rc);
  }
  ESMCI::VMK::wtime(&t1);
  dt = (t1-t0)/double(n);
  std::stringstream msg;
  msg << "perfFoundError: " << n << "\t iterations took " << t1-t0 <<
    "\t seconds. => " << dt << "\t per iteration.";
  ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_INFO);
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef ESMC_METHOD
#define ESMC_METHOD "perfMsgFoundError()"
int perfMsgFoundError(int n, double &dt){
  double t0, t1;
  int rc;
  ESMCI::VMK::wtime(&t0);
  for (int i=0; i<n; i++){
    ESMC_LogDefault.MsgFoundError(ESMF_SUCCESS, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, &rc);
  }
  ESMCI::VMK::wtime(&t1);
  dt = (t1-t0)/double(n);
  std::stringstream msg;
  msg << "perfMsgFoundError: " << n << "\t iterations took " << t1-t0 <<
    "\t seconds. => " << dt << "\t per iteration.";
  ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_INFO);
  return ESMF_SUCCESS;
}
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef ESMC_METHOD
#define ESMC_METHOD "main()"
int main(void){

  char name[80];
  char failMsg[80];
  int result = 0;
  int rc;
  double dt, dtTest;
  
  //----------------------------------------------------------------------------
  ESMC_TestStart(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Performance of ESMCI::LogErr::FoundError() 1000x Test");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = perfFoundError(1000, dt);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
    
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Performance of ESMCI::LogErr::FoundError() 10000x Test");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = perfFoundError(10000, dt);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
    
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Performance of ESMCI::LogErr::FoundError() 100000x Test");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = perfFoundError(100000, dt);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
    
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Performance of ESMCI::LogErr::FoundError() 1000000x Test");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = perfFoundError(1000000, dt);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Threshold check for ESMCI::LogErr::FoundError() 1000000x Test");
  strcpy(failMsg, "FoundError() performance problem");
  dtTest = 5.e-8; // this is expected to pass even in debug mode
  ESMC_Test((dt<dtTest), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
    
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Performance of ESMCI::LogErr::MsgFoundError() 1000x Test");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = perfMsgFoundError(1000, dt);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
    
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Performance of ESMCI::LogErr::MsgFoundError() 10000x Test");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = perfMsgFoundError(10000, dt);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
    
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Performance of ESMCI::LogErr::MsgFoundError() 100000x Test");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = perfMsgFoundError(100000, dt);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
    
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Performance of ESMCI::LogErr::MsgFoundError() 1000000x Test");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = perfMsgFoundError(1000000, dt);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Threshold check for ESMCI::LogErr::MsgFoundError() 1000000x Test");
  strcpy(failMsg, "MsgFoundError() performance problem");
  dtTest = 5.e-8; // this is expected to pass even in debug mode
  ESMC_Test((dt<dtTest), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
    
  //----------------------------------------------------------------------------
  ESMC_TestEnd(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  return 0;
}
