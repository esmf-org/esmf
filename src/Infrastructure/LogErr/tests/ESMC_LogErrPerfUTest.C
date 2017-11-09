// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2017, University Corporation for Atmospheric Research, 
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

#define ESMC_METHOD "perfMsgFoundErrorC"
int perfMsgFoundErrorC(int n, double &dt){
  double t0, t1;
  int rc;
  ESMCI::VMK::wtime(&t0);
  for (int i=0; i<n; i++){
    ESMC_LogDefault.MsgFoundError(ESMF_SUCCESS,
      ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc);
  }
  ESMCI::VMK::wtime(&t1);
  dt = (t1-t0)/double(n);
  std::stringstream msg;
  msg << "perfMsgFoundErrorC: " << n << "\t iterations took " << t1-t0 <<
    "\t seconds. => " << dt << "\t per iteration.";
  ESMC_LogDefault.Write(msg.str(), ESMC_LOGMSG_INFO);
  return ESMF_SUCCESS;
}

int main(void){

  char name[80];
  char failMsg[80];
  int result = 0;
  int rc;
  double dt;
  
  //----------------------------------------------------------------------------
  ESMC_TestStart(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Performance of MsgFoundError() 1000x Test");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = perfMsgFoundErrorC(1000, dt);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
    
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Performance of MsgFoundError() 10000x Test");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = perfMsgFoundErrorC(10000, dt);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
    
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Performance of MsgFoundError() 100000x Test");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = perfMsgFoundErrorC(100000, dt);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
    
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Performance of MsgFoundError() 1000000x Test");
  strcpy(failMsg, "Did not return ESMF_SUCCESS");
  rc = perfMsgFoundErrorC(1000000, dt);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "Threshold check for MsgFoundError() 1000000x Test");
  strcpy(failMsg, "MsgFoundError() performance problem");
  ESMC_Test((dt<5.e-8), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
    
  //----------------------------------------------------------------------------
  ESMC_TestEnd(__FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------
  
  return 0;
}
