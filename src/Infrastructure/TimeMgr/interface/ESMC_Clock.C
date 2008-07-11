// $Id: ESMC_Clock.C,v 1.1 2008/07/11 23:59:28 rosalind Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2008, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMC_Clock.C"
//==============================================================================
//
// ESMC Clock method implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the public C Clock methods declared
// in the companion file ESMC_Clock.h
//
//-----------------------------------------------------------------------------


// include ESMF headers
#include "ESMCI_Arg.h"
#include "ESMC_LogErr.h"
#include "ESMF_LogMacros.inc"             // for LogErr
#include "ESMCI_Clock.h"
#include "ESMC_Interface.h"
#include "ESMC_Time.h"
#include "ESMCI_Time.h"
#include "ESMC_TimeInterval.h"
#include "ESMCI_TimeInterval.h"


// include associated header file
#include "ESMC_Clock.h"

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id: ESMC_Clock.C,v 1.1 2008/07/11 23:59:28 rosalind Exp $";
//-----------------------------------------------------------------------------

extern "C" {

ESMC_Clock ESMC_ClockCreate(
      int                nameLen,      // in
      const char        *name,         // in
      ESMC_TimeInterval  timeStep,     // in
      ESMC_Time          startTime,    // in
      ESMC_Time          stopTime,     // in
      int               *rc) {         // out - return code
#undef ESMC_METHOD
#define ESMC_METHOD "ESMC_ClockCreate()"

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  *rc = ESMC_RC_NOT_IMPL;   // final return code

  ESMC_Clock clock;

  // call into ESMCI method
  ESMCI::TimeInterval* ptimeStep = (ESMCI::TimeInterval*)(timeStep.ptr);
  ESMCI::Time* pstartTime = (ESMCI::Time*)(startTime.ptr); 
  ESMCI::Time* pstopTime  = (ESMCI::Time*)(stopTime.ptr); 


  clock.ptr = (void *)
     ESMCI::ESMCI_ClockCreate(nameLen, name, ptimeStep,
     pstartTime, pstopTime,
     (ESMCI::TimeInterval*)NULL, (int*)NULL, (ESMCI::Time*)NULL, 
     &localrc);

  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)){
    clock.ptr = NULL;
    return clock;  // bail out
  }

  // return successfully
  *rc = ESMF_SUCCESS;
  return clock;

} // end ESMC_ClockCreate


int ESMC_ClockPrint(ESMC_Clock clock){

#undef ESMC_METHOD
#define ESMC_METHOD "ESMC_ClockPrint()"

  // initialize return code; assume routine not implemented
  int rc= ESMC_RC_NOT_IMPL;          // local return code
  int localrc = ESMC_RC_NOT_IMPL;    // local return code

  ESMCI::Clock *IntClock = (ESMCI::Clock*)(clock.ptr);
  localrc = IntClock->print();
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc)){
    clock.ptr = NULL;
    return rc;  // bail out
  }

  //return successfully
  return ESMF_SUCCESS;
 
} // end ESMC_ClockPrint

int ESMC_ClockDestroy(ESMC_Clock* pClock){
  int rc = ESMF_RC_NOT_IMPL;
  int localrc = ESMF_RC_NOT_IMPL;
   
  ESMCI::Clock **intClock = (ESMCI::Clock**)(pClock);
  localrc = ESMCI::ESMCI_ClockDestroy(intClock);
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc)){
    pClock = NULL;
    return rc;  // bail out
  }
  
  //return successfully
  return ESMF_SUCCESS;

} // end ESMC_ClockPrint

}; // extern "C"
