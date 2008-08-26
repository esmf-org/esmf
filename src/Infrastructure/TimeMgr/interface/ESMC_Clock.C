// $Id: ESMC_Clock.C,v 1.5 2008/08/26 15:46:37 theurich Exp $
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

// include associated header file
#include "ESMC_Clock.h"

// include ESMF headers
#include "ESMCI_Arg.h"
#include "ESMCI_LogErr.h"
#include "ESMF_LogMacros.inc"             // for LogErr
#include "ESMCI_Clock.h"
#include "ESMC_Interface.h"
#include "ESMC_Time.h"
#include "ESMCI_Time.h"
#include "ESMC_TimeInterval.h"
#include "ESMCI_TimeInterval.h"



//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id: ESMC_Clock.C,v 1.5 2008/08/26 15:46:37 theurich Exp $";
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

  if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)){
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
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc)){
    clock.ptr = NULL;
    return rc;  // bail out
  }

  //return successfully
  return ESMF_SUCCESS;
 
} // end ESMC_ClockPrint


int ESMC_ClockAdvance(ESMC_Clock clock){

#undef ESMC_METHOD
#define ESMC_METHOD "ESMC_ClockAdvance()"

  // initialize return code; assume routine not implemented
  int rc= ESMC_RC_NOT_IMPL;          // local return code
  int localrc = ESMC_RC_NOT_IMPL;    // local return code

  ESMCI::Clock *IntClock = (ESMCI::Clock*)(clock.ptr);
  localrc = IntClock->advance();
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc)){
    clock.ptr = NULL;
    return rc;  // bail out
  }
  //return successfully
  return ESMF_SUCCESS;

} // end ESMC_ClockAdvance


int ESMC_ClockGet(ESMC_Clock clock, ESMC_TimeInterval *currSimTime,
                                    ESMC_I8* advanceCount){
  int rc= ESMC_RC_NOT_IMPL;          // local return code
  int localrc = ESMC_RC_NOT_IMPL;    // local return code

  ESMCI::Clock* pClock = (ESMCI::Clock*)(clock.ptr);
  localrc = pClock->print();
  ESMCI::TimeInterval* cST = new(ESMCI::TimeInterval);
  localrc = pClock->get(0,
                      (int*)NULL,
                      (char*)NULL,
                      (ESMCI::TimeInterval*)NULL,
                      (ESMCI::Time*)NULL,
                      (ESMCI::Time*)NULL,
                      (ESMCI::TimeInterval*)NULL,
                      (ESMC_R8*)NULL,
                      (ESMCI::Time*)NULL,
                      (ESMCI::Time*)NULL,
                      (ESMCI::Time*)NULL,
                      cST,                 
                      (ESMCI::TimeInterval*)NULL,
                      (ESMCI::Calendar**)NULL,
                      (ESMC_CalendarType*)NULL,
                      (int*)NULL,
                      advanceCount,
                      (int*)NULL,
                      (ESMC_Direction*)NULL );
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc)){
    pClock = NULL;
    return localrc;  // bail out
  }
  (*currSimTime).ptr = (void*)cST;

//  (*currSimTime).ptr = (void*)(&cST);


   //return successfully
   return ESMF_SUCCESS;

}  //end ESMC_ClockGet

int ESMC_ClockDestroy(ESMC_Clock* pClock){
  int rc = ESMF_RC_NOT_IMPL;
  int localrc = ESMF_RC_NOT_IMPL;
   
  ESMCI::Clock **intClock = (ESMCI::Clock**)(pClock);
  localrc = ESMCI::ESMCI_ClockDestroy(intClock);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc)){
    pClock = NULL;
    return rc;  // bail out
  }
  
  //return successfully
  return ESMF_SUCCESS;

} // end ESMC_ClockPrint

}; // extern "C"
