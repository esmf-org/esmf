// $Id: ESMC_TimeInterval.C,v 1.1 2008/07/11 18:23:01 rosalind Exp $
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
#define ESMC_FILENAME "ESMC_TimeInterval.C"
//==============================================================================
//
// ESMC TimeInterval method implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the public C TimeInterval methods declared
// in the companion file ESMC_TimeInterval.h
//
//-----------------------------------------------------------------------------


// include ESMF headers
#include "ESMCI_Arg.h"
#include "ESMC_LogErr.h"
#include "ESMF_LogMacros.inc"             // for LogErr
#include "ESMCI_TimeInterval.h"
#include "ESMCI_Calendar.h"
#include "ESMC_Calendar.h"
#include "ESMC_Interface.h"

// include associated header file
#include "ESMC_TimeInterval.h"

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id: ESMC_TimeInterval.C,v 1.1 2008/07/11 18:23:01 rosalind Exp $";
//-----------------------------------------------------------------------------

extern "C" {

int  ESMC_TimeIntervalSet(ESMC_TimeInterval* timeInterval,       
                       ESMC_I4 h_I4){

#undef ESMC_METHOD
#define ESMC_METHOD "ESMC_TimeIntervalSet()"


  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;   // final return code

  ESMCI::TimeInterval* pTimeInterval = new(ESMCI::TimeInterval);

  localrc = pTimeInterval->set( (ESMC_I4*)NULL, (ESMC_I8*)NULL,
                                (ESMC_I4*)NULL, (ESMC_I8*)NULL,
                                (ESMC_I4*)NULL,  (ESMC_I8*)NULL,
                                         &h_I4,  (ESMC_I4*)NULL,
                                (ESMC_I4*)NULL,  (ESMC_I8*)NULL,
                                (ESMC_I4*)NULL, (ESMC_I4*)NULL,
                                (ESMC_I4*)NULL,
                                (ESMC_R8*)NULL,  (ESMC_R8*)NULL,
                                (ESMC_R8*)NULL,  (ESMC_R8*)NULL,
                                (ESMC_R8*)NULL, (ESMC_R8*)NULL,
                                (ESMC_R8*)NULL,
                                (ESMC_I4*)NULL, (ESMC_I4*)NULL,
                                (ESMCI::Time*)NULL, (ESMCI::Time*)NULL,
                                (ESMCI::Calendar**)NULL,
                                (ESMC_CalendarType*)NULL);

  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc)){
    (*timeInterval).ptr = NULL;
    return localrc;  // bail out
  }

  (*timeInterval).ptr = (void *)(pTimeInterval);

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;

} // end ESMC_TimeIntervalSet


int  ESMC_TimeIntervalGet(ESMC_TimeInterval timeInterval,
                       ESMC_I8* s_I8,
                       ESMC_R8* h_R8){
#undef ESMC_METHOD
#define ESMC_METHOD "ESMC_TimeIntervalGet()"


  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;   // final return code

  ESMCI::TimeInterval* pTimeInterval = (ESMCI::TimeInterval*)(timeInterval.ptr);

  localrc = pTimeInterval->get( (ESMC_I4*)NULL, (ESMC_I8*)NULL,
                             (ESMC_I4*)NULL, (ESMC_I8*)NULL,
                             (ESMC_I4*)NULL,  (ESMC_I8*)NULL,
                             (ESMC_I4*)NULL,  (ESMC_I4*)NULL,
                             (ESMC_I4*)NULL,  s_I8,
                             (ESMC_I4*)NULL, (ESMC_I4*)NULL,
                             (ESMC_I4*)NULL,
                             (ESMC_R8*)NULL,  h_R8,
                             (ESMC_R8*)NULL,  (ESMC_R8*)NULL,
                             (ESMC_R8*)NULL, (ESMC_R8*)NULL,
                             (ESMC_R8*)NULL,
                             (ESMC_I4*)NULL, (ESMC_I4*)NULL,
                             (ESMCI::Time*)NULL, (ESMCI::Time*)NULL,
                             (ESMCI::Calendar**)NULL,
                             (ESMC_CalendarType*)NULL,
                             (ESMCI::Time*)NULL, (ESMCI::Time*)NULL,
                             (ESMCI::Calendar**)NULL,
                             (ESMC_CalendarType*)NULL,
                             0, (int*)NULL,
                             (char*)NULL,
                             0,
                             (int*)NULL,
                             (char*)NULL);

  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc)){
    timeInterval.ptr = NULL;
    return localrc;  // bail out
  }

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;

} // end ESMC_TimeIntervalGet



int ESMC_TimeIntervalPrint(ESMC_TimeInterval timeInterval){

#undef ESMC_METHOD
#define ESMC_METHOD "ESMC_TimeIntervalPrint()"

  // initialize return code; assume routine not implemented
  int rc= ESMC_RC_NOT_IMPL;          // local return code
  int localrc = ESMC_RC_NOT_IMPL;    // local return code

  ESMCI::TimeInterval *pTimeInterval = (ESMCI::TimeInterval*)(timeInterval.ptr);
  localrc = pTimeInterval->print();
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc)){
    timeInterval.ptr = NULL;
    return rc;  // bail out
  }

  //return successfully
  return ESMF_SUCCESS;

} // end ESMC_TimeIntervalPrint


}; // extern "C"
