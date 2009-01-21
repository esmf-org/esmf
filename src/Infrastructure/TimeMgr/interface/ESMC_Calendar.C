// $Id: ESMC_Calendar.C,v 1.5 2009/01/21 21:38:01 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMC_Calendar.C"
//==============================================================================
//
// ESMC Calendar method implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the public C Calendar methods declared
// in the companion file ESMC_Calendar.h
//
//-----------------------------------------------------------------------------

// include associated header file
#include "ESMC_Calendar.h"

// include ESMF headers
#include "ESMCI_Arg.h"
#include "ESMCI_LogErr.h"
#include "ESMF_LogMacros.inc"             // for LogErr
#include "ESMCI_Calendar.h"
#include "ESMC_Interface.h"


//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id: ESMC_Calendar.C,v 1.5 2009/01/21 21:38:01 cdeluca Exp $";
//-----------------------------------------------------------------------------

extern "C" {

ESMC_Calendar ESMC_CalendarCreate(
      int                nameLen,      // in
      const char        *name,         // in
      ESMC_CalendarType  calendarType, // in
      int               *rc) {         // out - return code
#undef ESMC_METHOD
#define ESMC_METHOD "ESMC_CalendarCreate()"

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  *rc = ESMC_RC_NOT_IMPL;   // final return code

  ESMC_Calendar calendar;

  // call into ESMCI method

  calendar.ptr = (void *)
     ESMCI::ESMCI_CalendarCreate(nameLen, name, calendarType, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, rc)){
    calendar.ptr = NULL;
    return calendar;  // bail out
  }

  // return successfully
  *rc = ESMF_SUCCESS;
  return calendar;

} // end ESMC_CalendarCreate


int ESMC_CalendarPrint(ESMC_Calendar calendar){

#undef ESMC_METHOD
#define ESMC_METHOD "ESMC_CalendarPrint()"

  // initialize return code; assume routine not implemented
  int rc= ESMC_RC_NOT_IMPL;          // local return code
  int localrc = ESMC_RC_NOT_IMPL;    // local return code

  ESMCI::Calendar *IntCalendar = (ESMCI::Calendar*)(calendar.ptr);
  localrc = IntCalendar->print();
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc)){
    calendar.ptr = NULL;
    return rc;  // bail out
  }

  //return successfully
  return ESMF_SUCCESS;
 
} // end ESMC_CalendarPrint

int ESMC_CalendarDestroy(ESMC_Calendar* pCalendar){
  int rc = ESMF_RC_NOT_IMPL;
  int localrc = ESMF_RC_NOT_IMPL;
   
  ESMCI::Calendar **intCalendar = (ESMCI::Calendar**)(pCalendar);
  localrc = ESMCI::ESMCI_CalendarDestroy(intCalendar);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc)){
    pCalendar = NULL;
    return rc;  // bail out
  }
  
  //return successfully
  return ESMF_SUCCESS;

} // end ESMC_CalendarPrint

}; // extern "C"
