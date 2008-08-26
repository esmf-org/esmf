// $Id: ESMC_Time.C,v 1.3 2008/08/26 15:46:37 theurich Exp $
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
#define ESMC_FILENAME "ESMC_Time.C"
//==============================================================================
//
// ESMC Time method implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the public C Time methods declared
// in the companion file ESMC_Time.h
//
//-----------------------------------------------------------------------------

// include associated header file
#include "ESMC_Time.h"

// include ESMF headers
#include "ESMCI_Arg.h"
#include "ESMCI_LogErr.h"
#include "ESMF_LogMacros.inc"             // for LogErr
#include "ESMCI_Time.h"
#include "ESMCI_Calendar.h"
#include "ESMC_Calendar.h"
#include "ESMC_Interface.h"


//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id: ESMC_Time.C,v 1.3 2008/08/26 15:46:37 theurich Exp $";
//-----------------------------------------------------------------------------

extern "C" {

int  ESMC_TimeSet(ESMC_Time* time,       
                       ESMC_I4 yy,
                       ESMC_I4 h,
                       ESMC_Calendar calendar,
                       ESMC_CalendarType calendartype,
                       int timeZone){
#undef ESMC_METHOD
#define ESMC_METHOD "ESMC_TimeSet()"


  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;   // final return code

  ESMCI::Time* pTime = new(ESMCI::Time);

  localrc = pTime->set( &yy, 
               (ESMC_I8*)NULL, (int*)NULL, (int*)NULL, 
               (ESMC_I4*)NULL, (ESMC_I8*)NULL,
               &h,
               (ESMC_I4*)NULL, (ESMC_I4*)NULL, (ESMC_I8*)NULL, 
               (ESMC_I4*)NULL, (ESMC_I4*)NULL, 
               (ESMC_I4*)NULL, (ESMC_R8*)NULL, (ESMC_R8*)NULL, 
               (ESMC_R8*)NULL, (ESMC_R8*)NULL,
               (ESMC_R8*)NULL, (ESMC_R8*)NULL, (ESMC_R8*)NULL, 
               (ESMC_I4*)NULL, (ESMC_I4*)NULL,
               (ESMCI::Calendar**)(&calendar),
               &calendartype,
               &timeZone);

  if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc)){
    (*time).ptr = NULL;
    return localrc;  // bail out
  }

  (*time).ptr = (void *)(pTime);

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;

} // end ESMC_TimeSet


int  ESMC_TimeGet(ESMC_Time time,
                       ESMC_I4* yy,
                       ESMC_I4* h,
                       ESMC_Calendar* calendar,
                       ESMC_CalendarType* calendartype,
                       int* timeZone){
#undef ESMC_METHOD
#define ESMC_METHOD "ESMC_TimeGet()"


  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int rc = ESMC_RC_NOT_IMPL;   // final return code

  ESMCI::Time* pTime = (ESMCI::Time*)(time.ptr);
  ESMCI::Calendar* pcalendar;
  localrc = pTime->get( yy,
   (ESMC_I8*)(0), (int*)(0),     (int*)(0), 
   (ESMC_I4*)(0), (ESMC_I8*)(0), (ESMC_I4*)(0),               h, (ESMC_I4*)(0), 
   (ESMC_I8*)(0), (ESMC_I4*)(0), (ESMC_I4*)(0),     (ESMC_I4*)(0), (ESMC_R8*)(0), 
   (ESMC_R8*)(0), (ESMC_R8*)(0), (ESMC_R8*)(0),     (ESMC_R8*)(0), (ESMC_R8*)(0), 
   (ESMC_R8*)(0), (ESMC_I4*)(0), (ESMC_I4*)(0),      &pcalendar, calendartype,
        timeZone,      int(0),     (int*)(0),        (char*)(0),      (int)(0), 
       (int*)(0),    (char*)(0),     (int*)(0), (ESMCI::Time*)(0), (ESMC_I4*)(0), 
   (ESMC_R8*)(0), (ESMCI::TimeInterval*)(0)); 

  if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc)){
    time.ptr = NULL;
    return localrc;  // bail out
  }

  (*calendar).ptr = (void*)(pcalendar);
 
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;

} // end ESMC_TimeGet



int ESMC_TimePrint(ESMC_Time time){

#undef ESMC_METHOD
#define ESMC_METHOD "ESMC_TimePrint()"

  // initialize return code; assume routine not implemented
  int rc= ESMC_RC_NOT_IMPL;          // local return code
  int localrc = ESMC_RC_NOT_IMPL;    // local return code

  ESMCI::Time *pTime = (ESMCI::Time*)(time.ptr);
  localrc = pTime->print();
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, &rc)){
    time.ptr = NULL;
    return rc;  // bail out
  }

  //return successfully
  return ESMF_SUCCESS;

} // end ESMC_TimePrint


}; // extern "C"
