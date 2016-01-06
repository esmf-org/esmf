// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2016, University Corporation for Atmospheric Research,
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
#include "ESMCI_Time.h"
#include "ESMCI_Calendar.h"

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version =
  "$Id$";
//-----------------------------------------------------------------------------

// TODO: Implement more -native- C++ TimeMgr API alongside existing
//       C++ API, which was designed to support the F90 TimeMgr API,
//       (optional args).  E.g. separate get()'s for each property (or small
//       groups of properties) would eliminate sparsely populated arg lists
//       (lots of NULLs); instead each call would be guarded by a NULL check.

extern "C" {

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_TimeSet()"

int ESMC_TimeSet(ESMC_Time *time,       
                 ESMC_I4 yy,
                 ESMC_I4 h,
                 ESMC_Calendar calendar,
                 ESMC_CalKind_Flag calkindflag,
                 int timeZone) {

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int      rc = ESMC_RC_NOT_IMPL;         // final return code

  // ensure given time pointer is non-NULL
  if (time == NULL) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
                    ", invalid ESMC_Time object", ESMC_CONTEXT, &rc);
    return rc;  // bail out
  }

  // call into ESMCI method
  localrc = ((ESMCI::Time *)time)->set(&yy, 
             (ESMC_I8 *)NULL, (int *)NULL, (int*)NULL, 
             (ESMC_I4 *)NULL, (ESMC_I8 *)NULL,
                                       &h,
             (ESMC_I4 *)NULL, (ESMC_I4 *)NULL, (ESMC_I8 *)NULL, 
             (ESMC_I4 *)NULL, (ESMC_I4 *)NULL, (ESMC_I4 *)NULL,
             (ESMC_R8 *)NULL, (ESMC_R8 *)NULL, 
             (ESMC_R8 *)NULL, (ESMC_R8 *)NULL,
             (ESMC_R8 *)NULL, (ESMC_R8 *)NULL, (ESMC_R8 *)NULL, 
             (ESMC_I4 *)NULL, (ESMC_I8 *)NULL,
             (ESMC_I4 *)NULL, (ESMC_I8 *)NULL,
                  (ESMCI::Calendar **)&(calendar.ptr),
                                       &calkindflag,
                                       &timeZone);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                    &rc)) return rc;  // bail out
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;

} // end ESMC_TimeSet
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_TimeGet()"

int ESMC_TimeGet(ESMC_Time time,
                 ESMC_I4 *yy,
                 ESMC_I4 *h,
                 ESMC_Calendar *calendar,
                 ESMC_CalKind_Flag *calkindflag,
                 int *timeZone) {

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int      rc = ESMC_RC_NOT_IMPL;         // final return code

  // ensure given ESMC_Calendar pointer is non-NULL
  if (calendar == NULL) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
                    ", invalid ESMC_Calendar object", ESMC_CONTEXT, &rc);
    return rc;  // bail out
  }

  // Note: Don't need to check for passed-thru NULL input pointers currently;
  //       ESMCI::Time::get() interprets them as "not desired".
  //       (designed to support F90 not-present args)

  // call into ESMCI method
  localrc = ((ESMCI::Time *)&time)->get(yy,
             (ESMC_I8 *)NULL, (int *)NULL, (int*)NULL, 
             (ESMC_I4 *)NULL, (ESMC_I8 *)NULL,
                                        h,
             (ESMC_I4 *)NULL, (ESMC_I4 *)NULL, (ESMC_I8 *)NULL,
             (ESMC_I4 *)NULL, (ESMC_I4 *)NULL, (ESMC_I4 *)NULL,
             (ESMC_R8 *)NULL, (ESMC_R8 *)NULL,
             (ESMC_R8 *)NULL, (ESMC_R8 *)NULL,
             (ESMC_R8 *)NULL, (ESMC_R8 *)NULL, (ESMC_R8 *)NULL,
             (ESMC_I4 *)NULL, (ESMC_I8 *)NULL,
             (ESMC_I4 *)NULL, (ESMC_I8 *)NULL,
                  (ESMCI::Calendar **)&(calendar->ptr),
                                        calkindflag,
                                        timeZone,
             (int)0, (int *)NULL, (char *)NULL, (int)0, 
             (int *)NULL, (char *)NULL, (int *)NULL, (ESMCI::Time *)NULL,
             (ESMC_I4 *)NULL, (ESMC_R8 *)NULL, (ESMCI::TimeInterval *)NULL); 
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                    &rc)) return rc;  // bail out
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;

} // end ESMC_TimeGet
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_TimePrint()"

int ESMC_TimePrint(ESMC_Time time) {

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int      rc = ESMC_RC_NOT_IMPL;         // final return code

  // call into ESMCI method
  localrc = ((ESMCI::Time *)&time)->print("string");
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                    &rc)) return rc;  // bail out
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;

} // end ESMC_TimePrint
//-----------------------------------------------------------------------------

}; // extern "C"
