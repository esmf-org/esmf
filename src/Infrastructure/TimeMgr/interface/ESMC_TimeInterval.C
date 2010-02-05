// $Id: ESMC_TimeInterval.C,v 1.6.2.1 2010/02/05 20:00:13 svasquez Exp $
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

// include associated header file
#include "ESMC_TimeInterval.h"

// include ESMF headers
#include "ESMCI_Arg.h"
#include "ESMCI_LogErr.h"
#include "ESMC_LogMacros.inc"             // for LogErr
#include "ESMCI_TimeInterval.h"
#include "ESMCI_Calendar.h"

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version =
  "$Id: ESMC_TimeInterval.C,v 1.6.2.1 2010/02/05 20:00:13 svasquez Exp $";
//-----------------------------------------------------------------------------

// TODO: Implement more -native- C++ TimeMgr API alongside existing
//       C++ API, which was designed to support the F90 TimeMgr API,
//       (optional args).  E.g. separate get()'s for each property (or small
//       groups of properties) would eliminate sparsely populated arg lists
//       (lots of NULLs); instead each call would be guarded by a NULL check.

extern "C" {

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_TimeIntervalSet()"

int ESMC_TimeIntervalSet(ESMC_TimeInterval *timeInterval,       
                         ESMC_I4 h_I4) {

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int      rc = ESMC_RC_NOT_IMPL;         // final return code

  // ensure given timeInterval pointer is non-NULL
  if (timeInterval == NULL) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
                    ", invalid ESMC_TimeInterval object", ESMC_CONTEXT, &rc);
    return rc;  // bail out
  }

  // call into ESMCI method
  localrc = ((ESMCI::TimeInterval *)timeInterval)->set(
                                (ESMC_I4 *)NULL, (ESMC_I8 *)NULL,
                                (ESMC_I4 *)NULL, (ESMC_I8 *)NULL,
                                (ESMC_I4 *)NULL, (ESMC_I8 *)NULL,
                                               &h_I4,
                                (ESMC_I4 *)NULL,
                                (ESMC_I4 *)NULL, (ESMC_I8 *)NULL,
                                (ESMC_I4 *)NULL, (ESMC_I4 *)NULL,
                                (ESMC_I4 *)NULL,
                                (ESMC_R8 *)NULL, (ESMC_R8 *)NULL,
                                (ESMC_R8 *)NULL, (ESMC_R8 *)NULL,
                                (ESMC_R8 *)NULL, (ESMC_R8 *)NULL,
                                (ESMC_R8 *)NULL,
                                (ESMC_I4 *)NULL, (ESMC_I8 *)NULL,
                                (ESMC_I4 *)NULL, (ESMC_I8 *)NULL,
                                (ESMCI::Time *)NULL, (ESMCI::Time *)NULL,
                                (ESMCI::Calendar **)NULL,
                                (ESMC_CalendarType *)NULL);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, ESMC_CONTEXT,
                                    &rc)) return rc;  // bail out
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;

} // end ESMC_TimeIntervalSet
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_TimeIntervalGet()"

int ESMC_TimeIntervalGet(ESMC_TimeInterval timeInterval,
                         ESMC_I8 *s_I8,
                         ESMC_R8 *h_R8) {

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int      rc = ESMC_RC_NOT_IMPL;         // final return code

  // Note: Don't need to check for passed-thru NULL input pointers currently;
  //       ESMCI::TimeInterval::get() intreprets them as "not desired".
  //       (designed to support F90 not-present args)

  // call into ESMCI method
  localrc = ((ESMCI::TimeInterval *)&timeInterval)->get(
                             (ESMC_I4 *)NULL, (ESMC_I8 *)NULL,
                             (ESMC_I4 *)NULL, (ESMC_I8 *)NULL,
                             (ESMC_I4 *)NULL, (ESMC_I8 *)NULL,
                             (ESMC_I4 *)NULL, (ESMC_I4 *)NULL,
                             (ESMC_I4 *)NULL,
                                            s_I8,
                             (ESMC_I4 *)NULL, (ESMC_I4 *)NULL,
                             (ESMC_I4 *)NULL,
                             (ESMC_R8 *)NULL,
                                            h_R8,
                             (ESMC_R8 *)NULL, (ESMC_R8 *)NULL,
                             (ESMC_R8 *)NULL, (ESMC_R8 *)NULL,
                             (ESMC_R8 *)NULL,
                             (ESMC_I4 *)NULL, (ESMC_I8 *)NULL,
                             (ESMC_I4 *)NULL, (ESMC_I8 *)NULL,
                             (ESMCI::Time *)NULL, (ESMCI::Time *)NULL,
                             (ESMCI::Calendar **)NULL,
                             (ESMC_CalendarType *)NULL,
                             (ESMCI::Time *)NULL, (ESMCI::Time *)NULL,
                             (ESMCI::Calendar **)NULL,
                             (ESMC_CalendarType *)NULL,
                             (int)0, (int *)NULL, (char *)NULL,
                             (int)0, (int *)NULL, (char *)NULL);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, ESMC_CONTEXT,
                                    &rc)) return rc;  // bail out
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;

} // end ESMC_TimeIntervalGet
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_TimeIntervalPrint()"

int ESMC_TimeIntervalPrint(ESMC_TimeInterval timeInterval) {

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int      rc = ESMC_RC_NOT_IMPL;         // final return code

  // call into ESMCI method
  localrc = ((ESMCI::TimeInterval *)&timeInterval)->print("string");
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMF_ERR_PASSTHRU, ESMC_CONTEXT,
                                    &rc)) return rc;  // bail out
  //return successfully
  return ESMF_SUCCESS;

} // end ESMC_TimeIntervalPrint
//-----------------------------------------------------------------------------

}; // extern "C"
