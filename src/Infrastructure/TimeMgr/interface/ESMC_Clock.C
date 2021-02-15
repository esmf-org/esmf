// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2021, University Corporation for Atmospheric Research,
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

// include system headers
#include <string.h>

// include ESMF headers
#include "ESMCI_Arg.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_Clock.h"

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
#define ESMC_METHOD "ESMC_ClockCreate()"

ESMC_Clock ESMC_ClockCreate(
      const char        *name,         // in
      ESMC_TimeInterval  timeStep,     // in
      ESMC_Time          startTime,    // in
      ESMC_Time          stopTime,     // in
      int               *rc) {         // out - return code

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;           // local return code
  if (rc != NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  ESMC_Clock clock;

  // call into ESMCI method
  clock.ptr = (void *)
     ESMCI::ESMCI_ClockCreate(strlen(name), name,
                              (ESMCI::TimeInterval *)&timeStep,
                              (ESMCI::Time *)&startTime,
                              (ESMCI::Time *)&stopTime,
                              (ESMCI::TimeInterval *)NULL, (int *)NULL,
                              (ESMCI::Time *)NULL, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,                                       rc)) {
    clock.ptr = NULL;  // defensive; should already be set in ClockCreate()
    return clock;  // bail out
  }

  // return successfully
  if (rc != NULL) *rc = ESMF_SUCCESS;
  return clock;

} // end ESMC_ClockCreate
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_ClockDestroy()"

int ESMC_ClockDestroy(ESMC_Clock *clock) {

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int      rc = ESMC_RC_NOT_IMPL;         // final return code

  // ensure given clock pointer is non-NULL
  if (clock == NULL) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
                    ", invalid ESMC_Clock object", ESMC_CONTEXT, &rc);
    return rc;  // bail out
  }

  // call into ESMCI method; let it handle possible NULL ptr
  localrc = ESMCI::ESMCI_ClockDestroy((ESMCI::Clock **)&(clock->ptr));
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                    &rc)) return rc;  // bail out
  // invalidate pointer
  clock->ptr = NULL;  // defensive; should already be set in ClockDestroy()

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;

} // end ESMC_ClockDestroy
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_ClockGet()"

int ESMC_ClockGet(ESMC_Clock clock, ESMC_TimeInterval *currSimTime,
                                    ESMC_I8 *advanceCount) {

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int      rc = ESMC_RC_NOT_IMPL;         // final return code

  // ensure given clock pointer is non-NULL
  if (clock.ptr == NULL) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
                    ", invalid ESMC_Clock object", ESMC_CONTEXT, &rc);
    return rc;  // bail out
  }

  // Note: Don't need to check for passed-thru NULL input pointers currently;
  //       ESMCI::Clock::get() interprets them as "not desired".
  //       (designed to support F90 not-present args)

  // call into ESMCI method
  localrc = ((ESMCI::Clock *)(clock.ptr))->get(0,
                             (int *)NULL,
                             (char *)NULL,
                             (ESMCI::TimeInterval *)NULL,
                             (ESMCI::Time *)NULL,
                             (ESMCI::Time *)NULL,
                             (ESMCI::TimeInterval *)NULL,
                             (ESMC_R8 *)NULL,
                             (ESMCI::Time *)NULL,
                             (ESMCI::Time *)NULL,
                             (ESMCI::Time *)NULL,
                             (ESMCI::TimeInterval *)currSimTime,
                             (ESMCI::TimeInterval *)NULL,
                             (ESMCI::Calendar **)NULL,
                             (ESMC_CalKind_Flag *)NULL,
                             (int *)NULL,
                                                    advanceCount,
                             (int *)NULL,
                             (ESMC_Direction *)NULL);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                    &rc)) return rc;  // bail out
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;

} // end ESMC_ClockGet
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_ClockAdvance()"

int ESMC_ClockAdvance(ESMC_Clock clock) {

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int      rc = ESMC_RC_NOT_IMPL;         // final return code

  // ensure given clock pointer is non-NULL
  if (clock.ptr == NULL) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
                    ", invalid ESMC_Clock object", ESMC_CONTEXT, &rc);
    return rc;  // bail out
  }

  // call into ESMCI method
  localrc = ((ESMCI::Clock*)(clock.ptr))->advance((ESMCI::TimeInterval *)NULL,
                                                  (char *)NULL, (char *)NULL,
                                                  0, (int *)NULL);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                    &rc)) return rc;  // bail out
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;

} // end ESMC_ClockAdvance
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_ClockPrint()"

int ESMC_ClockPrint(ESMC_Clock clock) {

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int      rc = ESMC_RC_NOT_IMPL;         // final return code

  // ensure given clock pointer is non-NULL
  if (clock.ptr == NULL) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
                    ", invalid ESMC_Clock object", ESMC_CONTEXT, &rc);
    return rc;  // bail out
  }

  // call into ESMCI method
  localrc = ((ESMCI::Clock*)(clock.ptr))->print("string");
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                    &rc)) return rc;  // bail out
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;
 
} // end ESMC_ClockPrint
//-----------------------------------------------------------------------------

}; // extern "C"
