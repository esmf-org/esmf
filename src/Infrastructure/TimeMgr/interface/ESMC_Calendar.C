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

// include system headers
#include <string.h>

// include ESMF headers
#include "ESMCI_Arg.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_Calendar.h"
#include "ESMCI_Time.h"

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
#define ESMC_METHOD "ESMC_CalendarCreate()"

ESMC_Calendar ESMC_CalendarCreate(
      const char        *name,         // in
      ESMC_CalKind_Flag  calkindflag,  // in
      int               *rc) {         // out - return code

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;           // local return code
  if (rc != NULL) *rc = ESMC_RC_NOT_IMPL;   // final return code

  ESMC_Calendar calendar;

  // call into ESMCI method
  calendar.ptr = (void *)
     ESMCI::ESMCI_CalendarCreate(strlen(name), name, calkindflag, &localrc);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,                                       rc)) {
    calendar.ptr = NULL; // defensive; should already be set in CalendarCreate()
    return calendar;  // bail out
  }

  // return successfully
  if (rc != NULL) *rc = ESMF_SUCCESS;
  return calendar;

} // end ESMC_CalendarCreate
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_CalendarDestroy()"

int ESMC_CalendarDestroy(ESMC_Calendar *calendar) {
  
  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int      rc = ESMC_RC_NOT_IMPL;         // final return code

  // ensure given calendar pointer is non-NULL
  if (calendar == NULL) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
                    ", invalid ESMC_Calendar object", ESMC_CONTEXT, &rc);
    return rc;  // bail out
  }

  // call into ESMCI method; let it handle possible NULL ptr
  localrc = ESMCI::ESMCI_CalendarDestroy((ESMCI::Calendar **)&(calendar->ptr));
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                    &rc)) return rc;  // bail out
  // invalidate pointer
  calendar->ptr = NULL; // defensive; should already be set in CalendarDestroy()

  // return successfully
  rc = ESMF_SUCCESS;
  return rc;

} // end ESMC_CalendarDestroy
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_CalendarPrint()"

int ESMC_CalendarPrint(ESMC_Calendar calendar){

  // initialize return code; assume routine not implemented
  int localrc = ESMC_RC_NOT_IMPL;         // local return code
  int      rc = ESMC_RC_NOT_IMPL;         // final return code

  // ensure given calendar pointer is non-NULL
  if (calendar.ptr == NULL) {
    ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
                    ", invalid ESMC_Calendar object", ESMC_CONTEXT, &rc);
    return rc;  // bail out
  }

  // call into ESMCI method
  localrc = ((ESMCI::Calendar*)(calendar.ptr))->print((const char *)NULL,
                                                    (const ESMCI::Time *)NULL);
  if (ESMC_LogDefault.MsgFoundError(localrc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
                                    &rc)) return rc;  // bail out
  // return successfully
  rc = ESMF_SUCCESS;
  return rc;

} // end ESMC_CalendarPrint
//-----------------------------------------------------------------------------

}; // extern "C"
