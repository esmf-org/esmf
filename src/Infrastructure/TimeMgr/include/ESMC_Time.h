// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2020, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// This file is part of the pure C public ESMC API
//-----------------------------------------------------------------------------

//-------------------------------------------------------------------------
// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMC_Time_H
#define ESMC_Time_H

//-----------------------------------------------------------------------------
// ESMC_Time - Public C interface to the ESMF Time class
//
// The code in this file defines the public C Time interfaces and declares
// method signatures (prototypes).  The companion file {\tt ESMC\_Time.C}
// contains the definitions (full code bodies) for the Time methods.
//-----------------------------------------------------------------------------

#include "ESMC_Util.h"
#include "ESMC_Calendar.h"

#ifdef __cplusplus
extern "C" {
#endif

// Class declaration type
//-----------------------------------------------------------------------------
typedef struct {
  // private:  // Members opaque on C side, philosophically.
    // Allocate enough memory to store members on the C side.
    // Adjust if members are added, rounding up to multiples of
    // 8 bytes (64 bits - largest machine word size).  Add 8 bytes
    // extra padding; match 'type ESMF_Time' in ESMF_TimeType.F90.
    // TODO:  implement isInit initialization like in F90 API?
    char shallowMem[48];  // 5 8-byte members + 1 8-bytes extra = 6 * 8
} ESMC_Time;
//-----------------------------------------------------------------------------

// Class API

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_TimeGet - Get a Time value
//
// !INTERFACE:
int ESMC_TimeGet(
  ESMC_Time time,                         // in
  ESMC_I4 *yy,                            // out
  ESMC_I4 *h,                             // out
  ESMC_Calendar *calendar,                // out
  enum ESMC_CalKind_Flag *calkindflag,    // out
  int *timeZone                           // out
);

// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//
//  Gets the value of an {\tt ESMC\_Time} in units specified by the user.
//
//  The arguments are:
//  \begin{description}
//  \item[time]
//    {\tt ESMC\_Time} object to be queried.
//  \item[{[yy]}]
//    Integer year (>= 32-bit).
//  \item[{[h]}]
//    Integer hours.
//  \item[{[calendar]}]
//    Associated {\tt ESMC\_Calendar}.
//  \item[{[calkindflag]}]
//    Associated {\tt ESMC\_CalKind\_Flag}.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_TimePrint - Print a Time
//
// !INTERFACE:
int ESMC_TimePrint(
  ESMC_Time time   // in
);

// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//  Prints out an {\tt ESMC\_Time}'s properties to {\tt stdio}, 
//  in support of testing and debugging.
//
//  The arguments are:
//  \begin{description}
//  \item[time]
//    {\tt ESMC\_Time} object to be printed.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_TimeSet - Initialize or set a Time
//
// !INTERFACE:
int ESMC_TimeSet(
  ESMC_Time *time,                       // inout
  ESMC_I4 yy,                            // in
  ESMC_I4 h,                             // in
  ESMC_Calendar calendar,                // in
  enum ESMC_CalKind_Flag calkindflag,    // in
  int timeZone                           // in
);

// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//
//  Initializes an {\tt ESMC\_Time} with a set of user-specified units.
//
//  The arguments are:
//  \begin{description}
//  \item[time]
//    {\tt ESMC\_Time} object to initialize or set.
//  \item[yy]
//    Integer year (>= 32-bit).
//  \item[h]
//    Integer hours.
//  \item[calendar]
//    Associated {\tt ESMC\_Calendar}.  If not created, defaults to calendar
//    {\tt ESMC\_CALKIND\_NOCALENDAR} or default specified in
//    {\tt ESMC\_Initialize()}.  If created, has precedence over
//    calkindflag below.
//  \item[calkindflag]
//    Specifies associated {\tt ESMC\_Calendar} if calendar argument above
//    not created.  More convenient way of specifying a built-in calendar kind.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

#ifdef __cplusplus
} // extern "C"
#endif

#endif // ESMC_Time_H
