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

#ifndef ESMC_TimeInterval_H
#define ESMC_TimeInterval_H

//-----------------------------------------------------------------------------
// ESMC_TimeInterval - Public C interface to the ESMF TimeInterval class
//
// The code in this file defines the public C TimeInterval interfaces and
// declares method signatures (prototypes).  The companion file
// {\tt ESMC\_TimeInterval.C} contains the definitions (full code bodies) for
// the TimeInterval methods.
//-----------------------------------------------------------------------------

#include "ESMC_Util.h"
#include "ESMC_Time.h"
#include "ESMC_Calendar.h"

// TODO: these definitions need different home (shared with ESMCI_TimeInterval)
enum ESMC_ComparisonType {ESMC_EQ, ESMC_NE,
                          ESMC_LT, ESMC_GT,
                          ESMC_LE, ESMC_GE};
enum ESMC_AbsValueType {ESMC_POSITIVE_ABS, ESMC_NEGATIVE_ABS};

#ifdef __cplusplus
extern "C" {
#endif

// Class declaration type
//-----------------------------------------------------------------------------
typedef struct { 
  // private:  // Members opaque on C side, philosophically.
    // Allocate enough memory to store members on the C side.
    // Adjust if members are added, rounding up to multiples of
    // 8 bytes (64 bits - largest machine word size).  Add 8 bytes extra
    // padding; match 'type ESMF_TimeInterval' in ESMF_TimeIntervalType.F90.
    // TODO:  implement isInit initialization like in F90 API?
    char shallowMem[152];  // 18 8-byte members + 1 8-bytes extra = 19 * 8
} ESMC_TimeInterval;
//-----------------------------------------------------------------------------

// Class API

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_TimeIntervalGet - Get a TimeInterval value
//
// !INTERFACE:
int ESMC_TimeIntervalGet(
  ESMC_TimeInterval timeinterval,   // in
  ESMC_I8 *s_i8,                    // out
  ESMC_R8 *h_r8                     // out
);

// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//
//  Gets the value of an {\tt ESMC\_TimeInteval} in units specified by the user.
//
//  The arguments are:
//  \begin{description}
//  \item[timeinterval]
//    {\tt ESMC\_TimeInterval} object to be queried.
//  \item[{[s\_i8]}]
//    Integer seconds (large, >= 64-bit).
//  \item[{[h\_r8]}]
//    Double precision hours.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_TimeIntervalPrint - Print a TimeInterval
//
// !INTERFACE:
int ESMC_TimeIntervalPrint(
  ESMC_TimeInterval timeinterval   // in
);

// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//  Prints out an {\tt ESMC\_TimeInterval}'s properties to {\tt stdio}, 
//  in support of testing and debugging.
//
//  The arguments are:
//  \begin{description}
//  \item[timeinterval]
//    {\tt ESMC\_TimeInterval} object to be printed.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_TimeIntervalSet - Initialize or set a TimeInterval
//
// !INTERFACE:
int ESMC_TimeIntervalSet(
  ESMC_TimeInterval *timeinterval,   // inout
  ESMC_I4 h                          // in
);

// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//
//  Sets the value of the {\tt ESMC\_TimeInterval} in units specified by
//  the user.
//
//  The arguments are:
//  \begin{description}
//  \item[timeinterval]
//    {\tt ESMC\_TimeInterval} object to initialize or set.
//  \item[h]
//    Integer hours.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

#ifdef __cplusplus
} // extern "C"
#endif

#endif // ESMC_TimeInterval_H
