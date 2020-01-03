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

#ifndef ESMC_Clock_H
#define ESMC_Clock_H

//-----------------------------------------------------------------------------
// ESMC_Clock - Public C interface to the ESMF Clock class
//
// The code in this file defines the public C Clock interfaces and declares
// method signatures (prototypes).  The companion file {\tt ESMC\_Clock.C}
// contains the definitions (full code bodies) for the Clock methods.
//-----------------------------------------------------------------------------


#include "ESMC_Util.h"
#include "ESMC_Time.h"
#include "ESMC_TimeInterval.h"

#ifdef __cplusplus
extern "C" {
#endif

// Class declaration type
//-----------------------------------------------------------------------------
typedef struct {
  // private:  // members opaque on C side, philosophically.
    void *ptr;
    // TODO:  implement isInit initialization like in F90 API?
} ESMC_Clock;
//-----------------------------------------------------------------------------

// Class API

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_ClockAdvance - Advance a Clock's current time by one time step

//
// !INTERFACE:
int ESMC_ClockAdvance(
  ESMC_Clock clock   // in
);

// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//
//  Advances the {\tt ESMC\_Clock}'s current time by one time step.
//
//  The arguments are:
//  \begin{description}
//  \item[clock]
//    {\tt ESMC\_Clock} object to be advanced.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_ClockCreate - Create a Clock
//
// !INTERFACE:
ESMC_Clock ESMC_ClockCreate(
  const char *name,             // in
  ESMC_TimeInterval timeStep,   // in
  ESMC_Time startTime,          // in
  ESMC_Time stopTime,           // in
  int *rc                       // out
);

// !RETURN VALUE:
//  Newly created ESMC_Clock object.
//
// !DESCRIPTION:
//
// Creates and sets the initial values in a new {\tt ESMC\_Clock} object. 
//
//  The arguments are:
//  \begin{description}
//  \item[{[name]}]
//    The name for the newly created Clock.  If not specified, i.e. NULL,
//    a default unique name will be generated: "ClockNNN" where NNN
//    is a unique sequence number from 001 to 999.
//  \item[timeStep]
//    The {\tt ESMC\_Clock}'s time step interval, which can be
//    positive or negative.
//  \item[startTime]
//    The {\tt ESMC\_Clock}'s starting time.  Can be less than or
//    or greater than stopTime, depending on a positive or negative
//    timeStep, respectively, and whether a stopTime is specified;
//    see below.
//  \item[stopTime]
//    The {\tt ESMC\_Clock}'s stopping time.  Can be greater than or
//    less than the startTime, depending on a positive or negative
//    timeStep, respectively.
//  \item[{[rc]}]
//    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_ClockDestroy - Destroy a Clock
//
// !INTERFACE:
int ESMC_ClockDestroy(
  ESMC_Clock *clock   // inout
);

// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//
//  Releases all resources associated with this {\tt ESMC\_Clock}.
//
//  The arguments are:
//  \begin{description}
//  \item[clock]
//    Destroy contents of this {\tt ESMC\_Clock}.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_ClockGet - Get a Clock's properties
//
// !INTERFACE:
int ESMC_ClockGet(
  ESMC_Clock clock,                 // in
  ESMC_TimeInterval *currSimTime,   // out
  ESMC_I8 *advanceCount             // out
);

// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//
// Gets one or more of the properties of an {\tt ESMC\_Clock}.
//
//  The arguments are:
//  \begin{description}
//  \item[clock]
//    {\tt ESMC\_Clock} object to be queried.
//  \item[{[currSimTime]}]
//    The current simulation time.
//  \item[{[advanceCount]}]
//    The number of times the {\tt ESMC\_Clock} has been advanced.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_ClockPrint - Print the contents of a Clock
//
// !INTERFACE:
int ESMC_ClockPrint(
  ESMC_Clock clock   // in
);

// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//
//  Prints out an {\tt ESMC\_Clock}'s properties to {\tt stdio}, 
//  in support of testing and debugging.
//
//  The arguments are:
//  \begin{description}
//  \item[clock]
//    {\tt ESMC\_Clock} object to be printed.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

#ifdef __cplusplus
} // extern "C"
#endif

#endif // ESMC_Clock_H
