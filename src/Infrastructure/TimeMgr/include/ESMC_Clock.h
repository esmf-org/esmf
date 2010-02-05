// $Id: ESMC_Clock.h,v 1.61.2.1 2010/02/05 20:00:07 svasquez Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research,
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
//BOPI
// !CLASS:  ESMC_Clock - Public C interface to the ESMF Clock class
//
// !DESCRIPTION:
//
// The code in this file defines the public C Clock interfaces and declares
// method signatures (prototypes).  The companion file {\tt ESMC\_Clock.C}
// contains the definitions (full code bodies) for the Clock methods.
//
//EOPI
//-----------------------------------------------------------------------------

#include "ESMC_Util.h"
#include "ESMC_Time.h"
#include "ESMC_TimeInterval.h"

#ifdef __cplusplus
extern "C" {
#endif

// Class declaration type
typedef struct {
  // private:  // members opaque on C side, philosophically.
    void *ptr;
    // TODO:  implement isInit initialization like in F90 API?
} ESMC_Clock;

// Class API
ESMC_Clock ESMC_ClockCreate(const char *name,
                            ESMC_TimeInterval timeStep, ESMC_Time startTime,
                            ESMC_Time stopTime, int *rc);

int ESMC_ClockDestroy(ESMC_Clock *clock);

int ESMC_ClockGet(ESMC_Clock clock, ESMC_TimeInterval *currSimTime,
                  ESMC_I8 *advanceCount);

int ESMC_ClockAdvance(ESMC_Clock clock);

int ESMC_ClockPrint(ESMC_Clock clock);

#ifdef __cplusplus
} // extern "C"
#endif

#endif // ESMC_Clock_H
