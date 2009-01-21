// $Id: ESMC_Clock.h,v 1.60 2009/01/21 21:38:01 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research,
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

#include "ESMC_Time.h"
#include "ESMC_TimeInterval.h"

//-----------------------------------------------------------------------------
//BOPI
// !CLASS:  ESMC_Clock - Public C interface to the ESMF Clock class
//
// !DESCRIPTION:
//
// The code in this file defines the public C Clock class and declares method
// signatures (prototypes).  The companion file {\tt ESMC\_Clock.C} contains
// the definitions (full code bodies) for the Clock methods.
//
//EOPI
//-----------------------------------------------------------------------------


#ifdef __cplusplus
extern "C" {
#endif

// Class declaration type
typedef struct{
  void *ptr;
}ESMC_Clock;

// Class API

ESMC_Clock ESMC_ClockCreate(int, const char*, ESMC_TimeInterval, ESMC_Time,
  ESMC_Time, int*);

int ESMC_ClockPrint(ESMC_Clock);

int ESMC_ClockAdvance(ESMC_Clock);

int ESMC_ClockGet(ESMC_Clock, ESMC_TimeInterval* currSimTime,
                              ESMC_I8* advanceCount);

int ESMC_ClockDestroy(ESMC_Clock*);

#ifdef __cplusplus
} //extern "C"
#endif

#endif // ESMC_Clock_H
