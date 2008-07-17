// $Id: ESMC_Clock.h,v 1.56 2008/07/17 16:41:55 rosalind Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2008, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//-------------------------------------------------------------------------
// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMC_Clock_H
#define ESMC_Clock_H

#include "ESMC_Interface.h"

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


extern "C" {

// Class declaration type
typedef struct{
  void *ptr;
}ESMC_Clock;

// Class API

ESMC_Clock ESMC_ClockCreate(int, const char*,
                            ESMC_TimeInterval, 
                            ESMC_Time, ESMC_Time, int*);

int ESMC_ClockPrint(ESMC_Clock);

int ESMC_ClockAdvance(ESMC_Clock);

int ESMC_ClockGet(ESMC_Clock, ESMC_TimeInterval* currSimTime,
                              ESMC_I8* advanceCount);

int ESMC_ClockDestroy(ESMC_Clock*);

}; //extern "C"


#endif // ESMC_Clock_H
