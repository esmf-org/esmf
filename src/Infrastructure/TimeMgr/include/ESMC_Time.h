// $Id: ESMC_Time.h,v 1.50 2008/07/10 15:43:50 rosalind Exp $
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

#ifndef ESMC_Time_H
#define ESMC_Time_H

#include "ESMC_Interface.h"
#include "ESMC_Calendar.h"

//-----------------------------------------------------------------------------
//BOPI
// !CLASS:  ESMC_Time - Public C interface to the ESMF Time class
//
// !DESCRIPTION:
//
// The code in this file defines the public C Time class and declares method
// signatures (prototypes).  The companion file {\tt ESMC\_Time.C} contains
// the definitions (full code bodies) for the Time methods.
//
//EOPI
//-----------------------------------------------------------------------------

extern "C" {

// Class declaration type
typedef struct{
  void *ptr;
}ESMC_Time;

// Class API

int ESMC_TimeSet(ESMC_Time*, ESMC_I4, ESMC_I4, ESMC_Calendar,
                       ESMC_CalendarType, int);

int ESMC_TimeGet(ESMC_Time, ESMC_I4*, ESMC_I4*, ESMC_Calendar*,
                       ESMC_CalendarType*, int*);

int ESMC_TimePrint(ESMC_Time);
}; //extern "C"


#endif // ESMC_Time_H
