// $Id: ESMC_Time.h,v 1.53 2008/09/03 23:32:29 theurich Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2008, University Corporation for Atmospheric Research,
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

#include "ESMC_Calendar.h"
#include "ESMC_Util.h"

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

#ifdef __cplusplus
extern "C" {
#endif

// Class declaration type
typedef struct{
  void *ptr;
}ESMC_Time;

// Class API

int ESMC_TimeSet(ESMC_Time*, ESMC_I4, ESMC_I4, ESMC_Calendar,
                       enum ESMC_CalendarType, int);

int ESMC_TimeGet(ESMC_Time, ESMC_I4*, ESMC_I4*, ESMC_Calendar*,
                       enum ESMC_CalendarType*, int*);

int ESMC_TimePrint(ESMC_Time);

#ifdef __cplusplus
} //extern "C"
#endif

#endif // ESMC_Time_H
