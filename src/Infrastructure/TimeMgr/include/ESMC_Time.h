// $Id: ESMC_Time.h,v 1.55.2.1 2010/02/05 20:00:07 svasquez Exp $
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

#ifndef ESMC_Time_H
#define ESMC_Time_H

//-----------------------------------------------------------------------------
//BOPI
// !CLASS:  ESMC_Time - Public C interface to the ESMF Time class
//
// !DESCRIPTION:
//
// The code in this file defines the public C Time interfaces and declares
// method signatures (prototypes).  The companion file {\tt ESMC\_Time.C}
// contains the definitions (full code bodies) for the Time methods.
//
//EOPI
//-----------------------------------------------------------------------------

#include "ESMC_Util.h"
#include "ESMC_Calendar.h"

#ifdef __cplusplus
extern "C" {
#endif

// Class declaration type
typedef struct {
  // private:  // Members opaque on C side, philosophically.
    // Allocate enough memory to store members on the C side.
    // Adjust if members are added, rounding up to multiples of
    // 8 bytes (64 bits - largest machine word size).  Add 8 bytes
    // extra padding; match 'type ESMF_Time' in ESMF_TimeType.F90.
    // TODO:  implement isInit initialization like in F90 API?
    char shallowMem[48];  // 5 8-byte members + 1 8-bytes extra = 6 * 8
} ESMC_Time;

// Class API
int ESMC_TimeSet(ESMC_Time *time, ESMC_I4 yy, ESMC_I4 h,
                 ESMC_Calendar calendar, enum ESMC_CalendarType calendartype,
                 int timeZone);

int ESMC_TimeGet(ESMC_Time time, ESMC_I4 *yy, ESMC_I4 *h,
                 ESMC_Calendar *calendar, enum ESMC_CalendarType *calendartype,
                 int *timeZone);

int ESMC_TimePrint(ESMC_Time time);

#ifdef __cplusplus
} // extern "C"
#endif

#endif // ESMC_Time_H
