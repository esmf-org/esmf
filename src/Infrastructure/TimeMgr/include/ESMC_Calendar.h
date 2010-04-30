// $Id: ESMC_Calendar.h,v 1.67 2010/04/30 22:39:28 svasquez Exp $
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

#ifndef ESMC_Calendar_H
#define ESMC_Calendar_H

//-----------------------------------------------------------------------------

// TODO: these definitions need different home (shared with ESMCI_Calendar)
#define CALENDAR_TYPE_COUNT 8
enum ESMC_CalendarType {ESMC_CAL_GREGORIAN=1,
                        ESMC_CAL_JULIAN,
                        ESMC_CAL_JULIANDAY,   
                        ESMC_CAL_MODJULIANDAY,   
                        ESMC_CAL_NOLEAP,      // like Gregorian, except
                                              //   Feb always has 28 days
                        ESMC_CAL_360DAY,      // 12 months, 30 days each
                        ESMC_CAL_CUSTOM,      // user defined
                        ESMC_CAL_NOCALENDAR}; // track base time seconds
                                              //   only
                        // Note: add new calendars between ESMC_CAL_GREGORIAN
                        // and ESMC_CAL_NOCALENDAR so
                        // ESMCI::Calendar::validate() does not need
                        // to change.  Also add to static intializers at top
                        // of ESMCI_Calendar.C

#ifdef __cplusplus
extern "C" {
#endif

// Class declaration type
typedef struct {
  // private:  // members opaque on C side, philosophically.
    void *ptr;
    // TODO:  implement isInit initialization like in F90 API?
} ESMC_Calendar;

// Class API
//BOP
// !IROUTINE: ESMC_CalendarCreate - Create a Calendar
//
// !INTERFACE:
ESMC_Calendar ESMC_CalendarCreate(const char *name,
                                  enum ESMC_CalendarType calendarType, int *rc);  

// ARGUMENTS :
// 	char	name,		
//	enum ESMC_CalendarType calendarType,
//	int *rc
//
// 
// !DESCRIPTION:
//	Create a Calendar of calendar type.
//EOP


//BOP
// !IROUTINE: ESMC_CalendarDestroy - Destroy a Calendar
//
// !INTERFACE:
int ESMC_CalendarDestroy(ESMC_Calendar *calendar);

// ARGUMENTS :
//	ESMC_Calendar *calendar
//
//
// !DESCRIPTION:
//      Destroy a Calendar.
//EOP


//BOP
// !IROUTINE: ESMC_CalendarPrint - Print a Calendar
//
// !INTERFACE:
int ESMC_CalendarPrint(ESMC_Calendar calendar);

// ARGUMENTS :
//      ESMC_Calendar calendar
//
//
// !DESCRIPTION:
//      Print a Calendar.
//EOP



#ifdef __cplusplus
} // extern "C"
#endif

#endif // ESMC_Calendar_H
