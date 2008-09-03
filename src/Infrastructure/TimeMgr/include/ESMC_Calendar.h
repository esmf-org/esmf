// $Id: ESMC_Calendar.h,v 1.61 2008/09/03 05:56:37 eschwab Exp $
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

#ifndef ESMC_Calendar_H
#define ESMC_Calendar_H

//-----------------------------------------------------------------------------
//BOPI
// !CLASS:  ESMC_Calendar - Public C interface to the ESMF Calendar class
//
// !DESCRIPTION:
//
// The code in this file defines the public C Calendar class and declares method
// signatures (prototypes).  The companion file {\tt ESMC\_Calendar.C} contains
// the definitions (full code bodies) for the Calendar methods.
//
//EOPI
//-----------------------------------------------------------------------------

// (TMG 2.3.1, 2.3.2, 2.3.3, 2.3.4, 2.3.5)
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
                        // and ESMC_CAL_NOCALENDAR so Validate() doesn't need
                        // to change.  Also add to static intializers at top
                        // of ESMCI_Calendar.C



extern "C" {

// Class declaration type
typedef struct{
  void *ptr;
}ESMC_Calendar;

// Class API

ESMC_Calendar ESMC_CalendarCreate(int, const char*, ESMC_CalendarType, int*);  

int ESMC_CalendarPrint(ESMC_Calendar);

int ESMC_CalendarDestroy(ESMC_Calendar*);

}; //extern "C"


#endif // ESMC_Calendar_H
