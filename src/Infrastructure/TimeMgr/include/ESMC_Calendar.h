// $Id: ESMC_Calendar.h,v 1.9 2003/04/11 23:47:14 eschwab Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the GPL.
//
// ESMF Calendar C++ definition include file
//
// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
//
 // these lines prevent this file from being read more than once if it
 // ends up being included multiple times

#ifndef ESMC_CALENDAR_H
#define ESMC_CALENDAR_H

//-------------------------------------------------------------------------

 // put any constants or macros which apply to the whole component in this file.
 // anything public or esmf-wide should be up higher at the top level
 // include files.
 #include <ESMF_TimeMgr.inc>

//-------------------------------------------------------------------------
//BOP
// !CLASS: ESMC_Calendar - encapsulates calendar types and behavior
//
// !DESCRIPTION:
//
// The code in this file defines the C++ {\tt Calendar} members and method
// signatures (prototypes).  The companion file {\tt ESMC\_Calendar.C} contains
// the full code (bodies) for the {\tt Calendar} methods.
//
// The {\tt Calendar} class encapsulates the knowledge (attributes and
// behavior) of all required calendar types:  Gregorian, Julian, no-leap,
//  360-day, generic, and no-calendar.
//
// The {\tt Calendar} class encapsulates the definition of all required
// calendar types. For each calendar type, it contains the number of months
// per year, the number of days in each month, the number of seconds in a day,
// the number of days per year, and the number of fractional days per year.
// This flexible definition allows future calendars to be defined for any
// planetary body, not just Earth.
//
// The {\tt Calendar} class defines two methods for converting in both
// directions between the core {\tt BaseTime} class representation and a
// calendar date.  Calculations of time intervals (deltas) between
// timeiinstants is done by the base class {\tt BaseTime} in the core units
// of seconds and fractional seconds.  Thus,  a calendar is only needed for
// converting core time to calendar time and vice versa.
//
// Notes:
//    - Instantiate as few as possilbe; ideally no more than one calendar
//      type per application (for reference only, like a wall calendar)
//      But may have multiples for convienience such as one per component.
//    - Generic enough to define for any planetary body
//    - if SecondsPerDay != 86400, then how are minutes and hours defined ?
//      Assume always minute=60 seconds; hour=3600 seconds
//
//-------------------------------------------------------------------------
//  
// !USES:
 #include <ESMC_Base.h>           // inherited Base class
 #include <ESMC_BaseTime.h>
 #include <ESMC_Time.h>

#define MONTHS_PER_YEAR 12

// TODO: make function for Gregorian only?
#define ESMC_IS_LEAP_YEAR(year)  ( (year%400 == 0) || ((year%4 == 0) && \
                                                       (year%100 != 0)) )

// (TMG 2.3.1, 2.3.2, 2.3.3, 2.3.4, 2.3.5)
enum ESMC_CalendarType {ESMC_CAL_GREGORIAN=1,
                        ESMC_CAL_JULIAN,
                        ESMC_CAL_NOLEAP,      // like Gregorian, except
                                              //   Feb always has 28 days
                        ESMC_CAL_360DAY,      // 12 months, 30 days each
                        ESMC_CAL_GENERIC,     // user defined
                        ESMC_CAL_NOCALENDAR}; // track base time seconds
                                              //   only

// !PUBLIC TYPES:
 class ESMC_Calendar;

// !PRIVATE TYPES:
 // class configuration type:  not needed for Calendar

 // class definition type
class ESMC_Calendar {
// class ESMC_Calendar : public ESMC_Base { // TODO: inherit from ESMC_Base
                                            // class when fully aligned with
                                            //  F90 equiv

  private:   // corresponds to F90 module 'type ESMF_Calendar' members

    ESMC_CalendarType Type;    // Calendar type

    int DaysPerMonth[MONTHS_PER_YEAR];
    int SecondsPerDay;
    int SecondsPerYear;
    struct DaysPerYear_s
    {
        int D;        // integer number of days per year
        int Dn;       // fractional number of days per year (numerator)
        int Dd;       //                                    (denominator)
    } DaysPerYear;    // e.g. for Venus, D=0, Dn=926, Dd=1000

// !PUBLIC MEMBER FUNCTIONS:

  public:

    // Calendar is a shallow class, so only Init methods are needed
    int ESMC_CalendarInit(ESMC_CalendarType type);
    int ESMC_CalendarInitGeneric(int *daysPerMonth, int secondsPerDay,
                                 int daysPerYear,   int daysPerYearDn,
                                 int daysPerYearDd);

    // Calendar doesn't need configuration, hence GetConfig/SetConfig
    // methods are not required

    // conversions based on UTC: time zone offset done by client
    //  (TMG 2.4.5, 2.5.6)
    int ESMC_CalendarConvertToTime(int YR, int MM, int DD, int D,
                                   ESMC_BaseTime *T) const;
    int ESMC_CalendarConvertToDate(const ESMC_BaseTime *T,
                                   int *YR, int *MM, int *DD, int *D) const;

    // required methods inherited and overridden from the ESMC_Base class

    // for persistence/checkpointing
    virtual int ESMC_Read(ESMC_CalendarType type,
                          int *daysPerMonth, int secondsPerDay,
                          int daysPerYear,   int daysPerYearDn,
                          int daysPerYearDd);
    virtual int ESMC_Write(ESMC_CalendarType *type,
                           int *daysPerMonth,  int *secondsPerDay,
                           int *daysPerYear,   int *daysPerYearDn,
                           int *daysPerYearDd) const;

    // internal validation
    virtual int ESMC_Validate(const char *options=0) const;

    // for testing/debugging
    virtual int ESMC_Print(const char *options=0) const;

    // native C++ constructors/destructors
    ESMC_Calendar(void);
    ESMC_Calendar(ESMC_CalendarType Type);
    ESMC_Calendar(int *daysPerMonth, int secondsPerDay,
                  int daysPerYear,   int daysPerYearDn, int daysPerYearDd);
    ~ESMC_Calendar(void);

 // < declare the rest of the public interface methods here >
    
    friend class ESMC_Time;

// !PRIVATE MEMBER FUNCTIONS:
//
  private:
//
 // < declare private interface methods here >
//
//EOP
//-------------------------------------------------------------------------

};  // end class ESMC_Calendar

#endif // ESMC_CALENDAR_H
