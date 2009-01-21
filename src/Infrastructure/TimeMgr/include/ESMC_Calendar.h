// $Id: ESMC_Calendar.h,v 1.51.2.3 2009/01/21 21:25:23 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
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
#include "ESMC_Start.h"
#include "ESMF_TimeMgr.inc"

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
// behavior) of all required calendar types:  Gregorian, Julian, Julian Day,
// no-leap, 360-day, custom, and no-calendar.
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
// time instants is done by the base class {\tt BaseTime} in the core units
// of seconds and fractional seconds.  Thus,  a calendar is only needed for
// converting core time to calendar time and vice versa.
//
// Notes:
//    - Instantiate as few as possible; ideally no more than one calendar
//      type per application (for reference only, like a wall calendar)
//      But may have multiples for convienience such as one per component.
//    - Generic enough to define for any planetary body
//    - if secondsPerDay != 86400, then how are minutes and hours defined ?
//      Assume always minute=60 seconds; hour=3600 seconds
//
//-------------------------------------------------------------------------
//  
// !USES:
#include "ESMC_Base.h"           // inherited Base class
#include "ESMC_BaseTime.h"       // inherited BaseTime class

// forward reference to prevent #include recursion
class ESMC_Time;
class ESMC_TimeInterval;

// TODO: replace with monthsPerYear property
#define MONTHS_PER_YEAR 12

// (TMG 2.3.1, 2.3.2, 2.3.3, 2.3.4, 2.3.5)
#define CALENDAR_TYPE_COUNT 7
enum ESMC_CalendarType {ESMC_CAL_GREGORIAN=1,
                        ESMC_CAL_JULIAN,
                        ESMC_CAL_JULIANDAY,
                        ESMC_CAL_NOLEAP,      // like Gregorian, except
                                              //   Feb always has 28 days
                        ESMC_CAL_360DAY,      // 12 months, 30 days each
                        ESMC_CAL_CUSTOM,      // user defined
                        ESMC_CAL_NOCALENDAR}; // track base time seconds
                                              //   only
                        // Note: add new calendars between ESMC_CAL_GREGORIAN
                        // and ESMC_CAL_NOCALENDAR so Validate() doesn't need
                        // to change.  Also add to static intializers at top
                        // of ESMC_Calendar.C

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

    char              name[ESMF_MAXSTR];  // name of calendar
    ESMC_CalendarType calendarType;       // Calendar type

    int daysPerMonth[MONTHS_PER_YEAR];
    int monthsPerYear;
// TODO: make dynamically allocatable with monthsPerYear
    ESMC_I4 secondsPerDay;
    ESMC_I4 secondsPerYear;
    struct daysPerYear_s
    {
        ESMC_I4 d;    // integer number of days per year
        ESMC_I4 dN;   // fractional number of days per year (numerator)
        ESMC_I4 dD;   //                                    (denominator)
    } daysPerYear;    // e.g. for Venus, d=0, dN=926, dD=1000

    // array of calendar type name strings
    static const char *const calendarTypeName[CALENDAR_TYPE_COUNT];

    // one-of-each calendar type held automatically, as needed
    static ESMC_Calendar *internalCalendar[CALENDAR_TYPE_COUNT];
    static ESMC_Calendar *defaultCalendar;  // set-up upon ESMF_Initialize();
                                            // defaults to ESMC_CAL_NOCALENDAR

    int               id;         // unique identifier. used for equality
                                  //    checks and to generate unique default
                                  //    names.
                                  //    TODO: inherit from ESMC_Base class
    static int        count;      // number of calendars created. Thread-safe
                                  //   because int is atomic.
                                  //    TODO: inherit from ESMC_Base class

// !PUBLIC MEMBER FUNCTIONS:

  public:

    // set built-in calendar type
    int ESMC_CalendarSet(int               nameLen,
                         const char       *name,    // TODO: default (=0)
                         ESMC_CalendarType calendarType);

    // set custom calendar type
    int ESMC_CalendarSet(int           nameLen,      
                         const char   *name=0,
                         int          *daysPerMonth=0,
                         int           monthsPerYear=0,
                         ESMC_I4 *secondsPerDay=0,
                         ESMC_I4 *daysPerYear=0,
                         ESMC_I4 *daysPerYearDn=0,
                         ESMC_I4 *daysPerYearDd=0);

    // get properties of any calendar type
    int ESMC_CalendarGet(int                nameLen,
                         int               *tempNameLen,
                         char              *tempName,
                         ESMC_CalendarType *calendarType=0,
                         int               *daysPerMonth=0,
                         int                sizeofDaysPerMonth=0,
                         int               *monthsPerYear=0,
                         ESMC_I4      *secondsPerDay=0,
                         ESMC_I4      *secondsPerYear=0,
                         ESMC_I4      *daysPerYear=0,
                         ESMC_I4      *daysPerYeardN=0,
                         ESMC_I4      *daysPerYeardD=0);

    // Calendar doesn't need configuration, hence GetConfig/SetConfig
    // methods are not required

    // conversions based on UTC: time zone offset done by client
    //  (TMG 2.4.5, 2.5.6)
    int ESMC_CalendarConvertToTime(ESMC_I8 yy, int mm, int dd,
                                   ESMC_I8 d, ESMC_BaseTime *t) const;
    int ESMC_CalendarConvertToDate(ESMC_BaseTime *t,
                                   ESMC_I4 *yy=0, ESMC_I8 *yy_i8=0,
                                   int *mm=0, int *dd=0,
                                   ESMC_I4 *d=0, ESMC_I8 *d_i8=0,
                                   ESMC_R8 *d_r8=0) const;

    ESMC_Time ESMC_CalendarIncrement(const ESMC_Time *time,
                                     const ESMC_TimeInterval &timeinterval)
                                     const;

    ESMC_Time ESMC_CalendarDecrement(const ESMC_Time *time,
                                     const ESMC_TimeInterval &timeinterval)
                                     const;

    bool ESMC_CalendarIsLeapYear(ESMC_I8 yy, int *rc=0) const;

    bool operator==(const ESMC_Calendar &) const;
    bool operator==(const ESMC_CalendarType &) const;
    bool operator!=(const ESMC_Calendar &) const;
    bool operator!=(const ESMC_CalendarType &) const;

    // TODO:  add method to convert calendar interval to core time ?

    // required methods inherited and overridden from the ESMC_Base class

    // for persistence/checkpointing

    // friend to restore state
    friend ESMC_Calendar *ESMC_CalendarReadRestart(int, const char*,
                                                   ESMC_IOSpec*, int*);
    // save state
    int ESMC_CalendarWriteRestart(ESMC_IOSpec *iospec=0) const;

    // internal validation
    int ESMC_CalendarValidate(const char *options=0) const;

    // for testing/debugging
    int ESMC_CalendarPrint(const char *options=0, 
                           const ESMC_Time *time=0) const;

    // native C++ constructors/destructors
    ESMC_Calendar(void);
    ESMC_Calendar(const ESMC_Calendar &calendar);  // copy constructor
    ESMC_Calendar(const char *name, ESMC_CalendarType calendarType);
    ESMC_Calendar(const char *name, int *daysPerMonth, int monthsPerYear,
                  ESMC_I4 *secondsPerDay, ESMC_I4 *daysPerYear,
                  ESMC_I4 *daysPerYeardN, ESMC_I4 *daysPerYearDd);
    ~ESMC_Calendar(void);

 // < declare the rest of the public interface methods here >

    // friend function to allocate and initialize calendar from heap
    friend ESMC_Calendar *ESMC_CalendarCreate(int, const char*,
                                              ESMC_CalendarType, int*);

    // friend function to allocate and initialize internal calendar from heap
    friend int ESMC_CalendarCreate(ESMC_CalendarType);

    // friend function to allocate and initialize custom calendar from heap
    friend ESMC_Calendar *ESMC_CalendarCreate(int, const char*,
                                              int*, int,
                                              ESMC_I4*,
                                              ESMC_I4*,
                                              ESMC_I4*,
                                              ESMC_I4*, int*);

    // friend function to copy a calendar
    friend ESMC_Calendar *ESMC_CalendarCreate(ESMC_Calendar*, int*);

    // friend function to de-allocate calendar
    friend int ESMC_CalendarDestroy(ESMC_Calendar **);

    // friend function to de-allocate all internal calendars
    friend int ESMC_CalendarFinalize(void);
    
    // friend functions to initialize and set the default calendar
    friend int ESMC_CalendarInitialize(ESMC_CalendarType *);
    friend int ESMC_CalendarSetDefault(ESMC_Calendar **);
    friend int ESMC_CalendarSetDefault(ESMC_CalendarType *);

// !PRIVATE MEMBER FUNCTIONS:
//
  private:

    friend class ESMC_Time;
    friend class ESMC_TimeInterval;

//
 // < declare private interface methods here >
//
//EOP
//-------------------------------------------------------------------------

};  // end class ESMC_Calendar

    // Note: though seemingly redundant with the friend declarations within
    // the class definition above, the following declarations are necessary
    // to appease some compilers (most notably IBM), as well as ANSI C++.
    // These also establish defaults to match F90 optional args.

    // friend function to allocate and initialize calendar from heap
    ESMC_Calendar *ESMC_CalendarCreate(int               nameLen,
                                       const char       *name=0,
                                       ESMC_CalendarType calendarType=
                                                           ESMC_CAL_NOCALENDAR,
                                       int*              rc=0);

    // friend function to allocate and initialize internal calendar from heap
    int ESMC_CalendarCreate(ESMC_CalendarType calendarType);

    // friend function to allocate and initialize custom calendar from heap
    ESMC_Calendar *ESMC_CalendarCreate(int           nameLen,
                                       const char   *name=0,
                                       int          *daysPerMonth=0,
                                       int           monthsPerYear=0,
                                       ESMC_I4 *secondsPerDay=0,
                                       ESMC_I4 *daysPerYear=0,
                                       ESMC_I4 *daysPerYearDn=0,
                                       ESMC_I4 *daysPerYearDd=0,
                                       int          *rc=0);

    // friend function to copy a calendar
    ESMC_Calendar *ESMC_CalendarCreate(ESMC_Calendar *calendar, int *rc=0);

    // friend function to de-allocate calendar
    int ESMC_CalendarDestroy(ESMC_Calendar **calendar);

    // friend function to de-allocate all internal calendars
    int ESMC_CalendarFinalize(void);

    // friend to restore state
    ESMC_Calendar *ESMC_CalendarReadRestart(int nameLen,
                                            const char*  name=0,
                                            ESMC_IOSpec* iospec=0,
                                            int*         rc=0);

    // friend functions to initialize and set the default calendar
    int ESMC_CalendarInitialize(ESMC_CalendarType *calendarType);
    int ESMC_CalendarSetDefault(ESMC_Calendar **calendar);
    int ESMC_CalendarSetDefault(ESMC_CalendarType *calendarType);

#endif // ESMC_CALENDAR_H
