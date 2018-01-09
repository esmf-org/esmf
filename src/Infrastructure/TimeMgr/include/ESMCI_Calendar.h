// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2018, University Corporation for Atmospheric Research,
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

#ifndef ESMCI_CALENDAR_H
#define ESMCI_CALENDAR_H

//-------------------------------------------------------------------------

 // put any constants or macros which apply to the whole component in this file.
 // anything public or esmf-wide should be up higher at the top level
 // include files.
#include "ESMCI_Util.h"
#include "ESMCI_Macros.h"
#include "ESMF_TimeMgr.inc"

//-------------------------------------------------------------------------
//BOP
// !CLASS: ESMCI::Calendar - encapsulates calendar kinds and behavior
//
// !DESCRIPTION:
//
// The code in this file defines the C++ {\tt Calendar} members and method
// signatures (prototypes).  The companion file {\tt ESMC\_Calendar.C} contains
// the full code (bodies) for the {\tt Calendar} methods.
//
// The {\tt Calendar} class encapsulates the knowledge (attributes and
// behavior) of all required calendar kinds:  Gregorian, Julian, Julian Day,
// Modified Julian Day, no-leap, 360-day, custom, and no-calendar.
//
// The {\tt Calendar} class encapsulates the definition of all required
// calendar kinds. For each calendar kind, it contains the number of months
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
//      kind per application (for reference only, like a wall calendar)
//      But may have multiples for convenience such as one per component.
//    - Generic enough to define for any planetary body
//    - if secondsPerDay != 86400, then how are minutes and hours defined ?
//      Assume always minute=60 seconds; hour=3600 seconds
//
//-------------------------------------------------------------------------
//  
// !USES:
#include "ESMCI_Fraction.h"
#include "ESMCI_BaseTime.h"       // inherited BaseTime class
#include "ESMC_Calendar.h"        // for enum ESMC_CalKind_Flag

// TODO: replace with monthsPerYear property
#define MONTHS_PER_YEAR 12


namespace ESMCI{

// forward reference to prevent #include recursion
class Time;
class TimeInterval;

// !PUBLIC TYPES:
 class Calendar;

// !PRIVATE TYPES:
 // class configuration type:  not needed for Calendar

 // class definition type
class Calendar {
// class Calendar : public ESMC_Base { // TODO: inherit from ESMC_Base
                                            // class when fully aligned with
                                            //  F90 equiv

  private:   // corresponds to F90 module 'type ESMF_Calendar' members

    char              name[ESMF_MAXSTR];  // name of calendar
    ESMC_CalKind_Flag calkindflag;        // Calendar kind

    int daysPerMonth[MONTHS_PER_YEAR];
    int monthsPerYear;
// TODO: make dynamically allocatable with monthsPerYear
    ESMC_I4 secondsPerDay;        // TODO:  fractional secondsPerDay  sN/sD
    ESMC_I4 secondsPerYear;       // TODO:  fractional secondsPerYear yN/yD
    Fraction daysPerYear; // w: integer number of days per year
                          // n: fractional number of days per year (numerator)
                          // d:                                    (denominator)
                          // e.g. for Venus, w=0, n=926, d=1000

    // array of calendar kind name strings
    static const char *const calkindflagName[CALENDAR_KIND_COUNT];

    // one-of-each calendar kind held automatically, as needed
    static Calendar *internalCalendar[CALENDAR_KIND_COUNT];
    static Calendar *defaultCalendar;  // set-up upon ESMF_Initialize();
                                       // defaults to ESMC_CALKIND_NOCALENDAR

    int               id;         // unique identifier. used for equality
                                  //    checks and to generate unique default
                                  //    names.
                                  //    TODO: inherit from ESMC_Base class
    static int        count;      // number of calendars created. Thread-safe
                                  //   because int is atomic.
                                  //    TODO: inherit from ESMC_Base class

// !PUBLIC MEMBER FUNCTIONS:

  public:

    // set built-in calendar kind
    int set(int               nameLen,
            const char       *name,    // TODO: default (=0)
            ESMC_CalKind_Flag calkindflag);

    // set custom calendar kind
    int set(int           nameLen,      
            const char   *name=0,
            int          *daysPerMonth=0,
            int           monthsPerYear=0,
            ESMC_I4      *secondsPerDay=0,
            ESMC_I4      *daysPerYear=0,
            ESMC_I4      *daysPerYearDn=0,
            ESMC_I4      *daysPerYearDd=0);

    // get properties of any calendar kind
    int get(int                nameLen,
            int               *tempNameLen,
            char              *tempName,
            ESMC_CalKind_Flag *calkindflag=0,
            int               *daysPerMonth=0,
            int                sizeofDaysPerMonth=0,
            int               *monthsPerYear=0,
            ESMC_I4           *secondsPerDay=0,
            ESMC_I4           *secondsPerYear=0,
            ESMC_I4           *daysPerYear=0,
            ESMC_I4           *daysPerYeardN=0,
            ESMC_I4           *daysPerYeardD=0);

    // Calendar doesn't need configuration, hence GetConfig/SetConfig
    // methods are not required

    // conversions based on UTC: time zone offset done by client
    //  (TMG 2.4.5, 2.5.6)
    int convertToTime(ESMC_I8 yy, int mm, int dd,
                      ESMC_I8 d, ESMC_R8 d_r8, BaseTime *t) const;
    int convertToDate(BaseTime *t, ESMC_I4 *yy=0, ESMC_I8 *yy_i8=0,
                      int *mm=0, int *dd=0,
                      ESMC_I4 *d=0, ESMC_I8 *d_i8=0,
                      ESMC_R8 *d_r8=0) const;

    Time increment(const Time *time, const TimeInterval &timeinterval) const;

    Time decrement(const Time *time, const TimeInterval &timeinterval) const;

    bool isLeapYear(ESMC_I8 yy, int *rc=0) const;

    bool operator==(const Calendar &) const;
    bool operator==(const ESMC_CalKind_Flag &) const;
    bool operator!=(const Calendar &) const;
    bool operator!=(const ESMC_CalKind_Flag &) const;

    // TODO:  add method to convert calendar interval to core time ?

    // required methods inherited and overridden from the ESMC_Base class

    // for persistence/checkpointing

    // friend to restore state
    friend Calendar *ESMCI_CalendarReadRestart(int, const char*, int*);
    // save state
    int writeRestart(void) const;

    // internal validation
    int validate(const char *options=0) const;

    // for testing/debugging
    int print(const char *options=0, const Time *time=0) const;

    // native C++ constructors/destructors
    Calendar(void);
    Calendar(const Calendar &calendar);  // copy constructor
    Calendar(const char *name, ESMC_CalKind_Flag calkindflag);
    Calendar(const char *name, int *daysPerMonth, int monthsPerYear,
             ESMC_I4 *secondsPerDay, ESMC_I4 *daysPerYear,
             ESMC_I4 *daysPerYeardN, ESMC_I4 *daysPerYearDd);
    ~Calendar(void);

 // < declare the rest of the public interface methods here >

    // friend function to allocate and initialize calendar from heap
    friend Calendar *ESMCI_CalendarCreate(int, const char*,
                                          ESMC_CalKind_Flag, int*);

    // friend function to allocate and initialize internal calendar from heap
    friend int ESMCI_CalendarCreate(ESMC_CalKind_Flag);

    // friend function to allocate and initialize custom calendar from heap
    friend Calendar *ESMCI_CalendarCreate(int, const char*, int*, int,
                                          ESMC_I4*, ESMC_I4*,
                                          ESMC_I4*, ESMC_I4*, int*);

    // friend function to copy a calendar
    friend Calendar *ESMCI_CalendarCreate(Calendar*, int*);

    // friend function to de-allocate calendar
    friend int ESMCI_CalendarDestroy(Calendar **);

    // friend function to de-allocate all internal calendars
    friend int ESMCI_CalendarFinalize(void);
    
    // friend functions to initialize and set the default calendar
    friend int ESMCI_CalendarInitialize(ESMC_CalKind_Flag *);
    friend int ESMCI_CalendarSetDefault(Calendar **);
    friend int ESMCI_CalendarSetDefault(ESMC_CalKind_Flag *);

// !PRIVATE MEMBER FUNCTIONS:
//
  private:

    friend class Time;
    friend class TimeInterval;

//
 // < declare private interface methods here >
//
//EOP
//-------------------------------------------------------------------------

};  // end class Calendar

    // Note: though seemingly redundant with the friend declarations within
    // the class definition above, the following declarations are necessary
    // to appease some compilers (most notably IBM), as well as ANSI C++.
    // These also establish defaults to match F90 optional args.

    // friend function to allocate and initialize calendar from heap
    Calendar *ESMCI_CalendarCreate(int               nameLen,
                                   const char       *name=0,
                                   ESMC_CalKind_Flag calkindflag=
                                                       ESMC_CALKIND_NOCALENDAR,
                                   int*              rc=0);

    // friend function to allocate and initialize internal calendar from heap
    int ESMCI_CalendarCreate(ESMC_CalKind_Flag calkindflag);

    // friend function to allocate and initialize custom calendar from heap
    Calendar *ESMCI_CalendarCreate(int           nameLen,
                                   const char   *name=0,
                                   int          *daysPerMonth=0,
                                   int           monthsPerYear=0,
                                   ESMC_I4      *secondsPerDay=0,
                                   ESMC_I4      *daysPerYear=0,
                                   ESMC_I4      *daysPerYearDn=0,
                                   ESMC_I4      *daysPerYearDd=0,
                                   int          *rc=0);

    // friend function to copy a calendar
    Calendar *ESMCI_CalendarCreate(Calendar *calendar, int *rc=0);

    // friend function to de-allocate calendar
    int ESMCI_CalendarDestroy(Calendar **calendar);

    // friend function to de-allocate all internal calendars
    int ESMCI_CalendarFinalize(void);

    // friend to restore state
    Calendar *ESMCI_CalendarReadRestart(int nameLen, const char* name=0,
                                        int* rc=0);

    // friend functions to initialize and set the default calendar
    int ESMCI_CalendarInitialize(ESMC_CalKind_Flag *calkindflag);
    int ESMCI_CalendarSetDefault(Calendar **calendar);
    int ESMCI_CalendarSetDefault(ESMC_CalKind_Flag *calkindflag);

} // namespace ESMCI
#endif // ESMC_CALENDAR_H
