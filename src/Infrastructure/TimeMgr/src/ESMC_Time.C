//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the GPL.
//
// ESMC Time method code (body) file
//
//-------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ {\tt Time} methods declared
// in the companion file {\tt ESMC_Time.h}
//
//-------------------------------------------------------------------------

 // higher level, 3rd party or system includes
 #include <iostream.h>
 #include <math.h>     // modf()
 #include <time.h>
 #include <string.h>

 #include <ESMC_TimeInterval.h>

 // associated class definition file
 #include <ESMC_Time.h>

//-------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_Time.C,v 1.59 2004/04/09 20:13:57 eschwab Exp $";
//-------------------------------------------------------------------------

//
//-------------------------------------------------------------------------
//
// This section includes all the ESMC_Time routines
//
//
// TODO: override the BaseTime (-) method to first check if the two
// times' calendars are the same before performing the difference.
// The F90 interface will then call this overridden method, rather
// than the BaseTime method.
//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeSet - initializer to support F90 interface
//
// !INTERFACE:
      int ESMC_Time::ESMC_TimeSet(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMF_KIND_I4 *yy,        // in - integer year (>= 32-bit)
      ESMF_KIND_I8 *yy_i8,     // in - integer year (large, >= 64-bit)
      int *mm,                 // in - integer month
      int *dd,                 // in - integer day of the month
      ESMF_KIND_I4 *d,         // in - integer days (>= 32-bit)
      ESMF_KIND_I8 *d_i8,      // in - integer days (large, >= 64-bit)
      ESMF_KIND_I4 *h,         // in - integer hours
      ESMF_KIND_I4 *m,         // in - integer minutes
      ESMF_KIND_I4 *s,         // in - integer seconds (>= 32-bit)
      ESMF_KIND_I8 *s_i8,      // in - integer seconds (large, >= 64-bit)
      ESMF_KIND_I4 *ms,        // in - integer milliseconds
      ESMF_KIND_I4 *us,        // in - integer microseconds
      ESMF_KIND_I4 *ns,        // in - integer nanoseconds
      ESMF_KIND_R8 *d_r8,      // in - floating point days
      ESMF_KIND_R8 *h_r8,      // in - floating point hours
      ESMF_KIND_R8 *m_r8,      // in - floating point minutes
      ESMF_KIND_R8 *s_r8,      // in - floating point seconds
      ESMF_KIND_R8 *ms_r8,     // in - floating point milliseconds
      ESMF_KIND_R8 *us_r8,     // in - floating point microseconds
      ESMF_KIND_R8 *ns_r8,     // in - floating point nanoseconds
      ESMF_KIND_I4 *sN,        // in - fractional seconds numerator
      ESMF_KIND_I4 *sD,        // in - fractional seconds denominator
      ESMC_Calendar **calendar, // in - associated calendar
      ESMC_CalendarType *calendarType, // in - associated calendar type
      int *timeZone) {         // in - timezone (hours offset from UTC,
                               //      e.g. EST = -5)
//
// !DESCRIPTION:
//      Initialzes a {\tt Time} with values given in arg list. Supports
//      F90 interface.
//
//EOP
// !REQUIREMENTS:  

    // TODO: Since ESMC_Time is a shallow statically allocated class,
    //       ensure initialization if called via F90 interface;
    //       cannot call constructor, because destructor is subsequently
    //       called automatically, returning initialized values to garbage.

    // if any time value is specified, initialize core values first;
    // user is specifying a complete time and not relying on any past settings.
    // (if only calendar or timezone is specified, don't initialize, but do
    //  validate calendar and/or timezone)
    // (this if-else logic avoids the need for a separate ESMC_TimeSetup()
    //  method)

    // save current time to restore in case there is a failure
    ESMC_Time saveTime = *this;

    if (yy    != ESMC_NULL_POINTER || yy_i8 != ESMC_NULL_POINTER ||
        mm    != ESMC_NULL_POINTER || dd    != ESMC_NULL_POINTER ||
        d     != ESMC_NULL_POINTER || d_i8  != ESMC_NULL_POINTER ||
        h     != ESMC_NULL_POINTER || m     != ESMC_NULL_POINTER ||
        s     != ESMC_NULL_POINTER || s_i8  != ESMC_NULL_POINTER ||
        ms    != ESMC_NULL_POINTER || us    != ESMC_NULL_POINTER ||
        ns    != ESMC_NULL_POINTER || d_r8  != ESMC_NULL_POINTER ||
        h_r8  != ESMC_NULL_POINTER || m_r8  != ESMC_NULL_POINTER ||
        s_r8  != ESMC_NULL_POINTER || ms_r8 != ESMC_NULL_POINTER ||
        us_r8 != ESMC_NULL_POINTER || ns_r8 != ESMC_NULL_POINTER ||
        sN    != ESMC_NULL_POINTER || sD    != ESMC_NULL_POINTER) {
      this->s  = 0;
      this->sN = 0;
      this->sD = 1;                       // prevents divide-by-zero errors
      this->calendar = ESMC_NULL_POINTER; // to trap no calendar case below
      this->timeZone = 0;                 // default is UTC
    } else {
      // only calendar and/or timezone specified

      // set calendar type
      if (calendar != ESMC_NULL_POINTER) {             // 1st choice
        this->calendar = *calendar;
        if (ESMC_TimeValidate("calendar") != ESMF_SUCCESS)
          goto ESMC_TIMESET_FAILURE;

      } else if (calendarType != ESMC_NULL_POINTER) {  // 2nd choice
        // set to specified built-in type; create if necessary
        if (ESMC_CalendarCreate(*calendarType) != ESMF_SUCCESS)
          goto ESMC_TIMESET_FAILURE;
        this->calendar = ESMC_Calendar::internalCalendar[*calendarType-1];
      }

      // set timezone
      if (timeZone != ESMC_NULL_POINTER) {
        this->timeZone = *timeZone;
        if (ESMC_TimeValidate("timezone") != ESMF_SUCCESS)
          goto ESMC_TIMESET_FAILURE;
      }
      return(ESMF_SUCCESS);
    }
    
    // TODO: validate inputs (individual and combos), set basetime values
    //       e.g. integer and float specifiers are mutually exclusive

    // TODO: fractional, sub-seconds

    // set calendar type
    if (calendar != ESMC_NULL_POINTER) {             // 1st choice
      // set to user's calendar
      this->calendar = *calendar;

    } else if (calendarType != ESMC_NULL_POINTER) {  // 2nd choice
      // set to specified built-in type; create if necessary
      if (ESMC_CalendarCreate(*calendarType) != ESMF_SUCCESS)
        goto ESMC_TIMESET_FAILURE;
      this->calendar = ESMC_Calendar::internalCalendar[*calendarType-1];

    } else if (ESMC_Calendar::defaultCalendar != ESMC_NULL_POINTER) {
      // use default calendar                        // 3rd choice
      this->calendar = ESMC_Calendar::defaultCalendar;

    } else {                                         // 4th choice
      // create default calendar
      if (ESMC_CalendarSetDefault((ESMC_CalendarType *)ESMC_NULL_POINTER)
          != ESMF_SUCCESS) goto ESMC_TIMESET_FAILURE;
      this->calendar = ESMC_Calendar::defaultCalendar;
    }

    // set timezone
    if (timeZone != ESMC_NULL_POINTER) {
      this->timeZone = *timeZone;
    }

    // TODO: Timezone adjust

    // convert date to base time according to calendar type
    // TODO: fractional, sub-seconds
    // TODO: create two calendar conversion method entry points ?

    // is a yy/mm/dd style date specified?
    if (yy != ESMC_NULL_POINTER || yy_i8 != ESMC_NULL_POINTER ||
        mm != ESMC_NULL_POINTER || dd    != ESMC_NULL_POINTER) {

      // calendar required
      if (this->calendar == ESMC_NULL_POINTER) goto ESMC_TIMESET_FAILURE;

      // year required
      if (yy == ESMC_NULL_POINTER && yy_i8 == ESMC_NULL_POINTER)
        goto ESMC_TIMESET_FAILURE;

      // day without a month is not good
      if (dd != ESMC_NULL_POINTER && mm == ESMC_NULL_POINTER)
        goto ESMC_TIMESET_FAILURE;

      // use only one specified year (yy and yy_i8 are mutually exclusive)
      ESMF_KIND_I8 argYY = (yy != ESMC_NULL_POINTER) ? *yy : *yy_i8;
      
      // use month and day specified, otherwise use defaults
      int argMM=1, argDD=1;  // defaults
      if (mm != ESMC_NULL_POINTER) argMM = *mm;
      if (dd != ESMC_NULL_POINTER) argDD = *dd;

      // do the conversion
      if (this->calendar->ESMC_CalendarConvertToTime(argYY, argMM, argDD,
                                                     0, this)
            != ESMF_SUCCESS) goto ESMC_TIMESET_FAILURE;

    // is a Julian-days style date specified?
    } else if (d != ESMC_NULL_POINTER || d_i8 != ESMC_NULL_POINTER) {

      // calendar required
      if (this->calendar == ESMC_NULL_POINTER) goto ESMC_TIMESET_FAILURE;

      ESMF_KIND_I8 argD = (d != ESMC_NULL_POINTER) ? *d : *d_i8;
      if (this->calendar->ESMC_CalendarConvertToTime(0, 0, 0, argD, this) !=
          ESMF_SUCCESS) goto ESMC_TIMESET_FAILURE;

    } else if (d_r8 != ESMC_NULL_POINTER) {
      // calendar required
      if (this->calendar == ESMC_NULL_POINTER) goto ESMC_TIMESET_FAILURE;

      // integer part
      if (this->calendar->ESMC_CalendarConvertToTime(0, 0, 0, 
                                                     (ESMF_KIND_I8) *d_r8,
          this) != ESMF_SUCCESS) goto ESMC_TIMESET_FAILURE;

      // fractional part
      this->s += (ESMF_KIND_I8) (modf(*d_r8, ESMC_NULL_POINTER) * 
                                             this->calendar->secondsPerDay);
    }
    
    // use base class to convert sub-day values
    ESMC_BaseTimeSet(h, m, s, s_i8, ms, us, ns, h_r8, m_r8, s_r8,
                     ms_r8, us_r8, ns_r8, sD, sN);

    if (ESMC_TimeValidate() != ESMF_SUCCESS) goto ESMC_TIMESET_FAILURE;
    else return(ESMF_SUCCESS);

    // common failure handler
    ESMC_TIMESET_FAILURE:
      // restore previous value
      *this = saveTime;
      return(ESMF_FAILURE);

 }  // end ESMC_TimeSet

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeGet - Get a Time value; supports F90 interface
//
// !INTERFACE:
      int ESMC_Time::ESMC_TimeGet(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMF_KIND_I4 *yy,           // out - integer year (>= 32-bit)
      ESMF_KIND_I8 *yy_i8,        // out - integer year (large, >= 64-bit)
      int *mm,                    // out - integer month
      int *dd,                    // out - integer day of the month
      ESMF_KIND_I4 *d,            // out - integer days (>= 32-bit)
      ESMF_KIND_I8 *d_i8,         // out - integer days (large, >= 64-bit)
      ESMF_KIND_I4 *h,            // out - integer hours
      ESMF_KIND_I4 *m,            // out - integer minutes
      ESMF_KIND_I4 *s,            // out - integer seconds (>= 32-bit)
      ESMF_KIND_I8 *s_i8,         // out - integer seconds (large, >= 64-bit)
      ESMF_KIND_I4 *ms,           // out - integer milliseconds
      ESMF_KIND_I4 *us,           // out - integer microseconds
      ESMF_KIND_I4 *ns,           // out - integer nanoseconds
      ESMF_KIND_R8 *d_r8,         // out - floating point days
      ESMF_KIND_R8 *h_r8,         // out - floating point hours
      ESMF_KIND_R8 *m_r8,         // out - floating point minutes
      ESMF_KIND_R8 *s_r8,         // out - floating point seconds
      ESMF_KIND_R8 *ms_r8,        // out - floating point milliseconds
      ESMF_KIND_R8 *us_r8,        // out - floating point microseconds
      ESMF_KIND_R8 *ns_r8,        // out - floating point nanoseconds
      ESMF_KIND_I4 *sN,           // out - fractional seconds numerator
      ESMF_KIND_I4 *sD,           // out - fractional seconds denominator
      ESMC_Calendar **calendar,   // out - associated calendar
      ESMC_CalendarType *calendarType, // out - associated calendar type
      int           *timeZone,    // out - timezone (hours offset from UTC)
      int            timeStringLen,     // in  - F90 time string size
      int           *tempTimeStringLen, // out - temp F90 time string size
      char          *tempTimeString,    // out - ISO 8601 format
                                        //       YYYY-MM-DDThh:mm:ss
      int           *dayOfWeek,   // out - day of the week (Mon = 1, Sun = 7)
      ESMC_Time     *midMonth,    // out - middle of the month time instant
      ESMF_KIND_I4 *dayOfYear,    // out - day of the year as an integer
      ESMF_KIND_R8 *dayOfYear_r8, // out - day of the year as a floating point
      ESMC_TimeInterval *dayOfYear_intvl) const {  // out - day of the year
                                                   //       as a time interval
//
// !DESCRIPTION:
//      Gets a {\tt Time}'s values in user-specified format. This version
//      supports the F90 interface.
//
//EOP
// !REQUIREMENTS:  

    // TODO: fractional, sub-seconds

    // convert base time to date according to calendar type
    if (yy != ESMC_NULL_POINTER || yy_i8 != ESMC_NULL_POINTER ||
        mm != ESMC_NULL_POINTER || dd  != ESMC_NULL_POINTER ||
        d  != ESMC_NULL_POINTER || d_i8  != ESMC_NULL_POINTER ||
        d_r8 != ESMC_NULL_POINTER) {
      if (this->calendar == ESMC_NULL_POINTER) return (ESMF_FAILURE);
      if (this->calendar->ESMC_CalendarConvertToDate(this, yy, yy_i8, mm, dd,
                                                     d, d_i8, d_r8) ==
                          ESMF_FAILURE) return(ESMF_FAILURE);
    }

    // get seconds based date (64-bit s_i8 or real s_r8) if requested;
    //   base time get needs to convert entire base time.  Requirements: TMG2.1
    if (ESMC_BaseTimeGet(this->s, ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                  ESMC_NULL_POINTER, s_i8, ESMC_NULL_POINTER,
                                  ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                  ESMC_NULL_POINTER, ESMC_NULL_POINTER, s_r8,
                                  ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                  ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                  ESMC_NULL_POINTER) !=
                         ESMF_SUCCESS) return(ESMF_FAILURE);

    // get number of seconds in a day
    int secPerDay = 0;  // default if day not defined
    if (this->calendar != ESMC_NULL_POINTER) {
      secPerDay = this->calendar->secondsPerDay;
    }

    // 
    ESMF_KIND_I8 timeToConvert = (secPerDay != 0) ?
                                          this->s % secPerDay : this->s;

    // use base class to get all other sub-day values (within date's day)
    if (ESMC_BaseTimeGet((timeToConvert), h, m, s, ESMC_NULL_POINTER,
                         ms, us, ns, h_r8, m_r8, ESMC_NULL_POINTER, ms_r8,
                         us_r8, ns_r8, sN, sD) !=
                     ESMF_SUCCESS) return(ESMF_FAILURE);

    if (calendar != ESMC_NULL_POINTER) {
      *calendar = this->calendar;
    }
    if (calendarType != ESMC_NULL_POINTER) {
      if (this->calendar == ESMC_NULL_POINTER) return(ESMF_FAILURE);
      *calendarType = this->calendar->calendarType;
    }
    if (timeZone != ESMC_NULL_POINTER) {
      *timeZone = this->timeZone;
    }
    if (tempTimeString != ESMC_NULL_POINTER && timeStringLen > 0) {
      if (ESMC_TimeGetString(tempTimeString) != ESMF_SUCCESS)
        return(ESMF_FAILURE);
      *tempTimeStringLen = strlen(tempTimeString);
    }
    if (dayOfWeek != ESMC_NULL_POINTER) {
      if (ESMC_TimeGetDayOfWeek(dayOfWeek) != ESMF_SUCCESS)
        return(ESMF_FAILURE);
    }
    if (midMonth != ESMC_NULL_POINTER) {
      if (ESMC_TimeGetMidMonth(midMonth) != ESMF_SUCCESS)
        return(ESMF_FAILURE);
    }
    if (dayOfYear != ESMC_NULL_POINTER) {
      if (ESMC_TimeGetDayOfYear(dayOfYear) != ESMF_SUCCESS)
        return(ESMF_FAILURE);
    }
    if (dayOfYear_r8 != ESMC_NULL_POINTER) {
      if (ESMC_TimeGetDayOfYear(dayOfYear_r8) != ESMF_SUCCESS)
        return(ESMF_FAILURE);
    }
    if (dayOfYear_intvl != ESMC_NULL_POINTER) {
      if (ESMC_TimeGetDayOfYear(dayOfYear_intvl) != ESMF_SUCCESS)
        return(ESMF_FAILURE);
    }

    return(ESMF_SUCCESS);

 }  // end ESMC_TimeGet

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeGet - Get a Time value
//
// !INTERFACE:
      int ESMC_Time::ESMC_TimeGet(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *timeList,    // in  - time value specifier string
      ...) const {             // out - specifier values (variable args)
//
// !DESCRIPTION:
//      Gets a {\tt Time}'s values in user-specified format. This version
//      supports native C++ use.
//
//EOP
// !REQUIREMENTS:  

    // TODO
    return(ESMF_SUCCESS);

 }  // end ESMC_TimeGet

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeSet - Set a Time value (1)
//
// !INTERFACE:
      int ESMC_Time::ESMC_TimeSet(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_Calendar *calendar,  // in - associated calendar
      int timeZone,             // in - timezone
      const char *timeList,     // in - initializer specifier string
      ...) {                    // in - specifier values (variable args)
//
// !DESCRIPTION:
//      Initialzes a {\tt Time} with values given in variable arg list.
//      Supports native C++ use.
//
//EOP
// !REQUIREMENTS:  

    // set calendar type
    if (calendar != ESMC_NULL_POINTER) {
      this->calendar = calendar;
    }

    // set timezone
    this->timeZone = timeZone;

    // TODO

    return(ESMF_SUCCESS);

 }  // end ESMC_TimeSet

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeSet - Set a Time value (2)
//
// !INTERFACE:
      int ESMC_Time::ESMC_TimeSet(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *timeList,    // in - initializer specifier string
      ...) {                   // in - specifier values (variable args)
//
// !DESCRIPTION:
//      Initialzes a {\tt Time} with values given in variable arg list.
//      Supports native C++ use.
//
//EOP
// !REQUIREMENTS:  

    // TODO
    return(ESMF_SUCCESS);

 }  // end ESMC_TimeSet

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeSet - direct property initializer
//
// !INTERFACE:
      int ESMC_Time::ESMC_TimeSet(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMF_KIND_I8 s,          // in - integer seconds
      int sN,                  // in - fractional seconds, numerator
      int sD,                  // in - fractional seconds, denominator
      ESMC_Calendar *calendar, // in - associated calendar
      ESMC_CalendarType calendarType, // in - associated calendar type
      int timeZone) {          // in - timezone
//
// !DESCRIPTION:
//      Initialzes a {\tt Time} with given values.  Used to avoid constructor
//      to cover case when initial entry is from F90, since destructor is called
//      automatically when leaving scope to return to F90.
//
//EOP
// !REQUIREMENTS:  

    // use base class Set()
    if (ESMC_BaseTime::ESMC_BaseTimeSet(s, sN, sD) != ESMF_SUCCESS)
      return(ESMF_FAILURE);

    // set calendar type
    this->calendar = ESMC_NULL_POINTER;  // to detect invalid, unset time
                                         // TODO: replace with ESMC_Base logic

    if (calendar != ESMC_NULL_POINTER) {                // 1st choice
      // set to user's calendar
      this->calendar = calendar;

    } else if (calendarType != (ESMC_CalendarType)0) {  // 2nd choice
      // set to specified built-in type; create if necessary
      if (ESMC_CalendarCreate(calendarType) != ESMF_SUCCESS)
        return(ESMF_FAILURE);
      this->calendar = ESMC_Calendar::internalCalendar[calendarType-1];
    } // otherwise leave NULL, TODO: implement ESMC_Base logic, then can
      // set default calendar

    this->timeZone = timeZone;

    return(ESMF_SUCCESS);

 }  // end ESMC_TimeSet

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeIsSameCalendar - Compares 2 ESMC_Time's Calendar types
//
// !INTERFACE:
      bool ESMC_Time::ESMC_TimeIsSameCalendar(
//
// !RETURN VALUE:
//    bool true if same calendars, false if different calendars
//
// !ARGUMENTS:
      ESMC_Time *time,    // in - Time to compare Calendar types against
      int *rc) const {    // out - return code
//
// !DESCRIPTION:
//      Compares given {\tt Time}'s {\tt Calendar} type with this {\tt Time}'s
//      {\tt Calendar} type
//
//EOP
// !REQUIREMENTS:  

    if (this->calendar != ESMC_NULL_POINTER &&
        time->calendar != ESMC_NULL_POINTER)
    {
      if (rc != ESMC_NULL_POINTER) *rc = ESMF_SUCCESS;
      return(this->calendar->calendarType == time->calendar->calendarType);
    }
    else {
      if (rc != ESMC_NULL_POINTER) *rc = ESMF_FAILURE;
      return(false);
    }

 }  // end ESMC_TimeIsSameCalendar

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeSyncToRealTime - Sync this Time to wall clock time
//
// !INTERFACE:
      int ESMC_Time::ESMC_TimeSyncToRealTime(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      Sets a {\tt Time}'s value to wall clock time
//
//EOP
// !REQUIREMENTS:  

    // validate for calendar type
    if (this->calendar == ESMC_NULL_POINTER) return (ESMF_FAILURE);
    if (this->calendar->calendarType == ESMC_CAL_JULIANDAY ||
        this->calendar->calendarType == ESMC_CAL_NOCALENDAR) {
        return (ESMF_FAILURE);
    }

    time_t tm;
    struct tm wallClock;

    // get wall clock (system) time
    // TODO:  use POSIX real-time function to get nanosecond resolution
    if (time(&tm) < 0) return (ESMF_FAILURE);
    wallClock = *localtime(&tm);          
    ESMF_KIND_I8 yy_i8 = wallClock.tm_year + 1900;
    int          mm    = wallClock.tm_mon + 1;
    int          dd    = wallClock.tm_mday;
    int          h     = wallClock.tm_hour;
    int          m     = wallClock.tm_min;
    int          s     = wallClock.tm_sec;

    // set this time to wall clock time
    // TODO: use native C++ Set() version when ready
    ESMC_Calendar *cal = this->calendar; // allows reset
    int tz = this->timeZone;             //   after Set() clears it
    ESMC_TimeSet((ESMF_KIND_I4 *)ESMC_NULL_POINTER, &yy_i8, &mm, &dd,
                 ESMC_NULL_POINTER, ESMC_NULL_POINTER, &h, &m,
                 &s, ESMC_NULL_POINTER, ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                 ESMC_NULL_POINTER, ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                 ESMC_NULL_POINTER, ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                 ESMC_NULL_POINTER, ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                 ESMC_NULL_POINTER, &cal, ESMC_NULL_POINTER, &tz);

    return(ESMF_SUCCESS);

 }  // end ESMC_TimeSyncToRealTime

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Time(+) - Increment a Time with a TimeInterval
//
// !INTERFACE:
      ESMC_Time ESMC_Time::operator+(
//
// !RETURN VALUE:
//    ESMC_Time result
//
// !ARGUMENTS:
      const ESMC_TimeInterval &timeInterval) const {  // in - ESMC_TimeInterval
                                                      //      to add
//
// !DESCRIPTION:
//    Adds {\tt timeInterval} expression to this time.
//
//EOP
// !REQUIREMENTS:  

//    Implementation note:  This overrides the ESMC_BaseTime (+) operator
//                          in order to copy the ESMC_Time-only properties
//                          (calendar & timeZone) to the result. 
//
    // delegate the increment operation to my calendar associate
    return(calendar->ESMC_CalendarIncrement(this, timeInterval));

}  // end ESMC_Time::operator+

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Time(-) - Decrement a Time with a TimeInterval
//
// !INTERFACE:
      ESMC_Time ESMC_Time::operator-(
//
// !RETURN VALUE:
//    ESMC_Time result
//
// !ARGUMENTS:
      const ESMC_TimeInterval &timeInterval) const {  // in - ESMC_TimeInterval
                                                      //      to subtract
//
// !DESCRIPTION:
//    Subtracts {\tt timeInterval} expression from this time.
//
//EOP
// !REQUIREMENTS:  

//    Implementation note:  This overrides the ESMC_BaseTime (-) operator
//                          in order to copy the ESMC_Time-only properties
//                          (calendar & timeZone) to the result. 
//
    // delegate the decrement operation to my calendar associate
    return(calendar->ESMC_CalendarDecrement(this, timeInterval));

}  // end ESMC_Time::operator-

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Time(+=) - Increment a Time with a TimeInterval
//
// !INTERFACE:
      ESMC_Time& ESMC_Time::operator+=(
//
// !RETURN VALUE:
//    ESMC_Time& result
//
// !ARGUMENTS:
      const ESMC_TimeInterval &timeInterval) {  // in - ESMC_TimeInterval
                                                //      to add
//
// !DESCRIPTION:
//    Adds {\tt timeInterval} expression to this time.
//
//EOP
// !REQUIREMENTS:  

    // delegate the increment operation to my calendar associate
    *this = calendar->ESMC_CalendarIncrement(this, timeInterval);

    return(*this);

}  // end ESMC_Time::operator+=

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Time(-=) - Decrement a Time with a TimeInterval
//
// !INTERFACE:
      ESMC_Time& ESMC_Time::operator-=(
//
// !RETURN VALUE:
//    ESMC_Time& result
//
// !ARGUMENTS:
      const ESMC_TimeInterval &timeInterval) {  // in - ESMC_TimeInterval
                                                //      to subtract
//
// !DESCRIPTION:
//    Adds {\tt timeInterval} expression to this time.
//
//EOP
// !REQUIREMENTS:  

    // delegate the derement operation to my calendar associate
    *this = calendar->ESMC_CalendarDecrement(this, timeInterval);

    return(*this);

}  // end ESMC_Time::operator-=

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Time(-) - Return the difference between two Times
//
// !INTERFACE:
      ESMC_TimeInterval ESMC_Time::operator-(
//
// !RETURN VALUE:
//    ESMC_TimeInterval result
//
// !ARGUMENTS:
      const ESMC_Time &time) const {  // in - ESMC_Time to subtract
//
// !DESCRIPTION:
//    Subtracts given {\tt time} expression from this time, returns
//    result as {\tt ESMC\_TimeInterval}.
//
//EOP
// !REQUIREMENTS:  

//    Implementation note:  This overrides the 2nd ESMC_BaseTime (-) operator
//                          simply because the 1st (-) operator is overridden.
//                          Visibility into ESMC_BaseTime is lost; if not
//                          defined here, the compiler won't see the 2nd (-)
//                          operator defined at ESMC_BaseTime!
//
    ESMC_TimeInterval diff;

    // perform the decrement using the ESMC_BaseTime operator
    diff = ESMC_BaseTime::operator-(time);

    return(diff);

}  // end ESMC_Time::operator-

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeReadRestart - restore Time state
//
// !INTERFACE:
      int ESMC_Time::ESMC_TimeReadRestart(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int          nameLen,   // in
      const char  *name,      // in   
      ESMC_IOSpec *iospec) {  // in
//
// !DESCRIPTION:
//      restore {\tt Time} state for persistence/checkpointing.
//
//EOP
// !REQUIREMENTS:  

    int rc = ESMF_SUCCESS;

    // TODO:  read time state from iospec/name, then restore
    //        (share code with ESMC_TimeSet()).

    // TODO: use base class ReadRestart() first
    // rc = ESMC_BaseTime::ESMC_BaseTimeReadRestart(s, sN, sD);

    return(rc);

 }  // end ESMC_TimeReadRestart

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeWriteRestart - save Time state
//
// !INTERFACE:
      int ESMC_Time::ESMC_TimeWriteRestart(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_IOSpec *iospec) const {
//
// !DESCRIPTION:
//      Save {\tt Time} state for persistence/checkpointing
//
//EOP
// !REQUIREMENTS:  

    int rc = ESMF_SUCCESS;

    // TODO: use base class Write() first
    //  rc = ESMC_BaseTime::ESMC_BaseTimeWriteRestart(s, sN, sD);

    // calendar= this->calendar;  // TODO?: this only saves calendar pointer;
                               //  component must be sure to save corresponding
                               //  calendar.
    return(rc);

 }  // end ESMC_TimeWriteRestart

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeValidate - validate Time state
//
// !INTERFACE:
      int ESMC_Time::ESMC_TimeValidate(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {    // in - validate options
//
// !DESCRIPTION:
//      validate {\tt Time} state for testing/debugging
//
//EOP
// !REQUIREMENTS:  

    // parse options
    if (options != ESMC_NULL_POINTER) {

      // TODO:  put calendar and timezone validate logic into separate,
      //        private methods (inline?) to avoid duplicate code ?

      // validate calendar only, not time values
      if (strncmp(options, "calendar", 8) == 0) {
        if (this->calendar == ESMC_NULL_POINTER) return(ESMF_FAILURE);
        if (this->calendar->ESMC_CalendarValidate() != ESMF_SUCCESS) {
          return(ESMF_FAILURE);
        }
        return(ESMF_SUCCESS);

      // validate timezone only, not time values
      } else if (strncmp(options, "timezone", 8) == 0) {
        // TODO: valid Timezones ?
        return(ESMF_SUCCESS);
      }
    }

    if (ESMC_BaseTime::ESMC_BaseTimeValidate() != ESMF_SUCCESS)
      return(ESMF_FAILURE);

    if (this->calendar == ESMC_NULL_POINTER) return(ESMF_FAILURE);
    if (this->calendar->ESMC_CalendarValidate() != ESMF_SUCCESS)
      return(ESMF_FAILURE);

    // earliest Gregorian date representable by the Fliegel algorithm
    //  is -4800/3/1 == -32044 Julian days == -2,768,601,600 core seconds
    if (calendar->calendarType == ESMC_CAL_GREGORIAN && s < -2768601600LL)
        return(ESMF_FAILURE);

    // TODO: other calendar ranges ?

    // TODO: valid Timezones ?

    return(ESMF_SUCCESS);

 }  // end ESMC_TimeValidate

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimePrint - print Time state
//
// !INTERFACE:
      int ESMC_Time::ESMC_TimePrint(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {    // in - print options
//
// !DESCRIPTION:
//      print {\tt Time} state for testing/debugging
//
//EOP
// !REQUIREMENTS:  

    cout << "Time -----------------------------------" << endl;

    // parse options
    if (options != ESMC_NULL_POINTER) {
      if (strncmp(options, "string", 6) == 0) {
        char timeString[ESMF_MAXSTR];
        ESMC_TimeGetString(timeString);
        cout << timeString << endl;
      }
    } else {
      // default
      ESMC_BaseTime::ESMC_BaseTimePrint(options);
      if (this->calendar != ESMC_NULL_POINTER) {
        this->calendar->ESMC_CalendarPrint(options);
      }
      cout << "timeZone = " << timeZone << endl;
    }

    cout << "end Time -------------------------------" << endl << endl;

    return(ESMF_SUCCESS);

 }  // end ESMC_TimePrint

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Time - native default C++ constructor
//
// !INTERFACE:
      ESMC_Time::ESMC_Time(void) {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      Initializes a {\tt ESMC\_Time} with defaults
//
//EOP
// !REQUIREMENTS:  

//   ESMC_BaseTime(0, 0, 1) {  // TODO: F90 issue with base class constructor?
   s  = 0;
   sN = 0;
   sD = 1;
   calendar = ESMC_NULL_POINTER;  // to detect invalid, unset time
                                  // TODO: replace with ESMC_Base logic
   timeZone = 0;

 }  // end ESMC_Time

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Time - native C++ constructor
//
// !INTERFACE:
      ESMC_Time::ESMC_Time(
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
      ESMF_KIND_I8 s,           // in - integer seconds
      ESMF_KIND_I4 sN,          // in - fractional seconds, numerator
      ESMF_KIND_I4 sD,          // in - fractional seconds, denominator
      ESMC_Calendar *calendar,  // in - associated calendar
      ESMC_CalendarType calendarType,  // in - associated calendar type
      int timeZone) :           // in - timezone
//
// !DESCRIPTION:
//      Initializes a {\tt ESMC\_Time}
//
//EOP
// !REQUIREMENTS:  

   ESMC_BaseTime(s, sN, sD) {   // use base class constructor

  // set calendar type
  this->calendar = ESMC_NULL_POINTER;  // to detect invalid, unset time
                                       // TODO: replace with ESMC_Base logic

  if (calendar != ESMC_NULL_POINTER) {                // 1st choice
    // set to user's calendar
    this->calendar = calendar;

  } else if (calendarType != (ESMC_CalendarType)0) {  // 2nd choice
    // set to specified built-in type; create if necessary
    ESMC_CalendarCreate(calendarType);
    this->calendar = ESMC_Calendar::internalCalendar[calendarType-1];
  } // otherwise leave NULL, TODO: implement ESMC_Base logic, then can
    // set default calendar

  // TODO: catch & throw exceptions above, and/or LogErr

  this->timeZone = timeZone;

 }  // end ESMC_Time

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ~ESMC_Time - native default C++ destructor
//
// !INTERFACE:
      ESMC_Time::~ESMC_Time(void) {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      Default {\tt ESMC\_Time} destructor
//
//EOP
// !REQUIREMENTS:  

 }  // end ~ESMC_Time

//-------------------------------------------------------------------------
//  Private methods to support ESMC_TimeGet() API
//-------------------------------------------------------------------------

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeGetString - Get a Time value
//
// !INTERFACE:
      int ESMC_Time::ESMC_TimeGetString(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      char *timeString) const {    // out - time value in string format
//
// !DESCRIPTION:
//      Gets a {\tt Time}'s value in ISO 8601 character format
//      YYYY-MM-DDThh:mm:ss
//
//EOP
// !REQUIREMENTS:  

    // validate inputs
    if (timeString == ESMC_NULL_POINTER) return (ESMF_FAILURE);
    if (this->calendar == ESMC_NULL_POINTER) return (ESMF_FAILURE);
    if (this->calendar->calendarType == ESMC_CAL_JULIANDAY ||
        this->calendar->calendarType == ESMC_CAL_NOCALENDAR) {
        return (ESMF_FAILURE);
    }

    ESMF_KIND_I8 yy_i8;
    int mm, dd;
    ESMF_KIND_I4 h, m, s; 
    // TODO: use native C++ Get, not F90 entry point, when ready
    ESMC_TimeGet((ESMF_KIND_I4 *)ESMC_NULL_POINTER, &yy_i8, &mm, &dd,
                  ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                  &h, &m, &s, ESMC_NULL_POINTER);

    // ISO 8601 format YYYY-MM-DDThh:mm:ss
    sprintf(timeString, "%lld-%02d-%02dT%02d:%02d:%02d\0",
            yy_i8, mm, dd, h, m, s);

    return(ESMF_SUCCESS);

 }  // end ESMC_TimeGetString

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeGetDayOfWeek - Get a Time's day of the week value
//
// !INTERFACE:
      int ESMC_Time::ESMC_TimeGetDayOfWeek(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int *dayOfWeek) const {    // out - time's day of week value
//
// !DESCRIPTION:
//      Gets a {\tt Time}'s day of the week value in ISO 8601 format:
//      Monday = 1 through Sunday = 7
//
//EOP
// !REQUIREMENTS:  

    // validate inputs
    if (dayOfWeek == ESMC_NULL_POINTER) return (ESMF_FAILURE);
    if (this->calendar == ESMC_NULL_POINTER) return (ESMF_FAILURE);

    // date variables
    ESMF_KIND_I8 yy_i8;
    int mm, dd;

    //  The day of the week is simply modulo 7 from a reference date,
    //  adjusted for a 1-based count and negative deltas.
    //  The reference date is any known Monday (day of the week = 1)
    //  This method is valid for any calendar which uses 7-day weeks.

    switch (this->calendar->calendarType)
    {
        case ESMC_CAL_GREGORIAN:
        case ESMC_CAL_NOLEAP:    // TODO: ?
        case ESMC_CAL_360DAY:    // TODO: ?
          //  Can be any Monday after the Gregorian reformation of 9/14/1752
          yy_i8=1796; mm=7; dd=4;   // America's 20th birthday was a Monday !
          break;

        case ESMC_CAL_JULIANDAY:
          //  Can be any Monday before the Gregorian reformation of 9/2/1752
          yy_i8=1492; mm=10; dd=29;  // Columbus landed in Cuba on a Monday !
          break;

        case ESMC_CAL_NOCALENDAR:
        case ESMC_CAL_CUSTOM:
        default:
          return(ESMF_FAILURE);
          break;
    }

    // TODO: put the above reference dates into a pre-initialized lookup table
    //       to skip this step
    ESMC_Time referenceMonday;
    // TODO: use native C++ Set() when ready
    ESMC_Calendar *cal = this->calendar; // allows reset
    int tz = this->timeZone;             //   after Set() clears it
    referenceMonday.ESMC_TimeSet((ESMF_KIND_I4 *)ESMC_NULL_POINTER,
                                 &yy_i8, &mm, &dd, ESMC_NULL_POINTER,
                                 ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                 ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                 ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                 ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                 ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                 ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                 ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                 ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                 ESMC_NULL_POINTER, &cal,
                                 ESMC_NULL_POINTER, &tz);

    // calculate the difference in days between the given date and
    //  the reference date
    ESMC_TimeInterval delta;
    delta = *this - referenceMonday;
    ESMF_KIND_I8 diffDays;
    // TODO: use native C++ Get() when ready
    delta.ESMC_TimeIntervalGet((ESMF_KIND_I4 *)ESMC_NULL_POINTER,
                               ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                               ESMC_NULL_POINTER, ESMC_NULL_POINTER, &diffDays);

    // calculate day of the week as simply modulo 7 from the reference date,
    //  adjusted for a 1-based count and negative deltas
    int mod7 = diffDays % 7;  // (-6 to 0) or (0 to 6)
    if (mod7 < 0) mod7 += 7;  // ensure positive (0 to 6) range
    *dayOfWeek = mod7 + 1;    // adjust to 1-based count (1 to 7)

    return(ESMF_SUCCESS);

 }  // end ESMC_TimeGetDayOfWeek

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeGetMidMonth - Get a Time's middle of the month value
//
// !INTERFACE:
      int ESMC_Time::ESMC_TimeGetMidMonth(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_Time *midMonth) const {    // out - time's middle of month value
//
// !DESCRIPTION:
//      Gets a {\tt Time}'s middle of the month value
//
//EOP
// !REQUIREMENTS:  

    // validate inputs
    if (midMonth == ESMC_NULL_POINTER) return (ESMF_FAILURE);
    if (this->calendar == ESMC_NULL_POINTER) return (ESMF_FAILURE);
    if (this->calendar->calendarType == ESMC_CAL_JULIANDAY ||
        this->calendar->calendarType == ESMC_CAL_NOCALENDAR) {
        return (ESMF_FAILURE);
    }

    // TODO: use table lookup per calendar type (14, 14.5, 15, 15.5 days) ?

    // TODO: use native C++ Get()/Set() when ready

    // get this date
    ESMF_KIND_I8 yy_i8;
    int mm, dd;
    ESMC_TimeGet((ESMF_KIND_I4 *)ESMC_NULL_POINTER, &yy_i8, &mm, &dd);

    // set start of this month
    dd = 1;
    ESMC_Time startOfMonth;
    ESMC_Calendar *cal = this->calendar; // allows reset
    int tz = this->timeZone;             //   after Set() clears it
    startOfMonth.ESMC_TimeSet((ESMF_KIND_I4 *)ESMC_NULL_POINTER,
                              &yy_i8, &mm, &dd, ESMC_NULL_POINTER,
                              ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                              ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                              ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                              ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                              ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                              ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                              ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                              ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                              ESMC_NULL_POINTER, &cal,
                              ESMC_NULL_POINTER, &tz);

    // set end of this month (start of next month)
    // TODO: use calendar interval logic when ready
    dd = 1;
    mm++;
    if (mm > MONTHS_PER_YEAR) {
      mm = 1;
      yy_i8++;
    }
    ESMC_Time endOfMonth;
    endOfMonth.ESMC_TimeSet((ESMF_KIND_I4 *)ESMC_NULL_POINTER,
                            &yy_i8, &mm, &dd, ESMC_NULL_POINTER,
                            ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                            ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                            ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                            ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                            ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                            ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                            ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                            ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                            ESMC_NULL_POINTER, &cal,
                            ESMC_NULL_POINTER, &tz);

    // size of this month
    ESMC_TimeInterval month;
    month = endOfMonth - startOfMonth;

    // calculate and return the middle of this month
    *midMonth = startOfMonth;  // initialize calendar & timezone
    *midMonth = startOfMonth + month/2;

    // TODO: add C++ infrastructure to enable the following expression:
    // *midMonth = startOfMonth + (endOfMonth - startOfMonth)/2;

    return(ESMF_SUCCESS);

 }  // end ESMC_TimeGetMidMonth

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeGetDayOfYear - Get a Time's day of the year value
//
// !INTERFACE:
      int ESMC_Time::ESMC_TimeGetDayOfYear(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMF_KIND_I4 *dayOfYear) const {    // out - time's day of year value
//
// !DESCRIPTION:
//      Gets a {\tt Time}'s day of the year value as a integer value.
//
//EOP
// !REQUIREMENTS:  

    // validate inputs
    if (dayOfYear == ESMC_NULL_POINTER) return (ESMF_FAILURE);
    if (this->calendar == ESMC_NULL_POINTER) return (ESMF_FAILURE);
    if (this->calendar->calendarType == ESMC_CAL_JULIANDAY ||
        this->calendar->calendarType == ESMC_CAL_NOCALENDAR) {
        return (ESMF_FAILURE);
    }
    
    // get day of year as time interval between now and 1/1/yy
    ESMC_TimeInterval yearDay;
    if (ESMC_TimeGetDayOfYear(&yearDay) == ESMF_FAILURE) return(ESMF_FAILURE);

    // get difference in integer days
    ESMF_KIND_I8 diffDays;
    // TODO: use native C++ Get, not F90 entry point
    yearDay.ESMC_TimeIntervalGet((ESMF_KIND_I4 *)ESMC_NULL_POINTER,
                                 ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                 ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                 &diffDays);

    // day-of-year is one-based count; i.e. day-of-year for 1/1/yy is 1
    *dayOfYear = (int) diffDays + 1;

    return(ESMF_SUCCESS);

 }  // end ESMC_TimeGetDayOfYear

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeGetDayOfYear - Get a Time's day of the year value
//
// !INTERFACE:
      int ESMC_Time::ESMC_TimeGetDayOfYear(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMF_KIND_R8 *dayOfYear) const {    // out - time's day of year value
//
// !DESCRIPTION:
//      Gets a {\tt Time}'s day of the year value as a floating point value.
//      Whole number part is days; fractional part is fraction of a day, equal
//      to seconds (whole + fractional) divided by 86400, the number of seconds
//      in a day.
//
//EOP
// !REQUIREMENTS:  

    // validate inputs
    if (dayOfYear == ESMC_NULL_POINTER) return (ESMF_FAILURE);
    if (this->calendar == ESMC_NULL_POINTER) return (ESMF_FAILURE);
    if (this->calendar->calendarType == ESMC_CAL_JULIANDAY ||
        this->calendar->calendarType == ESMC_CAL_NOCALENDAR) {
        return (ESMF_FAILURE);
    }

    // get day of year as time interval between now and 1/1/yy
    ESMC_TimeInterval yearDay;
    if (ESMC_TimeGetDayOfYear(&yearDay) != ESMF_SUCCESS) return(ESMF_FAILURE);

    // get difference in floating point days
    ESMF_KIND_R8 diffDays;
    // TODO: use native C++ Get, not F90 entry point
    yearDay.ESMC_TimeIntervalGet((ESMF_KIND_I4 *)ESMC_NULL_POINTER,
                                 ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                 ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                 ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                 ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                 ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                 ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                 &diffDays);

    // day-of-year is one-based count; i.e. day-of-year for 1/1/yy is 1
    *dayOfYear = diffDays + 1;

    return(ESMF_SUCCESS);

 }  // end ESMC_TimeGetDayOfYear

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeGetDayOfYear - Get a Time's day of the year value
//
// !INTERFACE:
      int ESMC_Time::ESMC_TimeGetDayOfYear(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_TimeInterval *dayOfYear) const {   // out - time's day of year value
//
// !DESCRIPTION:
//      Gets a {\tt Time}'s day of the year value as an {\tt ESMC\_TimeInterval}
//
//EOP
// !REQUIREMENTS:  

    // validate inputs
    if (dayOfYear == ESMC_NULL_POINTER) return (ESMF_FAILURE);
    if (this->calendar == ESMC_NULL_POINTER) return (ESMF_FAILURE);
    if (this->calendar->calendarType == ESMC_CAL_JULIANDAY ||
        this->calendar->calendarType == ESMC_CAL_NOCALENDAR) {
        return (ESMF_FAILURE);
    }

    // get year of our (this) time
    ESMF_KIND_I8 yy_i8;
    int mm, dd;
    // TODO: use native C++ Get, not F90 entry point
    ESMC_TimeGet((ESMF_KIND_I4 *)ESMC_NULL_POINTER, &yy_i8, &mm, &dd);

    // create time for 1/1/yy
    ESMC_Time dayOne;
    mm=1, dd=1;
    // TODO: use native C++ Set(), not F90 entry point
    ESMC_Calendar *cal = this->calendar; // allows reset
    int tz = this->timeZone;             //   after Set() clears it
    dayOne.ESMC_TimeSet((ESMF_KIND_I4 *)ESMC_NULL_POINTER, &yy_i8, &mm, &dd,
                        ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                        ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                        ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                        ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                        ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                        ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                        ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                        ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                        ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                        &cal, ESMC_NULL_POINTER, &tz);

    // calculate difference between 1/1/yy and our (this) time
    *dayOfYear = *this - dayOne;
    
    return(ESMF_SUCCESS);

 }  // end ESMC_TimeGetDayOfYear
