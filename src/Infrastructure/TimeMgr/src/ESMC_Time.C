// $Id: ESMC_Time.C,v 1.39 2003/07/25 05:17:06 eschwab Exp $
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
 static const char *const version = "$Id: ESMC_Time.C,v 1.39 2003/07/25 05:17:06 eschwab Exp $";
//-------------------------------------------------------------------------

//
//-------------------------------------------------------------------------
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
// !IROUTINE:  ESMC_TimeGet - Get a Time value
//
// !INTERFACE:
      int ESMC_Time::ESMC_TimeGet(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *TimeList,    // in  - time value specifier string
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
      ESMC_Calendar *cal,      // in - associated calendar
      int tz,                  // in - timezone
      const char *timeList,    // in - initializer specifier string
      ...) {                   // in - specifier values (variable args)
//
// !DESCRIPTION:
//      Initialzes a {\tt Time} with values given in variable arg list.
//      Supports native C++ use.
//
//EOP
// !REQUIREMENTS:  

    // set calendar type
    if (cal != ESMC_NULL_POINTER) {
      Calendar = cal;
    }

    // set timezone
    Timezone = tz;

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
// !IROUTINE:  ESMC_TimeGet - Get a Time value; supports F90 interface
//
// !INTERFACE:
      int ESMC_Time::ESMC_TimeGet(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int *YR,                  // out - integer year (>= 32-bit)
      ESMF_IKIND_I8 *YRl,       // out - integer year (large, >= 64-bit)
      int *MM,                  // out - integer month
      int *DD,                  // out - integer day of the month
      int *D,                   // out - integer days (>= 32-bit)
      ESMF_IKIND_I8 *Dl,        // out - integer days (large, >= 64-bit)
      int *H,                   // out - integer hours
      int *M,                   // out - integer minutes
      int *S,                   // out - integer seconds (>= 32-bit)
      ESMF_IKIND_I8 *Sl,        // out - integer seconds (large, >= 64-bit)
      int *MS,                  // out - integer milliseconds
      int *US,                  // out - integer microseconds
      int *NS,                  // out - integer nanoseconds
      double *d_,               // out - floating point days
      double *h_,               // out - floating point hours
      double *m_,               // out - floating point minutes
      double *s_,               // out - floating point seconds
      double *ms_,              // out - floating point milliseconds
      double *us_,              // out - floating point microseconds
      double *ns_,              // out - floating point nanoseconds
      int *Sn,                  // out - fractional seconds numerator
      int *Sd) const {          // out - fractional seconds denominator
//
// !DESCRIPTION:
//      Gets a {\tt Time}'s values in user-specified format. This version
//      supports the F90 interface.
//
//EOP
// !REQUIREMENTS:  

    // TODO: fractional, sub-seconds

    // convert base time to date according to calendar type
    if (YR != ESMC_NULL_POINTER || YRl != ESMC_NULL_POINTER ||
        MM != ESMC_NULL_POINTER || DD  != ESMC_NULL_POINTER ||
        D  != ESMC_NULL_POINTER || Dl  != ESMC_NULL_POINTER ||
        d_ != ESMC_NULL_POINTER) {
      if (Calendar != ESMC_NULL_POINTER) {
        if (Calendar->ESMC_CalendarConvertToDate(this, YR, YRl, MM, DD,
                                                 D, Dl, d_) ==
            ESMF_FAILURE) return(ESMF_FAILURE);
      }
      else {
        return (ESMF_FAILURE);
      }
    }

    // get number of seconds in a day
    int secPerDay = SECONDS_PER_DAY;  // default
    if (Calendar != ESMC_NULL_POINTER) {
      secPerDay = Calendar->SecondsPerDay;
    }

    // use base class to get sub-day values
    return(ESMC_BaseTimeGet(secPerDay, H, M, S, Sl, MS, US, NS,
                            h_, m_, s_, ms_, us_, ns_, Sn, Sd));

 }  // end ESMC_TimeGet

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
      int *YR,                  // in - integer year (>= 32-bit)
      ESMF_IKIND_I8 *YRl,       // in - integer year (large, >= 64-bit)
      int *MM,                  // in - integer month
      int *DD,                  // in - integer day of the month
      int *D,                   // in - integer days (>= 32-bit)
      ESMF_IKIND_I8 *Dl,        // in - integer days (large, >= 64-bit)
      int *H,                   // in - integer hours
      int *M,                   // in - integer minutes
      int *S,                   // in - integer seconds (>= 32-bit)
      ESMF_IKIND_I8 *Sl,        // in - integer seconds (large, >= 64-bit)
      int *MS,                  // in - integer milliseconds
      int *US,                  // in - integer microseconds
      int *NS,                  // in - integer nanoseconds
      double *d_,               // in - floating point days
      double *h_,               // in - floating point hours
      double *m_,               // in - floating point minutes
      double *s_,               // in - floating point seconds
      double *ms_,              // in - floating point milliseconds
      double *us_,              // in - floating point microseconds
      double *ns_,              // in - floating point nanoseconds
      int *Sn,                  // in - fractional seconds numerator
      int *Sd,                  // in - fractional seconds denominator
      ESMC_Calendar *cal,       // in - associated calendar
      int *tz) {                // in - timezone (hours offset from GMT,
                                //      e.g. EST = -5)
//
// !DESCRIPTION:
//      Initialzes a {\tt Time} with values given in arg list. Supports
//      F90 interface.
//
//EOP
// !REQUIREMENTS:  

    // TODO: ensure initialization if called via F90 interface;
    //       cannot call constructor, because destructor is subsequently
    //       called automatically, returning initialized values to garbage.
    this->S  = 0;
    this->Sn = 0;
    this->Sd = 1;
    
    // TODO: validate inputs (individual and combos), set basetime values
    //       e.g. integer and float specifiers are mutually exclusive

    // TODO: fractional, sub-seconds

    // set calendar type
    if (cal != ESMC_NULL_POINTER) {
      Calendar = cal;
    }

    // set timezone
    Timezone = (tz != ESMC_NULL_POINTER) ? *tz : ESMC_NULL_POINTER;

    // TODO: Timezone adjust

    // convert date to base time according to calendar type
    // TODO: fractional, sub-seconds
    // TODO: create two calendar conversion method entry points ?
    if ((YR != ESMC_NULL_POINTER || YRl != ESMC_NULL_POINTER) &&
         MM !=ESMC_NULL_POINTER && DD != ESMC_NULL_POINTER) {
      if (Calendar != ESMC_NULL_POINTER) {
        ESMF_IKIND_I8 argYR = (YR != ESMC_NULL_POINTER) ? *YR : *YRl;
        if (Calendar->ESMC_CalendarConvertToTime(argYR, *MM, *DD, 0, this) ==
            ESMF_FAILURE) return (ESMF_FAILURE);
      } else return (ESMF_FAILURE);
    }
    if (D != ESMC_NULL_POINTER || Dl != ESMC_NULL_POINTER) {
      if (Calendar != ESMC_NULL_POINTER) {
        ESMF_IKIND_I8 argD = (D != ESMC_NULL_POINTER) ? *D : *Dl;
        if (Calendar->ESMC_CalendarConvertToTime(0, 0, 0, argD, this) ==
            ESMF_FAILURE) return (ESMF_FAILURE);
      } else return (ESMF_FAILURE);
    }
    if (d_ != ESMC_NULL_POINTER) {
      // integer part
      if (Calendar != ESMC_NULL_POINTER) {
        if (Calendar->ESMC_CalendarConvertToTime(0, 0, 0, (int) *d_, this) ==
            ESMF_FAILURE) return (ESMF_FAILURE);
      } else return (ESMF_FAILURE);

      // get number of seconds in a day
      int secPerDay = SECONDS_PER_DAY;  // default
      if (Calendar != ESMC_NULL_POINTER) {
        secPerDay = Calendar->SecondsPerDay;
      }

      // fractional part
      this->S +=
              (ESMF_IKIND_I8) (modf(*d_, ESMC_NULL_POINTER) * secPerDay);
    }
    
    // use base class for sub-day values
    ESMC_BaseTimeSet(H, M, S, Sl, MS, US, NS, h_, m_, s_,
                     ms_, us_, ns_, Sd, Sn);

    return (ESMC_TimeValidate());

 }  // end ESMC_TimeSet

#if 0
//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeSet - direct core value initializer
//
// !INTERFACE:
      int ESMC_Time::ESMC_TimeSet(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMF_IKIND_I8 S,      // in - integer seconds
      int Sn,               // in - fractional seconds, numerator
      int Sd,               // in - fractional seconds, denominator
      ESMC_Calendar *Cal,   // in - associated calendar
      int Tz) {             // in - timezone
//
// !DESCRIPTION:
//      Initialzes a {\tt Time} with given values
//
//EOP
// !REQUIREMENTS:  

    // use base class Set()
    if (ESMC_BaseTime::ESMC_BaseTimeSet(S, Sn, Sd) == ESMF_SUCCESS)
    {
        this->Calendar = Cal;
        this->Timezone = Tz;

        return(ESMF_SUCCESS);
    }
    else return(ESMF_FAILURE);

 }  // end ESMC_TimeSet
#endif

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeGetCalendar - Get a Time's associated calendar
//
// !INTERFACE:
      int ESMC_Time::ESMC_TimeGetCalendar(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_Calendar **calendar) const {    // out - Time's calendar
//
// !DESCRIPTION:
//      Gets a {\tt Time}'s associated calendar (pointer)
//
//EOP
// !REQUIREMENTS:  

    if (calendar != ESMC_NULL_POINTER) {
      *calendar = Calendar;  // return calendar pointer
      return(ESMF_SUCCESS);
    } else return(ESMF_FAILURE);

 }  // end ESMC_TimeGetCalendar

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeGetCalendar - Get a Time's associated calendar
//
// !INTERFACE:
      int ESMC_Time::ESMC_TimeGetCalendar(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_Calendar *calendar) const {    // out - Time's calendar
//
// !DESCRIPTION:
//      Gets a {\tt Time}'s associated calendar (copy)
//
//EOP
// !REQUIREMENTS:  

    if (calendar != ESMC_NULL_POINTER) {
      *calendar = *Calendar;  // return calendar copy
      return(ESMF_SUCCESS);
    } else return(ESMF_FAILURE);

 }  // end ESMC_TimeGetCalendar

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeSetCalendar - Set a Time's associated calendar
//
// !INTERFACE:
      int ESMC_Time::ESMC_TimeSetCalendar(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_Calendar *calendar) {    // in - Time's calendar
//
// !DESCRIPTION:
//      Sets a {\tt Time}'s associated calendar
//
//EOP
// !REQUIREMENTS:  

    if (calendar != ESMC_NULL_POINTER) {
      Calendar = calendar;
      return(ESMF_SUCCESS);
    }
    else {
      return(ESMF_FAILURE);
    }

 }  // end ESMC_TimeSetCalendar

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeIsSameCal - Compares 2 ESMC_Time's Calendar types
//
// !INTERFACE:
      bool ESMC_Time::ESMC_TimeIsSameCal(
//
// !RETURN VALUE:
//    bool true same calendars, false different calendars
//
// !ARGUMENTS:
      ESMC_Time *Time,    // in - Time to compare Calendar types against
      int *rc) const {    // out - return code
//
// !DESCRIPTION:
//      Compares given {\tt Time}'s {\tt Calendar} type with this {\tt Time}'s
//      {\tt Calendar} type
//
//EOP
// !REQUIREMENTS:  

    if (Calendar != ESMC_NULL_POINTER && Time->Calendar != ESMC_NULL_POINTER)
    {
      *rc = ESMF_SUCCESS;
      return(this->Calendar->Type == Time->Calendar->Type);
    }
    else {
      *rc = ESMF_FAILURE;
      return(false);
    }

 }  // end ESMC_TimeIsSameCal

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeGetTimeZone - Get a Time's associated timezone
//
// !INTERFACE:
      int ESMC_Time::ESMC_TimeGetTimeZone(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int *timezone) const {    // out - Time's timezone
//
// !DESCRIPTION:
//      Gets a {\tt Time}'s associated timezone
//
//EOP
// !REQUIREMENTS:  

    if (timezone != ESMC_NULL_POINTER) {
      *timezone = Timezone;
      return(ESMF_SUCCESS);
    }
    else {
      return(ESMF_FAILURE);
    }

 }  // end ESMC_TimeGetTimeZone

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeSetTimeZone - Set a Time's associated timezone
//
// !INTERFACE:
      int ESMC_Time::ESMC_TimeSetTimeZone(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int timezone) {    // in - Time's timezone
//
// !DESCRIPTION:
//      Sets a {\tt Time}'s associated timezone
//
//EOP
// !REQUIREMENTS:  

    Timezone = timezone;
    return(ESMF_SUCCESS);

 }  // end ESMC_TimeSetTimeZone

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
    if (Calendar == ESMC_NULL_POINTER) return (ESMF_FAILURE);
    if (Calendar->Type == ESMC_CAL_JULIAN ||
        Calendar->Type == ESMC_CAL_NOCALENDAR) return (ESMF_FAILURE);

    ESMF_IKIND_I8 YRl, Sl;
    int MM, DD, H, M; 
    // TODO: use native C++ Get, not F90 entry point, when ready
    ESMC_TimeGet(ESMC_NULL_POINTER, &YRl, &MM, &DD, ESMC_NULL_POINTER,
                 ESMC_NULL_POINTER, &H, &M, ESMC_NULL_POINTER, &Sl);

    // ISO 8601 format YYYY-MM-DDThh:mm:ss
    sprintf(timeString, "%lld-%02d-%02dT%02d:%02d:%02lld\0",
            YRl, MM, DD, H, M, Sl);

    return(ESMF_SUCCESS);

 }  // end ESMC_TimeGetString

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
      double *dayOfYear) const {    // out - time's day of year value
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
    if (Calendar == ESMC_NULL_POINTER) return (ESMF_FAILURE);
    if (Calendar->Type == ESMC_CAL_JULIAN ||
        Calendar->Type == ESMC_CAL_NOCALENDAR) return (ESMF_FAILURE);

    // get day of year as time interval between now and 1/1/YR
    ESMC_TimeInterval yearDay;
    if (ESMC_TimeGetDayOfYear(&yearDay) == ESMF_FAILURE) return(ESMF_FAILURE);

    // get difference in floating point days
    double diffDays;
    // TODO: use native C++ Get, not F90 entry point
    yearDay.ESMC_TimeIntervalGet(ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                 ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                 ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                 ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                 ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                 ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                 ESMC_NULL_POINTER, &diffDays);

    // day-of-year is one-based count; i.e. day-of-year for 1/1/YR is 1
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
      int *dayOfYear) const {    // out - time's day of year value
//
// !DESCRIPTION:
//      Gets a {\tt Time}'s day of the year value as a integer value.
//
//EOP
// !REQUIREMENTS:  

    // validate inputs
    if (dayOfYear == ESMC_NULL_POINTER) return (ESMF_FAILURE);
    if (Calendar == ESMC_NULL_POINTER) return (ESMF_FAILURE);
    if (Calendar->Type == ESMC_CAL_JULIAN ||
        Calendar->Type == ESMC_CAL_NOCALENDAR) return (ESMF_FAILURE);
    
    // get day of year as time interval between now and 1/1/YR
    ESMC_TimeInterval yearDay;
    if (ESMC_TimeGetDayOfYear(&yearDay) == ESMF_FAILURE) return(ESMF_FAILURE);

    // get difference in integer days
    ESMF_IKIND_I8 diffDays;
    // TODO: use native C++ Get, not F90 entry point
    yearDay.ESMC_TimeIntervalGet(ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                 ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                 ESMC_NULL_POINTER, &diffDays);

    // day-of-year is one-based count; i.e. day-of-year for 1/1/YR is 1
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
      ESMC_TimeInterval *dayOfYear) const {   // out - time's day of year value
//
// !DESCRIPTION:
//      Gets a {\tt Time}'s day of the year value as an {\tt ESMC\_TimeInterval}
//
//EOP
// !REQUIREMENTS:  

    // validate inputs
    if (dayOfYear == ESMC_NULL_POINTER) return (ESMF_FAILURE);
    if (Calendar == ESMC_NULL_POINTER) return (ESMF_FAILURE);
    if (Calendar->Type == ESMC_CAL_JULIAN ||
        Calendar->Type == ESMC_CAL_NOCALENDAR) return (ESMF_FAILURE);

    // get year of our (this) time
    ESMF_IKIND_I8 YRl;
    int MM, DD;
    // TODO: use native C++ Get, not F90 entry point
    ESMC_TimeGet(ESMC_NULL_POINTER, &YRl, &MM, &DD);

    // create time for 1/1/YR
    ESMC_Time dayOne = *this;  // initialize calendar & timezone
    MM=1, DD=1;
    // TODO: use native C++ Set(), not F90 entry point
    dayOne.ESMC_TimeSet(ESMC_NULL_POINTER, &YRl, &MM, &DD);

    // calculate difference between 1/1/YR and our (this) time
    *dayOfYear = *this - dayOne;
    
    return(ESMF_SUCCESS);

 }  // end ESMC_TimeGetDayOfYear

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
    if (Calendar == ESMC_NULL_POINTER) return (ESMF_FAILURE);

    // date variables
    ESMF_IKIND_I8 YRl;
    int MM, DD;

    //  The day of the week is simply modulo 7 from a reference date,
    //  adjusted for a 1-based count and negative deltas.
    //  The reference date is any known Monday (day of the week = 1)
    //  This method is valid for any calendar which uses 7-day weeks.

    switch (Calendar->Type)
    {
        case ESMC_CAL_GREGORIAN:
        case ESMC_CAL_NOLEAP:    // TODO: ?
        case ESMC_CAL_360DAY:    // TODO: ?
          //  Can be any Monday after the Gregorian reformation of 9/14/1752
          YRl=1796; MM=7; DD=4;   // America's 20th birthday was a Monday !
          break;

        case ESMC_CAL_JULIAN:
          //  Can be any Monday before the Gregorian reformation of 9/2/1752
          YRl=1492; MM=10; DD=29;  // Columbus landed in Cuba on a Monday !
          break;

        case ESMC_CAL_NOCALENDAR:
        case ESMC_CAL_GENERIC:
        default:
          return(ESMF_FAILURE);
          break;
    }

    // TODO: put the above reference dates into a pre-initialized lookup table
    //       to skip this step
    ESMC_Time referenceMonday = *this; // initialize calendar & timezone
    // TODO: use native C++ Set() when ready
    referenceMonday.ESMC_TimeSet(ESMC_NULL_POINTER, &YRl, &MM, &DD);

    // calculate the difference in days between the given date and
    //  the reference date
    ESMC_TimeInterval delta;
    delta = *this - referenceMonday;
    ESMF_IKIND_I8 diffDays;
    // TODO: use native C++ Get() when ready
    delta.ESMC_TimeIntervalGet(ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                               ESMC_NULL_POINTER, ESMC_NULL_POINTER, 
                               ESMC_NULL_POINTER, &diffDays);

    // calculate day of the week as simply modulo 7 from the reference date,
    //  adjusted for a 1-based count and negative deltas
    int mod7 = diffDays % 7;  // (-6 to 0) or (0 to 6)
    if (mod7 < 0) mod7 += 7;  // ensure positive (0 to 6) range
    *dayOfWeek = mod7 + 1;    // adjust to 1-based count (1 to 7)

    return(ESMF_SUCCESS);

 }  // end ESMC_TimeGetDayOfWeek

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeGetDayOfMonth - Get a Time's day of the month value
//
// !INTERFACE:
      int ESMC_Time::ESMC_TimeGetDayOfMonth(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int *dayOfMonth) const {    // out - time's day of month value
//
// !DESCRIPTION:
//      Gets a {\tt Time}'s day of the month value
//
//EOP
// !REQUIREMENTS:  

    // validate inputs
    if (dayOfMonth == ESMC_NULL_POINTER) return (ESMF_FAILURE);
    if (Calendar == ESMC_NULL_POINTER) return (ESMF_FAILURE);
    if (Calendar->Type == ESMC_CAL_JULIAN ||
        Calendar->Type == ESMC_CAL_NOCALENDAR) return (ESMF_FAILURE);

    // TODO: use native C++ Get() when ready
    ESMC_TimeGet(ESMC_NULL_POINTER, ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                 dayOfMonth);
 
    return(ESMF_SUCCESS);

 }  // end ESMC_TimeGetDayOfMonth

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
    if (Calendar == ESMC_NULL_POINTER) return (ESMF_FAILURE);
    if (Calendar->Type == ESMC_CAL_JULIAN ||
        Calendar->Type == ESMC_CAL_NOCALENDAR) return (ESMF_FAILURE);

    // TODO: use table lookup per calendar type (14, 14.5, 15, 15.5 days) ?

    // TODO: use native C++ Get()/Set() when ready

    // get this date
    ESMF_IKIND_I8 YRl;
    int MM, DD;
    ESMC_TimeGet(ESMC_NULL_POINTER, &YRl, &MM, &DD);

    // set start of this month
    DD = 1;
    ESMC_Time startOfMonth = *this;  // initialize calendar & timezone
    startOfMonth.ESMC_TimeSet(ESMC_NULL_POINTER, &YRl, &MM, &DD);

    // set end of this month (start of next month)
    // TODO: use calendar interval logic when ready
    DD = 1;
    MM++;
    if (MM > MONTHS_PER_YEAR) {
      MM = 1;
      YRl++;
    }
    ESMC_Time endOfMonth = *this;  // initialize calendar & timezone
    endOfMonth.ESMC_TimeSet(ESMC_NULL_POINTER, &YRl, &MM, &DD);

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
// !IROUTINE:  ESMC_TimeGetRealTime - Sync this Time to wall clock time
//
// !INTERFACE:
      int ESMC_Time::ESMC_TimeGetRealTime(void) {
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
    if (Calendar == ESMC_NULL_POINTER) return (ESMF_FAILURE);
    if (Calendar->Type == ESMC_CAL_JULIAN ||
        Calendar->Type == ESMC_CAL_NOCALENDAR) return (ESMF_FAILURE);

    time_t tm;
    struct tm wallClock;

    // get wall clock (system) time
    // TODO:  use POSIX real-time function to get nanosecond resolution
    if (time(&tm) < 0) return (ESMF_FAILURE);
    wallClock = *localtime(&tm);          
    ESMF_IKIND_I8 YRl = wallClock.tm_year + 1900;
    int           MM  = wallClock.tm_mon + 1;
    int           DD  = wallClock.tm_mday;
    int           H   = wallClock.tm_hour;
    int           M   = wallClock.tm_min;
    ESMF_IKIND_I8 Sl  = wallClock.tm_sec;

    // set this time to wall clock time
    // TODO: use native C++ version when ready
    ESMC_TimeSet(ESMC_NULL_POINTER, &YRl, &MM, &DD, ESMC_NULL_POINTER,
                 ESMC_NULL_POINTER, &H, &M, ESMC_NULL_POINTER, &Sl);

    return(ESMF_SUCCESS);

 }  // end ESMC_TimeGetRealTime

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Time(=) - copy or assign from BaseTime expression
//
// !INTERFACE:
      ESMC_Time& ESMC_Time::operator=(
//
// !RETURN VALUE:
//    ESMC_Time& result
//
// !ARGUMENTS:
      const ESMC_BaseTime &baseTime) {   // in - ESMC_BaseTime to copy
//
// !DESCRIPTION:
//    Assign {\tt BaseTime} expression to this time.  Supports inherited
//    operators from {\tt ESMC\_BaseTime}
//
//EOP
// !REQUIREMENTS:  

    // invoke base class assignment operator
    // TODO:  should be implicit ?
    ESMC_BaseTime::operator=(baseTime);

    return(*this);

}  // end ESMC_Time::operator=

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeRead - restore Time state
//
// !INTERFACE:
      int ESMC_Time::ESMC_TimeRead(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMF_IKIND_I8 S,    // in - integer seconds
      int Sn,             // in - fractional seconds, numerator
      int Sd,             // in - fractional seconds, denominator
      ESMC_Calendar *cal, // in - associated calendar
      int timezone) {     // in - associated timezone
//
// !DESCRIPTION:
//      restore {\tt Time} state for persistence/checkpointing
//
//EOP
// !REQUIREMENTS:  

    int rc;

    if (cal == ESMC_NULL_POINTER) {
      cout << "ESMC_Time::ESMC_TimeRead(): null pointer passed in" << endl;
      return(ESMF_FAILURE);
    }

    // use base class Read() first
    rc = ESMC_BaseTime::ESMC_BaseTimeRead(S, Sn, Sd);

    Calendar = cal;  // TODO?: this only restores calendar pointer; component
                     // must be sure to restore corresponding calendar first
    Timezone = timezone;
  
    return(rc);

 }  // end ESMC_TimeRead

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeWrite - return Time state
//
// !INTERFACE:
      int ESMC_Time::ESMC_TimeWrite(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMF_IKIND_I8 *S,          // out - integer seconds
      int *Sn,                   // out - fractional seconds, numerator
      int *Sd,                   // out - fractional seconds, denominator
      ESMC_Calendar *cal,        // out - associated calendar
      int *timezone) const {     // out - associated timezone
//
// !DESCRIPTION:
//      return {\tt Time} state for persistence/checkpointing
//
//EOP
// !REQUIREMENTS:  

    int rc;

    if (S  == ESMC_NULL_POINTER || Sn  == ESMC_NULL_POINTER ||
        Sd == ESMC_NULL_POINTER || cal == ESMC_NULL_POINTER ||
        timezone == ESMC_NULL_POINTER) {
      cout << "ESMC_Time::ESMC_TimeWrite(): null pointer(s) passed in" << endl;
      return(ESMF_FAILURE);
    }

    // use base class Write() first
    rc = ESMC_BaseTime::ESMC_BaseTimeWrite(S, Sn, Sd);

    cal = Calendar;  // TODO?: this only saves calendar pointer; component
                     // must be sure to save corresponding calendar
    *timezone = Timezone;
  
    return(rc);

 }  // end ESMC_TimeWrite

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

    if (ESMC_BaseTime::ESMC_BaseTimeValidate() != ESMF_SUCCESS)
      return(ESMF_FAILURE);

    if (Calendar == ESMC_NULL_POINTER) return(ESMF_FAILURE);
    if (Calendar->ESMC_CalendarValidate() != ESMF_SUCCESS) return(ESMF_FAILURE);

    // earliest Gregorian date representable by the Fliegel algorithm
    //  is -4800/3/1 == -32044 Julian days == -2,768,601,600 core seconds
    if (Calendar->Type == ESMC_CAL_GREGORIAN && S < -2768601600LL)
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
      if (Calendar != ESMC_NULL_POINTER) {
        Calendar->ESMC_CalendarPrint(options);
      }
      cout << "Timezone = " << Timezone << endl;
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
   S  = 0;
   Sn = 0;
   Sd = 0;
   Calendar = ESMC_NULL_POINTER;
   Timezone = 0;

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
      ESMF_IKIND_I8 S,     // in - integer seconds
      int Sn,              // in - fractional seconds, numerator
      int Sd,              // in - fractional seconds, denominator
      ESMC_Calendar *Cal,  // in - associated calendar
      int Tz) :            // in - timezone
//
// !DESCRIPTION:
//      Initializes a {\tt ESMC\_Time}
//
//EOP
// !REQUIREMENTS:  

   ESMC_BaseTime(S, Sn, Sd) {   // use base class constructor
   Calendar = Cal;
   Timezone = Tz;

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
