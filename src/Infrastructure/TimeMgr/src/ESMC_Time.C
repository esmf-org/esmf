// $Id: ESMC_Time.C,v 1.29 2003/04/29 23:05:26 eschwab Exp $
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
 #include <ESMC_TimeInterval.h>

 // associated class definition file
 #include <ESMC_Time.h>

//-------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_Time.C,v 1.29 2003/04/29 23:05:26 eschwab Exp $";
//-------------------------------------------------------------------------

//
//-------------------------------------------------------------------------
//-------------------------------------------------------------------------
//
// This section includes all the ESMC_Time routines
//
//

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeInit - initializer for native C++ use
//
// !INTERFACE:
      int ESMC_Time::ESMC_TimeInit(
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

    // TODO
    return(ESMF_SUCCESS);

 }  // end ESMC_TimeInit

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeInit - initializer to support F90 interface
//
// !INTERFACE:
      int ESMC_Time::ESMC_TimeInit(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMF_IKIND_I8 *YR,        // in - integer year
      int *MM,                  // in - integer month
      int *DD,                  // in - integer day of the month
      ESMF_IKIND_I8 *D,         // in - integer days
      int *H,                   // in - integer hours
      int *M,                   // in - integer minutes
      ESMF_IKIND_I8 *S,         // in - integer seconds 
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
    else {
      Calendar = ESMC_NULL_POINTER;
      // TODO: log err
      cout << "ESMC_Time::ESMC_TimeInit(): calendar not set" << endl;
      return (ESMF_FAILURE);
    }

    // set timezone
    Timezone = (tz != ESMC_NULL_POINTER) ? *tz : ESMC_NULL_POINTER;

    return(ESMC_TimeSet(YR, MM, DD, D, H, M, S, MS, US, NS, d_, h_, m_, s_,
                        ms_, us_, ns_, Sn, Sd));

 }  // end ESMC_TimeInit

#if 0
//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeInit - shallow class initializer 1
//
// !INTERFACE:
      int ESMC_Time::ESMC_TimeInit(
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

    // use base class Init()
    if (ESMC_BaseTime::ESMC_BaseTimeInit(S, Sn, Sd) == ESMF_SUCCESS)
    {
        this->Calendar = Cal;
        this->Timezone = Tz;

        return(ESMF_SUCCESS);
    }
    else return(ESMF_FAILURE);

 }  // end ESMC_TimeInit
#endif

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
// !IROUTINE:  ESMC_TimeSet - Set a Time value
//
// !INTERFACE:
      int ESMC_Time::ESMC_TimeSet(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *TimeList,    // in - time value specifier string
      ...) {                   // in - specifier values (variable args)
//
// !DESCRIPTION:
//      Sets a {\tt Time}'s values in user-specified values. This version
//      supports native C++ use.
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
      ESMF_IKIND_I8 *YR,        // out - integer year
      int *MM,                  // out - integer month
      int *DD,                  // out - integer day of the month
      ESMF_IKIND_I8 *D,         // out - integer days
      int *H,                   // out - integer hours
      int *M,                   // out - integer minutes
      ESMF_IKIND_I8 *S,         // out - integer seconds 
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
    if (YR != ESMC_NULL_POINTER || MM != ESMC_NULL_POINTER ||
        DD != ESMC_NULL_POINTER || D  != ESMC_NULL_POINTER ||
        d_ != ESMC_NULL_POINTER) {
      if (Calendar != ESMC_NULL_POINTER) {
        if (Calendar->ESMC_CalendarConvertToDate(this, YR, MM, DD, D, d_) ==
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
    return(ESMC_BaseTimeGet(secPerDay, H, M, S, MS, US, NS,
                            h_, m_, s_, ms_, us_, ns_, Sn, Sd));

 }  // end ESMC_TimeGet

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeSet - Set a Time value; supports F90 interface
//
// !INTERFACE:
      int ESMC_Time::ESMC_TimeSet(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMF_IKIND_I8 *YR,        // in - integer year
      int *MM,                  // in - integer month
      int *DD,                  // in - integer day of the month
      ESMF_IKIND_I8 *D,         // in - integer days
      int *H,                   // in - integer hours
      int *M,                   // in - integer minutes
      ESMF_IKIND_I8 *S,         // in - integer seconds 
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
      int *Sd) {                // in - fractional seconds denominator
//
// !DESCRIPTION:
//      Sets a {\tt Time}'s values in user-specified format. This version
//      supports the F90 interface.
//
//EOP
// !REQUIREMENTS:  

    // TODO: validate inputs (individual and combos), set basetime values
    //       e.g. integer and float specifiers are mutually exclusive

    // TODO: Timezone adjust

    // convert date to base time according to calendar type
    // TODO: fractional, sub-seconds
    // TODO: create two calendar conversion method entry points ?
    if (YR != ESMC_NULL_POINTER && MM !=ESMC_NULL_POINTER &&
        DD != ESMC_NULL_POINTER) {
      if (Calendar != ESMC_NULL_POINTER) {
        if (Calendar->ESMC_CalendarConvertToTime(*YR, *MM, *DD, 0, this) ==
            ESMF_FAILURE) return (ESMF_FAILURE);
      } else return (ESMF_FAILURE);
    }
    if (D != ESMC_NULL_POINTER) {
      if (Calendar != ESMC_NULL_POINTER) {
        if (Calendar->ESMC_CalendarConvertToTime(0, 0, 0, *D, this) ==
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
    return(ESMC_BaseTimeSet(H, M, S, MS, US, NS,
                            h_, m_, s_, ms_, us_, ns_, Sd, Sn));

 }  // end ESMC_TimeSet

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
//      Gets a {\tt Time}'s value in character format
//
//EOP
// !REQUIREMENTS:  

    // validate inputs
    if (timeString == ESMC_NULL_POINTER) return (ESMF_FAILURE);
    if (Calendar == ESMC_NULL_POINTER) return (ESMF_FAILURE);
    if (Calendar->Type == ESMC_CAL_JULIAN ||
        Calendar->Type == ESMC_CAL_NOCALENDAR) return (ESMF_FAILURE);

    ESMF_IKIND_I8 YR, S;
    int MM, DD, H, M; 
    // TODO: use native C++ Get, not F90 entry point, when ready
    ESMC_TimeGet(&YR, &MM, &DD, ESMC_NULL_POINTER, &H, &M, &S,
                 ESMC_NULL_POINTER, ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                 ESMC_NULL_POINTER, ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                 ESMC_NULL_POINTER, ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                 ESMC_NULL_POINTER, ESMC_NULL_POINTER, ESMC_NULL_POINTER);

    // ISO 8601 format CCYY-MM-DD HH:MM:SS
    sprintf(timeString, "%lld-%02d-%02d %02d:%02d:%02lld\0",
            YR, MM, DD, H, M, S);

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

    // get year of our (this) time
    ESMF_IKIND_I8 YR;
    int MM, DD;
    // TODO: use native C++ Get, not F90 entry point
    ESMC_TimeGet(&YR, &MM, &DD, ESMC_NULL_POINTER,
                 ESMC_NULL_POINTER, ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                 ESMC_NULL_POINTER, ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                 ESMC_NULL_POINTER, ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                 ESMC_NULL_POINTER, ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                 ESMC_NULL_POINTER, ESMC_NULL_POINTER, ESMC_NULL_POINTER);

    // create time for 1/1/YR
    ESMC_Time dayOne;
    MM=1, DD=1;
    // TODO: use native C++ Init, not F90 entry point
    dayOne.ESMC_TimeInit(&YR, &MM, &DD,
                         ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                         ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                         ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                         ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                         ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                         ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                         ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                         ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                         this->Calendar, (int*) &this->Timezone);

    // calculate difference between 1/1/YR and our (this) time
    ESMC_TimeInterval dayofYear;
    dayofYear = *this - dayOne;
    // TODO: add C++ infrastructure to enable the following expression:
    // ESMC_TimeInterval dayofYear = *this - dayOne;
    
    // get difference in floating point days
    double diffDays;
    // TODO: use native C++ Get, not F90 entry point
    dayofYear.ESMC_TimeIntervalGet(ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                   ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                   ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                   ESMC_NULL_POINTER, ESMC_NULL_POINTER,

                                   ESMC_NULL_POINTER, &diffDays,

                                   ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                   ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                   ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                   ESMC_NULL_POINTER, ESMC_NULL_POINTER);

    // day-of-year is one-based count; i.e. day-of-year for 1/1/YR is 1
    *dayOfYear = diffDays + 1;

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
//      Gets a {\tt Time}'s day of the week value
//
//EOP
// !REQUIREMENTS:  

    // validate inputs
    if (dayOfWeek == ESMC_NULL_POINTER) return (ESMF_FAILURE);
    if (Calendar == ESMC_NULL_POINTER) return (ESMF_FAILURE);
    if (Calendar->Type == ESMC_CAL_JULIAN ||
        Calendar->Type == ESMC_CAL_NOCALENDAR) return (ESMF_FAILURE);

    // TODO
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
    ESMC_TimeGet(ESMC_NULL_POINTER, ESMC_NULL_POINTER, dayOfMonth, 
                 ESMC_NULL_POINTER, ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                 ESMC_NULL_POINTER, ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                 ESMC_NULL_POINTER, ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                 ESMC_NULL_POINTER, ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                 ESMC_NULL_POINTER, ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                 ESMC_NULL_POINTER);
 
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
    ESMF_IKIND_I8 YR;
    int MM, DD;
    ESMC_TimeGet(&YR, &MM, &DD, 
                 ESMC_NULL_POINTER, ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                 ESMC_NULL_POINTER, ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                 ESMC_NULL_POINTER, ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                 ESMC_NULL_POINTER, ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                 ESMC_NULL_POINTER, ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                 ESMC_NULL_POINTER);

    // set start of this month
    DD = 1;
    ESMC_Time startOfMonth = *this;  // copy calendar and timezone
    startOfMonth.ESMC_TimeSet(&YR, &MM, &DD, 
                 ESMC_NULL_POINTER, ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                 ESMC_NULL_POINTER, ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                 ESMC_NULL_POINTER, ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                 ESMC_NULL_POINTER, ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                 ESMC_NULL_POINTER, ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                 ESMC_NULL_POINTER);

    // set end of this month (start of next month)
    // TODO: use calendar interval logic when ready
    DD = 1;
    MM++;
    if (MM > MONTHS_PER_YEAR) {
      MM = 1;
      YR++;
    }
    ESMC_Time endOfMonth = *this;  // copy calendar and timezone
    endOfMonth.ESMC_TimeSet(&YR, &MM, &DD, 
                 ESMC_NULL_POINTER, ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                 ESMC_NULL_POINTER, ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                 ESMC_NULL_POINTER, ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                 ESMC_NULL_POINTER, ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                 ESMC_NULL_POINTER, ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                 ESMC_NULL_POINTER);

    // size of this month
    ESMC_TimeInterval month;
    month = endOfMonth - startOfMonth;

    // calculate and return the middle of this month
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

    time_t tm;
    struct tm wallClock;

    // get wall clock (system) time
    if (time(&tm) < 0) return (ESMF_FAILURE);
    wallClock = *localtime(&tm);          
    int           YR = wallClock.tm_year + 1900;
    int           MM = wallClock.tm_mon + 1;
    int           DD = wallClock.tm_mday;
    int           H  = wallClock.tm_hour;
    int           M  = wallClock.tm_min;
    ESMF_IKIND_I8 S  = wallClock.tm_sec;

    // set this time to wall clock time
    // TODO: use native C++ version when ready
    ESMC_TimeSet((ESMF_IKIND_I8 *)&YR, &MM, &DD, ESMC_NULL_POINTER,
                 &H, &M, &S, ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                 ESMC_NULL_POINTER, ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                 ESMC_NULL_POINTER, ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                 ESMC_NULL_POINTER, ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                 ESMC_NULL_POINTER);

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

    // TODO
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
    // default
    } else {
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
//      Initializes a {\tt ESMC\_Time} with defaults via {\tt ESMC\_TimeInit}
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
//      Initializes a {\tt ESMC\_Time} via {\tt ESMC\_TimeInit}
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
