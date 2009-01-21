// $Id: ESMC_Time.C,v 1.87.2.2 2009/01/21 21:25:23 cdeluca Exp $"
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
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
//
 #define ESMC_FILENAME "ESMC_Time.C"

 // higher level, 3rd party or system includes
 #include <stdio.h>
 #include <math.h>     // modf()
 #include <time.h>
 #include <string.h>

 #include <ESMC_LogErr.h>
 #include <ESMF_LogMacros.inc>
 #include <ESMC_TimeInterval.h>

 // associated class definition file
 #include <ESMC_Time.h>

//-------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_Time.C,v 1.87.2.2 2009/01/21 21:25:23 cdeluca Exp $";
//-------------------------------------------------------------------------

//
//-------------------------------------------------------------------------
//
// This section includes all the ESMC_Time routines
//
//
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
      ESMC_I4 *yy,        // in - integer year (>= 32-bit)
      ESMC_I8 *yy_i8,     // in - integer year (large, >= 64-bit)
      int *mm,                 // in - integer month
      int *dd,                 // in - integer day of the month
      ESMC_I4 *d,         // in - integer days (>= 32-bit)
      ESMC_I8 *d_i8,      // in - integer days (large, >= 64-bit)
      ESMC_I4 *h,         // in - integer hours
      ESMC_I4 *m,         // in - integer minutes
      ESMC_I4 *s,         // in - integer seconds (>= 32-bit)
      ESMC_I8 *s_i8,      // in - integer seconds (large, >= 64-bit)
      ESMC_I4 *ms,        // in - integer milliseconds
      ESMC_I4 *us,        // in - integer microseconds
      ESMC_I4 *ns,        // in - integer nanoseconds
      ESMC_R8 *d_r8,      // in - floating point days
      ESMC_R8 *h_r8,      // in - floating point hours
      ESMC_R8 *m_r8,      // in - floating point minutes
      ESMC_R8 *s_r8,      // in - floating point seconds
      ESMC_R8 *ms_r8,     // in - floating point milliseconds
      ESMC_R8 *us_r8,     // in - floating point microseconds
      ESMC_R8 *ns_r8,     // in - floating point nanoseconds
      ESMC_I4 *sN,        // in - fractional seconds numerator
      ESMC_I4 *sD,        // in - fractional seconds denominator
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

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_TimeSet()"

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

    int rc = ESMF_SUCCESS;

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

      ESMC_FractionSet(0,0,1);  // set seconds = 0
                                // set fractional seconds numerator = 0
                                // set fractional seconds denominator = 1

      this->calendar = ESMC_NULL_POINTER; // to trap no calendar case below
      this->timeZone = 0;                 // default is UTC

    } else if (calendar     != ESMC_NULL_POINTER ||
               calendarType != ESMC_NULL_POINTER ||
               timeZone     != ESMC_NULL_POINTER) {
      // only calendar and/or timezone specified

      // TODO: ? validate calendar conversions? 
      // Allow  Gregorian <-> Julian Day <-> Julian (triangular paths)
      //        360-Day <-> No Leap
      //        Custom <-> any other calendar
      //        No Calendar <-> any other calendar
      // Forbid Gregorian <-> 360-Day, Gregorian <-> No Leap,
      //        Julian Day <-> 360-Day, Julian Day <-> No Leap
      // Otherwise, need conversion method/offset value: See TODO: ? in
      // ESMC_Calendar.C

      // set calendar type
      if (calendar != ESMC_NULL_POINTER) {             // 1st choice
        this->calendar = *calendar;
        rc = ESMC_TimeValidate("calendar");
        if (ESMC_LogDefault.ESMC_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &rc))
          { *this = saveTime; return(rc); }

      } else if (calendarType != ESMC_NULL_POINTER) {  // 2nd choice
        // set to specified built-in type; create if necessary
        rc = ESMC_CalendarCreate(*calendarType);
        if (ESMC_LogDefault.ESMC_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &rc))
          { *this = saveTime; return(rc); }
        this->calendar = ESMC_Calendar::internalCalendar[*calendarType-1];
      }

      // set timezone
      if (timeZone != ESMC_NULL_POINTER) {
        this->timeZone = *timeZone;
        rc = ESMC_TimeValidate("timezone");
        if (ESMC_LogDefault.ESMC_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &rc))
          { *this = saveTime; return(rc); }
      }
      return(ESMF_SUCCESS);

    } else {

      // no arguments specified, initialize and use defaults
      ESMC_FractionSet(0,0,1);  // set seconds = 0
                                // set fractional seconds numerator = 0
                                // set fractional seconds denominator = 1

      this->calendar = ESMC_NULL_POINTER; // to trap no calendar case below
      this->timeZone = 0;                 // default is UTC
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
      rc = ESMC_CalendarCreate(*calendarType);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &rc))
        { *this = saveTime; return(rc); }
      this->calendar = ESMC_Calendar::internalCalendar[*calendarType-1];

    } else if (ESMC_Calendar::defaultCalendar != ESMC_NULL_POINTER) {
      // use default calendar                        // 3rd choice
      this->calendar = ESMC_Calendar::defaultCalendar;

    } else {                                         // 4th choice
      // create default calendar
      rc = ESMC_CalendarSetDefault((ESMC_CalendarType *)ESMC_NULL_POINTER);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &rc))
        { *this = saveTime; return(rc); }
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
      if (this->calendar == ESMC_NULL_POINTER) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                                              ", calendar required.", &rc);
        *this = saveTime; return(rc);
      }

      // if specified, positive month-of-the-year required; further calendar-
      //   specific validation performed within ESMC_CalendarConvertToTime()
      if (mm != ESMC_NULL_POINTER) {
        if (*mm < 1) {
          char logMsg[ESMF_MAXSTR];
          sprintf(logMsg, "; month-of-the-year mm=%d (must be >=1).", *mm);
          ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_OUTOFRANGE,
                                                logMsg, &rc);
          *this = saveTime; return(rc);
        }
      }

      // if specified, positive day-of-the-month required; further calendar-
      //   specific validation performed within ESMC_CalendarConvertToTime()
      if (dd != ESMC_NULL_POINTER) {
        if (*dd < 1) {
          char logMsg[ESMF_MAXSTR];
          sprintf(logMsg, "; day-of-the-month dd=%d (must be >=1).", *dd);
          ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_OUTOFRANGE,
                                                logMsg, &rc);
          *this = saveTime; return(rc);
        }
      }

      // use only one specified year (yy and yy_i8 are mutually exclusive),
      //   otherwise use default
      ESMC_I8 argYY=0;  // default
      if      (yy    != ESMC_NULL_POINTER) argYY = *yy;
      else if (yy_i8 != ESMC_NULL_POINTER) argYY = *yy_i8;
      
      // use month and day specified, otherwise use defaults
      int argMM=1, argDD=1;  // defaults
      if (mm != ESMC_NULL_POINTER) argMM = *mm;
      if (dd != ESMC_NULL_POINTER) argDD = *dd;

      // do the conversion
      rc = this->calendar->ESMC_CalendarConvertToTime(argYY, argMM, argDD,
                                                      0, this);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &rc))
        { *this = saveTime; return(rc); }

    // is a Julian-days style date specified?
    } else if (d != ESMC_NULL_POINTER || d_i8 != ESMC_NULL_POINTER) {

      // calendar required
      if (this->calendar == ESMC_NULL_POINTER) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                                              ", calendar required.", &rc);
        *this = saveTime; return(rc);
      }

      ESMC_I8 argD = (d != ESMC_NULL_POINTER) ? *d : *d_i8;
      rc = this->calendar->ESMC_CalendarConvertToTime(0, 0, 0, argD, this);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &rc))
          { *this = saveTime; return(rc); }

    } else if (d_r8 != ESMC_NULL_POINTER) {
      // calendar required
      if (this->calendar == ESMC_NULL_POINTER) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                                              ", calendar required.", &rc);
        *this = saveTime; return(rc);
      }

      // integer part
      rc = this->calendar->ESMC_CalendarConvertToTime(0, 0, 0, 
                                              (ESMC_I8) *d_r8, this);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &rc))
          { *this = saveTime; return(rc); }

      // fractional part
      ESMC_FractionSetw(ESMC_FractionGetw() +
                         (ESMC_I8) (modf(*d_r8, ESMC_NULL_POINTER) *
                                         this->calendar->secondsPerDay));
    } else {
      // no year, month or day specified; set defaults per calendar, if any
      if (this->calendar != ESMC_NULL_POINTER) {
        if (this->calendar->calendarType != ESMC_CAL_NOCALENDAR) {
          // defaults:  yy=0, mm=1, dd=1 d=0
          rc = this->calendar->ESMC_CalendarConvertToTime(0, 1, 1, 0, this);
          if (ESMC_LogDefault.ESMC_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &rc))
            { *this = saveTime; return(rc); }
        }
      }
    }
    
    // use base class to convert sub-day values
    ESMC_BaseTimeSet(h, m, s, s_i8, ms, us, ns, h_r8, m_r8, s_r8,
                     ms_r8, us_r8, ns_r8, sN, sD);

    rc = ESMC_TimeValidate();
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &rc))
        { *this = saveTime; return(rc); }

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
      ESMC_I4 *yy,           // out - integer year (>= 32-bit)
      ESMC_I8 *yy_i8,        // out - integer year (large, >= 64-bit)
      int *mm,                    // out - integer month
      int *dd,                    // out - integer day of the month
      ESMC_I4 *d,            // out - integer days (>= 32-bit)
      ESMC_I8 *d_i8,         // out - integer days (large, >= 64-bit)
      ESMC_I4 *h,            // out - integer hours
      ESMC_I4 *m,            // out - integer minutes
      ESMC_I4 *s,            // out - integer seconds (>= 32-bit)
      ESMC_I8 *s_i8,         // out - integer seconds (large, >= 64-bit)
      ESMC_I4 *ms,           // out - integer milliseconds
      ESMC_I4 *us,           // out - integer microseconds
      ESMC_I4 *ns,           // out - integer nanoseconds
      ESMC_R8 *d_r8,         // out - floating point days
      ESMC_R8 *h_r8,         // out - floating point hours
      ESMC_R8 *m_r8,         // out - floating point minutes
      ESMC_R8 *s_r8,         // out - floating point seconds
      ESMC_R8 *ms_r8,        // out - floating point milliseconds
      ESMC_R8 *us_r8,        // out - floating point microseconds
      ESMC_R8 *ns_r8,        // out - floating point nanoseconds
      ESMC_I4 *sN,           // out - fractional seconds numerator
      ESMC_I4 *sD,           // out - fractional seconds denominator
      ESMC_Calendar **calendar,   // out - associated calendar
      ESMC_CalendarType *calendarType, // out - associated calendar type
      int     *timeZone,          // out - timezone (hours offset from UTC)
      int      timeStringLen,     // in  - F90 time string size
      int     *tempTimeStringLen, // out - temp F90 time string size
      char    *tempTimeString,    // out - hybrid format
                                  //       YYYY-MM-DDThh:mm:ss[:n/d]
      int   timeStringLenISOFrac,     // in  - F90 ISO time string size
      int  *tempTimeStringLenISOFrac, // out - temp F90 ISO time string size
      char *tempTimeStringISOFrac,    // out - ISO 8601 format
                                  //       YYYY-MM-DDThh:mm:ss[.f]
      int          *dayOfWeek,    // out - day of the week (Mon = 1, Sun = 7)
      ESMC_Time    *midMonth,     // out - middle of the month time instant
      ESMC_I4 *dayOfYear,    // out - day of the year as an integer
      ESMC_R8 *dayOfYear_r8, // out - day of the year as a floating point
      ESMC_TimeInterval *dayOfYear_intvl) const {  // out - day of the year
                                                   //       as a time interval
//
// !DESCRIPTION:
//      Gets a {\tt Time}'s values in user-specified format. This version
//      supports the F90 interface.
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_TimeGet()"

    // TODO: fractional, sub-seconds

    int rc = ESMF_SUCCESS;

    ESMC_Time timeToConvert = *this;

    // convert base time to date according to calendar type
    if (yy != ESMC_NULL_POINTER || yy_i8 != ESMC_NULL_POINTER ||
        mm != ESMC_NULL_POINTER || dd  != ESMC_NULL_POINTER ||
        d  != ESMC_NULL_POINTER || d_i8  != ESMC_NULL_POINTER ||
        d_r8 != ESMC_NULL_POINTER) {
      if (this->calendar == ESMC_NULL_POINTER) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                                 ", calendar required.", &rc); return(rc);
      }
      rc = this->calendar->ESMC_CalendarConvertToDate(&timeToConvert,
                                                      yy, yy_i8, mm, dd,
                                                      d, d_i8, d_r8);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &rc))
        return(rc);
    }

    // get any other "day" units
    if (dayOfYear != ESMC_NULL_POINTER) {
      rc = ESMC_TimeGetDayOfYear(dayOfYear);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &rc))
        return(rc);
    }
    if (dayOfYear_r8 != ESMC_NULL_POINTER) {
      rc = ESMC_TimeGetDayOfYear(dayOfYear_r8);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &rc))
        return(rc);
    }
    if (dayOfYear_intvl != ESMC_NULL_POINTER) {
      rc = ESMC_TimeGetDayOfYear(dayOfYear_intvl);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &rc))
        return(rc);
    }
    if (dayOfWeek != ESMC_NULL_POINTER) {
      rc = ESMC_TimeGetDayOfWeek(dayOfWeek);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &rc))
        return(rc);
    }

    // remove any "other" day units from timeToConvert
    if (dayOfYear       != ESMC_NULL_POINTER ||
        dayOfYear_r8    != ESMC_NULL_POINTER ||
        dayOfYear_intvl != ESMC_NULL_POINTER ||
        dayOfWeek       != ESMC_NULL_POINTER) {
      if (timeToConvert.calendar == ESMC_NULL_POINTER) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD,
                                 ", calendar required.", &rc); return(rc);
      }
      timeToConvert.ESMC_FractionSetw(timeToConvert.ESMC_FractionGetw() %
                                      timeToConvert.calendar->secondsPerDay);
    }

    // use base class to get all other non-calendar dependant units
    rc = ESMC_BaseTimeGet(&timeToConvert, h, m, s, s_i8,
                          ms, us, ns, h_r8, m_r8, s_r8, ms_r8,
                          us_r8, ns_r8, sN, sD);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &rc))
      return(rc);

    // handle remaining miscellaneous get arguments

    if (midMonth != ESMC_NULL_POINTER) {
      rc = ESMC_TimeGetMidMonth(midMonth);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &rc))
        return(rc);
    }
    if (calendar != ESMC_NULL_POINTER) {
      *calendar = this->calendar;
    }
    if (calendarType != ESMC_NULL_POINTER) {
      if (this->calendar == ESMC_NULL_POINTER) {
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
             ", calendarType requires a calendar to be set.", &rc); return(rc);
      }
      *calendarType = this->calendar->calendarType;
    }
    if (timeZone != ESMC_NULL_POINTER) {
      *timeZone = this->timeZone;
    }
    if (tempTimeString != ESMC_NULL_POINTER && timeStringLen > 0) {
      rc = ESMC_TimeGetString(tempTimeString);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &rc))
        return(rc);
      *tempTimeStringLen = strlen(tempTimeString);
      // see also method ESMC_TimePrint()
    }
    if (tempTimeStringISOFrac != ESMC_NULL_POINTER &&
        timeStringLenISOFrac > 0) {
      rc = ESMC_TimeGetString(tempTimeStringISOFrac, "isofrac");
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &rc))
        return(rc);
      *tempTimeStringLenISOFrac = strlen(tempTimeStringISOFrac);
      // see also method ESMC_TimePrint()
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
      ESMC_I8 s,          // in - integer seconds
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

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_TimeSet(direct)"

    int rc = ESMF_SUCCESS;

    // use base class Set()
    rc = ESMC_BaseTime::ESMC_BaseTimeSet(s, sN, sD);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &rc))
      return(rc);

    // set calendar type
    this->calendar = ESMC_NULL_POINTER;  // to detect invalid, unset time
                                         // TODO: replace with ESMC_Base logic

    if (calendar != ESMC_NULL_POINTER) {                // 1st choice
      // set to user's calendar
      this->calendar = calendar;

    } else if (calendarType != (ESMC_CalendarType)0) {  // 2nd choice
      // set to specified built-in type; create if necessary
      rc = ESMC_CalendarCreate(calendarType);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &rc))
        return(rc);
      this->calendar = ESMC_Calendar::internalCalendar[calendarType-1];
    } // otherwise leave NULL, TODO: implement ESMC_Base logic, then can
      // set default calendar

    this->timeZone = timeZone;

    return(ESMF_SUCCESS);

 }  // end ESMC_TimeSet

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeIsLeapYear - Determines if this time is in a leap year
//
// !INTERFACE:
      bool ESMC_Time::ESMC_TimeIsLeapYear(
//
// !RETURN VALUE:
//    bool true if this time is in a leap year, false otherwise.
//
// !ARGUMENTS:
      int *rc) const {    // out - return code
//
// !DESCRIPTION:
//      Determines if this {\tt Time}'s year is a leap year.
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_TimeIsLeapYear()"

    // must have a calendar
    if (this->calendar == ESMC_NULL_POINTER) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
                                            "; calendar is NULL.", rc);
      return(false);
    }

    // get the year of this time
    ESMC_I8 yy_i8;
    int rc2 = ESMC_TimeGet((ESMC_I4 *)ESMC_NULL_POINTER, &yy_i8);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(rc2, ESMF_ERR_PASSTHRU, rc)) {
      return(false);
    }

    // check if it's a leap year within this calendar
    return(this->calendar->ESMC_CalendarIsLeapYear(yy_i8, rc));

 }  // end ESMC_TimeIsLeapYear

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
      const ESMC_Time *time,    // in  - Time to compare Calendar types against
      int *rc) const {          // out - return code
//
// !DESCRIPTION:
//      Compares given {\tt Time}'s {\tt Calendar} type with this {\tt Time}'s
//      {\tt Calendar} type
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_TimeIsSameCalendar()"

    if (this->calendar != ESMC_NULL_POINTER &&
        time->calendar != ESMC_NULL_POINTER)
    {
      if (rc != ESMC_NULL_POINTER) *rc = ESMF_SUCCESS;
      return(this->calendar->calendarType == time->calendar->calendarType);
    }
    else {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
                           "; calendar1 and/or calendar2 is NULL.", rc);
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

// TODO: Add optional calendar/calendarType arguments; default to 
//       Gregorian if not specified and no calendar within this given ESMC_Time.

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_TimeSyncToRealTime()"

    int rc = ESMF_SUCCESS;

    // validate for calendar type
    if (this->calendar == ESMC_NULL_POINTER) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
                                            "; calendar is NULL.", &rc);
      return(rc);
    }

    if (this->calendar->calendarType == ESMC_CAL_JULIANDAY ||
        this->calendar->calendarType == ESMC_CAL_NOCALENDAR) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_WRONG,
                           "; calendarType is JULIANDAY or NOCALENDAR.", &rc);
      return(rc);
    }

    time_t tm;
    struct tm wallClock;

    // get wall clock (system) time
    // TODO:  Use POSIX real-time clock_gettime() function to get nanosecond
    //        resolution.  Use BSD gettimeofday() to get microsecond resolution
    if (time(&tm) < 0) return (ESMF_FAILURE);
    wallClock = *localtime(&tm);          
    ESMC_I8 yy_i8 = wallClock.tm_year + 1900;
    int          mm    = wallClock.tm_mon + 1;
    int          dd    = wallClock.tm_mday;
    int          h     = wallClock.tm_hour;
    int          m     = wallClock.tm_min;
    int          s     = wallClock.tm_sec;

    // set this time to wall clock time
    // TODO: use native C++ Set() version when ready
    ESMC_Calendar *cal = this->calendar; // allows reset
    int tz = this->timeZone;             //   after Set() clears it
    rc = ESMC_TimeSet((ESMC_I4 *)ESMC_NULL_POINTER, &yy_i8, &mm, &dd,
                 ESMC_NULL_POINTER, ESMC_NULL_POINTER, &h, &m,
                 &s, ESMC_NULL_POINTER, ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                 ESMC_NULL_POINTER, ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                 ESMC_NULL_POINTER, ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                 ESMC_NULL_POINTER, ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                 ESMC_NULL_POINTER, &cal, ESMC_NULL_POINTER, &tz);

    ESMC_LogDefault.ESMC_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &rc);

    return(rc);

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
      const ESMC_TimeInterval &timeinterval) const {  // in - ESMC_TimeInterval
                                                      //      to add
//
// !DESCRIPTION:
//    Adds {\tt timeinterval} expression to this time.
//
//EOP
// !REQUIREMENTS:  

//    Implementation note:  This overrides the ESMC_Fraction (+) operator
//                          in order to copy the ESMC_Time-only properties
//                          (calendar & timeZone) to the result. 
//
    // delegate the increment operation to my calendar associate
    return(calendar->ESMC_CalendarIncrement(this, timeinterval));

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
      const ESMC_TimeInterval &timeinterval) const {  // in - ESMC_TimeInterval
                                                      //      to subtract
//
// !DESCRIPTION:
//    Subtracts {\tt timeinterval} expression from this time.
//
//EOP
// !REQUIREMENTS:  

//    Implementation note:  This overrides the ESMC_Fraction (-) operator
//                          in order to copy the ESMC_Time-only properties
//                          (calendar & timeZone) to the result. 
//
    // delegate the decrement operation to my calendar associate
    return(calendar->ESMC_CalendarDecrement(this, timeinterval));

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
      const ESMC_TimeInterval &timeinterval) {  // in - ESMC_TimeInterval
                                                //      to add
//
// !DESCRIPTION:
//    Adds {\tt timeinterval} expression to this time.
//
//EOP
// !REQUIREMENTS:  

    // delegate the increment operation to my calendar associate
    *this = calendar->ESMC_CalendarIncrement(this, timeinterval);

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
      const ESMC_TimeInterval &timeinterval) {  // in - ESMC_TimeInterval
                                                //      to subtract
//
// !DESCRIPTION:
//    Adds {\tt timeinterval} expression to this time.
//
//EOP
// !REQUIREMENTS:  

    // delegate the derement operation to my calendar associate
    *this = calendar->ESMC_CalendarDecrement(this, timeinterval);

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

//    Implementation note:  This overrides the 2nd ESMC_Fraction (-) operator
//                          simply because the 1st (-) operator is overridden.
//                          Visibility into ESMC_Fraction is lost; if not
//                          defined here, the compiler won't see the 2nd (-)
//                          operator defined at ESMC_Fraction!

    // the calendars of the two times must be the same
    int rc;
    if (!ESMC_TimeIsSameCalendar(&time, &rc)) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SAMETYPE,
        "; The calendars of the two times to difference are not the same", &rc);
      return(rc);
    }

    // given times' calendars are the same, so set difference time interval's
    //   calendar to be the same
    ESMC_TimeInterval diff;
    diff.calendar = this->calendar;

    // perform the difference using the ESMC_Fraction operator
    diff = ESMC_Fraction::operator-(time);

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

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_TimeValidate()"

    int rc = ESMF_SUCCESS;
    bool check_initialized = false;

    // parse options
    if (options != ESMC_NULL_POINTER) {

      // only interested in rc ? (e.g. just to determine whether this time is
      // initialized (used), such as TimeInterval's optional startTime or
      // endTime.  TODO:  real solution is to use F95 initializers when fully
      //                  available across all platforms)
      check_initialized = strstr(options, "initialized") != ESMC_NULL_POINTER;

      // TODO:  put calendar and timezone validate logic into separate,
      //        private methods (inline?) to avoid duplicate code ?

      // validate calendar only, not time values
      if (strncmp(options, "calendar", 8) == 0) {
        if (this->calendar == ESMC_NULL_POINTER) {
          rc = ESMC_RC_PTR_NULL;
          if (!check_initialized)
            ESMC_LogDefault.ESMC_LogMsgFoundError(rc,"; calendar is NULL", &rc);
          return(rc);
        }
        rc = this->calendar->ESMC_CalendarValidate();
        ESMC_LogDefault.ESMC_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &rc);
        return(rc);

      // validate timezone only, not time values
      } else if (strncmp(options, "timezone", 8) == 0) {
        // TODO: valid Timezones ?
        return(rc);
      }
    }

    rc = ESMC_BaseTime::ESMC_BaseTimeValidate();
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &rc))
      return(rc);

    if (this->calendar == ESMC_NULL_POINTER) {
      rc = ESMC_RC_PTR_NULL;
      if (!check_initialized)
        ESMC_LogDefault.ESMC_LogMsgFoundError(rc, "; calendar is NULL", &rc);
      return(rc);
    }
    rc = this->calendar->ESMC_CalendarValidate();
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &rc))
        return(rc);

    // earliest Gregorian date representable by the Fliegel algorithm
    //  is -4800/3/1 == -32044 Julian days == -2,768,601,600 core seconds
    if (calendar->calendarType == ESMC_CAL_GREGORIAN &&
        ESMC_FractionGetw() < -2768601600LL) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_BAD,
                                 "; Gregorian time is before -4800/3/1", &rc);
      return(rc);
    }

    // earliest Julian date representable by the Hatcher algorithm
    //  is -4712/3/1 == 60 Julian days == 5,184,000 core seconds
    if (calendar->calendarType == ESMC_CAL_JULIAN &&
        ESMC_FractionGetw() < 5184000LL) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_BAD,
                                 "; Julian time is before -4712/3/1", &rc);
      return(rc);
    }

    // TODO: other calendar ranges ?

    // TODO: valid Timezones ?

    return(rc);

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

    printf("Time -----------------------------------\n");

    // parse options
    if (options != ESMC_NULL_POINTER) {
      if (strncmp(options, "string", 6) == 0) {
        char timeString[ESMF_MAXSTR];
        ESMC_TimeGetString(timeString, &options[6]);
        printf("%s\n", timeString);
        // see also method ESMC_TimeGet()
      }
    } else {
      // default
      ESMC_BaseTime::ESMC_BaseTimePrint(options);
      if (this->calendar != ESMC_NULL_POINTER) {
        this->calendar->ESMC_CalendarPrint(options, this);
      }
      printf("timeZone = %d\n", timeZone);
    }

    printf("end Time -------------------------------\n\n");

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

//   ESMC_BaseTime(0,0,1) {  // TODO: F90 issue with base class constructor?
   ESMC_FractionSet(0,0,1);  // set seconds = 0
                             // set fractional seconds numerator = 0
                             // set fractional seconds denominator = 1
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
      ESMC_I8 s,           // in - integer seconds
      ESMC_I4 sN,          // in - fractional seconds, numerator
      ESMC_I4 sD,          // in - fractional seconds, denominator
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
      char *timeString, const char *options) const {    // out - time value in
                                                        //       string format
                                                        // in  - format options
//
// !DESCRIPTION:
//      Gets a {\tt time}'s value in ISO 8601 string format
//      YYYY-MM-DDThh:mm:ss[:n/d]  (hybrid) (default, options == "")
//      or YYYY-MM-DDThh:mm:ss[.f] (strict) (options == "isofrac")
//      Supports {\tt ESMC\_TimeGet()} and {\tt ESMC\_TimePrint()}.
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_TimeGetString()"

    int rc = ESMF_SUCCESS;

    // validate inputs
    if (timeString == ESMC_NULL_POINTER) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
                                            "; timeString is NULL", &rc);
      return(rc);
    }
    if (this->calendar == ESMC_NULL_POINTER) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
                                            "; calendar is NULL", &rc);
      return(rc);
    }
    if (this->calendar->calendarType == ESMC_CAL_JULIANDAY ||
        this->calendar->calendarType == ESMC_CAL_NOCALENDAR) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_BAD,
                           "; calendarType is JULIANDAY or NOCALENDAR.", &rc);
      return(rc);
    }

    ESMC_I8 yy_i8;
    int mm, dd;
    ESMC_I4 h, m, s, sN, sD; 

    // TODO: use native C++ Get, not F90 entry point, when ready
    rc = ESMC_TimeGet((ESMC_I4 *)ESMC_NULL_POINTER, &yy_i8, &mm, &dd,
                  ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                  &h, &m, &s, ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                              ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                              ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                              ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                              ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                              ESMC_NULL_POINTER, &sN, &sD);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &rc))
      return(rc);

    // format everything except seconds
    sprintf(timeString, "%04lld-%02d-%02dT%02d:%02d:\0", yy_i8, mm, dd, h, m);

    // format seconds according to specified options
    bool isofrac = false;
    if (options != ESMC_NULL_POINTER) {
      if (strstr(options, "isofrac") != ESMC_NULL_POINTER) isofrac = true;
    }
    if (isofrac) {
      // strict ISO 8601 format YYYY-MM-DDThh:mm:ss[.f]

      // convert integer fractional seconds to decimal form
      ESMC_R8 fractionalSeconds = 0.0;
      if (sD != 0) fractionalSeconds = (ESMC_R8) sN / (ESMC_R8) sD;

      // if fractionalSeconds non-zero (>= 0.5 ns) append full fractional value
      if (fabs(fractionalSeconds) >= 5e-10) {
        sprintf(timeString, "%s%012.9f\0", timeString, (s + fractionalSeconds));
      } else { // no fractional seconds, just append integer seconds
        sprintf(timeString, "%s%02d\0", timeString, s);
      }
    } else { // not strict ISO fractional seconds format
      // hybrid ISO 8601 format YYYY-MM-DDThh:mm:ss[:n/d]

      // if fractionalSeconds non-zero (sN!=0) append full fractional value
      if (sN != 0) {
        sprintf(timeString, "%s%02d:%d/%d\0", timeString, s, sN, sD);
      } else { // no fractional seconds, just append integer seconds
        sprintf(timeString, "%s%02d\0", timeString, s);
      }
    }

    return(rc);

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

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_TimeGetDayOfWeek()"

    int rc = ESMF_SUCCESS;

    // validate inputs
    if (dayOfWeek == ESMC_NULL_POINTER) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
                                            "; dayOfWeek is NULL", &rc);
      return(rc);
    }
    if (this->calendar == ESMC_NULL_POINTER) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
                                            "; calendar is NULL", &rc);
      return(rc);
    }

    // date variables
    ESMC_I8 yy_i8;
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
          //  Can be any Monday after the Gregorian reformation of 10/15/1582
          yy_i8=1796; mm=7; dd=4;   // America's 20th birthday was a Monday !
          break;

        case ESMC_CAL_JULIAN:
        case ESMC_CAL_JULIANDAY:
          //  Can be any Monday before the Julian end of 10/4/1582
          yy_i8=1492; mm=10; dd=29;  // Columbus landed in Cuba on a Monday !
          break;

        case ESMC_CAL_NOCALENDAR:
        case ESMC_CAL_CUSTOM:
        default:
          ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_BAD,
                "; calendarType is NOCALENDAR, CUSTOM, or unrecognized.", &rc);
          return(rc);
          break;
    }

    // TODO: put the above reference dates into a pre-initialized lookup table
    //       to skip this step
    ESMC_Time referenceMonday;
    // TODO: use native C++ Set() when ready
    ESMC_Calendar *cal = this->calendar; // allows reset
    int tz = this->timeZone;             //   after Set() clears it
    rc = referenceMonday.ESMC_TimeSet((ESMC_I4 *)ESMC_NULL_POINTER,
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

    if (ESMC_LogDefault.ESMC_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &rc))
      return(rc);

    // calculate the difference in days between the given date and
    //  the reference date
    ESMC_TimeInterval delta;
    delta = *this - referenceMonday;
    delta.calendar = this->calendar;
    ESMC_I8 diffDays;
    // TODO: use native C++ Get() when ready
    rc = delta.ESMC_TimeIntervalGet((ESMC_I4 *)ESMC_NULL_POINTER,
                                     ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                     ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                     &diffDays);

    if (ESMC_LogDefault.ESMC_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &rc))
      return(rc);

    // calculate day of the week as simply modulo 7 from the reference date,
    //  adjusted for a 1-based count and negative deltas
    int mod7 = diffDays % 7;  // (-6 to 0) or (0 to 6)
    if (mod7 < 0) mod7 += 7;  // ensure positive (0 to 6) range
    *dayOfWeek = mod7 + 1;    // adjust to 1-based count (1 to 7)

    return(rc);

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

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_TimeGetMidMonth()"

    int rc = ESMF_SUCCESS;

    // validate inputs
    if (midMonth == ESMC_NULL_POINTER) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
                                            "; midMonth is NULL", &rc);
      return(rc);
    }
    if (this->calendar == ESMC_NULL_POINTER) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
                                            "; calendar is NULL", &rc);
      return(rc);
    }
    if (this->calendar->calendarType == ESMC_CAL_JULIANDAY ||
        this->calendar->calendarType == ESMC_CAL_NOCALENDAR) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_BAD,
                           "; calendarType is JULIANDAY or NOCALENDAR.", &rc);
      return(rc);
    }

    // TODO: use table lookup per calendar type (14, 14.5, 15, 15.5 days) ?

    // TODO: use native C++ Get()/Set() when ready

    // get this date
    ESMC_I8 yy_i8;
    int mm, dd;
    rc = ESMC_TimeGet((ESMC_I4 *)ESMC_NULL_POINTER, &yy_i8, &mm, &dd);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &rc))
      return(rc);

    // set start of this month
    dd = 1;
    ESMC_Time startOfMonth;
    ESMC_Calendar *cal = this->calendar; // allows reset
    int tz = this->timeZone;             //   after Set() clears it
    rc = startOfMonth.ESMC_TimeSet((ESMC_I4 *)ESMC_NULL_POINTER,
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

    if (ESMC_LogDefault.ESMC_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &rc))
      return(rc);

    // set end of this month (start of next month)
    // TODO: use calendar interval logic when ready
    dd = 1;
    mm++;
    if (mm > MONTHS_PER_YEAR) {
      mm = 1;
      yy_i8++;
    }
    ESMC_Time endOfMonth;
    rc = endOfMonth.ESMC_TimeSet((ESMC_I4 *)ESMC_NULL_POINTER,
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

    if (ESMC_LogDefault.ESMC_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &rc))
      return(rc);

    // size of this month
    ESMC_TimeInterval month;
    month = endOfMonth - startOfMonth;

    // calculate and return the middle of this month
    *midMonth = startOfMonth;  // initialize calendar & timezone
    *midMonth = startOfMonth + month/2;

    // TODO: add C++ infrastructure to enable the following expression:
    // *midMonth = startOfMonth + (endOfMonth - startOfMonth)/2;

    return(rc);

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
      ESMC_I4 *dayOfYear) const {    // out - time's day of year value
//
// !DESCRIPTION:
//      Gets a {\tt Time}'s day of the year value as a integer value.
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_TimeGetDayOfYear(integer)"

    int rc = ESMF_SUCCESS;

    // validate inputs
    if (dayOfYear == ESMC_NULL_POINTER) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
                                            "; dayOfYear is NULL", &rc);
      return(rc);
    }
    if (this->calendar == ESMC_NULL_POINTER) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
                                            "; calendar is NULL", &rc);
      return(rc);
    }
    if (this->calendar->calendarType == ESMC_CAL_JULIANDAY ||
        this->calendar->calendarType == ESMC_CAL_NOCALENDAR) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_BAD,
                           "; calendarType is JULIANDAY or NOCALENDAR.", &rc);
      return(rc);
    }
    
    // get day of year as time interval between now and 1/1/yy
    ESMC_TimeInterval yearDay;
    rc = ESMC_TimeGetDayOfYear(&yearDay);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &rc))
      return(rc);

    // get difference in integer days
    ESMC_I8 diffDays;
    // TODO: use native C++ Get, not F90 entry point
    rc = yearDay.ESMC_TimeIntervalGet((ESMC_I4 *)ESMC_NULL_POINTER,
                                      ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                      ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                      &diffDays);

    if (ESMC_LogDefault.ESMC_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &rc))
      return(rc);

    // day-of-year is one-based count; i.e. day-of-year for 1/1/yy is 1
    *dayOfYear = (int) diffDays + 1;

    return(rc);

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
      ESMC_R8 *dayOfYear) const {    // out - time's day of year value
//
// !DESCRIPTION:
//      Gets a {\tt Time}'s day of the year value as a floating point value.
//      Whole number part is days; fractional part is fraction of a day, equal
//      to seconds (whole + fractional) divided by 86400, the number of seconds
//      in a day.
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_TimeGetDayOfYear(ESMC_R8)"

    int rc = ESMF_SUCCESS;

    // validate inputs
    if (dayOfYear == ESMC_NULL_POINTER) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
                                            "; dayOfYear is NULL", &rc);
      return(rc);
    }
    if (this->calendar == ESMC_NULL_POINTER) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
                                            "; calendar is NULL", &rc);
      return(rc);
    }
    if (this->calendar->calendarType == ESMC_CAL_JULIANDAY ||
        this->calendar->calendarType == ESMC_CAL_NOCALENDAR) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_BAD,
                           "; calendarType is JULIANDAY or NOCALENDAR.", &rc);
      return(rc);
    }

    // get day of year as time interval between now and 1/1/yy
    ESMC_TimeInterval yearDay;
    rc = ESMC_TimeGetDayOfYear(&yearDay);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &rc))
      return(rc);

    // get difference in floating point days
    ESMC_R8 diffDays;
    // TODO: use native C++ Get, not F90 entry point
    rc = yearDay.ESMC_TimeIntervalGet((ESMC_I4 *)ESMC_NULL_POINTER,
                                      ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                      ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                      ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                      ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                      ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                      ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                      &diffDays);

    if (ESMC_LogDefault.ESMC_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &rc))
      return(rc);

    // day-of-year is one-based count; i.e. day-of-year for 1/1/yy is 1
    *dayOfYear = diffDays + 1;

    return(rc);

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

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_TimeGetDayOfYear(timeinterval)"

    int rc = ESMF_SUCCESS;

    // validate inputs
    if (dayOfYear == ESMC_NULL_POINTER) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
                                            "; dayOfYear is NULL", &rc);
      return(rc);
    }
    if (this->calendar == ESMC_NULL_POINTER) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
                                            "; calendar is NULL", &rc);
      return(rc);
    }
    if (this->calendar->calendarType == ESMC_CAL_JULIANDAY ||
        this->calendar->calendarType == ESMC_CAL_NOCALENDAR) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_BAD,
                           "; calendarType is JULIANDAY or NOCALENDAR.", &rc);
      return(rc);
    }

    // get year of our (this) time
    ESMC_I8 yy_i8;
    int mm, dd;
    // TODO: use native C++ Get, not F90 entry point
    rc = ESMC_TimeGet((ESMC_I4 *)ESMC_NULL_POINTER, &yy_i8, &mm, &dd);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &rc))
      return(rc);

    // create time for 1/1/yy
    ESMC_Time dayOne;
    mm=1, dd=1;
    // TODO: use native C++ Set(), not F90 entry point
    ESMC_Calendar *cal = this->calendar; // allows reset
    int tz = this->timeZone;             //   after Set() clears it
    rc = dayOne.ESMC_TimeSet((ESMC_I4 *)ESMC_NULL_POINTER,
                             &yy_i8, &mm, &dd,
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

    if (ESMC_LogDefault.ESMC_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &rc))
      return(rc);

    // calculate difference between 1/1/yy and our (this) time
    *dayOfYear = *this - dayOne;
    dayOfYear->calendar = this->calendar;
    
    return(rc);

 }  // end ESMC_TimeGetDayOfYear
