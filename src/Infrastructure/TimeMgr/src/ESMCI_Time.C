// $Id$"
//
// Earth System Modeling Framework
// Copyright 2002-2018, University Corporation for Atmospheric Research,
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
// in the companion file {\tt ESMCI_Time.h}
//
//-------------------------------------------------------------------------
#define ESMC_FILENAME "ESMCI_Time.C"

// associated class definition file
#include "ESMCI_Time.h"

// higher level, 3rd party or system includes
#include <stdio.h>
#include <math.h>     // modf()
#include <time.h>
#include <string.h>

#include "ESMCI_LogErr.h"
#include "ESMCI_TimeInterval.h"
#include "ESMCI_Fraction.h"

//-------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-------------------------------------------------------------------------

namespace ESMCI{

//
//-------------------------------------------------------------------------
//
// This section includes all the Time routines
//
//
//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  Time::set - initializer to support F90 interface
//
// !INTERFACE:
      int Time::set(
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
      ESMC_I8 *sN_i8,     // in - fractional seconds numerator
                          //                                 (large, >= 64-bit)
      ESMC_I4 *sD,        // in - fractional seconds denominator
      ESMC_I8 *sD_i8,     // in - fractional seconds denominator
                          //                                 (large, >= 64-bit)
      Calendar **calendar, // in - associated calendar
      ESMC_CalKind_Flag *calkindflag, // in - associated calendar kind
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
 #define ESMC_METHOD "ESMCI::Time::set()"

    // TODO: Since Time is a shallow statically allocated class,
    //       ensure initialization if called via F90 interface;
    //       cannot call constructor, because destructor is subsequently
    //       called automatically, returning initialized values to garbage.

    // if any time value is specified, initialize core values first;
    // user is specifying a complete time and not relying on any past settings.
    // (if only calendar or timezone is specified, don't initialize, but do
    //  validate basetime, calendar and/or timezone)
    // (this if-else logic avoids the need for a separate Time::setup()
    //  method)

    int rc = ESMF_SUCCESS;

    // save current time to restore in case there is a failure
    Time saveTime = *this;

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
        sN    != ESMC_NULL_POINTER || sN_i8 != ESMC_NULL_POINTER ||
        sD    != ESMC_NULL_POINTER || sD_i8 != ESMC_NULL_POINTER) {

      Fraction::set(0,0,1);  // set seconds = 0
                             // set fractional seconds numerator = 0
                             // set fractional seconds denominator = 1

      this->calendar = ESMC_NULL_POINTER; // to trap no calendar case below
      this->timeZone = 0;                 // default is UTC

    } else if (calendar     != ESMC_NULL_POINTER ||
               calkindflag  != ESMC_NULL_POINTER ||
               timeZone     != ESMC_NULL_POINTER) {
      // only calendar and/or timezone specified, do not re-initialize basetime

      // initialize basetime only if not done previously 
      if (BaseTime::validate() != ESMF_SUCCESS) {
        Fraction::set(0,0,1);  // set seconds = 0
                               // set fractional seconds numerator = 0
                               // set fractional seconds denominator = 1
      }

      // TODO: ? validate calendar conversions? 
      // Allow  Gregorian <-> Julian Day <-> Julian (triangular paths)
      //        360-Day <-> No Leap
      //        Custom <-> any other calendar
      //        No Calendar <-> any other calendar
      // Forbid Gregorian <-> 360-Day, Gregorian <-> No Leap,
      //        Julian Day <-> 360-Day, Julian Day <-> No Leap
      // Otherwise, need conversion method/offset value: See TODO: ? in
      // ESMCI_Calendar.C

      // set calendar kind
      if (calendar != ESMC_NULL_POINTER) {             // 1st choice
        this->calendar = *calendar;
        rc = Time::validate("calendar");
        if (ESMC_LogDefault.MsgFoundError(rc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, &rc))
          { *this = saveTime; return(rc); }

      } else if (calkindflag != ESMC_NULL_POINTER) {  // 2nd choice
        // set to specified built-in type; create if necessary
        rc = ESMCI_CalendarCreate(*calkindflag);
        if (ESMC_LogDefault.MsgFoundError(rc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, &rc))
          { *this = saveTime; return(rc); }
        this->calendar = Calendar::internalCalendar[*calkindflag-1];
      }

      // set timezone
      if (timeZone != ESMC_NULL_POINTER) {
        this->timeZone = *timeZone;
        rc = Time::validate("timezone");
        if (ESMC_LogDefault.MsgFoundError(rc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, &rc))
          { *this = saveTime; return(rc); }
      }
      return(ESMF_SUCCESS);

    } else {

      // no arguments specified, initialize and use defaults
      Fraction::set(0,0,1);  // set seconds = 0
                             // set fractional seconds numerator = 0
                             // set fractional seconds denominator = 1

      this->calendar = ESMC_NULL_POINTER; // to trap no calendar case below
      this->timeZone = 0;                 // default is UTC
    }
    
    // TODO: validate inputs (individual and combos), set basetime values
    //       e.g. integer and float specifiers are mutually exclusive

    // set calendar kind
    if (calendar != ESMC_NULL_POINTER) {             // 1st choice
      // set to user's calendar
      this->calendar = *calendar;

    } else if (calkindflag != ESMC_NULL_POINTER) {  // 2nd choice
      // set to specified built-in type; create if necessary
      rc = ESMCI_CalendarCreate(*calkindflag);
      if (ESMC_LogDefault.MsgFoundError(rc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, &rc))
        { *this = saveTime; return(rc); }
      this->calendar = Calendar::internalCalendar[*calkindflag-1];

    } else if (Calendar::defaultCalendar != ESMC_NULL_POINTER) {
      // use default calendar                        // 3rd choice
      this->calendar = Calendar::defaultCalendar;

    } else {                                         // 4th choice
      // create default calendar
      rc = ESMCI_CalendarSetDefault((ESMC_CalKind_Flag *)ESMC_NULL_POINTER);
      if (ESMC_LogDefault.MsgFoundError(rc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, &rc))
        { *this = saveTime; return(rc); }
      this->calendar = Calendar::defaultCalendar;
    }

    // set timezone
    if (timeZone != ESMC_NULL_POINTER) {
      this->timeZone = *timeZone;
    }

    // TODO: Timezone adjust

    // convert date to base time according to calendar date style:
    //  yy/d (planet), yy/mm/dd (Earth), or d (Earth or planet)

    // TODO: create two (or more) calendar conversion method entry points ?

    // is a yy/d style date specified (planet)?
    if ((yy != ESMC_NULL_POINTER || yy_i8 != ESMC_NULL_POINTER) && 
         (d != ESMC_NULL_POINTER || d_i8 != ESMC_NULL_POINTER) &&
          mm == ESMC_NULL_POINTER && dd == ESMC_NULL_POINTER) {

      // calendar required
      if (this->calendar == ESMC_NULL_POINTER) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
          ", calendar required.", ESMC_CONTEXT, &rc);
        *this = saveTime; return(rc);
      }

      // use only one specified year (yy and yy_i8 are mutually exclusive)
      ESMC_I8 argYY = (yy != ESMC_NULL_POINTER) ? *yy : *yy_i8;
      
      // use only one specified day count (d and d_i8 are mutually exclusive)
      ESMC_I8 argD = (d != ESMC_NULL_POINTER) ? *d : *d_i8;

      // do the conversion
      rc = this->calendar->convertToTime(argYY, 0, 0, argD, 0.0, this);
      if (ESMC_LogDefault.MsgFoundError(rc, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, &rc))
          { *this = saveTime; return(rc); }

    // is a yy/d_r8 style date specified (planet) ?
    } else if ((yy != ESMC_NULL_POINTER || yy_i8 != ESMC_NULL_POINTER) &&
                d_r8 != ESMC_NULL_POINTER &&
                mm == ESMC_NULL_POINTER && dd == ESMC_NULL_POINTER) {

      // calendar required
      if (this->calendar == ESMC_NULL_POINTER) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
          ", calendar required.", ESMC_CONTEXT, &rc);
        *this = saveTime; return(rc);
      }

      // use only one specified year (yy and yy_i8 are mutually exclusive)
      ESMC_I8 argYY = (yy != ESMC_NULL_POINTER) ? *yy : *yy_i8;
      
      // do the conversion
      rc = this->calendar->convertToTime(argYY, 0, 0, 0, *d_r8, this);
      if (ESMC_LogDefault.MsgFoundError(rc, ESMCI_ERR_PASSTHRU, 
          ESMC_CONTEXT, &rc))
          { *this = saveTime; return(rc); }

    // is a yy/mm/dd style date specified (Earth) ?
    } else if (yy != ESMC_NULL_POINTER || yy_i8 != ESMC_NULL_POINTER ||
               mm != ESMC_NULL_POINTER || dd    != ESMC_NULL_POINTER) {

      // calendar required
      if (this->calendar == ESMC_NULL_POINTER) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
          ", calendar required.", ESMC_CONTEXT, &rc);
        *this = saveTime; return(rc);
      }

      // if specified, positive month-of-the-year required; further calendar-
      //   specific validation performed within Calendar::convertToTime()
      if (mm != ESMC_NULL_POINTER) {
        if (*mm < 1) {
          char logMsg[ESMF_MAXSTR];
          sprintf(logMsg, "; month-of-the-year mm=%d (must be >=1).", *mm);
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_OUTOFRANGE,
            logMsg, ESMC_CONTEXT, &rc);
          *this = saveTime; return(rc);
        }
      }

      // if specified, positive day-of-the-month required; further calendar-
      //   specific validation performed within Calendar::convertToTime()
      if (dd != ESMC_NULL_POINTER) {
        if (*dd < 1) {
          char logMsg[ESMF_MAXSTR];
          sprintf(logMsg, "; day-of-the-month dd=%d (must be >=1).", *dd);
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_OUTOFRANGE,
            logMsg, ESMC_CONTEXT, &rc);
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
      rc = this->calendar->convertToTime(argYY, argMM, argDD, 0, 0.0, this);
      if (ESMC_LogDefault.MsgFoundError(rc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        &rc))
        { *this = saveTime; return(rc); }

    // is a Julian-days style date specified (Earth or planet) ?
    } else if (d != ESMC_NULL_POINTER || d_i8 != ESMC_NULL_POINTER) {

      // calendar required
      if (this->calendar == ESMC_NULL_POINTER) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
          ", calendar required.", ESMC_CONTEXT, &rc);
        *this = saveTime; return(rc);
      }

      ESMC_I8 argD = (d != ESMC_NULL_POINTER) ? *d : *d_i8;
      rc = this->calendar->convertToTime(0, 0, 0, argD, 0.0, this);
      if (ESMC_LogDefault.MsgFoundError(rc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
          &rc))
          { *this = saveTime; return(rc); }

    // is a floating-point Julian-days style date specified (Earth or planet) ?
    } else if (d_r8 != ESMC_NULL_POINTER) {

      // calendar required
      if (this->calendar == ESMC_NULL_POINTER) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
          ", calendar required.", ESMC_CONTEXT, &rc);
        *this = saveTime; return(rc);
      }

      rc = this->calendar->convertToTime(0, 0, 0, 0, *d_r8, this);
      if (ESMC_LogDefault.MsgFoundError(rc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
          &rc))
          { *this = saveTime; return(rc); }

    } else {
      // no year, month or day specified; set defaults per calendar, if any
      if (this->calendar != ESMC_NULL_POINTER) {
        if (this->calendar->calkindflag != ESMC_CALKIND_NOCALENDAR) {
          // defaults:  yy=0, mm=1, dd=1 d=0
          rc = this->calendar->convertToTime(0, 1, 1, 0, 0.0, this);
          if (ESMC_LogDefault.MsgFoundError(rc, ESMCI_ERR_PASSTHRU,
            ESMC_CONTEXT, &rc))
            { *this = saveTime; return(rc); }
        }
      }
    }
    
    // use base class to convert sub-day values
    BaseTime::set(h, m, s, s_i8, ms, us, ns, h_r8, m_r8, s_r8,
                     ms_r8, us_r8, ns_r8, sN, sN_i8, sD, sD_i8);

    rc = Time::validate();
    if (ESMC_LogDefault.MsgFoundError(rc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc))
        { *this = saveTime; return(rc); }

    return(ESMF_SUCCESS);

 }  // end Time::set

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  Time::get - Get a Time value; supports F90 interface
//
// !INTERFACE:
      int Time::get(
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
      ESMC_I8 *sN_i8,        // out - fractional seconds numerator
                             //                              (large, >= 64-bit)
      ESMC_I4 *sD,           // out - fractional seconds denominator
      ESMC_I8 *sD_i8,        // out - fractional seconds denominator
                             //                              (large, >= 64-bit)
      Calendar **calendar,   // out - associated calendar
      ESMC_CalKind_Flag *calkindflag, // out - associated calendar kind
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
      Time    *midMonth,     // out - middle of the month time instant
      ESMC_I4 *dayOfYear,    // out - day of the year as an integer
      ESMC_R8 *dayOfYear_r8, // out - day of the year as a floating point
      TimeInterval *dayOfYear_intvl) const {  // out - day of the year
                                                   //       as a time interval
//
// !DESCRIPTION:
//      Gets a {\tt Time}'s values in user-specified format. This version
//      supports the F90 interface.
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::Time::get()"

    int rc = ESMF_SUCCESS;

    Time timeToConvert = *this;

    // convert base time to date according to calendar kind
    if (yy != ESMC_NULL_POINTER || yy_i8 != ESMC_NULL_POINTER ||
        mm != ESMC_NULL_POINTER || dd  != ESMC_NULL_POINTER ||
        d  != ESMC_NULL_POINTER || d_i8  != ESMC_NULL_POINTER ||
        d_r8 != ESMC_NULL_POINTER) {
      if (this->calendar == ESMC_NULL_POINTER) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_OBJ_INIT,
          ", calendar required.", ESMC_CONTEXT, &rc); return(rc);
      }
      rc = this->calendar->convertToDate(&timeToConvert, yy, yy_i8, mm, dd,
                                                         d, d_i8, d_r8);
      if (ESMC_LogDefault.MsgFoundError(rc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        &rc))
        return(rc);
    }

    // get any other "day" units
    if (dayOfYear != ESMC_NULL_POINTER) {
      rc = Time::getDayOfYear(dayOfYear);
      if (ESMC_LogDefault.MsgFoundError(rc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        &rc))
        return(rc);
    }
    if (dayOfYear_r8 != ESMC_NULL_POINTER) {
      rc = Time::getDayOfYear(dayOfYear_r8);
      if (ESMC_LogDefault.MsgFoundError(rc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, &rc))
        return(rc);
    }
    if (dayOfYear_intvl != ESMC_NULL_POINTER) {
      rc = Time::getDayOfYear(dayOfYear_intvl);
      if (ESMC_LogDefault.MsgFoundError(rc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, &rc))
        return(rc);
    }
    if (dayOfWeek != ESMC_NULL_POINTER) {
      rc = Time::getDayOfWeek(dayOfWeek);
      if (ESMC_LogDefault.MsgFoundError(rc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, &rc))
        return(rc);
    }

    // remove any "other" day units from timeToConvert
    if (dayOfYear       != ESMC_NULL_POINTER ||
        dayOfYear_r8    != ESMC_NULL_POINTER ||
        dayOfYear_intvl != ESMC_NULL_POINTER ||
        dayOfWeek       != ESMC_NULL_POINTER) {
      if (timeToConvert.calendar == ESMC_NULL_POINTER) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_OBJ_INIT,
          ", calendar required.", ESMC_CONTEXT, &rc); return(rc);
      }
      timeToConvert.setw(timeToConvert.getw() %
                         timeToConvert.calendar->secondsPerDay);
    }

    // use base class to get all other non-calendar dependant units
    rc = BaseTime::get(&timeToConvert, h, m, s, s_i8,
                          ms, us, ns, h_r8, m_r8, s_r8, ms_r8,
                          us_r8, ns_r8, sN, sN_i8, sD, sD_i8);
    if (ESMC_LogDefault.MsgFoundError(rc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc))
      return(rc);

    // handle remaining miscellaneous get arguments

    if (midMonth != ESMC_NULL_POINTER) {
      rc = Time::getMidMonth(midMonth);
      if (ESMC_LogDefault.MsgFoundError(rc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        &rc))
        return(rc);
    }
    if (calendar != ESMC_NULL_POINTER) {
      *calendar = this->calendar;
    }
    if (calkindflag != ESMC_NULL_POINTER) {
      if (this->calendar == ESMC_NULL_POINTER) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
             ", calkindflag requires a calendar to be set.", ESMC_CONTEXT, &rc);
        return(rc);
      }
      *calkindflag = this->calendar->calkindflag;
    }
    if (timeZone != ESMC_NULL_POINTER) {
      *timeZone = this->timeZone;
    }
    if (tempTimeString != ESMC_NULL_POINTER && timeStringLen > 0) {
      rc = Time::getString(tempTimeString);
      if (ESMC_LogDefault.MsgFoundError(rc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        &rc))
        return(rc);
      *tempTimeStringLen = strlen(tempTimeString);
      // see also method Time::print()
    }
    if (tempTimeStringISOFrac != ESMC_NULL_POINTER &&
        timeStringLenISOFrac > 0) {
      rc = Time::getString(tempTimeStringISOFrac, "isofrac");
      if (ESMC_LogDefault.MsgFoundError(rc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        &rc))
        return(rc);
      *tempTimeStringLenISOFrac = strlen(tempTimeStringISOFrac);
      // see also method Time::print()
    }

    return(ESMF_SUCCESS);

 }  // end Time::get

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  Time::get - Get a Time value
//
// !INTERFACE:
//      int Time::get(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//      const char *timeList,    // in  - time value specifier string
//      ...) const {             // out - specifier values (variable args)
//
// !DESCRIPTION:
//      Gets a {\tt Time}'s values in user-specified format. This version
//      supports native C++ use.
//
//EOP
// !REQUIREMENTS:  
//
//    // TODO
//    return(ESMF_SUCCESS);
//
// }  // end Time::get

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  Time::set - Set a Time value (1)
//
// !INTERFACE:
//      int Time::set(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//      Calendar *calendar,  // in - associated calendar
//      int timeZone,             // in - timezone
//      const char *timeList,     // in - initializer specifier string
//      ...) {                    // in - specifier values (variable args)
//
// !DESCRIPTION:
//      Initialzes a {\tt Time} with values given in variable arg list.
//      Supports native C++ use.
//
//EOP
// !REQUIREMENTS:  
//
//    // set calendar kind
//    if (calendar != ESMC_NULL_POINTER) {
//      this->calendar = calendar;
//    }
//
//    // set timezone
//    this->timeZone = timeZone;
//
//    // TODO timeList
//
//    return(ESMF_SUCCESS);
//
// }  // end Time::set

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  Time::set - Set a Time value (2)
//
// !INTERFACE:
//      int Time::set(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//      const char *timeList,    // in - initializer specifier string
//      ...) {                   // in - specifier values (variable args)
//
// !DESCRIPTION:
//      Initialzes a {\tt Time} with values given in variable arg list.
//      Supports native C++ use.
//
//EOP
// !REQUIREMENTS:  
//
//    // TODO
//    return(ESMF_SUCCESS);
//
// }  // end Time::set

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  Time::set - direct property initializer
//
// !INTERFACE:
      int Time::set(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_I8 s,          // in - integer seconds
      ESMC_I8 sN,         // in - fractional seconds, numerator
      ESMC_I8 sD,         // in - fractional seconds, denominator
      Calendar *calendar, // in - associated calendar
      ESMC_CalKind_Flag calkindflag, // in - associated calendar kind
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
 #define ESMC_METHOD "ESMCI::Time::set(direct)"

    int rc = ESMF_SUCCESS;

    // use base class Set()
    rc = BaseTime::set(s, sN, sD);
    if (ESMC_LogDefault.MsgFoundError(rc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc))
      return(rc);

    // set calendar kind
    this->calendar = ESMC_NULL_POINTER;  // to detect invalid, unset time
                                         // TODO: replace with ESMC_Base logic

    if (calendar != ESMC_NULL_POINTER) {                // 1st choice
      // set to user's calendar
      this->calendar = calendar;

    } else if (calkindflag != (ESMC_CalKind_Flag)0) {  // 2nd choice
      // set to specified built-in type; create if necessary
      rc = ESMCI_CalendarCreate(calkindflag);
      if (ESMC_LogDefault.MsgFoundError(rc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        &rc))
        return(rc);
      this->calendar = Calendar::internalCalendar[calkindflag-1];
    } // otherwise leave NULL, TODO: implement ESMC_Base logic, then can
      // set default calendar

    this->timeZone = timeZone;

    return(ESMF_SUCCESS);

 }  // end Time::set

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  Time::isLeapYear - Determines if this time is in a leap year
//
// !INTERFACE:
      bool Time::isLeapYear(
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
 #define ESMC_METHOD "ESMCI::Time::isLeapYear()"

    // must have a calendar
    if (this->calendar == ESMC_NULL_POINTER) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_OBJ_INIT,
        "; calendar required.", ESMC_CONTEXT, rc);
      return(false);
    }

    // get the year of this time
    ESMC_I8 yy_i8;
    int rc2 = Time::get((ESMC_I4 *)ESMC_NULL_POINTER, &yy_i8);
    if (ESMC_LogDefault.MsgFoundError(rc2, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      rc)) {
      return(false);
    }

    // check if it's a leap year within this calendar
    return(this->calendar->isLeapYear(yy_i8, rc));

 }  // end Time::isLeapYear

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  Time::isSameCalendar - Compares 2 Time's Calendar kinds
//
// !INTERFACE:
      bool Time::isSameCalendar(
//
// !RETURN VALUE:
//    bool true if same calendars, false if different calendars
//
// !ARGUMENTS:
      const Time *time,    // in  - Time to compare Calendar kinds against
      int *rc) const {          // out - return code
//
// !DESCRIPTION:
//      Compares given {\tt Time}'s {\tt Calendar} type with this {\tt Time}'s
//      {\tt Calendar} type
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::Time::isSameCalendar()"

    if (this->calendar != ESMC_NULL_POINTER &&
        time->calendar != ESMC_NULL_POINTER)
    {
      if (rc != ESMC_NULL_POINTER) *rc = ESMF_SUCCESS;
      return(this->calendar->calkindflag == time->calendar->calkindflag);
    }
    else {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_OBJ_INIT,
        "; calendar required on both time1 and time2.", ESMC_CONTEXT, rc);
      return(false);
    }

 }  // end Time::isSameCalendar

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  Time::syncToRealTime - Sync this Time to wall clock time
//
// !INTERFACE:
      int Time::syncToRealTime(void) {
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

// TODO: Add optional calendar/calkindflag arguments; default to 
//       Gregorian if not specified and no calendar within this given Time.

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::Time::syncToRealTime()"

    int rc = ESMF_SUCCESS;

    // validate for calendar kind
    if (this->calendar == ESMC_NULL_POINTER) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_OBJ_INIT,
        "; calendar required.", ESMC_CONTEXT, &rc);
      return(rc);
    }

    if (this->calendar->calkindflag == ESMC_CALKIND_JULIANDAY ||
        this->calendar->calkindflag == ESMC_CALKIND_MODJULIANDAY ||
        this->calendar->calkindflag == ESMC_CALKIND_NOCALENDAR) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_WRONG,
        "; calkindflag is JULIANDAY, "
        "MODJULIANDAY or NOCALENDAR.", ESMC_CONTEXT, &rc);
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
    Calendar *cal = this->calendar; // allows reset
    int tz = this->timeZone;             //   after Set() clears it
    rc = Time::set((ESMC_I4 *)ESMC_NULL_POINTER, &yy_i8, &mm, &dd,
                 ESMC_NULL_POINTER, ESMC_NULL_POINTER, &h, &m,
                 &s, ESMC_NULL_POINTER, ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                 ESMC_NULL_POINTER, ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                 ESMC_NULL_POINTER, ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                 ESMC_NULL_POINTER, ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                 ESMC_NULL_POINTER, ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                 &cal, ESMC_NULL_POINTER, &tz);

    ESMC_LogDefault.MsgFoundError(rc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc);

    return(rc);

 }  // end Time::syncToRealTime

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  Time(+) - Increment a Time with a TimeInterval
//
// !INTERFACE:
      Time Time::operator+(
//
// !RETURN VALUE:
//    Time result
//
// !ARGUMENTS:
      const TimeInterval &timeinterval) const {  // in - TimeInterval
                                                 //      to add
//
// !DESCRIPTION:
//    Adds {\tt timeinterval} expression to this time.
//
//EOP
// !REQUIREMENTS:  

//    Implementation note:  This overrides the ESMCI::Fraction (+) operator
//                          in order to copy the Time-only properties
//                          (calendar & timeZone) to the result. 
//
    // delegate the increment operation to my calendar associate
    return(calendar->increment(this, timeinterval));

}  // end Time::operator+

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  Time(-) - Decrement a Time with a TimeInterval
//
// !INTERFACE:
      Time Time::operator-(
//
// !RETURN VALUE:
//    Time result
//
// !ARGUMENTS:
      const TimeInterval &timeinterval) const {  // in - TimeInterval
                                                      //      to subtract
//
// !DESCRIPTION:
//    Subtracts {\tt timeinterval} expression from this time.
//
//EOP
// !REQUIREMENTS:  

//    Implementation note:  This overrides the ESMCI::Fraction (-) operator
//                          in order to copy the Time-only properties
//                          (calendar & timeZone) to the result. 
//
    // delegate the decrement operation to my calendar associate
    return(calendar->decrement(this, timeinterval));

}  // end Time::operator-

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  Time(+=) - Increment a Time with a TimeInterval
//
// !INTERFACE:
      Time& Time::operator+=(
//
// !RETURN VALUE:
//    Time& result
//
// !ARGUMENTS:
      const TimeInterval &timeinterval) {  // in - TimeInterval
                                                //      to add
//
// !DESCRIPTION:
//    Adds {\tt timeinterval} expression to this time.
//
//EOP
// !REQUIREMENTS:  

    // delegate the increment operation to my calendar associate
    *this = calendar->increment(this, timeinterval);

    return(*this);

}  // end Time::operator+=

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  Time(-=) - Decrement a Time with a TimeInterval
//
// !INTERFACE:
      Time& Time::operator-=(
//
// !RETURN VALUE:
//    Time& result
//
// !ARGUMENTS:
      const TimeInterval &timeinterval) {  // in - TimeInterval
                                                //      to subtract
//
// !DESCRIPTION:
//    Adds {\tt timeinterval} expression to this time.
//
//EOP
// !REQUIREMENTS:  

    // delegate the derement operation to my calendar associate
    *this = calendar->decrement(this, timeinterval);

    return(*this);

}  // end Time::operator-=

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  Time(-) - Return the difference between two Times
//
// !INTERFACE:
      TimeInterval Time::operator-(
//
// !RETURN VALUE:
//    TimeInterval result
//
// !ARGUMENTS:
      const Time &time) const {  // in - Time to subtract
//
// !DESCRIPTION:
//    Subtracts given {\tt time} expression from this time, returns
//    result as {\tt ESMC\_TimeInterval}.
//
//EOP
// !REQUIREMENTS:  

//    Implementation note:  This overrides the 2nd ESMCI::Fraction (-) operator
//                          simply because the 1st (-) operator is overridden.
//                          Visibility into ESMCI::Fraction is lost; if not
//                          defined here, the compiler won't see the 2nd (-)
//                          operator defined at ESMCI::Fraction!

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::Time::operator-(time)"

    // the calendars of the two times must be the same
    int rc;
    if (!Time::isSameCalendar(&time, &rc)) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SAMETYPE,
        "; The calendars of the two times to difference are not the same", 
        ESMC_CONTEXT, &rc);
      return(rc);
    }

    // given times' calendars are the same, so set difference time interval's
    //   calendar to be the same
    TimeInterval diff;
    diff.calendar = this->calendar;

    // perform the difference using the ESMCI::Fraction operator
    diff = Fraction::operator-(time);

    return(diff);

}  // end Time::operator-

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  Time::readRestart - restore Time state
//
// !INTERFACE:
      int Time::readRestart(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int          nameLen,   // in
      const char  *name) {    // in   
//
// !DESCRIPTION:
//      restore {\tt Time} state for persistence/checkpointing.
//
//EOP
// !REQUIREMENTS:  

    int rc = ESMF_SUCCESS;

    // TODO:  read time state from name, then restore
    //        (share code with Time::set()).

    // TODO: use base class ReadRestart() first
    // rc = BaseTime::readRestart(s, sN, sD);

    return(rc);

 }  // end Time::readRestart

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  Time::writeRestart - save Time state
//
// !INTERFACE:
      int Time::writeRestart(void) const {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      Save {\tt Time} state for persistence/checkpointing
//
//EOP
// !REQUIREMENTS:  

    int rc = ESMF_SUCCESS;

    // TODO: use base class Write() first
    //  rc = BaseTime::writeRestart(s, sN, sD);

    // calendar= this->calendar;  // TODO?: this only saves calendar pointer;
                               //  component must be sure to save corresponding
                               //  calendar.
    return(rc);

 }  // end Time::writeRestart

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  Time::validate - validate Time state
//
// !INTERFACE:
      int Time::validate(
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
 #define ESMC_METHOD "ESMCI::Time::validate()"

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
            ESMC_LogDefault.MsgFoundError(rc,"; calendar is NULL", 
            ESMC_CONTEXT, &rc);
          return(rc);
        }
        rc = this->calendar->validate();
        ESMC_LogDefault.MsgFoundError(rc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
          &rc);
        return(rc);

      // validate timezone only, not time values
      } else if (strncmp(options, "timezone", 8) == 0) {
        // TODO: valid Timezones ?
        return(rc);
      }
    }

    rc = BaseTime::validate();
    if (ESMC_LogDefault.MsgFoundError(rc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc))
      return(rc);

    if (this->calendar == ESMC_NULL_POINTER) {
      rc = ESMC_RC_PTR_NULL;
      if (!check_initialized)
        ESMC_LogDefault.MsgFoundError(rc, "; calendar is NULL", ESMC_CONTEXT,
          &rc);
      return(rc);
    }
    rc = this->calendar->validate();
    if (ESMC_LogDefault.MsgFoundError(rc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        &rc))
      return(rc);

    // earliest Gregorian date representable by the Fliegel algorithm
    //  is -4800/3/1 == -32044 Julian days == -2,768,601,600 core seconds
    if (calendar->calkindflag == ESMC_CALKIND_GREGORIAN &&
        getw() < -2768601600LL) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_OBJ_BAD,
        "; Gregorian time is before -4800/3/1", ESMC_CONTEXT, &rc);
      return(rc);
    }

    // earliest Julian date representable by the Hatcher algorithm
    //  is -4712/3/1 == 60 Julian days == 5,184,000 core seconds
    if (calendar->calkindflag == ESMC_CALKIND_JULIAN &&
        getw() < 5184000LL) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_OBJ_BAD,
        "; Julian time is before -4712/3/1", ESMC_CONTEXT, &rc);
      return(rc);
    }

    // TODO: other calendar ranges ?

    // TODO: valid Timezones ?

    return(rc);

 }  // end Time::validate

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  Time::print - print Time state
//
// !INTERFACE:
      int Time::print(
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
        Time::getString(timeString, &options[6]);
        printf("%s\n", timeString);
        // see also method Time::get()
      }
    } else {
      // default
      BaseTime::print(options);
      if (this->calendar != ESMC_NULL_POINTER) {
        if (this->calendar->calkindflag != ESMC_CALKIND_NOCALENDAR) {
          this->calendar->print(options, this);
        }
      }
      printf("timeZone = %d\n", timeZone);
    }

    printf("end Time -------------------------------\n\n");

    return(ESMF_SUCCESS);

 }  // end Time::print

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  Time - native default C++ constructor
//
// !INTERFACE:
      Time::Time(void) {
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

//   BaseTime(0,0,1) {  // TODO: F90 issue with base class constructor?
   Fraction::set(0,0,1);  // set seconds = 0
                          // set fractional seconds numerator = 0
                          // set fractional seconds denominator = 1
   calendar = ESMC_NULL_POINTER;  // to detect invalid, unset time
                                  // TODO: replace with ESMC_Base logic
   timeZone = 0;

 }  // end Time

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  Time - native C++ constructor
//
// !INTERFACE:
      Time::Time(
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
      ESMC_I8 s,           // in - integer seconds
      ESMC_I8 sN,          // in - fractional seconds, numerator
      ESMC_I8 sD,          // in - fractional seconds, denominator
      Calendar *calendar,  // in - associated calendar
      ESMC_CalKind_Flag calkindflag,  // in - associated calendar kind
      int timeZone) :           // in - timezone
//
// !DESCRIPTION:
//      Initializes a {\tt ESMC\_Time}
//
//EOP
// !REQUIREMENTS:  

   BaseTime(s, sN, sD) {   // use base class constructor

  // set calendar kind
  this->calendar = ESMC_NULL_POINTER;  // to detect invalid, unset time
                                       // TODO: replace with ESMC_Base logic

  if (calendar != ESMC_NULL_POINTER) {                // 1st choice
    // set to user's calendar
    this->calendar = calendar;

  } else if (calkindflag != (ESMC_CalKind_Flag)0) {  // 2nd choice
    // set to specified built-in type; create if necessary
    ESMCI_CalendarCreate(calkindflag);
    this->calendar = Calendar::internalCalendar[calkindflag-1];
  } // otherwise leave NULL, TODO: implement ESMC_Base logic, then can
    // set default calendar

  // TODO: catch & throw exceptions above, and/or LogErr

  this->timeZone = timeZone;

 }  // end Time

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ~Time - native default C++ destructor
//
// !INTERFACE:
      Time::~Time(void) {
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

 }  // end ~Time

//-------------------------------------------------------------------------
//  Public methods to support Time::get() API
//-------------------------------------------------------------------------

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  Time::getString - Get a Time value
//
// !INTERFACE:
      int Time::getString(
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
//      Supports {\tt ESMC\_Time::get()} and {\tt ESMC\_Time::print()}.
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::Time::getString()"

    int rc = ESMF_SUCCESS;

    // validate inputs
    if (timeString == ESMC_NULL_POINTER) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
        "; timeString is NULL", ESMC_CONTEXT, &rc);
      return(rc);
    }
    if (this->calendar == ESMC_NULL_POINTER) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_OBJ_INIT,
        "; calendar required", ESMC_CONTEXT, &rc);
      return(rc);
    }
    if (this->calendar->calkindflag == ESMC_CALKIND_JULIANDAY ||
        this->calendar->calkindflag == ESMC_CALKIND_MODJULIANDAY ||
        this->calendar->calkindflag == ESMC_CALKIND_NOCALENDAR) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_OBJ_BAD,
        "; calkindflag is JULIANDAY, "
        "MODJULIANDAY or NOCALENDAR.", ESMC_CONTEXT, &rc);
      return(rc);
    }

    ESMC_I8 yy_i8;
    int mm, dd;
    ESMC_I4 h, m, s;
    ESMC_I8 sN, sD;

    // TODO: use native C++ Get, not F90 entry point, when ready
    rc = Time::get((ESMC_I4 *)ESMC_NULL_POINTER, &yy_i8, &mm, &dd,
                  ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                  &h, &m, &s, ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                              ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                              ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                              ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                              ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                              ESMC_NULL_POINTER, 
                              ESMC_NULL_POINTER, &sN,
                              ESMC_NULL_POINTER, &sD);
    if (ESMC_LogDefault.MsgFoundError(rc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc))
      return(rc);

    // format everything except seconds
    sprintf(timeString, "%04lld-%02d-%02dT%02d:%02d:", yy_i8, mm, dd, h, m);

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
        sprintf(timeString, "%s%012.9f", timeString, (s + fractionalSeconds));
      } else { // no fractional seconds, just append integer seconds
        sprintf(timeString, "%s%02d", timeString, s);
      }
    } else { // not strict ISO fractional seconds format
      // hybrid ISO 8601 format YYYY-MM-DDThh:mm:ss[:n/d]

      // if fractionalSeconds non-zero (sN!=0) append full fractional value
      if (sN != 0) {
        sprintf(timeString, "%s%02d:%lld/%lld", timeString, s, sN, sD);
      } else { // no fractional seconds, just append integer seconds
        sprintf(timeString, "%s%02d", timeString, s);
      }
    }

    return(rc);

 }  // end Time::getString

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  Time::getDayOfWeek - Get a Time's day of the week value
//
// !INTERFACE:
      int Time::getDayOfWeek(
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
 #define ESMC_METHOD "ESMCI::Time::getDayOfWeek()"

    int rc = ESMF_SUCCESS;

    // validate inputs
    if (dayOfWeek == ESMC_NULL_POINTER) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
        "; dayOfWeek is NULL", ESMC_CONTEXT, &rc);
      return(rc);
    }
    if (this->calendar == ESMC_NULL_POINTER) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_OBJ_INIT,
        "; calendar required", ESMC_CONTEXT, &rc);
      return(rc);
    }

    // date variables
    ESMC_I8 yy_i8;
    int mm, dd;

    //  The day of the week is simply modulo 7 from a reference date,
    //  adjusted for a 1-based count and negative deltas.
    //  The reference date is any known Monday (day of the week = 1)
    //  This method is valid for any calendar which uses 7-day weeks.

    switch (this->calendar->calkindflag)
    {
        case ESMC_CALKIND_GREGORIAN:
        case ESMC_CALKIND_NOLEAP:    // TODO: ?
        case ESMC_CALKIND_360DAY:    // TODO: ?
          //  Can be any Monday after the Gregorian reformation of 10/15/1582
          yy_i8=1796; mm=7; dd=4;   // America's 20th birthday was a Monday !
          break;

        case ESMC_CALKIND_JULIAN:
        case ESMC_CALKIND_JULIANDAY:
        case ESMC_CALKIND_MODJULIANDAY:
          //  Can be any Monday before the Julian end of 10/4/1582
          yy_i8=1492; mm=10; dd=29;  // Columbus landed in Cuba on a Monday !
          break;

        case ESMC_CALKIND_NOCALENDAR:
        case ESMC_CALKIND_CUSTOM:
        default:
          ESMC_LogDefault.MsgFoundError(ESMC_RC_OBJ_BAD,
            "; calkindflag is NOCALENDAR, CUSTOM, or unrecognized.",
            ESMC_CONTEXT, &rc);
          return(rc);
          break;
    }

    // TODO: put the above reference dates into a pre-initialized lookup table
    //       to skip this step
    Time referenceMonday;
    // TODO: use native C++ Set() when ready
    Calendar *cal = this->calendar; // allows reset
    int tz = this->timeZone;             //   after Set() clears it
    rc = referenceMonday.Time::set((ESMC_I4 *)ESMC_NULL_POINTER,
                                      &yy_i8, &mm, &dd, ESMC_NULL_POINTER,
                                      ESMC_NULL_POINTER, ESMC_NULL_POINTER,
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

    if (ESMC_LogDefault.MsgFoundError(rc, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, &rc))
      return(rc);

    // calculate the difference in days between the given date and
    //  the reference date
    TimeInterval delta;
    delta = *this - referenceMonday;
    delta.calendar = this->calendar;
    ESMC_I8 diffDays;
    // TODO: use native C++ Get() when ready
    rc = delta.TimeInterval::get((ESMC_I4 *)ESMC_NULL_POINTER,
                                     ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                     ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                     &diffDays);

    if (ESMC_LogDefault.MsgFoundError(rc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc))
      return(rc);

    // calculate day of the week as simply modulo 7 from the reference date,
    //  adjusted for a 1-based count and negative deltas
    int mod7 = diffDays % 7;  // (-6 to 0) or (0 to 6)
    if (mod7 < 0) mod7 += 7;  // ensure positive (0 to 6) range
    *dayOfWeek = mod7 + 1;    // adjust to 1-based count (1 to 7)

    return(rc);

 }  // end Time::getDayOfWeek

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  Time::getMidMonth - Get a Time's middle of the month value
//
// !INTERFACE:
      int Time::getMidMonth(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      Time *midMonth) const {    // out - time's middle of month value
//
// !DESCRIPTION:
//      Gets a {\tt Time}'s middle of the month value
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::Time::getMidMonth()"

    int rc = ESMF_SUCCESS;

    // validate inputs
    if (midMonth == ESMC_NULL_POINTER) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
        "; midMonth is NULL", ESMC_CONTEXT, &rc);
      return(rc);
    }
    if (this->calendar == ESMC_NULL_POINTER) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_OBJ_INIT,
        "; calendar required", ESMC_CONTEXT, &rc);
      return(rc);
    }
    if (this->calendar->calkindflag == ESMC_CALKIND_JULIANDAY ||
        this->calendar->calkindflag == ESMC_CALKIND_MODJULIANDAY ||
        this->calendar->calkindflag == ESMC_CALKIND_NOCALENDAR) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_OBJ_BAD,
        "; calkindflag is JULIANDAY, "
        "MODJULIANDAY or NOCALENDAR.", ESMC_CONTEXT, &rc);
      return(rc);
    }

    // TODO: use table lookup per calendar kind (14, 14.5, 15, 15.5 days) ?

    // TODO: use native C++ Get()/Set() when ready

    // get this date
    ESMC_I8 yy_i8;
    int mm, dd;
    rc = Time::get((ESMC_I4 *)ESMC_NULL_POINTER, &yy_i8, &mm, &dd);
    if (ESMC_LogDefault.MsgFoundError(rc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc))
      return(rc);

    // set start of this month
    dd = 1;
    Time startOfMonth;
    Calendar *cal = this->calendar; // allows reset
    int tz = this->timeZone;             //   after Set() clears it
    rc = startOfMonth.Time::set((ESMC_I4 *)ESMC_NULL_POINTER,
                                   &yy_i8, &mm, &dd, ESMC_NULL_POINTER,
                                   ESMC_NULL_POINTER, ESMC_NULL_POINTER,
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

    if (ESMC_LogDefault.MsgFoundError(rc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc))
      return(rc);

    // set end of this month (start of next month)
    // TODO: use calendar interval logic when ready
    dd = 1;
    mm++;
    if (mm > MONTHS_PER_YEAR) {
      mm = 1;
      yy_i8++;
    }
    Time endOfMonth;
    rc = endOfMonth.Time::set((ESMC_I4 *)ESMC_NULL_POINTER,
                                 &yy_i8, &mm, &dd, ESMC_NULL_POINTER,
                                 ESMC_NULL_POINTER, ESMC_NULL_POINTER,
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

    if (ESMC_LogDefault.MsgFoundError(rc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, 
      &rc))
      return(rc);

    // size of this month
    TimeInterval month;
    month = endOfMonth - startOfMonth;

    // calculate and return the middle of this month
    *midMonth = startOfMonth;  // initialize calendar & timezone
    *midMonth = startOfMonth + month/2;

    // TODO: add C++ infrastructure to enable the following expression:
    // *midMonth = startOfMonth + (endOfMonth - startOfMonth)/2;

    return(rc);

 }  // end Time::getMidMonth

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  Time::getDayOfYear - Get a Time's day of the year value
//
// !INTERFACE:
      int Time::getDayOfYear(
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
 #define ESMC_METHOD "ESMCI::Time::getDayOfYear(integer)"

    int rc = ESMF_SUCCESS;

    // validate inputs
    if (dayOfYear == ESMC_NULL_POINTER) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
        "; dayOfYear is NULL", ESMC_CONTEXT, &rc);
      return(rc);
    }
    if (this->calendar == ESMC_NULL_POINTER) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_OBJ_INIT,
        "; calendar required", ESMC_CONTEXT, &rc);
      return(rc);
    }
    if (this->calendar->calkindflag == ESMC_CALKIND_JULIANDAY ||
        this->calendar->calkindflag == ESMC_CALKIND_MODJULIANDAY ||
        this->calendar->calkindflag == ESMC_CALKIND_NOCALENDAR) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_OBJ_BAD,
        "; calkindflag is JULIANDAY, "
        "MODJULIANDAY or NOCALENDAR.", ESMC_CONTEXT, &rc);
      return(rc);
    }
    
    // get day of year as time interval between now and 1/1/yy
    TimeInterval yearDay;
    rc = Time::getDayOfYear(&yearDay);
    if (ESMC_LogDefault.MsgFoundError(rc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc))
      return(rc);

    // get difference in integer days
    ESMC_I8 diffDays;
    // TODO: use native C++ Get, not F90 entry point
    rc = yearDay.TimeInterval::get((ESMC_I4 *)ESMC_NULL_POINTER,
                                      ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                      ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                      &diffDays);

    if (ESMC_LogDefault.MsgFoundError(rc, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, &rc))
      return(rc);

    // day-of-year is one-based count; i.e. day-of-year for 1/1/yy is 1
    *dayOfYear = (int) diffDays + 1;

    return(rc);

 }  // end Time::getDayOfYear

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  Time::getDayOfYear - Get a Time's day of the year value
//
// !INTERFACE:
      int Time::getDayOfYear(
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
 #define ESMC_METHOD "ESMCI::Time::getDayOfYear(ESMC_R8)"

    int rc = ESMF_SUCCESS;

    // validate inputs
    if (dayOfYear == ESMC_NULL_POINTER) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
        "; dayOfYear is NULL", ESMC_CONTEXT, &rc);
      return(rc);
    }
    if (this->calendar == ESMC_NULL_POINTER) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_OBJ_INIT,
        "; calendar required", ESMC_CONTEXT, &rc);
      return(rc);
    }
    if (this->calendar->calkindflag == ESMC_CALKIND_JULIANDAY ||
        this->calendar->calkindflag == ESMC_CALKIND_MODJULIANDAY ||
        this->calendar->calkindflag == ESMC_CALKIND_NOCALENDAR) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_OBJ_BAD,
        "; calkindflag is JULIANDAY, "
        "MODJULIANDAY or NOCALENDAR.", ESMC_CONTEXT, &rc);
      return(rc);
    }

    // get day of year as time interval between now and 1/1/yy
    TimeInterval yearDay;
    rc = Time::getDayOfYear(&yearDay);
    if (ESMC_LogDefault.MsgFoundError(rc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc))
      return(rc);

    // get difference in floating point days
    ESMC_R8 diffDays;
    // TODO: use native C++ Get, not F90 entry point
    rc = yearDay.TimeInterval::get((ESMC_I4 *)ESMC_NULL_POINTER,
                                      ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                      ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                      ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                      ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                      ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                      ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                      &diffDays);

    if (ESMC_LogDefault.MsgFoundError(rc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc))
      return(rc);

    // day-of-year is one-based count; i.e. day-of-year for 1/1/yy is 1
    *dayOfYear = diffDays + 1;

    return(rc);

 }  // end Time::getDayOfYear

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  Time::getDayOfYear - Get a Time's day of the year value
//
// !INTERFACE:
      int Time::getDayOfYear(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      TimeInterval *dayOfYear) const {   // out - time's day of year value
//
// !DESCRIPTION:
//      Gets a {\tt Time}'s day of the year value as an {\tt TimeInterval}
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::Time::getDayOfYear(timeinterval)"

    int rc = ESMF_SUCCESS;

    // validate inputs
    if (dayOfYear == ESMC_NULL_POINTER) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
        "; dayOfYear is NULL", ESMC_CONTEXT, &rc);
      return(rc);
    }
    if (this->calendar == ESMC_NULL_POINTER) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_OBJ_INIT,
        "; calendar required", ESMC_CONTEXT, &rc);
      return(rc);
    }
    if (this->calendar->calkindflag == ESMC_CALKIND_JULIANDAY ||
        this->calendar->calkindflag == ESMC_CALKIND_MODJULIANDAY ||
        this->calendar->calkindflag == ESMC_CALKIND_NOCALENDAR) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_OBJ_BAD,
        "; calkindflag is JULIANDAY, "
        "MODJULIANDAY or NOCALENDAR.", ESMC_CONTEXT, &rc);
      return(rc);
    }

    // get year of our (this) time
    ESMC_I8 yy_i8;
    int mm, dd;
    // TODO: use native C++ Get, not F90 entry point
    rc = Time::get((ESMC_I4 *)ESMC_NULL_POINTER, &yy_i8, &mm, &dd);
    if (ESMC_LogDefault.MsgFoundError(rc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc))
      return(rc);

    // create time for 1/1/yy
    Time dayOne;
    mm=1, dd=1;
    // TODO: use native C++ Set(), not F90 entry point
    Calendar *cal = this->calendar; // allows reset
    int tz = this->timeZone;             //   after Set() clears it
    rc = dayOne.Time::set((ESMC_I4 *)ESMC_NULL_POINTER,
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
                             ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                             &cal, ESMC_NULL_POINTER, &tz);

    if (ESMC_LogDefault.MsgFoundError(rc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc))
      return(rc);

    // calculate difference between 1/1/yy and our (this) time
    *dayOfYear = *this - dayOne;
    dayOfYear->calendar = this->calendar;
    
    return(rc);

 }  // end Time::getDayOfYear

}   // namespace ESMCI
