// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2021, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
// ESMC TimeInterval method code (body) file
//
//-------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ {\tt ESMC\_TimeInterval} methods
// declared in the companion file {\tt ESMCI_TimeInterval.h}.
//
//-------------------------------------------------------------------------
#define ESMC_FILENAME "ESMCI_TimeInterval.C"

// associated class definition file
#include "ESMCI_TimeInterval.h"

// higher level, 3rd party or system includes
#include <stdio.h>
#include <math.h>
#include <limits.h>
#include <float.h>
#include <string.h>

#include "ESMCI_LogErr.h"
#include "ESMCI_Time.h"
#include "ESMCI_Fraction.h"
#include "ESMCI_Calendar.h"

//-------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-------------------------------------------------------------------------

//
//-------------------------------------------------------------------------
//-------------------------------------------------------------------------
//
// This section includes all the TimeInterval routines
//
//

//-------------------------------------------------------------------------
// Class TimeInterval Methods
//-------------------------------------------------------------------------

namespace ESMCI{
//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  TimeInterval::set - initializer to support F90 interface
//
// !INTERFACE:
      int TimeInterval::set(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_I4 *yy,        // in - integer number of interval years
                               //                           (>= 32-bit)
      ESMC_I8 *yy_i8,     // in - integer number of interval years
                               //                           (large, >= 64-bit)
      ESMC_I4 *mm,        // in - integer number of interval months
                               //                           (>= 32-bit)
      ESMC_I8 *mm_i8,     // in - integer number of interval months
                               //                           (large, >= 64-bit)
      ESMC_I4 *d,         // in - integer number of interval days
                               //                           (>= 32-bit)
      ESMC_I8 *d_i8,      // in - integer number of interval days
                               //                           (large, >= 64-bit)
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
      Time *startTime,    // in - starting time for absolute calendar
                               //      interval
      Time *endTime,      // in - ending time for absolute calendar
                               //      interval
      Calendar **calendar, // in - calendar for calendar interval
      ESMC_CalKind_Flag *calkindflag) { // in - calendar kind for calendar interval
//
// !DESCRIPTION:
//      Initialzes a {\tt ESMC\_TimeInterval} with values given in F90
//      variable arg list.
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::TimeInterval::set()"

    // TODO: Since TimeInterval is a shallow statically allocated class,
    //       ensure initialization if called via F90 interface;
    //       cannot call constructor, because destructor is subsequently
    //       called automatically, returning initialized values to garbage.

    int rc = ESMF_SUCCESS;

    // save current value to restore in case of failure
    TimeInterval saveTimeInterval = *this;

    Fraction::set(0,0,1);  // set seconds = 0
                           // set fractional seconds numerator = 0
                           // set fractional seconds denominator = 1
    this->yy = 0;
    this->mm = 0;
    this->d  = 0;
    this->d_r8 = 0.0;
    this->startTime.Time::set((ESMC_I8) 0); // |
    this->endTime.Time::set((ESMC_I8) 0);   //  > init to invalid, unset
    this->calendar = ESMC_NULL_POINTER;             // |    state
                                                    // TODO: replace with
                                                    //       ESMC_Base logic
    
    // TODO: validate inputs (individual and combos), set basetime values
    //       e.g. integer and float specifiers are mutually exclusive

    // absolute, i.e. calendar-specific interval
    if (startTime != ESMC_NULL_POINTER) {
      this->startTime = *startTime;
      this->calendar = startTime->calendar;              // 1st choice
    }
    if (endTime != ESMC_NULL_POINTER) {
      if (startTime != ESMC_NULL_POINTER) {
        // when both are given, check that their calendars are the same
        if (!startTime->Time::isSameCalendar(endTime)) {
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SAMETYPE,
            "; startTime & endTime calendars not the same", ESMC_CONTEXT, &rc);
          *this = saveTimeInterval; return(rc);
        }
      }
      this->endTime = *endTime;
      this->calendar = endTime->calendar;                // 2nd choice
    }

    // set specified calendar kind if neither startTime nor endTime specified
    if (startTime == ESMC_NULL_POINTER && endTime == ESMC_NULL_POINTER) {
      if (calendar != ESMC_NULL_POINTER) {
        // set to user's calendar
        this->calendar = *calendar;                      // 3rd choice

      } else if (calkindflag != ESMC_NULL_POINTER) {
        // set to specified built-in type; create if necessary
        rc = ESMCI_CalendarCreate(*calkindflag);
        if (ESMC_LogDefault.MsgFoundError(rc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
          &rc))
          { *this = saveTimeInterval; return(rc); }
        this->calendar = Calendar::internalCalendar[*calkindflag-1];
                                                         // 4th choice

      } else if (Calendar::defaultCalendar != ESMC_NULL_POINTER) {
        // use default calendar
        this->calendar = Calendar::defaultCalendar; // 5th choice

      } else {
        // create default calendar
        rc = ESMCI_CalendarSetDefault((ESMC_CalKind_Flag *)ESMC_NULL_POINTER);
        if (ESMC_LogDefault.MsgFoundError(rc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
          &rc))
          { *this = saveTimeInterval; return(rc); }
        this->calendar = Calendar::defaultCalendar; // 6th choice
      }
    }

    // Note:  No matter what calendar is associated with the time interval,
    // the time interval can still "float" across all calendars.  A calendar
    // is used only at the point of evaluation (when performing a unit
    // conversion, arithmetic operation, or comparison), so the time interval's
    // own calendar property can be overridden at that point by a subject
    // Time's calendar or a method argument.

    // hold yy, mm, d parts as relative calendar interval to allow "float"
    // across all calendars (see note above).
    if (yy != ESMC_NULL_POINTER) {
      this->yy = *yy;  // >= 32-bit
    } else if (yy_i8 != ESMC_NULL_POINTER) {
      this->yy = *yy_i8; // >= 64-bit
    }
    if (mm != ESMC_NULL_POINTER) {
      this->mm = *mm;  // >= 32-bit
    } else if (mm_i8 != ESMC_NULL_POINTER) {
      this->mm = *mm_i8; // >= 64-bit
    }
    if (d != ESMC_NULL_POINTER) {
      this->d = *d;  // >= 32-bit
    } else if (d_i8 != ESMC_NULL_POINTER) {
      this->d = *d_i8; // >= 64-bit
    }
    if (d_r8 != ESMC_NULL_POINTER) {
      this->d_r8 = *d_r8;
    }

    // use base class set for sub-day values
    rc = BaseTime::set(h, m, s, s_i8, ms, us, ns, h_r8, m_r8, s_r8,
                          ms_r8, us_r8, ns_r8, sN, sN_i8, sD, sD_i8);
    if (ESMC_LogDefault.MsgFoundError(rc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc))
      { *this = saveTimeInterval; return(rc); }

    rc = TimeInterval::validate();
    if (ESMC_LogDefault.MsgFoundError(rc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc))
      { *this = saveTimeInterval; return(rc); }

    return(ESMF_SUCCESS);

 }  // end TimeInterval::set

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  TimeInterval::get - Get a TimeInterval value; supports F90 interface
//
// !INTERFACE:
      int TimeInterval::get(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_I4 *yy,         // out - integer number of interval years
                                //                           (>= 32-bit)
      ESMC_I8 *yy_i8,      // out - integer number of interval years
                                //                           (large, >= 64-bit)
      ESMC_I4 *mm,         // out - integer number of interval months
                                //                           (>= 32-bit)
      ESMC_I8 *mm_i8,      // out - integer number of interval months
                                //                           (large, >= 64-bit)
      ESMC_I4 *d,          // out - integer number of interval days
                                //                           (>= 32-bit)
      ESMC_I8 *d_i8,       // out - integer number of interval days
                                //                           (large, >= 64-bit)
      ESMC_I4 *h,          // out - integer hours
      ESMC_I4 *m,          // out - integer minutes
      ESMC_I4 *s,          // out - integer seconds (>= 32-bit)
      ESMC_I8 *s_i8,       // out - integer seconds (large, >= 64-bit)
      ESMC_I4 *ms,         // out - integer milliseconds
      ESMC_I4 *us,         // out - integer microseconds
      ESMC_I4 *ns,         // out - integer nanoseconds
      ESMC_R8 *d_r8,       // out - floating point days
      ESMC_R8 *h_r8,       // out - floating point hours
      ESMC_R8 *m_r8,       // out - floating point minutes
      ESMC_R8 *s_r8,       // out - floating point seconds
      ESMC_R8 *ms_r8,      // out - floating point milliseconds
      ESMC_R8 *us_r8,      // out - floating point microseconds
      ESMC_R8 *ns_r8,      // out - floating point nanoseconds
      ESMC_I4 *sN,         // out - fractional seconds numerator
      ESMC_I8 *sN_i8,      // out - fractional seconds numerator
                           //                                (large, >= 64-bit)
      ESMC_I4 *sD,         // out - fractional seconds denominator
      ESMC_I8 *sD_i8,      // out - fractional seconds denominator
                           //                                (large, >= 64-bit)
      Time *startTime,     // out - starting time of absolute calendar
                                //       interval
      Time *endTime,       // out - ending time of absolute calendar
                                //       interval
      Calendar **calendar, // out - calendar of calendar interval
      ESMC_CalKind_Flag *calkindflag,  // out - calendar kind of
                                       //       calendar interval
      Time *startTimeIn,   // in  - starting time for calendar interval
                                //       unit conversions
      Time *endTimeIn,     // in  - ending time for calendar interval
                                //       unit conversions
      Calendar **calendarIn, // in  - calendar for calendar interval
                                  //       unit conversions
      ESMC_CalKind_Flag *calkindflagIn,  // in  - calendar kind for calendar
                                         //       interval unit conversions
      int   timeStringLen,             // in  - F90 time string size
      int  *tempTimeStringLen,         // out - temp F90 time string size
      char *tempTimeString,            // out - hybrid format
                                       //       PyYmMdDThHmMs[:n/d]S
      int   timeStringLenISOFrac,          // in  - F90 ISO time string size
      int  *tempTimeStringLenISOFrac,      // out - tmp F90 ISO time string size
      char *tempTimeStringISOFrac) const { // out - ISO 8601 format
                                       //       PyYmMdDThHmMs[.f]S
//
// !DESCRIPTION:
//      Gets a {\tt ESMC\_TimeInterval}'s values in user-specified format.
//      This version supports the F90 interface.
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::TimeInterval::get()"

    // TODO: put calendar logic under test for any non-zero yy, mm, d ?

    // TODO: reduce size of this method by creating seperate methods on
    //       TimeInterval and Calendar ?

    int rc = ESMF_SUCCESS;

    // timeinterval-to-convert.  This is used to reduce this time
    // interval's units, which is later used to to convert to user-requested
    // units.  Represents remaining unconverted time; any years, months or days
    // later requested from timeinterval-to-convert will be removed from
    // timeinterval-to-convert.  In this way, a requested unit is bounded
    // (normalized) by the next higher requested unit.

    TimeInterval tiToConvert = *this;

    //---------------------------------------------------------------------
    // Determine startTime, endTime, and/or calendar, if any, we have to
    //   work with.
    //---------------------------------------------------------------------
    // get any calendar specified in Get() or Set(), on which to perform
    //   calendar (yy, mm, d) conversions
    tiToConvert.calendar = ESMC_NULL_POINTER;

    if (startTimeIn != ESMC_NULL_POINTER) {            // 1st choice
      // use specified startTime
      tiToConvert.calendar = startTimeIn->calendar;

    } else if (endTimeIn != ESMC_NULL_POINTER) {       // 2nd choice
      // use specified endTime
      tiToConvert.calendar = endTimeIn->calendar;

    } else if (calendarIn != ESMC_NULL_POINTER) {      // 3rd choice
      // use specified input calendar
      tiToConvert.calendar = *calendarIn;

    } else if (calkindflagIn != ESMC_NULL_POINTER) {  // 4th choice
      // use specified built-in type; create if necessary
      rc = ESMCI_CalendarCreate(*calkindflag);
      if (ESMC_LogDefault.MsgFoundError(rc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        &rc))
        return(rc);
      tiToConvert.calendar = Calendar::internalCalendar[*calkindflag-1];

    } else if (this->calendar != ESMC_NULL_POINTER) {  // 5th choice
      // use this time interval's calendar property
      tiToConvert.calendar = this->calendar;
    }

    // at least default calendar must have been determined
    if (tiToConvert.calendar == ESMC_NULL_POINTER) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_INCONS,
        ", no default calendar.", ESMC_CONTEXT, &rc); return(rc);
    }

    // if no calendar info, then if any relative calendar unit was Set(),
    //   must Get() it back, otherwise impossible conversion is implied.
    if (tiToConvert.calendar->calkindflag == ESMC_CALKIND_NOCALENDAR) {
      // if yy was set, must get it
      if (this->yy != 0 &&
          (yy == ESMC_NULL_POINTER && yy_i8 == ESMC_NULL_POINTER)) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
            ", must Get() yy or yy_i8, since it was Set() with "
            "ESMC_CALKIND_NOCALENDAR; impossible conversion implied.",
            ESMC_CONTEXT, &rc); return(rc);
      }
      // if mm was set, must get it
      if (this->mm != 0 &&
          (mm == ESMC_NULL_POINTER && mm_i8 == ESMC_NULL_POINTER)) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
            ", must Get() mm or mm_i8, since it was Set() with "
            "ESMC_CALKIND_NOCALENDAR; impossible conversion implied.",
            ESMC_CONTEXT, &rc); return(rc);
      }
      // if d was set, must get it
      if (this->d != 0 &&
          (d == ESMC_NULL_POINTER && d_i8 == ESMC_NULL_POINTER)) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
            ", must Get() d or d_i8, since it was Set() with "
            "ESMC_CALKIND_NOCALENDAR; impossible conversion implied.",
            ESMC_CONTEXT, &rc); return(rc);
      }
      // if d_r8 was set, must get it
      if (this->d_r8 != 0 && (d_r8 == ESMC_NULL_POINTER)) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
            ", must Get() d_r8, since it was Set() with "
            "ESMC_CALKIND_NOCALENDAR; impossible conversion implied.",
            ESMC_CONTEXT, &rc); return(rc);
      }
    }

    // get any startTime and/or endTime specified in Get() or Set()
    tiToConvert.startTime = this->startTime;
    tiToConvert.endTime = this->endTime;
    if (startTimeIn != ESMC_NULL_POINTER) {
      tiToConvert.startTime = *startTimeIn;
    }
    if (endTimeIn != ESMC_NULL_POINTER) {
      tiToConvert.endTime = *endTimeIn;
    }

    //---------------------------------------------------------------------
    // Reduce this time interval's units to the smallest and least number
    //   of units possible (ideally all seconds)
    //---------------------------------------------------------------------

    tiToConvert.TimeInterval::reduce();

    //---------------------------------------------------------------------
    // convert from reduced units to user-requested units, keeping them
    //   bound (normalized) to the next larger requested unit
    //---------------------------------------------------------------------

    // convert from reduced units to any user-requested years
    if (yy != ESMC_NULL_POINTER || yy_i8 != ESMC_NULL_POINTER) {
      // total relative and absolute years
      ESMC_I8 years = tiToConvert.yy;
      tiToConvert.yy = 0;
      switch (tiToConvert.calendar->calkindflag)  
      {
        case ESMC_CALKIND_GREGORIAN:
        case ESMC_CALKIND_JULIAN:
        case ESMC_CALKIND_NOLEAP:
          {
            // TODO: use TimeInterval operators (/) and (-) when ready ?

            // get years using startTime/endTime, if available
            if (tiToConvert.startTime.Time::validate("initialized")
                  == ESMF_SUCCESS ||
                tiToConvert.endTime.Time::validate("initialized")
                  == ESMF_SUCCESS) {
              TimeInterval oneYear(0, 0, 1, 1);
              Time iTime = tiToConvert.startTime + oneYear;
              years = 0;
              while (iTime <= tiToConvert.endTime) {
                years++;
                iTime += oneYear;
              }

              // move tiToConvert.startTime up to end of last year converted
              tiToConvert.startTime = iTime - oneYear;

              // calculate remaining baseTimeToConvert (remove years we got)
              TimeInterval ti = tiToConvert.endTime - tiToConvert.startTime;
              tiToConvert.setw(ti.getw());
              //TODO: tiToConvert = tiToConvert.endTime - tiToConvert.startTime;
              //      should just copy base class part wholesale, rather than
              //      individual properties (including fraction sN/sD); breaks
              //      encapsulation principle.

            } else { // no startTime or endTime available, convert what we can
                     //   from (mm, s)
              years = 0;
              // convert mm for either ESMC_CALKIND_GREGORIAN, 
              // ESMC_CALKIND_JULIAN, or ESMC_CALKIND_NOLEAP
              if (tiToConvert.mm != 0) {
                years = tiToConvert.mm / tiToConvert.calendar->monthsPerYear;
                tiToConvert.mm %= tiToConvert.calendar->monthsPerYear;
              } 

              // convert remaining seconds to years
              if (tiToConvert.calendar->calkindflag == ESMC_CALKIND_NOLEAP) {
                years += tiToConvert.getw() /
                         tiToConvert.calendar->secondsPerYear;
                tiToConvert.setw(tiToConvert.getw() %
                                 tiToConvert.calendar->secondsPerYear);

              } else { // ESMC_CALKIND_GREGORIAN or ESMC_CALKIND_JULIAN
                // note: can't use abs() or labs() since result is (long long)
                //       could use ISO llabs() if supported on all platforms
                if (tiToConvert.getw() >=
                    tiToConvert.calendar->secondsPerYear ||
                    tiToConvert.getw() <=
                   -tiToConvert.calendar->secondsPerYear){
                  // tiToConvert.getw() >= 1 year =>
                  //  can't determine leap years without startTime or endTime !
                  char logMsg[160];
                  sprintf(logMsg, 
                            "yy or yy_i8 because for %s time interval "
                            ">= 1 year, can't determine leap years without "
                            "startTime or endTime.", 
                            Calendar::calkindflagName[
                              tiToConvert.calendar->calkindflag-1]);
                  ESMC_LogDefault.MsgFoundError(ESMC_RC_CANNOT_GET,
                            logMsg, ESMC_CONTEXT, &rc); return(rc);
                }
              }
            }
          }
          break;
        case ESMC_CALKIND_360DAY:
          years = tiToConvert.getw() /
                  tiToConvert.calendar->secondsPerYear;
          tiToConvert.setw(tiToConvert.getw() %
                                        tiToConvert.calendar->secondsPerYear);
          break;
        case ESMC_CALKIND_JULIANDAY:
        case ESMC_CALKIND_MODJULIANDAY:
          // years not defined!
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
                ", years (yy or yy_i8) not defined for ESMC_CALKIND_JULIANDAY "
                "or ESMC_CALKIND_MODJULIANDAY calendars.", ESMC_CONTEXT, &rc);
          return(rc);
        case ESMC_CALKIND_NOCALENDAR:
          // years not defined, but allow for requesting what was set
          break;
        case ESMC_CALKIND_CUSTOM:
          // TODO:
          break;
        default:
          // unknown calendar kind
          char logMsg[160];
          sprintf(logMsg, "; unknown calendar kind %d.", 
                          tiToConvert.calendar->calkindflag);
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, logMsg, 
            ESMC_CONTEXT, &rc);
          return(rc);
          break;
      };

      // return requested years value
      if (yy != ESMC_NULL_POINTER) {
        // ensure fit in given int
        if (years < INT_MIN || years > INT_MAX) {
          char logMsg[160];
          sprintf(logMsg, "; years value %lld won't fit in given yy integer.",
                  years);
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE, logMsg, ESMC_CONTEXT,
            &rc);
          return(rc);
        }
        *yy = (ESMC_I4) years;  // >= 32-bit
      }
      if (yy_i8 != ESMC_NULL_POINTER) {
        *yy_i8 = years;  // >= 64-bit
      }
    }

    // convert from remaining reduced units to any user-requested months
    if (mm != ESMC_NULL_POINTER || mm_i8 != ESMC_NULL_POINTER) {
      // total relative months
      ESMC_I8 months = tiToConvert.mm;
      tiToConvert.mm = 0;
      switch (tiToConvert.calendar->calkindflag)
      {
        case ESMC_CALKIND_GREGORIAN:
        case ESMC_CALKIND_JULIAN:
        case ESMC_CALKIND_NOLEAP:

          // TODO: use TimeInterval operators (/) and (-) when ready ?

          // get months using startTime, if available
          if (tiToConvert.startTime.Time::validate("initialized")
                == ESMF_SUCCESS ||
              tiToConvert.endTime.Time::validate("initialized")
                == ESMF_SUCCESS) {
            TimeInterval oneMonth(0, 0, 1, 0, 1);
            Time iTime = tiToConvert.startTime + oneMonth;
            months = 0;
            while (iTime <= tiToConvert.endTime) {
              months++;
              iTime += oneMonth;
            }

            // move tiToConvert.startTime up to end of last month converted
            tiToConvert.startTime = iTime - oneMonth;

            // calculate remaining baseTimeToConvert (remove months we got)
            TimeInterval ti = tiToConvert.endTime - tiToConvert.startTime;
            tiToConvert.setw(ti.getw());
            //TODO: tiToConvert = tiToConvert.endTime - tiToConvert.startTime;
            //      should just copy base class part wholesale, rather than
            //      individual properties (including fraction sN/sD); breaks
            //      encapsulation principle.

          } else { // no startTime or endTime available, convert what we can
            if (tiToConvert.getw() >=
                              (28 * tiToConvert.calendar->secondsPerDay)
                                  ||
               tiToConvert.getw() <=
                             (-28 * tiToConvert.calendar->secondsPerDay))
                // TODO: can't use abs() or labs() since result is (long long)
                //       could use ISO llabs() if supported on all platforms
            {
              // can't determine months without startTime or endTime !
              // TODO:  leave alone and let d,h,m,s be more than a month ?
              //        (unbounded or unnormalized ?) with ESMF_WARNING ?
              char logMsg[160];
              sprintf(logMsg, 
                        "mm or mm_i8 because for %s time "
                        "interval >= 28 days, can't determine months without "
                        "startTime or endTime.", 
                            Calendar::calkindflagName[
                              tiToConvert.calendar->calkindflag-1]);
              ESMC_LogDefault.MsgFoundError(ESMC_RC_CANNOT_GET,
                            logMsg, ESMC_CONTEXT, &rc); return(rc);
            }
          }
          break;
        case ESMC_CALKIND_360DAY:
          months = tiToConvert.getw() /
                                   (30 * tiToConvert.calendar->secondsPerDay);
          tiToConvert.setw(tiToConvert.getw() %
                                   (30 * tiToConvert.calendar->secondsPerDay));
          break;
        case ESMC_CALKIND_JULIANDAY:
        case ESMC_CALKIND_MODJULIANDAY:
          // months not defined!
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_INCOMP,
               ", months (mm or mm_i8) not defined for ESMC_CALKIND_JULIANDAY "
               "or ESMC_MODJULIANDAY calendar.", ESMC_CONTEXT, &rc);
          return(rc);
        case ESMC_CALKIND_NOCALENDAR:
          // months not defined, but allow for requesting what was set
          break;
        case ESMC_CALKIND_CUSTOM:
          // TODO:
          break;
        default:
          // unknown calendar kind
          char logMsg[160];
          sprintf(logMsg, "; unknown calendar kind %d.", 
                          tiToConvert.calendar->calkindflag);
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, logMsg, ESMC_CONTEXT,
            &rc);
          return(rc);
          break;
      };
    
      // return requested months value
      if (mm != ESMC_NULL_POINTER) {
        // ensure fit in given int
        if (months < INT_MIN || months > INT_MAX) {
          char logMsg[160];
          sprintf(logMsg, "; months value %lld won't fit in given mm integer.",
                  months);
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE, logMsg, ESMC_CONTEXT,
            &rc);
          return(rc);
        }
        *mm = (ESMC_I4) months;  // >= 32-bit
      }
      if (mm_i8 != ESMC_NULL_POINTER) {
        *mm_i8 = months;   // >= 64-bit
      }
    }

    // convert from remaining reduced units to any user-requested days
    if (d != ESMC_NULL_POINTER || d_i8 != ESMC_NULL_POINTER ||
        d_r8 != ESMC_NULL_POINTER) {
      // total relative and absolute days
      ESMC_I8 days = tiToConvert.d; // relative part 
      tiToConvert.d = 0;           // TODO: don't need tiToConvert.d,
                                   // always = 0?

      // don't need to check for years (tiToConvert.yy) to convert to days
      //   since for all calendar kinds (except custom TODO), years will
      //   have already been reduced to either months (tiToConvert.mm) or
      //   seconds (tiToConvert.s).

      // check for any months to convert to days
      switch (tiToConvert.calendar->calkindflag)  
      {
        case ESMC_CALKIND_GREGORIAN:
        case ESMC_CALKIND_JULIAN:
          if (tiToConvert.mm != 0) {
            // no startTime or endTime available, can't do
            char logMsg[160];
            sprintf(logMsg, "need startTime or endTime to convert %lld months "
                            "to days on %s calendar.", tiToConvert.mm,
                            Calendar::calkindflagName[
                              tiToConvert.calendar->calkindflag-1]);
            ESMC_LogDefault.MsgFoundError(ESMC_RC_CANNOT_GET, logMsg,
                                                  ESMC_CONTEXT, &rc);
            return(rc);
          }
          break;
        case ESMC_CALKIND_NOLEAP:
          if (tiToConvert.mm != 0) {
            // no startTime or endTime available, can only do if months
            // are an integral number of years
            //   note: can't use abs() or labs() since result is (long long)
            //         could use ISO llabs() if supported on all platforms
            if ( (tiToConvert.mm >=  tiToConvert.calendar->monthsPerYear || 
                  tiToConvert.mm <= -tiToConvert.calendar->monthsPerYear) &&
                  tiToConvert.mm  %  tiToConvert.calendar->monthsPerYear == 0) {
              days += (tiToConvert.mm / tiToConvert.calendar->monthsPerYear) *
                                     tiToConvert.calendar->daysPerYear.getw();
              tiToConvert.mm = 0;
            } else {
              // can't do
              char logMsg[160];
              sprintf(logMsg, "need startTime or endTime to convert %lld months "
                              "to days on ESMC_CALKIND_NOLEAP calendar, since "
                              "months are not an integral number of years.",
                               tiToConvert.mm);
              ESMC_LogDefault.MsgFoundError(ESMC_RC_CANNOT_GET, logMsg,
                                                    ESMC_CONTEXT, &rc);
              return(rc);
            }
          }
          break;
        case ESMC_CALKIND_360DAY:
          // nothing to do: 360Day => months already reduced to seconds;
          break;
        case ESMC_CALKIND_JULIANDAY:
        case ESMC_CALKIND_MODJULIANDAY:
        case ESMC_CALKIND_NOCALENDAR:
          //   JulianDay, Modified JulianDay and NoCalendar =>
          //      months don't apply
          if (tiToConvert.mm != 0) {
            // can't convert months to days without appropriate calendar!
            char logMsg[160];
            sprintf(logMsg, ", can't convert %lld months to days "
                     "on ESMC_CALKIND_JULIANDAY, ESMC_CALKIND_MODJULIANDAY or "
                     "ESMC_CALKIND_NOCALENDAR calendars.", tiToConvert.mm);
            ESMC_LogDefault.MsgFoundError(ESMC_RC_CANNOT_GET, logMsg,
                                                    ESMC_CONTEXT, &rc);
            return(rc);
          }
          break;
        case ESMC_CALKIND_CUSTOM:
          // TODO:
          break;
        default:
          // unknown calendar kind
          char logMsg[160];
          sprintf(logMsg, "; unknown calendar kind %d.", 
                          tiToConvert.calendar->calkindflag);
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_VALUE, logMsg, ESMC_CONTEXT,
              &rc);
          return(rc);
          break;
      };

      // convert any days from base time
      if (tiToConvert.calendar->secondsPerDay != 0) {
        // absolute part
        days += tiToConvert.getw() / tiToConvert.calendar->secondsPerDay;
                      // TODO: assign, not add, if tiToConvert.d always = 0 ?
        // remove days from base conversion time to get remaining sub-day
        // units (h,m,s)
        tiToConvert.setw(tiToConvert.getw() %
                         tiToConvert.calendar->secondsPerDay);
      }

      // return requested days value
      if (d != ESMC_NULL_POINTER) {
        // ensure fit in given int
        if (days < INT_MIN || days > INT_MAX) {
          char logMsg[160];
          sprintf(logMsg, "; days value %lld won't fit in given d integer.",
                  days);
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SIZE, logMsg, ESMC_CONTEXT,
              &rc);
          return(rc);
        }
        *d = (ESMC_I4) days;  // >= 32-bit
      }
      if (d_i8 != ESMC_NULL_POINTER) {
        *d_i8 = days;  // >= 64-bit
      }
      if (d_r8 != ESMC_NULL_POINTER) {
        if (tiToConvert.calendar->secondsPerDay == 0) {
          char logMsg[320];
          sprintf(logMsg, "; can't get d_r8; must specify a calendar or "
             "calkindflag which defines days (non-zero seconds per day), "
             "e.g. ESMC_CALKIND_GREGORIAN, ESMC_CALKIND_JULIAN, "
             "ESMC_CALKIND_JULIANDAY, ESMC_MODJULIANDAY, ESMC_CALKIND_NOLEAP, "
             "ESMC_CALKIND_360DAY");
          ESMC_LogDefault.MsgFoundError(ESMC_RC_DIV_ZERO, logMsg, ESMC_CONTEXT,
              &rc);
          return(rc);
        }
        // avoid error introduced by floating point divide by performing an
        // integer divide first, then converting to floating point.
        BaseTime rdays = tiToConvert;
        rdays /= tiToConvert.calendar->secondsPerDay;
        *d_r8 = (ESMC_R8) days + rdays.getr();
      }
    }

    // use base class to get sub-day values (h,m,s) on remaining
    //   unconverted base time
    rc = BaseTime::get(&tiToConvert, h, m, s, s_i8,
                          ms, us, ns, h_r8, m_r8, s_r8,
                          ms_r8, us_r8, ns_r8, sN, sN_i8, sD, sD_i8);
    if (ESMC_LogDefault.MsgFoundError(rc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc))
      return(rc);

    // return any requested properties startTime, endTime, calendar
    // TODO: return error if this-><values> are unintialized (depends on F95
    //       initializers)
    if (startTime != ESMC_NULL_POINTER) {
      *startTime = this->startTime;
    }
    if (endTime != ESMC_NULL_POINTER) {
      *endTime = this->endTime;
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

    // if requested, return time interval in string format
    if (tempTimeString != ESMC_NULL_POINTER && timeStringLen > 0) {
      rc = TimeInterval::getString(tempTimeString);
      if (ESMC_LogDefault.MsgFoundError(rc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        &rc))
        return(rc);
      *tempTimeStringLen = strlen(tempTimeString);
      // see also method TimeInterval::print()
    }
    if (tempTimeStringISOFrac != ESMC_NULL_POINTER &&
        timeStringLenISOFrac > 0) {
      rc = TimeInterval::getString(tempTimeStringISOFrac, "isofrac");
      if (ESMC_LogDefault.MsgFoundError(rc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        &rc))
        return(rc);
      *tempTimeStringLenISOFrac = strlen(tempTimeStringISOFrac);
      // see also method TimeInterval::print()
    }

    return(ESMF_SUCCESS);

 }  // end TimeInterval::get

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  TimeInterval::set - Set a TimeInterval value
//
// !INTERFACE:
//      int TimeInterval::set(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//      const char *timeList,    // in - time interval value specifier string
//      ...) {                   // in - specifier values (variable args)
//
// !DESCRIPTION:
//      Sets a {\tt ESMC\_TimeInterval}'s values in user-specified values.
//      Supports native C++ use.
//
//EOP
// !REQUIREMENTS:  
//
//    // TODO
//    return(ESMF_SUCCESS);
//
// }  // end TimeInterval::set

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  TimeInterval::get - Get a TimeInterval value
//
// !INTERFACE:
//      int TimeInterval::get(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//      const char *timeList,    // in  - time interval value specifier string
//      ...) const {             // out - specifier values (variable args)
//
// !DESCRIPTION:
//      Gets a {\tt ESMC\_TimeInterval}'s values in user-specified format.
//      Supports native C++ use.
//
//EOP
// !REQUIREMENTS:  
//
//    // TODO
//    return(ESMF_SUCCESS);
//
// }  // end TimeInterval::get

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  TimeInterval::set - direct property initializer
//
// !INTERFACE:
      int TimeInterval::set(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_I8 s,            // in - integer seconds
      ESMC_I8 sN,           // in - fractional seconds, numerator
      ESMC_I8 sD,           // in - fractional seconds, denominator
      ESMC_I8 yy,           // in - calendar interval number of years
      ESMC_I8 mm,           // in - calendar interval number of months
      ESMC_I8 d,            // in - calendar interval number of integer days
      ESMC_R8 d_r8,         // in - calendar interval number of real days
      Time *startTime,      // in - interval startTime
      Time *endTime,        // in - interval endTime
      Calendar *calendar,   // in - associated calendar
      ESMC_CalKind_Flag calkindflag) { // in - associated calendar kind
//
// !DESCRIPTION:
//      Initialzes a {\tt TimeInterval} with given values.  Used to avoid
//      constructor to cover case when initial entry is from F90, since
//      destructor is called automatically when leaving scope to return to F90.
//
//EOP
// !REQUIREMENTS:

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::TimeInterval::set(direct)"

    int rc = ESMF_SUCCESS;

    // use base class Set()
    rc = BaseTime::set(s, sN, sD);
    if (ESMC_LogDefault.MsgFoundError(rc, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, &rc))
      return(rc);

    this->yy = yy;
    this->mm = mm;
    this->d  = d;
    this->d_r8 = d_r8;

    if (startTime != ESMC_NULL_POINTER) {
      this->startTime = *startTime;
    } else this->startTime.Time::set((ESMC_I8) 0);

    if (endTime != ESMC_NULL_POINTER) {
      this->endTime = *endTime;
    } else this->endTime.Time::set((ESMC_I8) 0);

    if (calendar != ESMC_NULL_POINTER) {
      // set to user's calendar
      this->calendar = calendar;                       // 1st choice

    } else if (calkindflag != (ESMC_CalKind_Flag)0) {
      // set to specified built-in type; create if necessary
      rc = ESMCI_CalendarCreate(calkindflag);
      if (ESMC_LogDefault.MsgFoundError(rc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        &rc))
        return(rc);
      this->calendar = Calendar::internalCalendar[calkindflag-1];
                                                       // 2nd choice

    } else if (Calendar::defaultCalendar != ESMC_NULL_POINTER) {
      // use default calendar
      this->calendar = Calendar::defaultCalendar; // 3rd choice

    } else {
      // create default calendar
      rc = ESMCI_CalendarSetDefault((ESMC_CalKind_Flag *)ESMC_NULL_POINTER);
      if (ESMC_LogDefault.MsgFoundError(rc, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, &rc))
        return(rc);
      this->calendar = Calendar::defaultCalendar; // 4th choice
    }

    return(ESMF_SUCCESS);

}  // end TimeInterval::set

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  TimeInterval::absValue - Get a Time Interval's absolute value
//
// !INTERFACE:
      TimeInterval TimeInterval::absValue(void) const{
//
// !RETURN VALUE:
//    TimeInterval result
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      Gets a {\tt ESMC\_TimeInterval}'s absolute value
//
//EOP
// !REQUIREMENTS:  TMG 1.5.8

   return(TimeInterval::absValue(ESMC_POSITIVE_ABS));

 }  // end TimeInterval::absValue

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  TimeInterval::negAbsValue - Return a Time Interval's negative absolute value
//
// !INTERFACE:
      TimeInterval TimeInterval::negAbsValue(void) const {
//
// !RETURN VALUE:
//    TimeInterval result
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      Returns a {\tt ESMC\_TimeInterval}'s negative absolute value
//
//EOP
// !REQUIREMENTS:  TMG 1.5.8

   return(TimeInterval::absValue(ESMC_NEGATIVE_ABS));

 }  // end TimeInterval::negAbsValue

//-------------------------------------------------------------------------
//BOPI
// !IROUTINE:  TimeInterval::absValue - TimeInterval absolute value common method
//
// !INTERFACE:
      TimeInterval TimeInterval::absValue(
//
// !RETURN VALUE:
//    TimeInterval result
//
// !ARGUMENTS:
      ESMC_AbsValueType absValueType) const { // in - positive or negative type
//
// !DESCRIPTION:
//      Captures common logic for performing positive or negative
//      absolute value on this time interval.
//
//EOPI
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::TimeInterval::absValue()"

    // TODO: use function pointer table instead of if-else to perform abs
    //       type?  or some other form of polymorphism ?
    // TODO: fractions

    // note: can't use abs() or labs() since values will be (long long)
    //       (64-bit) on some platforms.  TODO: could use ISO llabs() if
    //       supported on all platforms.

    TimeInterval errorResult;  // zero

    // calendar must be defined
    if (this->calendar == ESMC_NULL_POINTER) {
        ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_INCONS,
          ", no default calendar.", ESMC_CONTEXT, ESMC_NULL_POINTER);
      return(errorResult);
    }

    // initialize result to subject time interval
    TimeInterval absValue = *this;

    // Reduce both time interval's units to the smallest and least number
    // of units possible
    absValue.TimeInterval::reduce();

    // if absolute, simply perform absolute value on baseTime seconds and return
    if (absValue.yy == 0 && absValue.mm == 0 && absValue.d == 0 &&
        absValue.d_r8 == 0.0) {
      if (absValueType == ESMC_POSITIVE_ABS) {
        if (absValue.getw() < 0) {
          absValue.setw(absValue.getw() * -1);
          // TODO: fractions => abs method in Fraction class
        }
      } else { // ESMC_NEGATIVE_ABS
        if (absValue.getw() > 0) {
          absValue.setw(absValue.getw() * -1);
          // TODO: fractions
        }
      }
      return(absValue);
    }

    // TODO:  fractional seconds
    switch (absValue.calendar->calkindflag)
    {
      case ESMC_CALKIND_GREGORIAN:
      case ESMC_CALKIND_JULIAN:
      case ESMC_CALKIND_NOLEAP:
        if (absValue.yy != 0 || absValue.d != 0 || absValue.d_r8 != 0.0) {
          // shouldn't be here - yy and d already reduced in Reduce() call
          // above. 
          ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_INCONS,
            ", yy and/or d non-zero:  should already be "
            "reduced.", ESMC_CONTEXT, ESMC_NULL_POINTER);
          return(errorResult);

        } else if (absValue.mm == 0) {
          // shouldn't be here - yy, mm and d all zero caught above
          // above TODO: write LogErr message (internal error)
          ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_INCONS,
            ", yy, mm, d should not be all zero.",
            ESMC_CONTEXT, ESMC_NULL_POINTER);
          return(errorResult);

        // all relative case (yy (reduced to mm), mm, no seconds)
        } else if (absValue.getw() == 0) {
          if (absValueType == ESMC_POSITIVE_ABS) {
            if (absValue.mm < 0) absValue.mm *= -1;  // invert sign
          } else { // ESMC_NEGATIVE_ABS
            if (absValue.mm > 0) absValue.mm *= -1;  // invert sign
          }
          return(absValue);

        // mixed absolute (mm) and relative (s) case
        } else {
          // mm & s must have same sign
          if ( (absValueType == ESMC_POSITIVE_ABS && 
                  absValue.mm < 0 && absValue.getw() < 0) ||
               (absValueType == ESMC_NEGATIVE_ABS && 
                  absValue.mm > 0 && absValue.getw() > 0) ) {
              absValue.mm *= -1;  // invert sign
              absValue.setw(absValue.getw() * -1);
              return(absValue);
          } else if ((absValueType == ESMC_POSITIVE_ABS &&
                      absValue.mm >= 0 && absValue.getw() >= 0) ||
                     (absValueType == ESMC_NEGATIVE_ABS &&
                      absValue.mm <= 0 && absValue.getw() <= 0) ) {
              return(absValue);   // return as is
          } else {
            // mixed signs
            // TODO: write LogErr message (can't do: mixed mm and s signs)
            ESMC_LogDefault.MsgFoundError(ESMC_RC_CANNOT_GET,
              ", sign of mm and s not the same.", 
              ESMC_CONTEXT, ESMC_NULL_POINTER);
            return(errorResult);
          }
        }

        break;
      case ESMC_CALKIND_360DAY:
        // shouldn't be here - yy, mm, d already reduced in Reduce() call above
        // TODO: write LogErr message (internal error)
        ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_INCONS,
          ", yy,mm and/or d non-zero:  should already be "
          "reduced.", ESMC_CONTEXT, ESMC_NULL_POINTER);
        return(errorResult);
        break;
      case ESMC_CALKIND_JULIANDAY:
      case ESMC_CALKIND_MODJULIANDAY:
        if (absValue.yy != 0 || absValue.mm != 0) {
          ESMC_LogDefault.MsgFoundError(ESMC_RC_CANNOT_GET,
            ", years and months not defined for "
            "ESMC_CALKIND_JULIANDAY or "
            "ESMC_CALKIND_MODJULIANDAY calendars.",
            ESMC_CONTEXT, ESMC_NULL_POINTER);
          return(errorResult);
        }
        if (absValue.d != 0 || absValue.d_r8 != 0.0) {
          // shouldn't be here - days already reduced in Reduce() call above
          ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_INCONS,
             ", d non-zero: should already be reduced.",
             ESMC_CONTEXT, ESMC_NULL_POINTER);
          return(errorResult);
        }
        break;
      case ESMC_CALKIND_NOCALENDAR:
        // all units must have same sign
        if ( (absValueType == ESMC_POSITIVE_ABS &&
                absValue.yy < 0 && absValue.mm < 0 &&
                absValue.d  < 0 && absValue.d_r8 < 0.0 &&
                absValue.getw()  < 0) ||
             (absValueType == ESMC_NEGATIVE_ABS &&
                absValue.yy > 0 && absValue.mm > 0 &&
                absValue.d  > 0 && absValue.d_r8 > 0.0 &&
                absValue.getw()  > 0) ) {
          absValue.yy *= -1;   // invert sign
          absValue.mm *= -1;
          absValue.d  *= -1;
          absValue.d_r8 *= -1.0;
          absValue.setw(absValue.getw() * -1);
          return(absValue);
        } else if ( (absValueType == ESMC_POSITIVE_ABS &&
                      absValue.yy >= 0 && absValue.mm >= 0 &&
                      absValue.d  >= 0 && absValue.d_r8 >= 0.0 &&
                      absValue.getw() >= 0) ||
                    (absValueType == ESMC_NEGATIVE_ABS &&
                      absValue.yy <= 0 && absValue.mm <= 0 &&
                      absValue.d  <= 0 && absValue.d_r8 <= 0.0 &&
                      absValue.getw() <= 0) ) {
          return(absValue);    // return as is
        } else {
          // mixed signs
          ESMC_LogDefault.MsgFoundError(ESMC_RC_CANNOT_GET,
            ", for ESMC_CALKIND_NOCALENDAR, all units must "
            "be of the same sign.",
            ESMC_CONTEXT, ESMC_NULL_POINTER);
          return(errorResult);
        }
        break;
      case ESMC_CALKIND_CUSTOM:
        // TODO:
        return(errorResult);
        break;
      default:
        char logMsg[160];
        sprintf(logMsg, "; unknown calendar kind %d.", 
                        absValue.calendar->calkindflag);
        ESMC_LogDefault.MsgFoundError(ESMC_RC_OBJ_BAD, logMsg,
          ESMC_CONTEXT, ESMC_NULL_POINTER);
        return(errorResult);
        break;
    };

    // shouldn't be here
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      ", shouldn't be here.",
      ESMC_CONTEXT, ESMC_NULL_POINTER);
    return(errorResult);

}  // end TimeInterval::absValue (common)

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  TimeInterval(/) - Divide two time intervals, return double precision result
//
// !INTERFACE:
      ESMC_R8 TimeInterval::operator/(
//
// !RETURN VALUE:
//    ESMC_R8 result
//
// !ARGUMENTS:
      const TimeInterval &timeinterval) const {  // in - TimeInterval
                                                      //        to divide by
//
// !DESCRIPTION:
//    Returns this time interval divided by given time interval as a ESMC_R8
//    precision quotient.
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::TimeInterval::operator/(timeinterval)"

    // TODO: use some form of polymorphism to share logic with operator% and
    //       Compare method ?

    // calendars must be defined
    if (this->calendar == ESMC_NULL_POINTER ||
        timeinterval.calendar == ESMC_NULL_POINTER) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        ", calendar required for both "
        "timeinterval arguments.",
        ESMC_CONTEXT, ESMC_NULL_POINTER);
      return(0.0);
    }

    // create zero basetime for comparison
    BaseTime zeroBaseTime;

    // create local copies to manipulate and divide 
    TimeInterval ti1 = *this;
    TimeInterval ti2 = timeinterval;

    // Reduce both time interval's units to the smallest and least number
    // of units possible
    ti1.TimeInterval::reduce();
    ti2.TimeInterval::reduce();

    // if both absolute, simply divide baseTime seconds and return
    if (ti1.yy == 0 && ti2.yy == 0 &&
        ti1.mm == 0 && ti2.mm == 0 &&
        ti1.d  == 0 && ti2.d  == 0 &&
        ti1.d_r8 == 0.0 && ti2.d_r8 == 0.0) {
      return(ti1.BaseTime::operator/(ti2));
    }

    // calendars must be the same for divide on relative parts
    // TODO: relax this restriction (e.g. 10 days Gregorian % 3 days 360-day)
    if (ti1.calendar->calkindflag != ti2.calendar->calkindflag) {
      // TODO: write LogErr message (timeinterval calendars not the same)
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SAMETYPE,
        "; timeinterval calendars not the same.", ESMC_CONTEXT,
        ESMC_NULL_POINTER);
      return(0.0);
    }

    // Perform relative division based on calendar kind
    // TODO:  fractional seconds
    switch (ti1.calendar->calkindflag)  
    {
      case ESMC_CALKIND_GREGORIAN:
      case ESMC_CALKIND_JULIAN:
      case ESMC_CALKIND_NOLEAP:
        if (ti1.yy != 0 || ti2.yy != 0 ||
            ti1.d  != 0 || ti2.d  != 0 ||
            ti1.d_r8 != 0.0 || ti2.d_r8 != 0.0) {
          // shouldn't be here - yy and d already reduced in Reduce() call
          ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
            ", shouldn't be here.",
            ESMC_CONTEXT, ESMC_NULL_POINTER);
          return(0.0);

        // all relative case (yy (reduced to mm), mm, no seconds)
        } else if (zeroBaseTime == ti1 && zeroBaseTime == ti2) {
          if (ti2.mm != 0) {
            return((ESMC_R8) ti1.mm / (ESMC_R8) ti2.mm);
          } else {
            ESMC_LogDefault.FoundError(ESMC_RC_DIV_ZERO, ESMC_CONTEXT,
              ESMC_NULL_POINTER);
            return(0.0);
          }

        // below here are mixed (relative, absolute) cases

        // dividend zero
        } else if (ti1.mm == 0 && zeroBaseTime == ti1) {
          return(0.0); // ok

        // divisor zero
        } else if (ti2.mm == 0 && zeroBaseTime == ti2) {
          ESMC_LogDefault.FoundError(ESMC_RC_DIV_ZERO,ESMC_CONTEXT,
            ESMC_NULL_POINTER);
          return(0.0);

        // all other combinations
        } else {
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
            "can't divide without startTime or endtime", ESMC_CONTEXT,
            ESMC_NULL_POINTER);
          return(0.0);
        }
        break;
      case ESMC_CALKIND_360DAY:
        // shouldn't be here - yy, mm, d already reduced in Reduce() call above
        ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
          ", shouldn't be here.",
          ESMC_CONTEXT, ESMC_NULL_POINTER);
        return(0.0);
        break;
      case ESMC_CALKIND_JULIANDAY:
      case ESMC_CALKIND_MODJULIANDAY:
        if (ti1.yy != 0 || ti2.yy != 0 ||
            ti1.mm != 0 || ti2.mm != 0) {
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
            ", years and months not defined for "
            "ESMC_CALKIND_JULIANDAY or "
            "ESMC_CALKIND_MODJULIANDAY calendars.",
            ESMC_CONTEXT, ESMC_NULL_POINTER);
          return(0.0);
        }
        if (ti1.d != 0 || ti2.d != 0 ||
            ti1.d_r8 != 0.0 || ti2.d_r8 != 0.0) {
          // shouldn't be here - days already reduced in Reduce() call above
          ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
            ", shouldn't be here.",
            ESMC_CONTEXT, ESMC_NULL_POINTER);
          return(0.0);
        }
        break;
      case ESMC_CALKIND_NOCALENDAR:
        // can divide like units only
        if ( (ti1.yy != 0 || ti2.yy != 0) &&
              ti1.mm == 0 && ti2.mm == 0  &&
              ti1.d  == 0 && ti2.d  == 0  &&
              ti1.d_r8 == 0.0 && ti2.d_r8 == 0.0 &&
              zeroBaseTime == ti1 && zeroBaseTime == ti2) {
          // divide years only
          if (ti2.yy != 0) {
            return((ESMC_R8) ti1.yy / (ESMC_R8) ti2.yy);
          } else {
            ESMC_LogDefault.FoundError(ESMC_RC_DIV_ZERO, ESMC_CONTEXT,
              ESMC_NULL_POINTER);
            return(0.0);
          }
        } else if ( ti1.yy == 0 && ti2.yy == 0   &&
                   (ti1.mm != 0 || ti2.mm != 0)  &&
                    ti1.d  == 0 && ti2.d  == 0   &&
                    ti1.d_r8 == 0.0 && ti2.d_r8 == 0.0 &&
                    zeroBaseTime == ti1 && zeroBaseTime == ti2) {
          // divide months only
          if (ti2.mm != 0) {
            return((ESMC_R8) ti1.mm / (ESMC_R8) ti2.mm);
          } else {
            ESMC_LogDefault.FoundError(ESMC_RC_DIV_ZERO, ESMC_CONTEXT,
              ESMC_NULL_POINTER);
            return(0.0);
          }
        // TODO:  merge d and d_r8 (below)
        //        handle all cases of mixing d and d_r8 within a ti and
        //        between ti1 and ti2
        } else if ( ti1.yy == 0 && ti2.yy == 0  &&
                    ti1.mm == 0 && ti2.mm == 0  &&
                   (ti1.d  != 0 || ti2.d  != 0) &&
                    ti1.d_r8 == 0.0 && ti2.d_r8 == 0.0 &&
                    zeroBaseTime == ti1 && zeroBaseTime == ti2) {
          // divide days only
          if (ti2.d != 0) {
            return((ESMC_R8) ti1.d / (ESMC_R8) ti2.d);
          } else {
            ESMC_LogDefault.FoundError(ESMC_RC_DIV_ZERO, ESMC_CONTEXT,
              ESMC_NULL_POINTER);
            return(0.0);
          }
        // TODO:  merge d and d_r8 (above)
        //        handle all cases of mixing d and d_r8 within a ti and
        //        between ti1 and ti2
        } else if ( ti1.yy == 0 && ti2.yy == 0 &&
                    ti1.mm == 0 && ti2.mm == 0 &&
                    ti1.d  == 0 && ti2.d  == 0 &&
                   (ti1.d_r8 != 0.0 || ti2.d_r8 != 0.0) &&
                    zeroBaseTime == ti1 && zeroBaseTime == ti2) {
          // divide days only
          if (fabs(ti2.d_r8) >= 1e-10) {
            return(ti1.d_r8 / ti2.d_r8);
          } else {
            ESMC_LogDefault.FoundError(ESMC_RC_DIV_ZERO, ESMC_CONTEXT,
              ESMC_NULL_POINTER);
            return(0.0);
          }
        } else {
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
            ", can't divide mixed units.", ESMC_CONTEXT, ESMC_NULL_POINTER);
          return(0.0);
        }
        break;
      case ESMC_CALKIND_CUSTOM:
        // TODO:
        return(0.0);
        break;
      default:
        char logMsg[160];
        sprintf(logMsg, "; unknown calendar kind %d.", 
                        ti1.calendar->calkindflag);
        ESMC_LogDefault.MsgFoundError(ESMC_RC_OBJ_BAD, logMsg,
          ESMC_CONTEXT, ESMC_NULL_POINTER);
        return(0.0);
        break;
    };

    // shouldn't be here
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      ", shouldn't be here.", ESMC_CONTEXT, ESMC_NULL_POINTER);
    return(0.0);

}  // end TimeInterval::operator/

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  TimeInterval(/) - Divide time interval by an integer, return time interval result
//
// !INTERFACE:
      TimeInterval TimeInterval::operator/(
//
// !RETURN VALUE:
//    TimeInterval result
//
// !ARGUMENTS:
      const ESMC_I4 &divisor) const {   // in - integer divisor
//
// !DESCRIPTION:
//    Divides a {\tt ESMC\_TimeInterval} by an integer divisor,
//    returns quotient as a {\tt ESMC\_TimeInterval}.
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::TimeInterval::operator/(integer)"

    // copy calendar, startTime, endTime from this time interval
    TimeInterval quotient = *this;

    // reduce to smallest and least number of units
    quotient.TimeInterval::reduce();

    // TODO: fractional interval parts

    // divide relative yy, mm, d parts
    if (divisor != 0) {
      quotient.yy /= divisor;
      quotient.mm /= divisor;
      quotient.d  /= divisor;
      quotient.d_r8 /= (ESMC_R8) divisor;

      // divide absolute seconds (and any fractional) part
      quotient.BaseTime::operator/=(divisor);

    } else {
      // TODO: write LogErr message (divide-by-zero)
      ESMC_LogDefault.FoundError(ESMC_RC_DIV_ZERO, ESMC_CONTEXT, 
        ESMC_NULL_POINTER);
      TimeInterval zeroInterval;
      return(zeroInterval);
    }

    // note: result not normalized here -- it is done during a Get() or use
    // in an arithmetic or comparison operation.

    return(quotient);

}  // end TimeInterval::operator/

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  TimeInterval(/=) - Divide time interval by an integer
//
// !INTERFACE:
      TimeInterval& TimeInterval::operator/=(
//
// !RETURN VALUE:
//    TimeInterval& result
//
// !ARGUMENTS:
      const ESMC_I4 &divisor) {   // in - integer divisor
//
// !DESCRIPTION:
//    Divides a {\tt ESMC\_TimeInterval} by an integer divisor
//
//EOP
// !REQUIREMENTS:  

    return(*this = *this / divisor);

}  // end TimeInterval::operator/=

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  TimeInterval(/) - Divide time interval by a double precision, return time interval result
//
// !INTERFACE:
      TimeInterval TimeInterval::operator/(
//
// !RETURN VALUE:
//    TimeInterval result
//
// !ARGUMENTS:
      const ESMC_R8 &divisor) const {   // in - double precision divisor
//
// !DESCRIPTION:
//    Divides a {\tt ESMC\_TimeInterval} by an ESMC_R8 divisor,
//    returns quotient as a {\tt ESMC\_TimeInterval}
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::TimeInterval::operator/(ESMC_R8)"

    // copy calendar, startTime, endTime from this time interval
    TimeInterval quotient = *this;

    // reduce to smallest and least number of units
    quotient.TimeInterval::reduce();

    // TODO: fractional interval parts

    // divide relative yy, mm, d parts
    if (fabs(divisor) > FLT_EPSILON) {
      quotient.yy = (ESMC_I8) ((ESMC_R8) quotient.yy / divisor);
      quotient.mm = (ESMC_I8) ((ESMC_R8) quotient.mm / divisor);
      quotient.d  = (ESMC_I8) ((ESMC_R8) quotient.d  / divisor);
      quotient.d_r8 = quotient.d_r8 / divisor;

      // divide absolute s part  // TODO: fractions
      quotient.setr(quotient.getr() / divisor);

    } else {
      // TODO: write LogErr message (divide-by-zero)
      ESMC_LogDefault.FoundError(ESMC_RC_DIV_ZERO, ESMC_CONTEXT, 
                                 ESMC_NULL_POINTER);
      TimeInterval zeroInterval;
      return(zeroInterval);
    }

    // note: result not normalized here -- it is done during a Get() or use
    // in an arithmetic or comparison operation.

    return(quotient);

}  // end TimeInterval::operator/

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  TimeInterval(/=) - Divide time interval by a double precision
//
// !INTERFACE:
      TimeInterval& TimeInterval::operator/=(
//
// !RETURN VALUE:
//    TimeInterval& result
//
// !ARGUMENTS:
      const ESMC_R8 &divisor) {   // in - double precision divisor
//
// !DESCRIPTION:
//    Divides a {\tt ESMC\_TimeInterval} by a double precision divisor
//
//EOP
// !REQUIREMENTS:  

    return(*this = *this / divisor);

}  // end TimeInterval::operator/=

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  TimeInterval::div - Divide two time intervals, return fraction result
//
// !INTERFACE:
      Fraction TimeInterval::div(
//
// !RETURN VALUE:
//    Fraction result
//
// !ARGUMENTS:
      const TimeInterval &timeinterval) const {  // in - TimeInterval
                                                      //        to divide by
//
// !DESCRIPTION:
//    Returns this time interval divided by given time interval as a fractional
//    quotient.
//
//EOP
// !REQUIREMENTS:  

    Fraction quotient;

    // TODO:

    return(quotient);

}  // end TimeInterval::div

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  TimeInterval(\%) - Divide two time intervals, return time interval remainder
//
// !INTERFACE:
      TimeInterval TimeInterval::operator%(
//
// !RETURN VALUE:
//    TimeInterval result
//
// !ARGUMENTS:
      const TimeInterval &timeinterval) const {  // in - TimeInterval
                                                      //        to modulo by
//
// !DESCRIPTION:
//    Returns this time interval modulo by given time interval as a 
//    {\tt ESMC\_TimeInterval}
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::TimeInterval::operator%(timeinterval)"

    // TODO: use some form of polymorphism to share logic with
    //       operator/ (return real) and Compare method ?

    // create zero basetime for comparison
    BaseTime zeroBaseTime;

    // initialize result to zero
    TimeInterval remainder;

    // copy calendar, startTime, endTime from this time interval
    remainder.calendar  = this->calendar;
    remainder.startTime = this->startTime;
    remainder.endTime   = this->endTime;

    // calendars must be defined
    if (this->calendar == ESMC_NULL_POINTER ||
        timeinterval.calendar == ESMC_NULL_POINTER) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        ", calendar required for both "
        "timeinterval arguments.",
        ESMC_CONTEXT, ESMC_NULL_POINTER);
      return(remainder);
    }

    // create local copies to manipulate and modulus 
    TimeInterval ti1 = *this;
    TimeInterval ti2 = timeinterval;

    // Reduce both time interval's units to the smallest and least number
    // of units possible
    ti1.TimeInterval::reduce();
    ti2.TimeInterval::reduce();

    // if both absolute, simply modulus baseTime seconds and return
    if (ti1.yy == 0 && ti2.yy == 0 &&
        ti1.mm == 0 && ti2.mm == 0 &&
        ti1.d  == 0 && ti2.d  == 0 &&
        ti1.d_r8 == 0.0 && ti2.d_r8 == 0.0) {
      remainder = ti1.BaseTime::operator%(ti2);
      return(remainder);
    }

    // calendars must be the same for modulus on relative parts
    // TODO: relax this restriction (e.g. 10 days Gregorian % 3 days 360-day)
    if (ti1.calendar->calkindflag != ti2.calendar->calkindflag) {
      // TODO: write LogErr message (timeinterval calendars not the same)
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SAMETYPE,
        "; timeinterval calendars not the same.", ESMC_CONTEXT,
        ESMC_NULL_POINTER);
      return(remainder);
    }

    // Perform relative modulus based on calendar kind
    // TODO:  fractional seconds
    switch (ti1.calendar->calkindflag)  
    {
      case ESMC_CALKIND_GREGORIAN:
      case ESMC_CALKIND_JULIAN:
      case ESMC_CALKIND_NOLEAP:
        if (ti1.yy != 0 || ti2.yy != 0 ||
            ti1.d  != 0 || ti2.d  != 0 ||
            ti1.d_r8 != 0.0 || ti2.d_r8 != 0.0) {
          // shouldn't be here - yy and d already reduced in Reduce() call
          ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
            ", shouldn't be here.",
            ESMC_CONTEXT, ESMC_NULL_POINTER);
          return(remainder);

        // all relative case (yy (reduced to mm), mm, no seconds)
        } else if (zeroBaseTime == ti1 && zeroBaseTime == ti2) {
          if (ti2.mm != 0) {
            remainder.mm = ti1.mm % ti2.mm;
            return(remainder);
          } else {
            ESMC_LogDefault.FoundError(ESMC_RC_DIV_ZERO, ESMC_CONTEXT,
              ESMC_NULL_POINTER);
            return(remainder);
          }

        // below here are mixed (relative, absolute) cases

        // dividend zero
        } else if (ti1.mm == 0 && zeroBaseTime == ti1) {
          return(remainder); // ok

        // divisor zero
        } else if (ti2.mm == 0 && zeroBaseTime == ti2) {
          ESMC_LogDefault.FoundError(ESMC_RC_DIV_ZERO, ESMC_CONTEXT,
            ESMC_NULL_POINTER);
          return(remainder);

        // all other combinations
        } else {
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
            "can't modulus without startTime or endtime", ESMC_CONTEXT,
            ESMC_NULL_POINTER);
          return(remainder);
        }
        break;
      case ESMC_CALKIND_360DAY:
        // shouldn't be here - yy, mm, d already reduced in Reduce() call above
        ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
          ", shouldn't be here.", ESMC_CONTEXT, ESMC_NULL_POINTER);
        return(remainder);
        break;
      case ESMC_CALKIND_JULIANDAY:
      case ESMC_CALKIND_MODJULIANDAY:
        if (ti1.yy != 0 || ti2.yy != 0 ||
            ti1.mm != 0 || ti2.mm != 0) {
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
            ", years and months not defined for "
            "ESMC_CALKIND_JULIANDAY or "
            "ESMC_CALKIND_MODJULIANDAY calendars.",
            ESMC_CONTEXT, ESMC_NULL_POINTER);
          return(remainder);
        }
        if (ti1.d != 0 || ti2.d != 0 ||
            ti1.d_r8 != 0.0 || ti2.d_r8 != 0.0) {
          // shouldn't be here - days already reduced in Reduce() call above
          ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
            ", shouldn't be here.",
            ESMC_CONTEXT, ESMC_NULL_POINTER);
          return(remainder);
        }
        break;
      case ESMC_CALKIND_NOCALENDAR:
        // can modulus like units only
        if ( (ti1.yy != 0 || ti2.yy != 0) &&
              ti1.mm == 0 && ti2.mm == 0  &&
              ti1.d  == 0 && ti2.d  == 0  &&
              ti1.d_r8 == 0.0 && ti2.d_r8 == 0.0  &&
              zeroBaseTime == ti1 && zeroBaseTime == ti2) {
          // modulus years only
          if (ti2.yy != 0) {
            remainder.yy = ti1.yy % ti2.yy;
            return(remainder);
          } else {
            ESMC_LogDefault.FoundError(ESMC_RC_DIV_ZERO, ESMC_CONTEXT,
              ESMC_NULL_POINTER);
            return(remainder);
          }
        } else if ( ti1.yy == 0 && ti2.yy == 0  &&
                   (ti1.mm != 0 || ti2.mm != 0) &&
                    ti1.d  == 0 && ti2.d  == 0  &&
                    ti1.d_r8 == 0.0 && ti2.d_r8 == 0.0 &&
                    zeroBaseTime == ti1 && zeroBaseTime == ti2) {
          // modulus months only
          if (ti2.mm != 0) {
            remainder.mm = ti1.mm % ti2.mm;
            return(remainder);
          } else {
            ESMC_LogDefault.FoundError(ESMC_RC_DIV_ZERO, ESMC_CONTEXT,
              ESMC_NULL_POINTER);
            return(remainder);
          }
        // TODO: merge d and d_r8
        //       handle all cases of mixing d and d_r8 within a ti and
        //       between ti1 and ti2 ?
        } else if ( ti1.yy == 0 && ti2.yy == 0  &&
                    ti1.mm == 0 && ti2.mm == 0  &&
                   (ti1.d  != 0 || ti2.d  != 0) &&
                    ti1.d_r8 == 0.0 && ti2.d_r8 == 0.0 &&
                    zeroBaseTime == ti1 && zeroBaseTime == ti2) {
          // modulus days only
          if (ti2.d != 0) {
            remainder.d = ti1.d % ti2.d;
            return(remainder);
          } else {
            ESMC_LogDefault.FoundError(ESMC_RC_DIV_ZERO, ESMC_CONTEXT,
              ESMC_NULL_POINTER);
            return(remainder);
          }
        } else {
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
            ", can't modulus mixed units.", ESMC_CONTEXT, ESMC_NULL_POINTER);
          return(remainder);
        }
        break;
      case ESMC_CALKIND_CUSTOM:
        // TODO:
        return(remainder);
        break;
      default:
        char logMsg[160];
        sprintf(logMsg, "; unknown calendar kind %d.", 
                        ti1.calendar->calkindflag);
        ESMC_LogDefault.MsgFoundError(ESMC_RC_OBJ_BAD, logMsg,
          ESMC_CONTEXT, ESMC_NULL_POINTER);
        return(remainder);
        break;
    };

    // shouldn't be here
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      ", shouldn't be here.", ESMC_CONTEXT, ESMC_NULL_POINTER);
    return(remainder);

}  // end TimeInterval::operator%

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  TimeInterval(\%=) - Takes the modulus of two time intervals
//
// !INTERFACE:
      TimeInterval& TimeInterval::operator%=(
//
// !RETURN VALUE:
//    TimeInterval& result
//
// !ARGUMENTS:
      const TimeInterval &timeinterval) {  // in - TimeInterval
                                                //        to modulo by
//
// !DESCRIPTION:
//    Returns this time interval modulo by given time interval
//
//EOP
// !REQUIREMENTS:  

    return(*this = *this % timeinterval);

}  // end TimeInterval::operator%=

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  TimeInterval(*) - Multiply a time interval by an integer
//
// !INTERFACE:
      TimeInterval TimeInterval::operator*(
//
// !RETURN VALUE:
//    TimeInterval result
//
// !ARGUMENTS:
      const ESMC_I4 &multiplier) const {   // in - integer multiplier
//
// !DESCRIPTION:
//     Multiply a {\tt ESMC\_TimeInterval} by an integer, return product as a
//    {\tt ESMC\_TimeInterval}
//
//EOP
// !REQUIREMENTS:  

    // copy calendar, startTime, endTime from this time interval
    TimeInterval product = *this;

    // TODO: fractional interval parts
    // TODO: check for overflow/underflow, return 0 with LogErr message

    // multiply relative yy, mm, d parts
    product.yy *= multiplier;
    product.mm *= multiplier;
    product.d  *= multiplier;
    product.d_r8 *= (ESMC_R8) multiplier;

    // multiply absolute seconds (and any fractional) part
    product.BaseTime::operator*=(multiplier);

    // note: result not normalized here -- it is done during a Get() or use
    // in an arithmetic or comparison operation.

    return(product);

}  // end TimeInterval::operator*

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  TimeInterval(*) - Multiply a time interval by an integer
//
// !INTERFACE:
      TimeInterval operator*(
//
// !RETURN VALUE:
//    TimeInterval result
//
// !ARGUMENTS:
      const ESMC_I4 &multiplier,  // in - integer multiplier
      const TimeInterval &ti) {   // in - TimeInterval multiplicand
//
// !DESCRIPTION:
//     Multiply a {\tt ESMC\_TimeInterval} by an integer, return product as a
//    {\tt ESMC\_TimeInterval}.  Commutative complement to member operator*
//
//EOP
// !REQUIREMENTS:  

    // use commutative complement
    return(ti * multiplier);

}  // end TimeInterval::operator*

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  TimeInterval(*=) - Multiply a time interval by an integer
//
// !INTERFACE:
      TimeInterval& TimeInterval::operator*=(
//
// !RETURN VALUE:
//    TimeInterval& result
//
// !ARGUMENTS:
      const ESMC_I4 &multiplier) {   // in - integer multiplier
//
// !DESCRIPTION:
//     Multiply a {\tt ESMC\_TimeInterval} by an integer
//
//EOP
// !REQUIREMENTS:  

    return(*this = *this * multiplier);

}  // end TimeInterval::operator*=

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  TimeInterval(*) - Multiply a time interval by an fraction
//
// !INTERFACE:
      TimeInterval TimeInterval::operator*(
//
// !RETURN VALUE:
//    TimeInterval result
//
// !ARGUMENTS:
      const Fraction &multiplier) const {   // in - fraction multiplier
//
// !DESCRIPTION:
//     Multiply a {\tt ESMC\_TimeInterval} by an fraction, return product as a
//    {\tt ESMC\_TimeInterval}
//
//EOP
// !REQUIREMENTS:  

    TimeInterval product;

    // TODO: whole, fractional & calendar interval parts

    return(product);

}  // end TimeInterval::operator*

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  TimeInterval(*) - Multiply a time interval by an fraction
//
// !INTERFACE:
      TimeInterval operator*(
//
// !RETURN VALUE:
//    TimeInterval result
//
// !ARGUMENTS:
      const Fraction &multiplier, // in - fraction multiplier
      const TimeInterval &ti) {   // in - TimeInterval multiplicand
//
// !DESCRIPTION:
//     Multiply a {\tt ESMC\_TimeInterval} by an fraction, return product as a
//    {\tt ESMC\_TimeInterval}
//
//EOP
// !REQUIREMENTS:  

    // use commutative complement
    return(ti * multiplier);

}  // end TimeInterval::operator*

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  TimeInterval(*=) - Multiply a time interval by an fraction
//
// !INTERFACE:
      TimeInterval& TimeInterval::operator*=(
//
// !RETURN VALUE:
//    TimeInterval& result
//
// !ARGUMENTS:
      const Fraction &multiplier) {   // in - fraction multiplier
//
// !DESCRIPTION:
//     Multiply a {\tt ESMC\_TimeInterval} by a fraction
//
//EOP
// !REQUIREMENTS:  

    return(*this = *this * multiplier);

}  // end TimeInterval::operator*=

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  TimeInterval(*) - Multiply a time interval by a double precision
//
// !INTERFACE:
      TimeInterval TimeInterval::operator*(
//
// !RETURN VALUE:
//    TimeInterval result
//
// !ARGUMENTS:
      const ESMC_R8 &multiplier) const {   // in - double precision
                                                 //   multiplier
//
// !DESCRIPTION:
//     Multiply a {\tt ESMC\_TimeInterval} by an double precision,
//     return product as a {\tt ESMC\_TimeInterval}
//
//EOP
// !REQUIREMENTS:  

    // copy calendar, startTime, endTime from this time interval
    TimeInterval product = *this;

    // reduce to smallest and least number of units
    product.TimeInterval::reduce();

    // TODO: fractional interval parts
    // TODO: check for overflow/underflow, return 0 with LogErr message

    // multiply relative yy, mm, d parts
    product.yy = (ESMC_I8) ((ESMC_R8) product.yy * multiplier);
    product.mm = (ESMC_I8) ((ESMC_R8) product.mm * multiplier);
    product.d  = (ESMC_I8) ((ESMC_R8) product.d  * multiplier);
    product.d_r8 = product.d_r8 * multiplier;

    // multiply absolute seconds (and any fractional) part
    product.setr(product.getr() * multiplier);

    // note: result not normalized here -- it is done during a Get() or use
    // in an arithmetic or comparison operation.

    return(product);

}  // end TimeInterval::operator*

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  TimeInterval(*) - Multiply a time interval by a double precision
//
// !INTERFACE:
      TimeInterval operator*(
//
// !RETURN VALUE:
//    TimeInterval result
//
// !ARGUMENTS:
      const ESMC_R8 &multiplier,  // in - double precision
      const TimeInterval &ti) {   // in - TimeInterval multiplicand
                                                 //   multiplier
//
// !DESCRIPTION:
//     Multiply a {\tt ESMC\_TimeInterval} by an double precision,
//     return product as a {\tt ESMC\_TimeInterval}
//
//EOP
// !REQUIREMENTS:  

    // use commutative complement
    return(ti * multiplier);

}  // end TimeInterval::operator*

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  TimeInterval(*=) - Multiply a time interval by a double precision
//
// !INTERFACE:
      TimeInterval& TimeInterval::operator*=(
//
// !RETURN VALUE:
//    TimeInterval& result
//
// !ARGUMENTS:
      const ESMC_R8 &multiplier) {   // in - double precision multiplier
//
// !DESCRIPTION:
//     Multiply a {\tt ESMC\_TimeInterval} by a double precision
//
//EOP
// !REQUIREMENTS:  

    return(*this = *this * multiplier);

}  // end TimeInterval::operator*=

//-------------------------------------------------------------------------
//BOP 
// !IROUTINE:  TimeInterval(+) - Sum of two TimeIntervals
//    
// !INTERFACE:
      TimeInterval TimeInterval::operator+(
//    
// !RETURN VALUE:
//    TimeInterval result
//    
// !ARGUMENTS:
      const TimeInterval &timeinterval) const {  // in - TimeInterval
                                                      //      to add
//
// !DESCRIPTION:
//    Adds given {\tt timeinterval} expression to this {\tt timeinterval}.
//
//EOP
// !REQUIREMENTS:

    // copy calendar, startTime, endTime from this time interval
    TimeInterval sum = *this;

    // TODO: fractional interval parts
    // TODO: check for overflow/underflow, return 0 with LogErr message

    // add relative yy, mm, d parts
    sum.yy += timeinterval.yy;
    sum.mm += timeinterval.mm;
    sum.d  += timeinterval.d;
    sum.d_r8 += timeinterval.d_r8;

    // add absolute seconds (and any fractional part) using BaseTime operator
    sum.BaseTime::operator+=(timeinterval);

    // note: result not normalized here -- it is done during a Get() or use
    // in an arithmetic or comparison operation.

    return(sum);

}  // end TimeInterval::operator+

//-------------------------------------------------------------------------
//BOP 
// !IROUTINE:  TimeInterval(-) - Difference between two TimeIntervals
//    
// !INTERFACE:
      TimeInterval TimeInterval::operator-(
//    
// !RETURN VALUE:
//    TimeInterval result
//    
// !ARGUMENTS:
      const TimeInterval &timeinterval) const {  // in - TimeInterval
                                                      //      to subtract
//
// !DESCRIPTION:
//    Subtracts given {\tt timeinterval} expression from this 
//    {\tt timeinterval}.
//
//EOP
// !REQUIREMENTS:

    // copy calendar, startTime, endTime from this time interval
    TimeInterval diff = *this;

    // TODO: fractional interval parts
    // TODO: check for overflow/underflow, return 0 with LogErr message

    // subtract relative yy, mm, d parts
    diff.yy -= timeinterval.yy;
    diff.mm -= timeinterval.mm;
    diff.d  -= timeinterval.d;
    diff.d_r8 -= timeinterval.d_r8;

    // subtract absolute seconds (and any fractional part) using BaseTime
    // operator
    diff.BaseTime::operator-=(timeinterval);

    // note: result not normalized here -- it is done during a Get() or use
    // in an arithmetic or comparison operation.

    return(diff);

}  // end TimeInterval::operator-

//-------------------------------------------------------------------------
//BOP 
// !IROUTINE:  TimeInterval(-) - Unary negation of a TimeInterval
//    
// !INTERFACE:
      TimeInterval TimeInterval::operator-(void) const {
//    
// !RETURN VALUE:
//    TimeInterval result
//    
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//    Negates this {\tt timeinterval} and returns the result.
//
//EOP
// !REQUIREMENTS:  TMG1.5.10

    // simply re-use multiplication by an integer!
    return( *this * -1);
  //return(-1 * *this);

}  // end TimeInterval::operator-

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  TimeInterval(==) - TimeInterval equality comparison
//
// !INTERFACE:
      bool TimeInterval::operator==(
//
// !RETURN VALUE:
//    bool result
//
// !ARGUMENTS:
      const TimeInterval &timeinterval) const {  // in - TimeInterval
                                                      //      to compare
//
// !DESCRIPTION:
//      Compare for equality the current object's (this)
//      {\tt ESMC\_TimeInterval} with given {\tt ESMC\_TimeInterval},
//      return result.
//
//EOP
// !REQUIREMENTS:  

    return(TimeInterval::compare(timeinterval, ESMC_EQ));

}  // end TimeInterval::operator==

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  TimeInterval(!=) - TimeInterval inequality comparison
//
// !INTERFACE:
      bool TimeInterval::operator!=(
//
// !RETURN VALUE:
//    bool result
//
// !ARGUMENTS:
      const TimeInterval &timeinterval) const {  // in - TimeInterval
                                                      //      to compare
//
// !DESCRIPTION:
//      Compare for inequality the current object's (this)
//      {\tt ESMC\_TimeInterval} with given {\tt ESMC\_TimeInterval},
//      return result.
//
//EOP
// !REQUIREMENTS:  

    // TODO: define as !(*this == timeinterval) ?

    return(TimeInterval::compare(timeinterval, ESMC_NE));

}  // end TimeInterval::operator!=

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  TimeInterval(<) - TimeInterval less than comparison
//
// !INTERFACE:
      bool TimeInterval::operator<(
//
// !RETURN VALUE:
//    bool result
//
// !ARGUMENTS:
      const TimeInterval &timeinterval) const {  // in - TimeInterval
                                                      //      to compare
//
// !DESCRIPTION:
//      Compare for less than the current object's (this)
//      {\tt ESMC\_TimeInterval} with given {\tt ESMC\_TimeInterval},
//      return result.
//
//EOP
// !REQUIREMENTS:  

    return(TimeInterval::compare(timeinterval, ESMC_LT));

}  // end TimeInterval::operator<

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  TimeInterval(>) - TimeInterval greater than comparison
//
// !INTERFACE:
      bool TimeInterval::operator>(
//
// !RETURN VALUE:
//    bool result
//
// !ARGUMENTS:
      const TimeInterval &timeinterval) const {  // in - TimeInterval
                                                      //      to compare
//
// !DESCRIPTION:
//      Compare for greater than the current object's (this)
//      {\tt ESMC\_TimeInterval} with given {\tt ESMC\_TimeInterval},
//      return result.
//
//EOP
// !REQUIREMENTS:  

    return(TimeInterval::compare(timeinterval, ESMC_GT));

}  // end TimeInterval::operator>

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  TimeInterval(<=) - TimeInterval less or equal than comparison
//
// !INTERFACE:
      bool TimeInterval::operator<=(
//
// !RETURN VALUE:
//    bool result
//
// !ARGUMENTS:
      const TimeInterval &timeinterval) const {  // in - TimeInterval
                                                      //      to compare
//
// !DESCRIPTION:
//      Compare for less than or equal the current object's (this)
//      {\tt ESMC\_TimeInterval} with given {\tt ESMC\_TimeInterval},
//      return result.
//
//EOP
// !REQUIREMENTS:  

    // TODO: define as !(*this > timeinterval) ?

    return(TimeInterval::compare(timeinterval, ESMC_LE));

}  // end TimeInterval::operator<=

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  TimeInterval(>=) - TimeInterval greater than or equal comparison
//
// !INTERFACE:
      bool TimeInterval::operator>=(
//
// !RETURN VALUE:
//    bool result
//
// !ARGUMENTS:
      const TimeInterval &timeinterval) const {  // in - TimeInterval
                                                      //      to compare
//
// !DESCRIPTION:
//      Compare for greater than or equal the current object's (this)
//      {\tt ESMC\_TimeInterval} with given {\tt ESMC\_TimeInterval},
//      return result.
//
//EOP
// !REQUIREMENTS:  

    // TODO: define as !(*this < timeinterval) ?

    return(TimeInterval::compare(timeinterval, ESMC_GE));

}  // end TimeInterval::operator>=

//-------------------------------------------------------------------------
//BOPI
// !IROUTINE:  TimeInterval::compare - TimeInterval comparison common method
//
// !INTERFACE:
      bool TimeInterval::compare(
//
// !RETURN VALUE:
//    bool result
//
// !ARGUMENTS:
      const TimeInterval &timeinterval,            // in - 2nd to compare
            ESMC_ComparisonType comparisonType) const { // in - operator type
//
// !DESCRIPTION:
//      Captures common logic for comparing two time intervals, *this and
//      timeinterval.
//
//EOPI
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::TimeInterval::compare()"

    // TODO: use function pointer table instead of switch to perform comparison
    //       type?  or some other form of polymorphism ?

    // TODO: use some form of polymorphism to share logic with operator% and
    //       operator/ (return real) ?

    // For ESMC_EQ, first check if all members are identical (e.g. after
    // an assignment statement copy:  ti2 = ti1).  If so, return true,
    // otherwise must perform further analysis based on calendar kind
    // (e.g. for Gregorian, yy=1 is equal to mm=12)
    if (comparisonType == ESMC_EQ) {
      if (this->startTime == timeinterval.startTime &&
          this->endTime   == timeinterval.endTime &&
          this->calendar  == timeinterval.calendar &&
          this->yy        == timeinterval.yy &&
          this->mm        == timeinterval.mm &&
          this->d         == timeinterval.d &&
          this->d_r8      == timeinterval.d_r8 &&
          this->BaseTime::operator==(timeinterval)) {
        return(true);
      }
    }

    // calendars must be defined
    if (this->calendar == ESMC_NULL_POINTER ||
        timeinterval.calendar == ESMC_NULL_POINTER) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
        ", calendar required for both "
        "timeinterval arguments.",
        ESMC_CONTEXT, ESMC_NULL_POINTER);
      return(false);
    }

    // create zero basetime for comparison
    BaseTime zeroBaseTime;

    // create local copies to manipulate and compare 
    TimeInterval ti1 = *this;
    TimeInterval ti2 = timeinterval;

    // Reduce both time interval's units to the smallest and least number
    // of units possible
    ti1.TimeInterval::reduce();
    ti2.TimeInterval::reduce();

    // if both absolute, simply compare baseTime seconds and return
    if (ti1.yy == 0 && ti2.yy == 0 &&
        ti1.mm == 0 && ti2.mm == 0 &&
        ti1.d  == 0 && ti2.d  == 0 &&
        ti1.d_r8 == 0.0 && ti2.d_r8 == 0.0) {
      switch (comparisonType)
      {
        case ESMC_EQ:
          return(ti1.BaseTime::operator==(ti2));
        case ESMC_NE:
          return(ti1.BaseTime::operator!=(ti2));
        case ESMC_LT:
          return(ti1.BaseTime::operator<(ti2));
        case ESMC_GT:
          return(ti1.BaseTime::operator>(ti2));
        case ESMC_LE:
          return(ti1.BaseTime::operator<=(ti2));
        case ESMC_GE:
          return(ti1.BaseTime::operator>=(ti2));
      };
    }

    // calendars must be the same for relative comparison
    // TODO: relax this restriction (e.g. 1 month Gregorian > 27 day 360-day)
    if (ti1.calendar->calkindflag != ti2.calendar->calkindflag) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_SAMETYPE,
        "; timeinterval calendars not the same.", ESMC_CONTEXT,
        ESMC_NULL_POINTER);
      return(false);
    }

    // Perform relative comparisons based on calendar kind
    // TODO:  fractional seconds
    switch (ti1.calendar->calkindflag)  
    {
      case ESMC_CALKIND_GREGORIAN:
      case ESMC_CALKIND_JULIAN:
      case ESMC_CALKIND_NOLEAP:
        if (ti1.yy != 0 || ti2.yy != 0 ||
            ti1.d  != 0 || ti2.d  != 0 ||
            ti1.d_r8 != 0.0 || ti2.d_r8 != 0.0) {
          // shouldn't be here - yy and d already reduced in Reduce() call
          ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
            ", shouldn't be here.", ESMC_CONTEXT, ESMC_NULL_POINTER);
          return(false);

        // all relative case (yy (reduced to mm), mm, no seconds)
        } else if (zeroBaseTime == ti1 && zeroBaseTime == ti2) {
          switch (comparisonType)
          {
            case ESMC_EQ:
              return(ti1.mm == ti2.mm);
            case ESMC_NE:
              return(ti1.mm != ti2.mm);
            case ESMC_LT:
              return(ti1.mm < ti2.mm);
            case ESMC_GT:
              return(ti1.mm > ti2.mm);
            case ESMC_LE:
              return(ti1.mm <= ti2.mm);
            case ESMC_GE:
              return(ti1.mm >= ti2.mm);
          };

        // below here are mixed (relative, absolute) cases
        } else if (((ti1.mm != 0 || zeroBaseTime != ti1) &&
                     ti2.mm == 0 && zeroBaseTime == ti2)
                                                ||
                   ((ti2.mm != 0 || zeroBaseTime != ti2) &&
                     ti1.mm == 0 && zeroBaseTime == ti1)) {
          // ti1 non-zero and ti2 zero or vice versa
          // TODO: sign analysis ?  can't do if mixed signs ?
          switch (comparisonType)
          {
            case ESMC_EQ:
              return(false);
            case ESMC_NE:
              return(true);
            case ESMC_LT:
              return(false);  // TODO:
            case ESMC_GT:
              return(false);  // TODO:
            case ESMC_LE:
              return(false);  // TODO:
            case ESMC_GE:
              return(false);  // TODO:
          };

        } else {
          // TODO: can do if ti1.s and/or ti2.s < 28 days
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
            "can't compare without startTime or endtime", ESMC_CONTEXT,
            ESMC_NULL_POINTER);
          return(false);
        }
        break;
      case ESMC_CALKIND_360DAY:
        // shouldn't be here - yy, mm, d already reduced in Reduce() call above
        // TODO: write LogErr message (internal error)
        ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
          ", shouldn't be here.", ESMC_CONTEXT, ESMC_NULL_POINTER);
        return(false);
        break;
      case ESMC_CALKIND_JULIANDAY:
      case ESMC_CALKIND_MODJULIANDAY:
        if (ti1.yy != 0 || ti2.yy != 0 ||
            ti1.mm != 0 || ti2.mm != 0) {
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
            ", years and months not defined for "
            "ESMC_CALKIND_JULIANDAY or "
            "ESMC_CALKIND_MODJULIANDAY calendars.",
            ESMC_CONTEXT, ESMC_NULL_POINTER);
          return(false);
        }
        if (ti1.d != 0 || ti2.d != 0 ||
            ti1.d_r8 != 0.0 || ti2.d_r8 != 0.0) {
          // shouldn't be here - days already reduced in Reduce() call above
          ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
            ", shouldn't be here.", ESMC_CONTEXT, ESMC_NULL_POINTER);
          return(false);
        }
        break;
      case ESMC_CALKIND_NOCALENDAR:
        // can compare like units only
        if ( (ti1.yy != 0 || ti2.yy != 0) &&
              ti1.mm == 0 && ti2.mm == 0  &&
              ti1.d  == 0 && ti2.d  == 0  &&
              ti1.d_r8 == 0.0 && ti2.d_r8 == 0.0 &&
              zeroBaseTime == ti1 && zeroBaseTime == ti2) {
          // compare years only
          switch (comparisonType)
          {
            case ESMC_EQ:
              return(ti1.yy == ti2.yy);
            case ESMC_NE:
              return(ti1.yy != ti2.yy);
            case ESMC_LT:
              return(ti1.yy < ti2.yy);
            case ESMC_GT:
              return(ti1.yy > ti2.yy);
            case ESMC_LE:
              return(ti1.yy <= ti2.yy);
            case ESMC_GE:
              return(ti1.yy >= ti2.yy);
           };
        } else if ( ti1.yy == 0 && ti2.yy == 0  &&
                   (ti1.mm != 0 || ti2.mm != 0) &&
                    ti1.d  == 0 && ti2.d  == 0  &&
                    ti1.d_r8 == 0.0 && ti2.d_r8 == 0.0  &&
                    zeroBaseTime == ti1 && zeroBaseTime == ti2) {
          // compare months only
          switch (comparisonType)
          {
            case ESMC_EQ:
              return(ti1.mm == ti2.mm);
            case ESMC_NE:
              return(ti1.mm != ti2.mm);
            case ESMC_LT:
              return(ti1.mm < ti2.mm);
            case ESMC_GT:
              return(ti1.mm > ti2.mm);
            case ESMC_LE:
              return(ti1.mm <= ti2.mm);
            case ESMC_GE:
              return(ti1.mm >= ti2.mm);
           };
        // TODO:  merge d and d_r8 logic (below)
        //        handle all cases of mixing d and d_r8 within a ti and
        //        between ti1 and ti2
        } else if ( ti1.yy == 0 && ti2.yy == 0  &&
                    ti1.mm == 0 && ti2.mm == 0  &&
                   (ti1.d  != 0 || ti2.d  != 0) &&
                   ti1.d_r8 == 0.0 && ti2.d_r8 == 0.0 &&
                    zeroBaseTime == ti1 && zeroBaseTime == ti2) {
          // compare days only
          switch (comparisonType)
          {
            case ESMC_EQ:
              return(ti1.d == ti2.d);
            case ESMC_NE:
              return(ti1.d != ti2.d);
            case ESMC_LT:
              return(ti1.d < ti2.d);
            case ESMC_GT:
              return(ti1.d > ti2.d);
            case ESMC_LE:
              return(ti1.d <= ti2.d);
            case ESMC_GE:
              return(ti1.d >= ti2.d);
           };
        // TODO:  merge d and d_r8 logic (above)
        //        handle all cases of mixing d and d_r8 within a ti and
        //        between ti1 and ti2
        } else if ( ti1.yy == 0 && ti2.yy == 0  &&
                    ti1.mm == 0 && ti2.mm == 0  &&
                    ti1.d  == 0 && ti2.d  == 0 &&
                   (ti1.d_r8 != 0.0 || ti2.d_r8 != 0.0) &&
                    zeroBaseTime == ti1 && zeroBaseTime == ti2) {
          // compare days only
          switch (comparisonType)
          {
            case ESMC_EQ:
              return(fabs(ti1.d_r8 - ti2.d_r8) < 1e-10);
            case ESMC_NE:
              return(fabs(ti1.d_r8 - ti2.d_r8) >= 1e-10);
            case ESMC_LT:
              return(ti1.d_r8 < ti2.d_r8);
            case ESMC_GT:
              return(ti1.d_r8 > ti2.d_r8);
            case ESMC_LE:
              return(ti1.d_r8 <= ti2.d_r8);
            case ESMC_GE:
              return(ti1.d_r8 >= ti2.d_r8);
           };
        } else {
          ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_BAD,
            ", can't compare mixed units.", ESMC_CONTEXT, ESMC_NULL_POINTER);
          return(false);
        }
        break;
      case ESMC_CALKIND_CUSTOM:
        // TODO:
        return(false);
        break;
      default:
        char logMsg[160];
        sprintf(logMsg, "; unknown calendar kind %d.", 
                        ti1.calendar->calkindflag);
        ESMC_LogDefault.MsgFoundError(ESMC_RC_OBJ_BAD, logMsg,
          ESMC_CONTEXT, ESMC_NULL_POINTER);
        return(false);
        break;
    };

    // shouldn't be here
    // TODO: write LogErr message (internal error)
    ESMC_LogDefault.MsgFoundError(ESMC_RC_INTNRL_BAD,
      ", shouldn't be here.", ESMC_CONTEXT, ESMC_NULL_POINTER);
    return(false);

}  // end TimeInterval::compare

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  TimeInterval(=) - copy or assign from Fraction expression
//
// !INTERFACE:
      TimeInterval& TimeInterval::operator=(
//
// !RETURN VALUE:
//    TimeInterval& result
//
// !ARGUMENTS:
      const Fraction &fraction) {   // in - Fraction to copy
//
// !DESCRIPTION:
//    Assign {\tt ESMC\_Fraction} expression to this time interval.
//    Supports inherited operators from {\tt ESMC\_Fraction}.
//
//EOP
// !REQUIREMENTS:  

    // invoke fraction assignment operator; supports Time1-Time2 operator
    // in Time.  TODO:  should be implicit ?
    Fraction::operator=(fraction);

    return(*this);

}  // end TimeInterval::operator=

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  TimeInterval::readRestart - restore TimeInterval state
//
// !INTERFACE:
      int TimeInterval::readRestart(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int          nameLen,   // in
      const char  *name) {    // in
//
// !DESCRIPTION:
//      restore {\tt TimeInterval} state for persistence/checkpointing.
//
//EOP
// !REQUIREMENTS:

    int rc = ESMF_SUCCESS;

    // TODO:  read time interval state from name, then restore
    //        (share code with TimeInterval::set()).

    // TODO: use base class ReadRestart() first
    // rc = BaseTime::readRestart(s, sN, sD);

    return(rc);

 }  // end TimeInterval::readRestart

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  TimeInterval::writeRestart - return TimeInterval state
//
// !INTERFACE:
      int TimeInterval::writeRestart(void) const {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      Save {\tt TimeInterval} state for persistence/checkpointing
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

 }  // end TimeInterval::writeRestart

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  TimeInterval::validate - validate TimeInterval state
//
// !INTERFACE:
      int TimeInterval::validate(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {    // in - validate options
//
// !DESCRIPTION:
//      validate {\tt ESMC\_TimeInterval} state for testing/debugging
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::TimeInterval::validate()"

    int rc = ESMF_SUCCESS;

    rc = BaseTime::validate();
    ESMC_LogDefault.MsgFoundError(rc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc);

    return(rc);

 }  // end TimeInterval::validate

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  TimeInterval::print - print TimeInterval state
//
// !INTERFACE:
      int TimeInterval::print(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {    // in - print options
//
// !DESCRIPTION:
//      print {\tt ESMC\_TimeInterval} state for testing/debugging
//
//EOP
// !REQUIREMENTS:  

    printf("TimeInterval ---------------------------\n");

    // parse options
    if (options != ESMC_NULL_POINTER) {
      if (strncmp(options, "string", 6) == 0) {
        char timeString[160];
        TimeInterval::getString(timeString, &options[6]);
        printf("%s\n", timeString);
        // see also method TimeInterval::get()
      }
    } else {
      // default
      BaseTime::print(options);
      printf("yy    = %lld\n", yy);
      printf("mm    = %lld\n", mm);
      printf("d     = %lld\n", d);
      printf("d_r8  = %g\n", d_r8);
      if (this->calendar != ESMC_NULL_POINTER) {
        if (this->calendar->calkindflag != ESMC_CALKIND_NOCALENDAR) {
          printf("Calendar = \n");
          this->calendar->print(options);
        }
      }
      // TODO:  startTime, endTime (need to check whether initialized first)
    }

    printf("end TimeInterval -----------------------\n\n");

    return(ESMF_SUCCESS);

 }  // end TimeInterval::print

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  TimeInterval - native default C++ constructor
//
// !INTERFACE:
      TimeInterval::TimeInterval(void) {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      Initializes an {\tt ESMC\_TimeInterval} with defaults
//
//EOP
// !REQUIREMENTS:

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::TimeInterval(void) constructor"

//   BaseTime(0, 0, 1) { // TODO: F90 issue with base class constructor?

   Fraction::set(0,0,1);

   yy    = 0;
   mm    = 0;
   d     = 0;
   d_r8  = 0.0;

   if (Calendar::defaultCalendar != ESMC_NULL_POINTER) {
     // use default calendar
     calendar = Calendar::defaultCalendar; // 1st choice

   } else {
     // create default calendar
     int rc = ESMCI_CalendarSetDefault((ESMC_CalKind_Flag *)ESMC_NULL_POINTER);
     if (ESMC_LogDefault.MsgFoundError(rc, ESMCI_ERR_PASSTHRU,
       ESMC_CONTEXT, &rc))
       return;
     calendar = Calendar::defaultCalendar; // 2nd choice
   }

   // startTime, endTime initialized via their own constructor when
   // time interval instantiated.

} // end TimeInterval

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  TimeInterval - native C++ constructor
//
// !INTERFACE:
     TimeInterval::TimeInterval(
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
      ESMC_I8 s,             // in - integer seconds
      ESMC_I8 sN,            // in - fractional seconds, numerator
      ESMC_I8 sD,            // in - fractional seconds, denominator
      ESMC_I8 yy,            // in - calendar interval number of years
      ESMC_I8 mm,            // in - calendar interval number of months
      ESMC_I8 d,             // in - calendar interval number of days
      ESMC_R8 d_r8,          // in - calendar interval number of real days
      Time *startTime,       // in - interval start time
      Time *endTime,         // in - interval end time
      Calendar *calendar,    // in - calendar
      ESMC_CalKind_Flag calkindflag) :   // in - calendar kind
//
// !DESCRIPTION:
//      Initializes a {\tt ESMC\_TimeInterval} via {\tt ESMC\_BaseTime}
//      base class
//
//EOP
// !REQUIREMENTS:

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::TimeInterval(direct) constructor"

   BaseTime(s, sN, sD) {  // pass to base class constructor

   this->yy   = yy;
   this->mm   = mm;
   this->d    = d;
   this->d_r8 = d_r8;

   // startTime, endTime initialized via their own constructor when
   // time interval instantiated; override with user values here
   if (startTime != ESMC_NULL_POINTER) {
     this->startTime = *startTime;
   }
   if (endTime != ESMC_NULL_POINTER) {
     this->endTime = *endTime;
   }

   if (calendar != ESMC_NULL_POINTER) {
     // set to user's calendar
     this->calendar = calendar;                       // 1st choice

   } else if (calkindflag != (ESMC_CalKind_Flag)0) {
     // set to specified built-in type; create if necessary
     int rc = ESMCI_CalendarCreate(calkindflag);
     if (ESMC_LogDefault.MsgFoundError(rc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        &rc))
       return;
     this->calendar = Calendar::internalCalendar[calkindflag-1];
                                                      // 2nd choice

   } else if (Calendar::defaultCalendar != ESMC_NULL_POINTER) {
     // use default calendar
     this->calendar = Calendar::defaultCalendar; // 3rd choice

   } else {
     // create default calendar
     int rc = ESMCI_CalendarSetDefault((ESMC_CalKind_Flag *)ESMC_NULL_POINTER);
     if (ESMC_LogDefault.MsgFoundError(rc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
       &rc))
       return;
     this->calendar = Calendar::defaultCalendar; // 4th choice
   }

   // TODO: catch & throw exceptions above, and/or LogErr

} // end TimeInterval

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ~TimeInterval - native default C++ destructor
//
// !INTERFACE:
      TimeInterval::~TimeInterval(void) {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      Default {\tt ESMC\_TimeInterval} destructor
//
//EOP
// !REQUIREMENTS:

}  // end ~TimeInterval

//-------------------------------------------------------------------------
//  Private methods
//-------------------------------------------------------------------------

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  TimeInterval::getString - Get a Time Interval value in string format
//
// !INTERFACE:
      int TimeInterval::getString(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      char *timeString, const char *options) const {    // out - time interval
                                                        //       value in
                                                        //       string format
//
// !DESCRIPTION:
//      Gets a {\tt ESMC\_TimeInterval}'s value in ISO 8601 string format
//      PyYmMdDThHmMs[:n/d]S (hybrid) (default, options == "")
//      PyYmMdDThHmMs[.f]S   (strict) (options == "isofrac")
//      Supports {\tt ESMC\_TimeIntervalGet()} and
//               {\tt ESMC\_TimeInterval::print()}
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::TimeInterval::getString()"

    int rc = ESMF_SUCCESS;

    // validate input
    if (timeString == ESMC_NULL_POINTER) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
        "; timeString is NULL", ESMC_CONTEXT, &rc);
      return(rc);
    }

    ESMC_I8 yy_i8, mm_i8, d_i8;
    ESMC_I4 h, m, s;
    ESMC_I8 sN, sD;

    // TODO: d_r8 merged into d_i8 ?
    // TODO: use native C++ Get, not F90 entry point, when ready
    rc = TimeInterval::get((ESMC_I4 *)ESMC_NULL_POINTER,
                          &yy_i8, ESMC_NULL_POINTER,
                          &mm_i8, ESMC_NULL_POINTER,
                          &d_i8, &h, &m, &s, ESMC_NULL_POINTER,
                          ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                          ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                          ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                          ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                          ESMC_NULL_POINTER, ESMC_NULL_POINTER, 
                          ESMC_NULL_POINTER, &sN,
                          ESMC_NULL_POINTER, &sD);

    if (ESMC_LogDefault.MsgFoundError(rc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc))
      return(rc);

    // format everything except seconds
    sprintf(timeString, "P%lldY%lldM%lldDT%dH%dM", yy_i8, mm_i8, d_i8, h, m);

    // format seconds according to specified options
    bool isofrac = false;
    if (options != ESMC_NULL_POINTER) {
      if (strstr(options, "isofrac") != ESMC_NULL_POINTER) isofrac = true;
    }
    if (isofrac) {
      // strict ISO 8601 format PyYmMdDThHmMs[.f]S 

      // convert integer fractional seconds to decimal form
      ESMC_R8 fractionalSeconds = 0.0;
      if (sD != 0) fractionalSeconds = (ESMC_R8) sN / (ESMC_R8) sD;

      // if fractionalSeconds non-zero (>= 0.5 ns) append full fractional value
      if (fabs(fractionalSeconds) >= 5e-10) {
        sprintf(timeString, "%s%.9fS", timeString, (s + fractionalSeconds));
      } else { // no fractional seconds, just append integer seconds
        sprintf(timeString, "%s%dS", timeString, s);
      }
    } else { // not strict ISO fractional seconds format
      // hybrid ISO 8601 format PyYmMdDThHmMs[:n/d]S 

      // if fractionalSeconds non-zero (sN!=0) append full fractional value
      if (sN != 0) {
        sprintf(timeString, "%s%d:%lld/%lldS", timeString, s, sN, sD);
      } else { // no fractional seconds, just append integer seconds
        sprintf(timeString, "%s%dS", timeString, s);
      }
    }

    return(rc);

 }  // end TimeInterval::getString

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  TimeInterval::reduce - reduce a Time Interval to the smallest and least number of units
//
// !INTERFACE:
      int TimeInterval::reduce(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//     Determines the magnitude of a given {\tt ESMC\_TimeInterval} by reducing
//     to the smallest and least number of units possible, ideally only seconds.//     Takes into account the time interval's calendar and its start time
//     and/or end time, if set.
//
//EOP
// !REQUIREMENTS:

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::TimeInterval::reduce()"

    // Note:  Don't return ESMF_FAILURE; just reduce what we can.  Failure is
    //        determined by the method which tries to satisfy the user's
    //        request.

    // reduce any yy,mm,d to base time units (seconds) if possible,
    //   or at least to months and/or days if not
    switch (calendar->calkindflag)
    {
      case ESMC_CALKIND_GREGORIAN:
      case ESMC_CALKIND_JULIAN:
      case ESMC_CALKIND_NOLEAP:
        // use startTime to reduce yy,mm,d to seconds
        if (startTime.Time::validate("initialized") == ESMF_SUCCESS) {
          endTime = startTime + *this;
          TimeInterval ti = endTime - startTime;
          setw(ti.getw());
          //TODO: *this = endTime - startTime;
          //      should just copy base class part wholesale, rather than
          //      individual properties (including fraction sN/sD); breaks
          //      encapsulation principle.
          yy = mm = d = 0;  // yy, mm, d all reduced to base seconds

        // use endTime to reduce yy,mm,d to seconds
        } else if (endTime.Time::validate("initialized") == ESMF_SUCCESS) {
          startTime = endTime - *this;
          TimeInterval ti = endTime - startTime;
          setw(ti.getw());
          //TODO: *this = endTime - startTime;
          //      should just copy base class part wholesale, rather than
          //      individual properties (including fraction sN/sD); breaks
          //      encapsulation principle.
          yy = mm = d = 0; // yy, mm, d all reduced to base seconds

        } else { // no startTime or endTime available, reduce what we can
                 //   to (mm, s)
          if (calendar->calkindflag == ESMC_CALKIND_GREGORIAN ||
              calendar->calkindflag == ESMC_CALKIND_JULIAN) {
            // cannot reduce yy or mm to seconds, but can reduce yy to mm
            if (yy != 0) {
              mm += yy * calendar->monthsPerYear;
              yy = 0;
            }
          } else { // ESMC_CALKIND_NOLEAP
            // reduce yy to seconds
            if (yy != 0) {
              setw(getw() + yy * calendar->secondsPerYear);
              yy = 0;
            }
            // cannot reduce mm to seconds
          }
          // reduce d to seconds
          if (d != 0) {
            setw(getw() + d * calendar->secondsPerDay);
            d = 0;
          }
          // reduce d_r8 to seconds
          if (d_r8 != 0.0) {
            // avoid error introduced by floating point multiply by setting a
            // fraction in days, then performing an integer multiply by
            // the secondsPerDay unit conversion factor.
            Fraction rdays(d_r8);
            Fraction::operator+=(rdays * calendar->secondsPerDay);
            d_r8 = 0.0;
          }
          // we now have (mm, s); yy and d have been reduced
        }
        break;
      case ESMC_CALKIND_360DAY:
        if (yy != 0) {
          setw(getw() + yy*calendar->secondsPerYear);
          yy = 0;
        }
        if (mm != 0) {
          setw(getw() + mm * 30 * calendar->secondsPerDay);
          mm = 0;
        }
        if (d != 0) {
          setw(getw() + d * calendar->secondsPerDay);
          d = 0;
        }
        if (d_r8 != 0.0) {
          // avoid error introduced by floating point multiply by setting a
          // fraction in days, then performing an integer multiply by
          // the secondsPerDay unit conversion factor.
          Fraction rdays(d_r8);
          Fraction::operator+=(rdays * calendar->secondsPerDay);
          d_r8 = 0.0;
        }
        // yy, mm, d all reduced to base seconds
        break;
      case ESMC_CALKIND_JULIANDAY:
      case ESMC_CALKIND_MODJULIANDAY:
        // ignore years and months

        // reduce days to seconds
        if (d != 0) {
          setw(getw() + d * calendar->secondsPerDay);
          d = 0;
        }
        if (d_r8 != 0.0) {
          // avoid error introduced by floating point multiply by setting a
          // fraction in days, then performing an integer multiply by
          // the secondsPerDay unit conversion factor.
          Fraction rdays(d_r8);
          Fraction::operator+=(rdays * calendar->secondsPerDay);
          d_r8 = 0.0;
        }
        break;
      case ESMC_CALKIND_NOCALENDAR:
        // ignore years, months and days
        break;
      case ESMC_CALKIND_CUSTOM:
        // TODO:
        break;
      default:
        // unknown calendar kind
        char logMsg[160];
        sprintf(logMsg, "; unknown calendar kind %d.", 
                        calendar->calkindflag);
        ESMC_LogDefault.MsgFoundError(ESMC_RC_OBJ_BAD, logMsg,
          ESMC_CONTEXT, ESMC_NULL_POINTER);
        break;
    };

    return(ESMF_SUCCESS);

}  // end TimeInterval::reduce

 }
