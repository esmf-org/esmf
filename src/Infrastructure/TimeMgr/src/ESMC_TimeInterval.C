// $Id: ESMC_TimeInterval.C,v 1.52 2004/04/20 20:59:23 eschwab Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the GPL.
//
// ESMC TimeInterval method code (body) file
//
//-------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ {\tt ESMC\_TimeInterval} methods
// declared in the companion file {\tt ESMC_TimeInterval.h}.
//
//-------------------------------------------------------------------------

 // higher level, 3rd party or system includes
 #include <iostream.h>
 #include <math.h>
 #include <limits.h>
 #include <float.h>
 #include <string.h>

 #include <ESMC_Time.h>

 // associated class definition file
 #include <ESMC_TimeInterval.h>

//-------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_TimeInterval.C,v 1.52 2004/04/20 20:59:23 eschwab Exp $";
//-------------------------------------------------------------------------

//
//-------------------------------------------------------------------------
//-------------------------------------------------------------------------
//
// This section includes all the ESMC_TimeInterval routines
//
//

//-------------------------------------------------------------------------
// Class ESMC_TimeInterval Methods
//-------------------------------------------------------------------------

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeIntervalSet - initializer to support F90 interface
//
// !INTERFACE:
      int ESMC_TimeInterval::ESMC_TimeIntervalSet(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMF_KIND_I4 *yy,        // in - integer number of interval years
                               //                           (>= 32-bit)
      ESMF_KIND_I8 *yy_i8,     // in - integer number of interval years
                               //                           (large, >= 64-bit)
      ESMF_KIND_I4 *mm,        // in - integer number of interval months
                               //                           (>= 32-bit)
      ESMF_KIND_I8 *mm_i8,     // in - integer number of interval months
                               //                           (large, >= 64-bit)
      ESMF_KIND_I4 *d,         // in - integer number of interval days
                               //                           (>= 32-bit)
      ESMF_KIND_I8 *d_i8,      // in - integer number of interval days
                               //                           (large, >= 64-bit)
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
      ESMC_Time *startTime,    // in - starting time for absolute calendar
                               //      interval
      ESMC_Time *endTime,      // in - ending time for absolute calendar
                               //      interval
      ESMC_Calendar **calendar, // in - calendar for calendar interval
      ESMC_CalendarType *calendarType) { // in - calendar type for calendar interval
//
// !DESCRIPTION:
//      Initialzes a {\tt ESMC\_TimeInterval} with values given in F90
//      variable arg list.
//
//EOP
// !REQUIREMENTS:  

    // TODO: Since ESMC_TimeInterval is a shallow statically allocated class,
    //       ensure initialization if called via F90 interface;
    //       cannot call constructor, because destructor is subsequently
    //       called automatically, returning initialized values to garbage.

    // save current value to restore in case of failure
    ESMC_TimeInterval saveTimeInterval = *this;

    this->s  = 0;
    this->sN = 0;
    this->sD = 1;
    this->yy = 0;
    this->mm = 0;
    this->d  = 0;
    this->startTime.ESMC_TimeSet((ESMF_KIND_I8) 0); // |
    this->endTime.ESMC_TimeSet((ESMF_KIND_I8) 0);   //  > init to invalid, unset
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
        if (!startTime->ESMC_TimeIsSameCalendar(endTime))
          goto ESMC_TIMEINTERVALSET_FAILURE;
      }
      this->endTime = *endTime;
      this->calendar = endTime->calendar;                // 2nd choice
    }

    // set specified calendar type if neither startTime nor endTime specified
    if (startTime == ESMC_NULL_POINTER && endTime == ESMC_NULL_POINTER) {
      if (calendar != ESMC_NULL_POINTER) {
        // set to user's calendar
        this->calendar = *calendar;                      // 3rd choice

      } else if (calendarType != ESMC_NULL_POINTER) {
        // set to specified built-in type; create if necessary
        if (ESMC_CalendarCreate(*calendarType) != ESMF_SUCCESS)
          goto ESMC_TIMEINTERVALSET_FAILURE;
        this->calendar = ESMC_Calendar::internalCalendar[*calendarType-1];
                                                         // 4th choice

      } else if (ESMC_Calendar::defaultCalendar != ESMC_NULL_POINTER) {
        // use default calendar
        this->calendar = ESMC_Calendar::defaultCalendar; // 5th choice

      } else {
        // create default calendar
        if (ESMC_CalendarSetDefault((ESMC_CalendarType *)ESMC_NULL_POINTER)
            != ESMF_SUCCESS) goto ESMC_TIMEINTERVALSET_FAILURE;
        this->calendar = ESMC_Calendar::defaultCalendar; // 6th choice
      }
    }

    // Note:  No matter what calendar is associated with the time interval,
    // the time interval can still "float" across all calendars.  A calendar
    // is used only at the point of evaluation (when performing a unit
    // conversion, arithmetic operation, or comparison), so the time interval's
    // own calendar property can be overridden at that point by a subject
    // ESMC_Time's calendar or a method argument.

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

    // use base class set for sub-day values
    ESMC_BaseTimeSet(h, m, s, s_i8, ms, us, ns, h_r8, m_r8, s_r8,
                     ms_r8, us_r8, ns_r8, sN, sD);

    if (ESMC_TimeIntervalValidate() != ESMF_SUCCESS)
      goto ESMC_TIMEINTERVALSET_FAILURE;
    else return(ESMF_SUCCESS);

    // common failure handler
    ESMC_TIMEINTERVALSET_FAILURE:
      // restore previous value
      *this = saveTimeInterval;
      return(ESMF_FAILURE);

 }  // end ESMC_TimeIntervalSet

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeIntervalGet - Get a TimeInterval value; supports F90 interface
//
// !INTERFACE:
      int ESMC_TimeInterval::ESMC_TimeIntervalGet(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMF_KIND_I4 *yy,         // out - integer number of interval years
                                //                           (>= 32-bit)
      ESMF_KIND_I8 *yy_i8,      // out - integer number of interval years
                                //                           (large, >= 64-bit)
      ESMF_KIND_I4 *mm,         // out - integer number of interval months
                                //                           (>= 32-bit)
      ESMF_KIND_I8 *mm_i8,      // out - integer number of interval months
                                //                           (large, >= 64-bit)
      ESMF_KIND_I4 *d,          // out - integer number of interval days
                                //                           (>= 32-bit)
      ESMF_KIND_I8 *d_i8,       // out - integer number of interval days
                                //                           (large, >= 64-bit)
      ESMF_KIND_I4 *h,          // out - integer hours
      ESMF_KIND_I4 *m,          // out - integer minutes
      ESMF_KIND_I4 *s,          // out - integer seconds (>= 32-bit)
      ESMF_KIND_I8 *s_i8,       // out - integer seconds (large, >= 64-bit)
      ESMF_KIND_I4 *ms,         // out - integer milliseconds
      ESMF_KIND_I4 *us,         // out - integer microseconds
      ESMF_KIND_I4 *ns,         // out - integer nanoseconds
      ESMF_KIND_R8 *d_r8,       // out - floating point days
      ESMF_KIND_R8 *h_r8,       // out - floating point hours
      ESMF_KIND_R8 *m_r8,       // out - floating point minutes
      ESMF_KIND_R8 *s_r8,       // out - floating point seconds
      ESMF_KIND_R8 *ms_r8,      // out - floating point milliseconds
      ESMF_KIND_R8 *us_r8,      // out - floating point microseconds
      ESMF_KIND_R8 *ns_r8,      // out - floating point nanoseconds
      ESMF_KIND_I4 *sN,         // out - fractional seconds numerator
      ESMF_KIND_I4 *sD,         // out - fractional seconds denominator
      ESMC_Time *startTime,     // out - starting time of absolute calendar
                                //       interval
      ESMC_Time *endTime,       // out - ending time of absolute calendar
                                //       interval
      ESMC_Calendar **calendar, // out - calendar of calendar interval
      ESMC_CalendarType *calendarType, // out - calendar type of
                                       //       calendar interval
      ESMC_Time *startTimeIn,   // in  - starting time for calendar interval
                                //       unit conversions
      ESMC_Time *endTimeIn,     // in  - ending time for calendar interval
                                //       unit conversions
      ESMC_Calendar **calendarIn, // in  - calendar for calendar interval
                                  //       unit conversions
      ESMC_CalendarType *calendarTypeIn, // in  - calendar type for calendar
                                         //       interval unit conversions
      char *timeString) const {   // out - ISO 8601 format PyYmMdDThHmMsS
//
// !DESCRIPTION:
//      Gets a {\tt ESMC\_TimeInterval}'s values in user-specified format.
//      This version supports the F90 interface.
//
//EOP
// !REQUIREMENTS:  

    // TODO: fractional, sub-seconds

    // TODO: put calendar logic under test for any non-zero yy, mm, d ?

    // TODO: reduce size of this method by creating seperate methods on
    //       ESMC_TimeInterval and ESMC_Calendar ?

    // timeInterval-to-convert.  This is used to reduce this time
    // interval's units, which is later used to to convert to user-requested
    // units.  Represents remaining unconverted time; any years, months or days
    // later requested from timeInterval-to-convert will be removed from
    // timeInterval-to-convert.  In this way, a requested unit is bounded
    // (normalized) by the next higher requested unit.

    ESMC_TimeInterval tiToConvert = *this;

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

    } else if (calendarTypeIn != ESMC_NULL_POINTER) {  // 4th choice
      // use specified built-in type; create if necessary
      if (ESMC_CalendarCreate(*calendarType) != ESMF_SUCCESS)
        return (ESMF_FAILURE);
      tiToConvert.calendar = ESMC_Calendar::internalCalendar[*calendarType-1];

    } else if (this->calendar != ESMC_NULL_POINTER) {  // 5th choice
      // use this time interval's calendar property
      tiToConvert.calendar = this->calendar;
    }

    // at least default calendar must have been determined
    if (tiToConvert.calendar == ESMC_NULL_POINTER) return (ESMF_FAILURE);

    // if no calendar info, then if any relative calendar unit was Set(),
    //   must Get() it back, otherwise impossible conversion is implied.
    if (tiToConvert.calendar->calendarType == ESMC_CAL_NOCALENDAR) {
      // if yy was set, must get it
      if (this->yy != 0 &&
          (yy == ESMC_NULL_POINTER && yy_i8 == ESMC_NULL_POINTER))
            return(ESMF_FAILURE);
      // if mm was set, must get it
      if (this->mm != 0 &&
          (mm == ESMC_NULL_POINTER && mm_i8 == ESMC_NULL_POINTER))
            return(ESMF_FAILURE);
      // if d was set, must get it
      if (this->d != 0 &&
          (d == ESMC_NULL_POINTER && d_i8 == ESMC_NULL_POINTER))
            return(ESMF_FAILURE);
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
    //   of units possible
    //---------------------------------------------------------------------

    tiToConvert.calendar->ESMC_CalendarIntervalMagnitude(tiToConvert);

    //---------------------------------------------------------------------
    // convert from reduced units to user-requested units, keeping them
    //   bound (normalized) to the next larger requested unit
    //---------------------------------------------------------------------

    // convert from reduced units to any user-requested years
    if (yy != ESMC_NULL_POINTER || yy_i8 != ESMC_NULL_POINTER) {
      // total relative and absolute years
      ESMF_KIND_I8 years = tiToConvert.yy;
      tiToConvert.yy = 0;
      switch (tiToConvert.calendar->calendarType)  
      {
        case ESMC_CAL_GREGORIAN:
        case ESMC_CAL_NOLEAP:
          {
            // TODO: use TimeInterval operators (/) and (-) when ready ?

            // get years using startTime/endTime, if available
            if (tiToConvert.startTime.ESMC_TimeValidate() == ESMF_SUCCESS ||
                tiToConvert.endTime.ESMC_TimeValidate() == ESMF_SUCCESS) {
              ESMC_TimeInterval oneYear(0, 0, 1, 1);
              ESMC_Time iTime = tiToConvert.startTime + oneYear;
              years = 0;
              while (iTime <= tiToConvert.endTime) {
                years++;
                iTime += oneYear;
              }

              // move tiToConvert.startTime up to end of last year converted
              tiToConvert.startTime = iTime - oneYear;

              // calculate remaining baseTimeToConvert (remove years we got)
              ESMC_TimeInterval ti =
                                   tiToConvert.endTime - tiToConvert.startTime;
              tiToConvert.s = ti.s;
              //TODO: tiToConvert = tiToConvert.endTime - tiToConvert.startTime;
              //      should just copy base class part wholesale, rather than
              //      individual properties (including fraction sN/sD); breaks
              //      encapsulation principle.

            } else { // no startTime or endTime available, convert what we can
                     //   from (mm, s)
              years = 0;
              // convert mm for either ESMC_CAL_GREGORIAN or ESMC_CAL_NOLEAP
              if (tiToConvert.mm != 0) {
                years = tiToConvert.mm / tiToConvert.calendar->monthsPerYear;
                tiToConvert.mm %= tiToConvert.calendar->monthsPerYear;
              } 

              // convert remaining seconds to years
              if (tiToConvert.calendar->calendarType == ESMC_CAL_NOLEAP) {
                years += tiToConvert.s / tiToConvert.calendar->secondsPerYear;
                tiToConvert.s %= tiToConvert.calendar->secondsPerYear;

              } else { // ESMC_CAL_GREGORIAN
                // note: can't use abs() or labs() since result is (long long)
                //       could use ISO llabs() if supported on all platforms
                if (tiToConvert.s >= tiToConvert.calendar->secondsPerYear ||
                    tiToConvert.s <= -tiToConvert.calendar->secondsPerYear){
                  // tiToConvert.s >= 1 year => can't determine leap years
                  //   without startTime or endTime !
                  return (ESMF_FAILURE);
                }
              }
            }
          }
          break;
        case ESMC_CAL_360DAY:
          years = tiToConvert.s / tiToConvert.calendar->secondsPerYear;
          tiToConvert.s %= tiToConvert.calendar->secondsPerYear;
          break;
        case ESMC_CAL_JULIANDAY:
          // years not defined!
          return(ESMF_FAILURE);
        case ESMC_CAL_NOCALENDAR:
          // years not defined, but allow for requesting what was set
          break;
        case ESMC_CAL_CUSTOM:
          // TODO:
          break;
        default:
          // unknown calendar type
          break;
      };

      // return requested years value
      if (yy != ESMC_NULL_POINTER) {
        // ensure fit in given int
        if (years < INT_MIN || years > INT_MAX) return(ESMF_FAILURE);
        *yy = (ESMF_KIND_I4) years;  // >= 32-bit
      }
      if (yy_i8 != ESMC_NULL_POINTER) {
        *yy_i8 = years;  // >= 64-bit
      }
    }

    // convert from remaining reduced units to any user-requested months
    if (mm != ESMC_NULL_POINTER || mm_i8 != ESMC_NULL_POINTER) {
      // total relative months
      ESMF_KIND_I8 months = tiToConvert.mm;
      tiToConvert.mm = 0;
      switch (tiToConvert.calendar->calendarType)
      {
        case ESMC_CAL_GREGORIAN:
        case ESMC_CAL_NOLEAP:

          // TODO: use TimeInterval operators (/) and (-) when ready ?

          // get months using startTime, if available
          if (tiToConvert.startTime.ESMC_TimeValidate() == ESMF_SUCCESS ||
              tiToConvert.endTime.ESMC_TimeValidate() == ESMF_SUCCESS) {
            ESMC_TimeInterval oneMonth(0, 0, 1, 0, 1);
            ESMC_Time iTime = tiToConvert.startTime + oneMonth;
            months = 0;
            while (iTime <= tiToConvert.endTime) {
              months++;
              iTime += oneMonth;
            }

            // move tiToConvert.startTime up to end of last month converted
            tiToConvert.startTime = iTime - oneMonth;

            // calculate remaining baseTimeToConvert (remove months we got)
            ESMC_TimeInterval ti = tiToConvert.endTime - tiToConvert.startTime;
            tiToConvert.s = ti.s;
            //TODO: tiToConvert = tiToConvert.endTime - tiToConvert.startTime;
            //      should just copy base class part wholesale, rather than
            //      individual properties (including fraction sN/sD); breaks
            //      encapsulation principle.

          } else { // no startTime or endTime available, convert what we can
            if (tiToConvert.s >= (28 * tiToConvert.calendar->secondsPerDay)
                                  ||
               tiToConvert.s <= (-28 * tiToConvert.calendar->secondsPerDay))
                // note: can't use abs() or labs() since result is (long long)
                //       could use ISO llabs() if supported on all platforms
            {
              // can't determine months without startTime or endTime !
              // TODO:  leave alone and let d,h,m,s be more than a month ?
              //        (unbounded or unnormalized ?) with ESMF_WARNING ?
              return (ESMF_FAILURE);
            }
          }
          break;
        case ESMC_CAL_360DAY:
          months = tiToConvert.s / (30 * tiToConvert.calendar->secondsPerDay);
          tiToConvert.s %= (30 * tiToConvert.calendar->secondsPerDay);
          break;
        case ESMC_CAL_JULIANDAY:
          // months not defined!
          return(ESMF_FAILURE);
        case ESMC_CAL_NOCALENDAR:
          // months not defined, but allow for requesting what was set
          break;
        case ESMC_CAL_CUSTOM:
          // TODO:
          break;
        default:
          // unknown calendar type
          break;
      };
    
      // return requested months value
      if (mm != ESMC_NULL_POINTER) {
        // ensure fit in given int
        if (months < INT_MIN || months > INT_MAX) return(ESMF_FAILURE);
        *mm = (ESMF_KIND_I4) months;  // >= 32-bit
      }
      if (mm_i8 != ESMC_NULL_POINTER) {
        *mm_i8 = months;   // >= 64-bit
      }
    }

    // convert from remaining reduced units to any user-requested days
    if (d != ESMC_NULL_POINTER || d_i8 != ESMC_NULL_POINTER) {
      // total relative and absolute days
      ESMF_KIND_I8 days = tiToConvert.d; // relative part 
      tiToConvert.d = 0;           // TODO: don't need tiToConvert.d,
                                   // always = 0?

      // don't need to check for years (tiToConvert.yy) to convert to days
      //   since for all calendar types (except custom TODO), years will
      //   have already been reduced to either months (tiToConvert.mm) or
      //   seconds (tiToConvert.s).

      // check for any months to convert to days
      switch (tiToConvert.calendar->calendarType)  
      {
        case ESMC_CAL_GREGORIAN:
          if (tiToConvert.mm != 0) {
            // no startTime or endTime available, can't do
            return (ESMF_FAILURE);
          }
          break;
        case ESMC_CAL_NOLEAP:
          if (tiToConvert.mm != 0) {
            // no startTime or endTime available, can only do if months
            // are an integral number of years
            //   note: can't use abs() or labs() since result is (long long)
            //         could use ISO llabs() if supported on all platforms
            if ( (tiToConvert.mm >=  tiToConvert.calendar->monthsPerYear || 
                  tiToConvert.mm <= -tiToConvert.calendar->monthsPerYear) &&
                  tiToConvert.mm  %  tiToConvert.calendar->monthsPerYear == 0) {
              days += (tiToConvert.mm / tiToConvert.calendar->monthsPerYear) *
                                     tiToConvert.calendar->daysPerYear.d;
              tiToConvert.mm = 0;
            } else {
              // can't do
              return (ESMF_FAILURE);
            }
          }
          break;
        case ESMC_CAL_360DAY:
          // nothing to do: 360Day => months already reduced to seconds;
          break;
        case ESMC_CAL_JULIANDAY:
        case ESMC_CAL_NOCALENDAR:
          //   JulianDay and NoCalendar => months don't apply
          if (tiToConvert.mm != 0) {
            // can't convert months to days without appropriate calendar!
            return(ESMF_FAILURE);
          }
          break;
        case ESMC_CAL_CUSTOM:
          // TODO:
          break;
        default:
          // unknown calendar type
          break;
      };

      // convert any days from base time
      if (tiToConvert.calendar->secondsPerDay != 0) {
        // absolute part
        days += tiToConvert.s / tiToConvert.calendar->secondsPerDay;
                      // TODO: assign, not add, if tiToConvert.d always = 0 ?
        // remove days from base conversion time to get remaining sub-day
        // units (h,m,s)
        tiToConvert.s %= tiToConvert.calendar->secondsPerDay;
      }

      // return requested days value
      if (d != ESMC_NULL_POINTER) {
        // ensure fit in given int
        if (days < INT_MIN || days > INT_MAX) return(ESMF_FAILURE);
        *d = (ESMF_KIND_I4) days;  // >= 32-bit
      }
      if (d_i8 != ESMC_NULL_POINTER) {
        *d_i8 = days;  // >= 64-bit
      }
    }

    // use base class to get sub-day values (h,m,s) on remaining
    //   unconverted base time
    if (ESMC_BaseTimeGet(tiToConvert.s, h, m, s, s_i8, ms, us, ns,
                         h_r8, m_r8, s_r8, ms_r8, us_r8, ns_r8, sN, sD) !=
                         ESMF_SUCCESS) return(ESMF_FAILURE);

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
    if (calendarType != ESMC_NULL_POINTER) {
      if (this->calendar == ESMC_NULL_POINTER) return(ESMF_FAILURE);
      *calendarType = this->calendar->calendarType;
    }

    // if requested, return time interval in string format
    if (timeString != ESMC_NULL_POINTER) {
      if (ESMC_TimeIntervalGetString(timeString) != ESMF_SUCCESS)
        return(ESMF_FAILURE);
    }

    return(ESMF_SUCCESS);

 }  // end ESMC_TimeIntervalGet

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeIntervalSet - Set a TimeInterval value
//
// !INTERFACE:
      int ESMC_TimeInterval::ESMC_TimeIntervalSet(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *timeList,    // in - time interval value specifier string
      ...) {                   // in - specifier values (variable args)
//
// !DESCRIPTION:
//      Sets a {\tt ESMC\_TimeInterval}'s values in user-specified values.
//      Supports native C++ use.
//
//EOP
// !REQUIREMENTS:  

    // TODO
    return(ESMF_SUCCESS);

 }  // end ESMC_TimeIntervalSet

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeIntervalGet - Get a TimeInterval value
//
// !INTERFACE:
      int ESMC_TimeInterval::ESMC_TimeIntervalGet(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *timeList,    // in  - time interval value specifier string
      ...) const {             // out - specifier values (variable args)
//
// !DESCRIPTION:
//      Gets a {\tt ESMC\_TimeInterval}'s values in user-specified format.
//      Supports native C++ use.
//
//EOP
// !REQUIREMENTS:  

    // TODO
    return(ESMF_SUCCESS);

 }  // end ESMC_TimeIntervalGet

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeIntervalSet - direct property initializer
//
// !INTERFACE:
      int ESMC_TimeInterval::ESMC_TimeIntervalSet(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMF_KIND_I8 s,            // in - integer seconds
      ESMF_KIND_I4 sN,           // in - fractional seconds, numerator
      ESMF_KIND_I4 sD,           // in - fractional seconds, denominator
      ESMF_KIND_I8 yy,           // in - calendar interval number of years
      ESMF_KIND_I8 mm,           // in - calendar interval number of months
      ESMF_KIND_I8 d,            // in - calendar interval number of days
      ESMC_Time *startTime,      // in - interval startTime
      ESMC_Time *endTime,        // in - interval endTime
      ESMC_Calendar *calendar,   // in - associated calendar
      ESMC_CalendarType calendarType) { // in - associated calendar type
//
// !DESCRIPTION:
//      Initialzes a {\tt TimeInterval} with given values.  Used to avoid
//      constructor to cover case when initial entry is from F90, since
//      destructor is called automatically when leaving scope to return to F90.
//
//EOP
// !REQUIREMENTS:

    // use base class Set()
    if (ESMC_BaseTime::ESMC_BaseTimeSet(s, sN, sD) != ESMF_SUCCESS)
      return(ESMF_FAILURE);

    this->yy = yy;
    this->mm = mm;
    this->d  = d;

    if (startTime != ESMC_NULL_POINTER) {
      this->startTime = *startTime;
    } else this->startTime.ESMC_TimeSet((ESMF_KIND_I8) 0);

    if (endTime != ESMC_NULL_POINTER) {
      this->endTime = *endTime;
    } else this->endTime.ESMC_TimeSet((ESMF_KIND_I8) 0);

    if (calendar != ESMC_NULL_POINTER) {
      // set to user's calendar
      this->calendar = calendar;                       // 1st choice

    } else if (calendarType != (ESMC_CalendarType)0) {
      // set to specified built-in type; create if necessary
      if (ESMC_CalendarCreate(calendarType) != ESMF_SUCCESS)
        return(ESMF_FAILURE);
      this->calendar = ESMC_Calendar::internalCalendar[calendarType-1];
                                                       // 2nd choice

    } else if (ESMC_Calendar::defaultCalendar != ESMC_NULL_POINTER) {
      // use default calendar
      this->calendar = ESMC_Calendar::defaultCalendar; // 3rd choice

    } else {
      // create default calendar
      if (ESMC_CalendarSetDefault((ESMC_CalendarType *)ESMC_NULL_POINTER)
        != ESMF_SUCCESS) return (ESMF_FAILURE);
      this->calendar = ESMC_Calendar::defaultCalendar; // 4th choice
    }

    return(ESMF_SUCCESS);

}  // end ESMC_TimeIntervalSet

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeIntervalAbsValue - Get a Time Interval's absolute value
//
// !INTERFACE:
      ESMC_TimeInterval
                     ESMC_TimeInterval::ESMC_TimeIntervalAbsValue(void) const{
//
// !RETURN VALUE:
//    ESMC_TimeInterval result
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      Gets a {\tt ESMC\_TimeInterval}'s absolute value
//
//EOP
// !REQUIREMENTS:  TMG 1.5.8

    // initialize result to subject time interval
    ESMC_TimeInterval absValue = *this;

    // check individual components (should be all positive or all negative)
    //   note: can't use abs() or labs() since these will be (long long)
    //         (64-bit) on some platforms.  Could use ISO llabs() if
    //         supported on all platforms
    if (s  < 0) absValue.s  *= -1;
    if (sN < 0) absValue.sN *= -1;
    if (yy < 0) absValue.yy *= -1;
    if (mm < 0) absValue.mm *= -1;
//   TODO: use when Calendar Intervals implemented
//    if (d  < 0) absValue.d  *= -1;

    return(absValue);

 }  // end ESMC_TimeIntervalAbsValue

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeIntervalNegAbsValue - Get a Time Interval's negative absolute value
//
// !INTERFACE:
      ESMC_TimeInterval
                  ESMC_TimeInterval::ESMC_TimeIntervalNegAbsValue(void) const {
//
// !RETURN VALUE:
//    ESMC_TimeInterval result
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      Gets a {\tt ESMC\_TimeInterval}'s negative absolute value
//
//EOP
// !REQUIREMENTS:  TMG 1.5.8

    // initialize result to subject time interval
    ESMC_TimeInterval negAbsValue = *this;

    // check individual components (should be all positive or all negative)
    //   note: can't use abs() or labs() since these will be (long long)
    //         (64-bit) on some platforms.  Could use ISO llabs() if
    //         supported on all platforms
    if (s  > 0) negAbsValue.s  *= -1;
    if (sN > 0) negAbsValue.sN *= -1;
    if (yy > 0) negAbsValue.yy *= -1;
    if (mm > 0) negAbsValue.mm *= -1;
//   TODO: use when Calendar Intervals implemented
//    if (d  > 0) negAbsValue.d  *= -1;

    return(negAbsValue);

 }  // end ESMC_TimeIntervalNegAbsValue

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeInterval(/) - Divide two time intervals, return double precision result
//
// !INTERFACE:
      ESMF_KIND_R8 ESMC_TimeInterval::operator/(
//
// !RETURN VALUE:
//    ESMF_KIND_R8 result
//
// !ARGUMENTS:
      const ESMC_TimeInterval &timeInterval) const {  // in - ESMC_TimeInterval
                                                      //        to divide by
//
// !DESCRIPTION:
//    Returns this time interval divided by given time interval as a double
//    precision quotient.
//
//EOP
// !REQUIREMENTS:  

    ESMF_KIND_R8 quotient;

    // TODO: fractional & calendar interval parts

    if (timeInterval.s != 0) {
      quotient = (ESMF_KIND_R8) this->s / (ESMF_KIND_R8) timeInterval.s;
    }
    // TODO:  else throw exception ?

    return(quotient);

}  // end ESMC_TimeInterval::operator/

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeInterval(/) - Divide time interval by an integer, return time interval result
//
// !INTERFACE:
      ESMC_TimeInterval ESMC_TimeInterval::operator/(
//
// !RETURN VALUE:
//    ESMC_TimeInterval result
//
// !ARGUMENTS:
      const ESMF_KIND_I4 &divisor) const {   // in - integer divisor
//
// !DESCRIPTION:
//    Divides a {\tt ESMC\_TimeInterval} by an integer divisor,
//    returns quotient as a {\tt ESMC\_TimeInterval}.
//
//EOP
// !REQUIREMENTS:  

    ESMC_TimeInterval quotient;

    // TODO: fractional & calendar interval parts

    if (divisor != 0) {
      quotient.s = this->s / divisor;
    }
    // TODO:  else throw exception ?

    return(quotient);

}  // end ESMC_TimeInterval::operator/

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeInterval(/=) - Divide time interval by an integer
//
// !INTERFACE:
      ESMC_TimeInterval& ESMC_TimeInterval::operator/=(
//
// !RETURN VALUE:
//    ESMC_TimeInterval& result
//
// !ARGUMENTS:
      const ESMF_KIND_I4 &divisor) {   // in - integer divisor
//
// !DESCRIPTION:
//    Divides a {\tt ESMC\_TimeInterval} by an integer divisor
//
//EOP
// !REQUIREMENTS:  

    // TODO: fractional & calendar interval parts

    if (divisor != 0) {
      this->s /= divisor;
    }
    // TODO:  else throw exception ?

    return(*this);

}  // end ESMC_TimeInterval::operator/=

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeInterval(/) - Divide time interval by a double precision, return time interval result
//
// !INTERFACE:
      ESMC_TimeInterval ESMC_TimeInterval::operator/(
//
// !RETURN VALUE:
//    ESMC_TimeInterval result
//
// !ARGUMENTS:
      const ESMF_KIND_R8 &divisor) const {   // in - double precision divisor
//
// !DESCRIPTION:
//    Divides a {\tt ESMC\_TimeInterval} by an double divisor,
//    returns quotient as a {\tt ESMC\_TimeInterval}
//
//EOP
// !REQUIREMENTS:  

    ESMC_TimeInterval quotient;

    // TODO: fractional & calendar interval parts

    if (fabs(divisor) > FLT_EPSILON) {
      quotient.s = (ESMF_KIND_I8) ((ESMF_KIND_R8) this->s / divisor);
    }
    // TODO:  else throw exception ?

    return(quotient);

}  // end ESMC_TimeInterval::operator/

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeInterval(/=) - Divide time interval by a double precision
//
// !INTERFACE:
      ESMC_TimeInterval& ESMC_TimeInterval::operator/=(
//
// !RETURN VALUE:
//    ESMC_TimeInterval& result
//
// !ARGUMENTS:
      const ESMF_KIND_R8 &divisor) {   // in - double precision divisor
//
// !DESCRIPTION:
//    Divides a {\tt ESMC\_TimeInterval} by a double precision divisor
//
//EOP
// !REQUIREMENTS:  

    // TODO: fractional & calendar interval parts

    if (fabs(divisor) > FLT_EPSILON) {
      this->s = (ESMF_KIND_I8) ((ESMF_KIND_R8) this->s / divisor);
    }
    // TODO:  else throw exception ?

    return(*this);

}  // end ESMC_TimeInterval::operator/=

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeIntervalDiv - Divide two time intervals, return fraction result
//
// !INTERFACE:
      ESMC_Fraction ESMC_TimeInterval::ESMC_TimeIntervalDiv(
//
// !RETURN VALUE:
//    ESMC_Fraction result
//
// !ARGUMENTS:
      const ESMC_TimeInterval &timeInterval) const {  // in - ESMC_TimeInterval
                                                      //        to divide by
//
// !DESCRIPTION:
//    Returns this time interval divided by given time interval as a fractional
//    quotient.
//
//EOP
// !REQUIREMENTS:  

    ESMC_Fraction quotient;

    // TODO:

    return(quotient);

}  // end ESMC_TimeInterval::ESMC_TimeIntervalDiv

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeInterval(\%) - Divide two time intervals, return time interval remainder
//
// !INTERFACE:
      ESMC_TimeInterval ESMC_TimeInterval::operator%(
//
// !RETURN VALUE:
//    ESMC_TimeInterval result
//
// !ARGUMENTS:
      const ESMC_TimeInterval &timeInterval) const {  // in - ESMC_TimeInterval
                                                      //        to modulo by
//
// !DESCRIPTION:
//    Returns this time interval modulo by given time interval as a 
//    {\tt ESMC\_TimeInterval}
//
//EOP
// !REQUIREMENTS:  

    ESMC_TimeInterval remainder;

    // TODO: fractional & calendar interval parts

    if (timeInterval.s != 0) {
      remainder.s = this->s % timeInterval.s;
    }
    // TODO:  else throw exception ?

    return(remainder);

}  // end ESMC_TimeInterval::operator%

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeInterval(\%=) - Takes the modulus of two time intervals
//
// !INTERFACE:
      ESMC_TimeInterval& ESMC_TimeInterval::operator%=(
//
// !RETURN VALUE:
//    ESMC_TimeInterval& result
//
// !ARGUMENTS:
      const ESMC_TimeInterval &timeInterval) {  // in - ESMC_TimeInterval
                                                //        to modulo by
//
// !DESCRIPTION:
//    Returns this time interval modulo by given time interval
//
//EOP
// !REQUIREMENTS:  

    // TODO: fractional & calendar interval parts

    if (timeInterval.s != 0) {
      this->s %= timeInterval.s;
    }
    // TODO:  else throw exception ?

    return(*this);

}  // end ESMC_TimeInterval::operator%=

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeInterval(*) - Multiply a time interval by an integer
//
// !INTERFACE:
      ESMC_TimeInterval ESMC_TimeInterval::operator*(
//
// !RETURN VALUE:
//    ESMC_TimeInterval result
//
// !ARGUMENTS:
      const ESMF_KIND_I4 &multiplier) const {   // in - integer multiplier
//
// !DESCRIPTION:
//     Multiply a {\tt ESMC\_TimeInterval} by an integer, return product as a
//    {\tt ESMC\_TimeInterval}
//
//EOP
// !REQUIREMENTS:  

    ESMC_TimeInterval product;

    // TODO: fractional interval parts

    // multiply relative yy, mm, d parts
    product.yy = this->yy * multiplier;
    product.mm = this->mm * multiplier;
    product.d  = this->d  * multiplier;

    // multiply absolute s part
    product.s = this->s * multiplier;

    return(product);

}  // end ESMC_TimeInterval::operator*

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeInterval(*) - Multiply a time interval by an integer
//
// !INTERFACE:
      ESMC_TimeInterval operator*(
//
// !RETURN VALUE:
//    ESMC_TimeInterval result
//
// !ARGUMENTS:
      const ESMF_KIND_I4 &multiplier,  // in - integer multiplier
      const ESMC_TimeInterval &ti) {   // in - TimeInterval multiplicand
//
// !DESCRIPTION:
//     Multiply a {\tt ESMC\_TimeInterval} by an integer, return product as a
//    {\tt ESMC\_TimeInterval}
//
//EOP
// !REQUIREMENTS:  

    ESMC_TimeInterval product;

    // TODO: fractional interval parts

    // multiply relative yy, mm, d parts
    product.yy = multiplier * ti.yy;
    product.mm = multiplier * ti.mm;
    product.d  = multiplier * ti.d;

    // multiply absolute s part
    product.s = multiplier * ti.s;

    return(product);

}  // end operator*

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeInterval(*=) - Multiply a time interval by an integer
//
// !INTERFACE:
      ESMC_TimeInterval& ESMC_TimeInterval::operator*=(
//
// !RETURN VALUE:
//    ESMC_TimeInterval& result
//
// !ARGUMENTS:
      const ESMF_KIND_I4 &multiplier) {   // in - integer multiplier
//
// !DESCRIPTION:
//     Multiply a {\tt ESMC\_TimeInterval} by an integer
//
//EOP
// !REQUIREMENTS:  

    // TODO: fractional interval parts

    // multiply relative yy, mm, d parts
    this->yy *= multiplier;
    this->mm *= multiplier;
    this->d  *= multiplier;

    // multiply absolute s part
    this->s *= multiplier;

    return(*this);

}  // end ESMC_TimeInterval::operator*=

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeInterval(*) - Multiply a time interval by an fraction
//
// !INTERFACE:
      ESMC_TimeInterval ESMC_TimeInterval::operator*(
//
// !RETURN VALUE:
//    ESMC_TimeInterval result
//
// !ARGUMENTS:
      const ESMC_Fraction &multiplier) const {   // in - fraction multiplier
//
// !DESCRIPTION:
//     Multiply a {\tt ESMC\_TimeInterval} by an fraction, return product as a
//    {\tt ESMC\_TimeInterval}
//
//EOP
// !REQUIREMENTS:  

    ESMC_TimeInterval product;

    // TODO: whole, fractional & calendar interval parts

    return(product);

}  // end ESMC_TimeInterval::operator*

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeInterval(*) - Multiply a time interval by an fraction
//
// !INTERFACE:
      ESMC_TimeInterval operator*(
//
// !RETURN VALUE:
//    ESMC_TimeInterval result
//
// !ARGUMENTS:
      const ESMC_Fraction &multiplier, // in - fraction multiplier
      const ESMC_TimeInterval &ti) {   // in - TimeInterval multiplicand
//
// !DESCRIPTION:
//     Multiply a {\tt ESMC\_TimeInterval} by an fraction, return product as a
//    {\tt ESMC\_TimeInterval}
//
//EOP
// !REQUIREMENTS:  

    ESMC_TimeInterval product;

    // TODO: whole, fractional & calendar interval parts

    return(product);

}  // end operator*

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeInterval(*=) - Multiply a time interval by an fraction
//
// !INTERFACE:
      ESMC_TimeInterval& ESMC_TimeInterval::operator*=(
//
// !RETURN VALUE:
//    ESMC_TimeInterval& result
//
// !ARGUMENTS:
      const ESMC_Fraction &multiplier) {   // in - fraction multiplier
//
// !DESCRIPTION:
//     Multiply a {\tt ESMC\_TimeInterval} by a fraction
//
//EOP
// !REQUIREMENTS:  

    // TODO: whole, fractional & calendar interval parts

    return(*this);

}  // end ESMC_TimeInterval::operator*=

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeInterval(*) - Multiply a time interval by a double precision
//
// !INTERFACE:
      ESMC_TimeInterval ESMC_TimeInterval::operator*(
//
// !RETURN VALUE:
//    ESMC_TimeInterval result
//
// !ARGUMENTS:
      const ESMF_KIND_R8 &multiplier) const {   // in - double precision
                                                 //   multiplier
//
// !DESCRIPTION:
//     Multiply a {\tt ESMC\_TimeInterval} by an double precision,
//     return product as a {\tt ESMC\_TimeInterval}
//
//EOP
// !REQUIREMENTS:  

    ESMC_TimeInterval product;

    // TODO: fractional & calendar interval parts

    product.s = (ESMF_KIND_I8) ((ESMF_KIND_R8) this->s * multiplier);

    return(product);

}  // end ESMC_TimeInterval::operator*

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeInterval(*) - Multiply a time interval by a double precision
//
// !INTERFACE:
      ESMC_TimeInterval operator*(
//
// !RETURN VALUE:
//    ESMC_TimeInterval result
//
// !ARGUMENTS:
      const ESMF_KIND_R8 &multiplier,  // in - double precision
      const ESMC_TimeInterval &ti) {   // in - TimeInterval multiplicand
                                                 //   multiplier
//
// !DESCRIPTION:
//     Multiply a {\tt ESMC\_TimeInterval} by an double precision,
//     return product as a {\tt ESMC\_TimeInterval}
//
//EOP
// !REQUIREMENTS:  

    ESMC_TimeInterval product;

    // TODO: fractional & calendar interval parts

    product.s = (ESMF_KIND_I8) (multiplier * (ESMF_KIND_R8) ti.s);

    return(product);

}  // end operator*

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeInterval(*=) - Multiply a time interval by a double precision
//
// !INTERFACE:
      ESMC_TimeInterval& ESMC_TimeInterval::operator*=(
//
// !RETURN VALUE:
//    ESMC_TimeInterval& result
//
// !ARGUMENTS:
      const ESMF_KIND_R8 &multiplier) {   // in - double precision multiplier
//
// !DESCRIPTION:
//     Multiply a {\tt ESMC\_TimeInterval} by a double precision
//
//EOP
// !REQUIREMENTS:  

    // TODO: fractional & calendar interval parts

    this->s = (ESMF_KIND_I8) ((ESMF_KIND_R8) this->s * multiplier);

    return(*this);

}  // end ESMC_TimeInterval::operator*=

//-------------------------------------------------------------------------
//BOP 
// !IROUTINE:  ESMC_TimeInterval(+) - Sum of two TimeIntervals
//    
// !INTERFACE:
      ESMC_TimeInterval ESMC_TimeInterval::operator+(
//    
// !RETURN VALUE:
//    ESMC_TimeInterval result
//    
// !ARGUMENTS:
      const ESMC_TimeInterval &timeInterval) const {  // in - ESMC_TimeInterval
                                                      //      to add
//
// !DESCRIPTION:
//    Adds given {\tt timeInterval} expression to this {\tt timeInterval}.
//
//EOP
// !REQUIREMENTS:

    ESMC_TimeInterval sum;

    // TODO: fractional interval parts

    // add relative yy, mm, d parts
    sum.yy = this->yy + timeInterval.yy;
    sum.mm = this->mm + timeInterval.mm;
    sum.d  = this->d  + timeInterval.d;

    // add absolute seconds part using ESMC_BaseTime operator
    sum = ESMC_BaseTime::operator+(timeInterval);

    return(sum);

}  // end ESMC_TimeInterval::operator+

//-------------------------------------------------------------------------
//BOP 
// !IROUTINE:  ESMC_TimeInterval(-) - Difference between two TimeIntervals
//    
// !INTERFACE:
      ESMC_TimeInterval ESMC_TimeInterval::operator-(
//    
// !RETURN VALUE:
//    ESMC_TimeInterval result
//    
// !ARGUMENTS:
      const ESMC_TimeInterval &timeInterval) const {  // in - ESMC_TimeInterval
                                                      //      to subtract
//
// !DESCRIPTION:
//    Subtracts given {\tt timeInterval} expression from this 
//    {\tt timeInterval}.
//
//EOP
// !REQUIREMENTS:

    ESMC_TimeInterval diff;

    // TODO: fractional interval parts

    // subtract relative yy, mm, d parts
    diff.yy = this->yy - timeInterval.yy;
    diff.mm = this->mm - timeInterval.mm;
    diff.d  = this->d  - timeInterval.d;

    // subtract absolute seconds part using ESMC_BaseTime operator
    diff = ESMC_BaseTime::operator-(timeInterval);

    return(diff);

}  // end ESMC_TimeInterval::operator-

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeInterval(=) - copy or assign from BaseTime expression
//
// !INTERFACE:
      ESMC_TimeInterval& ESMC_TimeInterval::operator=(
//
// !RETURN VALUE:
//    ESMC_TimeInterval& result
//
// !ARGUMENTS:
      const ESMC_BaseTime &baseTime) {   // in - ESMC_BaseTime to copy
//
// !DESCRIPTION:
//    Assign {\tt ESMC\_BaseTime} expression to this time interval.
//    Supports inherited operators from {\tt ESMC\_BaseTime}
//
//EOP
// !REQUIREMENTS:  

    // invoke base class assignment operator
    // TODO:  should be implicit ?
    ESMC_BaseTime::operator=(baseTime);

    return(*this);

}  // end ESMC_TimeInterval::operator=

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeIntervalReadRestart - restore TimeInterval state
//
// !INTERFACE:
      int ESMC_TimeInterval::ESMC_TimeIntervalReadRestart(
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
//      restore {\tt TimeInterval} state for persistence/checkpointing.
//
//EOP
// !REQUIREMENTS:

    int rc = ESMF_SUCCESS;

    // TODO:  read time interval state from iospec/name, then restore
    //        (share code with ESMC_TimeIntervalSet()).

    // TODO: use base class ReadRestart() first
    // rc = ESMC_BaseTime::ESMC_BaseTimeReadRestart(s, sN, sD);

    return(rc);

 }  // end ESMC_TimeIntervalReadRestart

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeIntervalWriteRestart - return TimeInterval state
//
// !INTERFACE:
      int ESMC_TimeInterval::ESMC_TimeIntervalWriteRestart(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_IOSpec *iospec) const {
//
// !DESCRIPTION:
//      Save {\tt TimeInterval} state for persistence/checkpointing
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

 }  // end ESMC_TimeIntervalWriteRestart

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeIntervalValidate - validate TimeInterval state
//
// !INTERFACE:
      int ESMC_TimeInterval::ESMC_TimeIntervalValidate(
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

    return (ESMC_BaseTime::ESMC_BaseTimeValidate());

 }  // end ESMC_TimeIntervalValidate

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeIntervalPrint - print TimeInterval state
//
// !INTERFACE:
      int ESMC_TimeInterval::ESMC_TimeIntervalPrint(
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

    cout << "TimeInterval ---------------------------" << endl;

    // parse options
    if (options != ESMC_NULL_POINTER) {
      if (strncmp(options, "string", 6) == 0) {
        char timeString[ESMF_MAXSTR];
        ESMC_TimeIntervalGetString(timeString);
        cout << timeString << endl;
      }
    } else {
      // default
      ESMC_BaseTime::ESMC_BaseTimePrint(options);
      cout << "yy = " << yy << endl;
      cout << "mm = " << mm << endl;
      cout << "d  = " << d << endl;
    }

    cout << "end TimeInterval -----------------------" << endl << endl;

    return(ESMF_SUCCESS);

 }  // end ESMC_TimeIntervalPrint

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeInterval - native default C++ constructor
//
// !INTERFACE:
      ESMC_TimeInterval::ESMC_TimeInterval(void) {
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

//   ESMC_BaseTime(0, 0, 1) { // TODO: F90 issue with base class constructor?
   s  = 0;
   sN = 0;
   sD = 1;
   yy = 0;
   mm = 0;
   d  = 0;

   if (ESMC_Calendar::defaultCalendar != ESMC_NULL_POINTER) {
     // use default calendar
     calendar = ESMC_Calendar::defaultCalendar; // 1st choice

   } else {
     // create default calendar
     ESMC_CalendarSetDefault((ESMC_CalendarType *)ESMC_NULL_POINTER);
     calendar = ESMC_Calendar::defaultCalendar; // 2nd choice
   }

   // startTime, endTime initialized via their own constructor when
   // time interval instantiated.

} // end ESMC_TimeInterval

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeInterval - native C++ constructor
//
// !INTERFACE:
     ESMC_TimeInterval::ESMC_TimeInterval(
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
      ESMF_KIND_I8 s,             // in - integer seconds
      ESMF_KIND_I4 sN,            // in - fractional seconds, numerator
      ESMF_KIND_I4 sD,            // in - fractional seconds, denominator
      ESMF_KIND_I8 yy,            // in - calendar interval number of years
      ESMF_KIND_I8 mm,            // in - calendar interval number of months
      ESMF_KIND_I8 d,             // in - calendar interval number of days
      ESMC_Time *startTime,       // in - interval start time
      ESMC_Time *endTime,         // in - interval end time
      ESMC_Calendar *calendar,    // in - calendar
      ESMC_CalendarType calendarType) :  // in - calendar type
//
// !DESCRIPTION:
//      Initializes a {\tt ESMC\_TimeInterval} via {\tt ESMC\_BaseTime}
//      base class
//
//EOP
// !REQUIREMENTS:

   ESMC_BaseTime(s, sN, sD) {  // pass to base class constructor

   this->yy = yy;
   this->mm = mm;
   this->d  = d;

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

   } else if (calendarType != (ESMC_CalendarType)0) {
     // set to specified built-in type; create if necessary
     ESMC_CalendarCreate(calendarType);
     this->calendar = ESMC_Calendar::internalCalendar[calendarType-1];
                                                      // 2nd choice

   } else if (ESMC_Calendar::defaultCalendar != ESMC_NULL_POINTER) {
     // use default calendar
     this->calendar = ESMC_Calendar::defaultCalendar; // 3rd choice

   } else {
     // create default calendar
     ESMC_CalendarSetDefault((ESMC_CalendarType *)ESMC_NULL_POINTER);
     this->calendar = ESMC_Calendar::defaultCalendar; // 4th choice
   }

   // TODO: catch & throw exceptions above, and/or LogErr

} // end ESMC_TimeInterval

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ~ESMC_TimeInterval - native default C++ destructor
//
// !INTERFACE:
      ESMC_TimeInterval::~ESMC_TimeInterval(void) {
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

}  // end ~ESMC_TimeInterval

//-------------------------------------------------------------------------
//  Private methods to support ESMC_TimeIntervalGet() API
//-------------------------------------------------------------------------

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeIntervalGetString - Get a Time Interval value in string format
//
// !INTERFACE:
      int ESMC_TimeInterval::ESMC_TimeIntervalGetString(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      char *timeString) const {    // out - time interval value in string format
//
// !DESCRIPTION:
//      Gets a {\tt ESMC\_TimeInterval}'s value in character format
//
//EOP
// !REQUIREMENTS:  

    // validate input
    if (timeString == ESMC_NULL_POINTER) return (ESMF_FAILURE);

    ESMF_KIND_I8 d_i8;
    ESMF_KIND_I4 h, m, s;

    // TODO: use native C++ Get, not F90 entry point, when ready
    ESMC_TimeIntervalGet((ESMF_KIND_I4 *)ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                          ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                          ESMC_NULL_POINTER, &d_i8, &h, &m,
                          &s, ESMC_NULL_POINTER);
    //ESMC_TimeIntervalGet(&yy_i8, &mm, &d, &h, &m, &s); // TODO: when
                                                     // calendar intervals
                                                     //  implemented

    // ISO 8601 format PyYmMdDThHmMsS
    sprintf(timeString, "P%lldDT%dH%dM%dS\0", d_i8, h, m, s);
    //sprintf(timeString, "P%lldY%dM%dDT%dH%dM%dS\0", // TODO: when calendar
    //        yy_i8, mm, d, h, m, s);                 //  intervals implemented

    return(ESMF_SUCCESS);

 }  // end ESMC_TimeIntervalGetString
