// $Id: ESMC_TimeInterval.C,v 1.49 2004/03/24 00:43:43 eschwab Exp $
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
 static const char *const version = "$Id: ESMC_TimeInterval.C,v 1.49 2004/03/24 00:43:43 eschwab Exp $";
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
      ESMC_Calendar **calendar) { // in - calendar for calendar interval
//
// !DESCRIPTION:
//      Initialzes a {\tt ESMC\_TimeInterval} with values given in F90
//      variable arg list.
//
//EOP
// !REQUIREMENTS:  

    // TODO: ensure initialization if called via F90 interface;
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
    this->startTime.ESMC_TimeSet((ESMF_KIND_I8) 0);
    this->endTime.ESMC_TimeSet((ESMF_KIND_I8) 0);
    this->calendar = ESMC_NULL_POINTER;
    
    // TODO: validate inputs (individual and combos), set basetime values
    //       e.g. integer and float specifiers are mutually exclusive

    // absolute or calendar-specific interval
    // TODO:  when more than one given, check that calendar is the same
    if (startTime != ESMC_NULL_POINTER) {
      this->startTime = *startTime;
      this->calendar = startTime->calendar;
    }
    if (endTime != ESMC_NULL_POINTER) {
      this->endTime = *endTime;
      this->calendar = endTime->calendar;
    }
    if (calendar != ESMC_NULL_POINTER) {
      this->calendar = *calendar;
    }

    // relative calendar interval
    //  TODO:  when calendar, startTime, endTime specified, convert to
    //         absolute seconds instead of holding in relative yy, mm, d.
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

    // TODO: use Calendar-defined SecondsPerDay; default to 86400 (earth-cal)
    //       (really can't do if calendar, hence days, is undefined!)
    // get number of seconds in a day
//  int secPerDay = SECONDS_PER_DAY;  // default to Earth-type calendar
//  if (calendar != ESMC_NULL_POINTER) {  // TODO: or startTime or endTime
//    secPerDay = calendar->secondsPerDay;
//  }
//  if (d != ESMC_NULL_POINTER) {
//    this->s += ((ESMF_KIND_I8) *d) * secPerDay;  // >= 32-bit
//  } else if (d_i8 != ESMC_NULL_POINTER) {
//    this->s += *d_i8 * secPerDay; // >= 64-bit
//  } else if (d_r8 != ESMC_NULL_POINTER) {
//    this->s += (ESMF_KIND_I8) (*d_r8 * secPerDay);
//  }

    // use base class set for sub-day values
    ESMC_BaseTimeSet(h, m, s, s_i8, ms, us, ns, h_r8, m_r8, s_r8,
                     ms_r8, us_r8, ns_r8, sN, sD);

    if (ESMC_TimeIntervalValidate() == ESMF_SUCCESS) return(ESMF_SUCCESS);
    else {
      // restore previous value
      *this = saveTimeInterval;
      return(ESMF_FAILURE);
    }

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
      ESMC_Time *startTimeIn,   // in  - starting time for calendar interval
                                //       unit conversions
      ESMC_Time *endTimeIn,     // in  - ending time for calendar interval
                                //       unit conversions
      ESMC_Calendar **calendarIn, // in  - calendar for calendar interval
                                  //       unit conversions
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

    //---------------------------------------------------------------------
    // Determine startTime, endTime, and/or calendar, if any, we have to
    //   work with.
    //---------------------------------------------------------------------

    // get any calendar specified in Get() or Set(), on which to perform
    //   calendar (yy, mm, d) conversions
    ESMC_Calendar *conversionCalendar = ESMC_NULL_POINTER;
    if (startTimeIn != ESMC_NULL_POINTER) {
      conversionCalendar = startTimeIn->calendar;
    } else if (endTimeIn != ESMC_NULL_POINTER) {
      conversionCalendar = endTimeIn->calendar;
    } else if (calendarIn != ESMC_NULL_POINTER) {
      conversionCalendar = *calendarIn;
    } else if (this->calendar != ESMC_NULL_POINTER) {
      conversionCalendar = this->calendar;
    }

    // if no calendar info, then if any relative calendar unit was Set(),
    //   must Get() it back, otherwise impossible conversion is implied.
    if (conversionCalendar == ESMC_NULL_POINTER) {
      // if yy was set, must get it
      if (this->yy != 0 &&
          (yy == ESMC_NULL_POINTER && yy_i8 == ESMC_NULL_POINTER))
            return(ESMF_FAILURE);
      // if mm was set, must get it
      if (this->mm != 0 &&
          (mm == ESMC_NULL_POINTER && mm_i8 == ESMC_NULL_POINTER))
            return(ESMF_FAILURE);
// TODO: require calendar for d <-> h,m,s conversions ? or default to 86400 ?
      // if d was set, must get it
//    if (this->d != 0 &&
//        (d == ESMC_NULL_POINTER && d_i8 == ESMC_NULL_POINTER))
//          return(ESMF_FAILURE);
    }

    // get any startTime and/or endTime specified in Get() or Set()
    ESMC_Time conversionStartTime = this->startTime;
    ESMC_Time conversionEndTime   = this->endTime;
    if (startTimeIn != ESMC_NULL_POINTER) {
      conversionStartTime = *startTimeIn;
    }
    if (endTimeIn != ESMC_NULL_POINTER) {
      conversionEndTime = *endTimeIn;
    }

    //---------------------------------------------------------------------
    // Reduce this time interval's units to the smallest and least number
    //   of units possible
    //---------------------------------------------------------------------

    // Note:  Don't return ESMF_FAILURE in this section; just reduce what we
    //        can.  Failure is determined in the conversion section below
    //        which tries to satisfy the user's request for specific units.

    // Initialize time-to-convert values.  These are used to reduce this time
    // interval's units from which to convert to user-requested units.  
    // Represents remaining unconverted time; any years, months or days
    //   requested will be removed from these values.  In this way, a requested
    //   unit is bounded (normalized) by the next higher requested unit.
    ESMF_KIND_I8       yyToConvert = this->yy;
    ESMF_KIND_I8       mmToConvert = this->mm;
    ESMF_KIND_I8        dToConvert = this->d;
    ESMF_KIND_I8 baseTimeToConvert = this->s;

    ESMC_Time calculatedEndTime;   // from conversionStartTime
    ESMC_Time calculatedStartTime; // from conversionEndTime

    // reduce any yy,mm,d to base time units (seconds) if possible,
    //   or at least to months and/or days if not
    if (conversionCalendar != ESMC_NULL_POINTER) {
      switch (conversionCalendar->calendarType)
      {
        case ESMC_CAL_GREGORIAN:
        case ESMC_CAL_NOLEAP:
          // use startTime to reduce yy,mm,d to seconds
          if (conversionStartTime.ESMC_TimeValidate() == ESMF_SUCCESS) {
            calculatedEndTime = conversionStartTime + *this;
            ESMC_TimeInterval baseTimeInterval =
                                       calculatedEndTime - conversionStartTime;
            baseTimeToConvert = baseTimeInterval.s;
            yyToConvert = mmToConvert = dToConvert = 0;
                                        // yy, mm, d all reduced; only have
                                        //   baseTimeToConvert remaining

          // use endTime to reduce yy,mm,d to seconds
          } else if (conversionEndTime.ESMC_TimeValidate() == ESMF_SUCCESS) {
            calculatedStartTime = conversionEndTime - *this;
            ESMC_TimeInterval baseTimeInterval =
                                       conversionEndTime - calculatedStartTime;
            baseTimeToConvert = baseTimeInterval.s;
            yyToConvert = mmToConvert = dToConvert = 0;
                                        // yy, mm, d all reduced; only have
                                        //   baseTimeToConvert remaining

          } else { // no startTime or endTime available, reduce what we can
                   //   to (mm, s)
            if (conversionCalendar->calendarType == ESMC_CAL_GREGORIAN) {
              // cannot reduce yy or mm to seconds, but can reduce yy to mm
              if (yyToConvert != 0) {
                mmToConvert += yyToConvert * conversionCalendar->monthsPerYear;
                yyToConvert = 0;
              }
            } else { // ESMC_CAL_NOLEAP
              // reduce yy to seconds
              if (yyToConvert != 0) {
                baseTimeToConvert +=
                              yyToConvert * conversionCalendar->secondsPerYear;
                yyToConvert = 0;
              }
              // cannot reduce mm to seconds
            }
            // reduce d to seconds
            if (dToConvert != 0) {
              baseTimeToConvert +=
                               dToConvert * conversionCalendar->secondsPerDay;
              dToConvert = 0;
            }
            // we now have mmToConvert and baseTimeToConvert (mm, s);
            //   yyToConvert and dToConvert have been reduced
          }
          break;
        case ESMC_CAL_360DAY:
          if (yyToConvert != 0) {
            baseTimeToConvert +=
                          yyToConvert * conversionCalendar->secondsPerYear;
            yyToConvert = 0;
          }
          if (mmToConvert != 0) {
            baseTimeToConvert +=
                          mmToConvert * 30 * conversionCalendar->secondsPerDay;
            mmToConvert = 0;
          }
          if (dToConvert  != 0) {
            baseTimeToConvert +=
                          dToConvert * conversionCalendar->secondsPerDay;
            dToConvert = 0;
          }
          // yy, mm, d all reduced; only have baseTimeToConvert remaining
          break;
        case ESMC_CAL_JULIANDAY:
          // ignore years and months

          // reduce days to seconds
          if (dToConvert  != 0) {
            baseTimeToConvert +=
                          dToConvert * conversionCalendar->secondsPerDay;
            dToConvert = 0;
          }
          break;
        case ESMC_CAL_NOCALENDAR:
          // ignore years, months and days
          break;
        case ESMC_CAL_CUSTOM:
          // TODO:
          break;
        default:
          // unknown calendar type
          break;
      };
    } else { // no calendar specified
      // TODO:  default days to 86400 seconds ?
      // reduce days to seconds
      if (dToConvert != 0) {
          baseTimeToConvert += dToConvert * SECONDS_PER_DAY;
          dToConvert = 0;
      }
    }

    //---------------------------------------------------------------------
    // convert from reduced units to user-requested units, keeping them
    //   bound (normalized) to the next larger requested unit
    //---------------------------------------------------------------------

    // convert from reduced units to any user-requested years
    if (yy != ESMC_NULL_POINTER || yy_i8 != ESMC_NULL_POINTER) {
      // total relative and absolute years
      ESMF_KIND_I8 years = yyToConvert;
      yyToConvert = 0;
      if (conversionCalendar != ESMC_NULL_POINTER) {
        switch (conversionCalendar->calendarType)  
        {
          case ESMC_CAL_GREGORIAN:
          case ESMC_CAL_NOLEAP:
            {
              // TODO: use TimeInterval operators (/) and (-) when ready ?

              // get years using startTime, if available
              if (conversionStartTime.ESMC_TimeValidate() == ESMF_SUCCESS) {
                ESMC_TimeInterval oneYear(0, 0, 1, 1);
                ESMC_Time iTime = conversionStartTime + oneYear;
                years = 0;
                while (iTime <= calculatedEndTime) {
                  years++;
                  iTime += oneYear;
                }

                // move conversionStartTime up to end of last year converted
                conversionStartTime = iTime - oneYear;

                // calculate remaining baseTimeToConvert (remove years we got)
                ESMC_TimeInterval baseTimeInterval =
                                       calculatedEndTime - conversionStartTime;
                baseTimeToConvert = baseTimeInterval.s;

              // else get years using endTime, if available
              } else if (conversionEndTime.ESMC_TimeValidate() == ESMF_SUCCESS){
                ESMC_TimeInterval oneYear(0, 0, 1, 1);
                ESMC_Time iTime = calculatedStartTime + oneYear;
                years = 0;
                while (iTime <= conversionEndTime) {
                  years++;
                  iTime += oneYear;
                }

                // move calculatedStartTime up to end of last year converted
                calculatedStartTime = iTime - oneYear;

                // calculate remaining baseTimeToConvert (remove years we got)
                ESMC_TimeInterval baseTimeInterval =
                                       conversionEndTime - calculatedStartTime;
                baseTimeToConvert = baseTimeInterval.s;

              } else { // no startTime or endTime available, convert what we can
                       //   from (mm, s)
                years = 0;
                // convert mm for either ESMC_CAL_GREGORIAN or ESMC_CAL_NOLEAP
                if (mmToConvert != 0) {
                  years = mmToConvert / conversionCalendar->monthsPerYear;
                  mmToConvert %= conversionCalendar->monthsPerYear;
                } 

                // convert remaining seconds to years
                if (conversionCalendar->calendarType == ESMC_CAL_NOLEAP) {
                  years +=
                         baseTimeToConvert / conversionCalendar->secondsPerYear;
                  baseTimeToConvert %= conversionCalendar->secondsPerYear;

                } else { // ESMC_CAL_GREGORIAN
                  // note: can't use abs() or labs() since result is (long long)
                  //       could use ISO llabs() if supported on all platforms
                  if (baseTimeToConvert >= conversionCalendar->secondsPerYear ||
                      baseTimeToConvert <= -conversionCalendar->secondsPerYear){
                    // baseTimeToConvert >= 1 year => can't determine leap years
                    //   without startTime or endTime !
                    return (ESMF_FAILURE);
                  }
                }
              }
            }
            break;
          case ESMC_CAL_360DAY:
            years = baseTimeToConvert / conversionCalendar->secondsPerYear;
            baseTimeToConvert %= conversionCalendar->secondsPerYear;
            break;
          case ESMC_CAL_JULIANDAY:
          case ESMC_CAL_NOCALENDAR:
            // years not defined!
            return(ESMF_FAILURE);
            break;
          case ESMC_CAL_CUSTOM:
            // TODO:
            break;
          default:
            // unknown calendar type
            break;
        };
      } // else { // no calendar specified
        // will just return yy as specified by the user in Set(); other units
        // cannot be converted to years.  TODO:  return rc = ESMF_WARNING ?
   // }

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
      ESMF_KIND_I8 months = mmToConvert;
      mmToConvert = 0;
      if (conversionCalendar != ESMC_NULL_POINTER) {
        switch (conversionCalendar->calendarType)  
        {
          case ESMC_CAL_GREGORIAN:
          case ESMC_CAL_NOLEAP:

            // TODO: use TimeInterval operators (/) and (-) when ready ?

            // get months using startTime, if available
            if (conversionStartTime.ESMC_TimeValidate() == ESMF_SUCCESS) {
              ESMC_TimeInterval oneMonth(0, 0, 1, 0, 1);
              ESMC_Time iTime = conversionStartTime + oneMonth;
              months = 0;
              while (iTime <= calculatedEndTime) {
                months++;
                iTime += oneMonth;
              }

              // move conversionStartTime up to end of last month converted
              conversionStartTime = iTime - oneMonth;

              // calculate remaining baseTimeToConvert (remove months we got)
              ESMC_TimeInterval baseTimeInterval =
                                     calculatedEndTime - conversionStartTime;
              baseTimeToConvert = baseTimeInterval.s;

            // else get months using endTime, if available
            } else if (conversionEndTime.ESMC_TimeValidate() == ESMF_SUCCESS) {
              ESMC_TimeInterval oneMonth(0, 0, 1, 0, 1);
              ESMC_Time iTime = calculatedStartTime + oneMonth;
              months = 0;
              while (iTime <= conversionEndTime) {
                months++;
                iTime += oneMonth;
              }

              // move calculatedStartTime up to end of last month converted
              calculatedStartTime = iTime - oneMonth;

              // calculate remaining baseTimeToConvert (remove months we got)
              ESMC_TimeInterval baseTimeInterval =
                                     conversionEndTime - calculatedStartTime;
              baseTimeToConvert = baseTimeInterval.s;

            } else { // no startTime or endTime available, convert what we can
              if (baseTimeToConvert >= (28 * conversionCalendar->secondsPerDay)
                                    ||
                 baseTimeToConvert <= (-28 * conversionCalendar->secondsPerDay))
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
            months =
              baseTimeToConvert / (30 * conversionCalendar->secondsPerDay);
            baseTimeToConvert %= (30 * conversionCalendar->secondsPerDay);
            break;
          case ESMC_CAL_JULIANDAY:
          case ESMC_CAL_NOCALENDAR:
            // months not defined!
            return(ESMF_FAILURE);
            break;
          case ESMC_CAL_CUSTOM:
            // TODO:
            break;
          default:
            // unknown calendar type
            break;
        };
      } // else { // calendar not defined
        // will just return mm as specified by the user in Set(); other units
        // cannot be converted to months.  TODO:  return rc = ESMF_WARNING ?
   // }
      
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
      ESMF_KIND_I8 days = dToConvert; // relative part 
      dToConvert = 0;           // TODO: don't need dtConvert, always = 0?

      // don't need to check for years (yyToConvert) to convert to days
      //   since for all calendar types (except custom TODO), years will
      //   have already been reduced to either months (mmToConvert) or
      //   seconds (baseTimeToConvert).

      // check for any months to convert to days
      if (conversionCalendar != ESMC_NULL_POINTER) {
        switch (conversionCalendar->calendarType)  
        {
          case ESMC_CAL_GREGORIAN:
            if (mmToConvert != 0) {
              // no startTime or endTime available, can't do
              return (ESMF_FAILURE);
            }
            break;
          case ESMC_CAL_NOLEAP:
            if (mmToConvert != 0) {
              // no startTime or endTime available, can only do if months
              // are an integral number of years
              //   note: can't use abs() or labs() since result is (long long)
              //         could use ISO llabs() if supported on all platforms
              if ( (mmToConvert >=  conversionCalendar->monthsPerYear || 
                    mmToConvert <= -conversionCalendar->monthsPerYear) &&
                    mmToConvert  %  conversionCalendar->monthsPerYear == 0) {
                days += (mmToConvert / conversionCalendar->monthsPerYear) *
                                       conversionCalendar->daysPerYear.d;
                mmToConvert = 0;
              } else {
                // can't do
                return (ESMF_FAILURE);
              }
            }
            break;
          case ESMC_CAL_360DAY:
          case ESMC_CAL_JULIANDAY:
          case ESMC_CAL_NOCALENDAR:
            // nothing to do: 360Day => months already reduced to seconds;
            //   JulianDay and NoCalendar => months don't apply
            break;
          case ESMC_CAL_CUSTOM:
            // TODO:
            break;
          default:
            // unknown calendar type
            break;
        };
      } else { // no calendar available
        if (mmToConvert != 0) {
          // can't convert months to days without calendar!
          return(ESMF_FAILURE);
        }
      }

      // get number of seconds in a day
      int secPerDay = SECONDS_PER_DAY;  // TODO: default to Earth-type calendar?
      if (conversionCalendar != ESMC_NULL_POINTER) {
        secPerDay = conversionCalendar->secondsPerDay;
      }

      days += baseTimeToConvert / secPerDay;  // absolute part
                           // TODO: assign, not add, if dtToConvert always = 0 ?
      baseTimeToConvert %= secPerDay;  // remove days from base conversion time
                                       // to get remaining sub-day units (h,m,s)

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
    if (ESMC_BaseTimeGet(baseTimeToConvert, h, m, s, s_i8, ms, us, ns,
                         h_r8, m_r8, s_r8, ms_r8, us_r8, ns_r8, sN, sD) ==
                         ESMF_FAILURE) return(ESMF_FAILURE);

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

    // if requested, return time interval in string format
    if (timeString != ESMC_NULL_POINTER) {
      if (ESMC_TimeIntervalGetString(timeString) == ESMF_FAILURE)
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
      ESMC_Calendar *calendar) { // in - associated calendar
//
// !DESCRIPTION:
//      Initialzes a {\tt TimeInterval} with given values.  Used to avoid
//      constructor to cover case when initial entry is from F90, since
//      destructor is called automatically when leaving scope to return to F90.
//
//EOP
// !REQUIREMENTS:

    // use base class Set()
    if (ESMC_BaseTime::ESMC_BaseTimeSet(s, sN, sD) == ESMF_SUCCESS)
    {
      this->yy = yy;
      this->mm = mm;
      this->d  = d;

      if (startTime != ESMC_NULL_POINTER) {
        this->startTime = *startTime;
      } else this->startTime.ESMC_TimeSet((ESMF_KIND_I8) 0);

      if (endTime != ESMC_NULL_POINTER) {
        this->endTime = *endTime;
      } else this->endTime.ESMC_TimeSet((ESMF_KIND_I8) 0);

      this->calendar = calendar;

      return(ESMF_SUCCESS);
    }
    else return(ESMF_FAILURE);

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

    // TODO: fractional & calendar interval parts

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

    // TODO: fractional & calendar interval parts

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

    // TODO: fractional & calendar interval parts

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

    // TODO: fractional & calendar interval parts

    // then perform the addition using ESMC_BaseTime operator
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

    // TODO: fractional & calendar interval parts

    // then perform the subtraction using ESMC_BaseTime operator
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
//    Assign {\tt ESMC\_BaseTime} expression to this time intervals.
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
   calendar = ESMC_NULL_POINTER;
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
      ESMC_Calendar *calendar) :  // in - calendar
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

   this->calendar = calendar;

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
