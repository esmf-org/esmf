// $Id: ESMC_Calendar.C,v 1.30 2003/08/29 05:31:58 eschwab Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the GPL.
//
// ESMC Calendar method code (body) file
//
//-------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ {\tt EMSC\_Calendar} methods declared
// in the companion file {\tt ESMC\_Calendar.h}
//
//-------------------------------------------------------------------------

 // higher level, 3rd party or system includes
 #include <iostream.h>
 #include <limits.h>

 // associated class definition file
 #include <ESMC_Calendar.h>

//-------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_Calendar.C,v 1.30 2003/08/29 05:31:58 eschwab Exp $";
//-------------------------------------------------------------------------

//
//-------------------------------------------------------------------------
//-------------------------------------------------------------------------
//
// This section includes all the ESMC_Calendar routines
//
//

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CalendarSet - calendar initializer
//
// !INTERFACE:
      int ESMC_Calendar::ESMC_CalendarSet(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_CalendarType type) {   // in - initialize to be Calendar type
//
// !DESCRIPTION:
//      Initialzes a {\tt EMSC\_Calendar} to be of a specific type
//
//EOP
// !REQUIREMENTS:

    // TODO: ensure initialization if called via F90 interface;
    //       cannot call constructor, because destructor is subsequently
    //       called automatically, returning initialized values to garbage.
    for (int i=0; i<MONTHS_PER_YEAR; i++) daysPerMonth[i] = 0;
    secondsPerDay  = 0;
    secondsPerYear = 0;
    daysPerYear.d  = 0;
    daysPerYear.dN = 0;
    daysPerYear.dD = 1;
    
    int rc; // return code 

    this->type = type;

    switch (type)
    {
        case ESMC_CAL_GREGORIAN:
        case ESMC_CAL_NOLEAP:
            // specific leap year is property of a Time instant, not Calendar ??
            //    OR calculated on-the-fly during Time instant calculations ??
            //    Calendar type only determines whether leap year is used
            daysPerMonth[0]  = 31; daysPerMonth[1]  = 28;
            daysPerMonth[2]  = 31; daysPerMonth[3]  = 30;
            daysPerMonth[4]  = 31; daysPerMonth[5]  = 30;
            daysPerMonth[6]  = 31; daysPerMonth[7]  = 31;
            daysPerMonth[8]  = 30; daysPerMonth[9]  = 31;
            daysPerMonth[10] = 30; daysPerMonth[11] = 31;
            secondsPerDay  = SECONDS_PER_DAY;
            secondsPerYear = SECONDS_PER_DAY * 365;
            daysPerYear.d  = 365;
            daysPerYear.dN = 0;
            daysPerYear.dD = 1;
            rc = ESMF_SUCCESS;
            break;

        case ESMC_CAL_JULIANDAY:
            // Days is the highest resolution of time, i.e. there is no
            //   concept of months or years ??
            for (int i=0; i<MONTHS_PER_YEAR; i++) daysPerMonth[i] = 0;
            secondsPerDay  = SECONDS_PER_DAY;
            secondsPerYear = 0;
            daysPerYear.d  = 0;
            daysPerYear.dN = 0;
            daysPerYear.dD = 1;
            rc = ESMF_SUCCESS;
            break;

        case ESMC_CAL_360DAY:
            // 12 months of 30 days each
            for (int i=0; i<MONTHS_PER_YEAR; i++) daysPerMonth[i] = 30;
            secondsPerDay  = SECONDS_PER_DAY;
            secondsPerYear = SECONDS_PER_DAY * 360;
            daysPerYear.d  = 360;
            daysPerYear.dN = 0;
            daysPerYear.dD = 1;
            rc = ESMF_SUCCESS;
            break;

        case ESMC_CAL_NOCALENDAR:
            // no calendar needed, convert base time up to days only
            for (int i=0; i<MONTHS_PER_YEAR; i++) daysPerMonth[i] = 0;
            secondsPerDay  = 0;
            secondsPerYear = 0;
            daysPerYear.d  = 0;
            daysPerYear.dN = 0;
            daysPerYear.dD = 1;
            rc = ESMF_SUCCESS;
            break;

        case ESMC_CAL_GENERIC:
            // user defined; need more info; user must call
            //   SetGeneric() instead
            rc = ESMF_FAILURE;
            break;

        default:
            // unknown calendar type
            rc = ESMF_FAILURE;
            break;
    }
    return(rc);

}  // end ESMC_CalendarSet

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CalendarSetGeneric - generic calendar initializer
//
// !INTERFACE:
      int ESMC_Calendar::ESMC_CalendarSetGeneric(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int          *daysPerMonth,     // in
      ESMF_IKIND_I4 secondsPerDay,    // in
      ESMF_IKIND_I4 daysPerYear,      // in
      ESMF_IKIND_I4 daysPerYeardN,    // in
      ESMF_IKIND_I4 daysPerYeardD) {  // in
// 
// !DESCRIPTION:
//      Initialzes a {\tt EMSC\_Calendar} to be a custom, user-defined type
// 
//EOP
// !REQUIREMENTS:

    // TODO: ensure initialization if called via F90 interface;
    //       cannot call constructor, because destructor is subsequently
    //       called automatically, returning initialized values to garbage.
    for (int i=0; i<MONTHS_PER_YEAR; i++) this->daysPerMonth[i] = 0;
    this->secondsPerDay  = 0;
    this->secondsPerYear = 0;
    this->daysPerYear.d  = 0;
    this->daysPerYear.dN = 0;
    this->daysPerYear.dD = 1;
    
    if (daysPerMonth == 0) {
      cout << "ESMC_Calendar::ESMC_CalendarSetGeneric():  daysPerMonth "
           << "pointer passed in is zero.";
      return(ESMF_FAILURE);
    }

    this->type = ESMC_CAL_GENERIC;

    for(int i=0; i<MONTHS_PER_YEAR; i++)
    { 
      this->daysPerMonth[i] = daysPerMonth[i];
    }
    this->secondsPerDay  = secondsPerDay;
    this->daysPerYear.d  = daysPerYear;
    this->daysPerYear.dN = daysPerYeardN;
    this->daysPerYear.dD = daysPerYeardD;
    this->secondsPerYear = secondsPerDay * daysPerYear;

    return(ESMF_SUCCESS);

}  // end ESMC_CalendarSetGeneric

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CalendarGet - get calendar properties
//
// !INTERFACE:
      int ESMC_Calendar::ESMC_CalendarGet(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_CalendarType *type,           // out - Calendar type
      int *daysPerMonth,                 // out
      ESMF_IKIND_I4 *secondsPerDay,      // out
      ESMF_IKIND_I4 *secondsPerYear,     // out
      ESMF_IKIND_I4 *daysPerYear,        // out
      ESMF_IKIND_I4 *daysPerYeardN,      // out
      ESMF_IKIND_I4 *daysPerYeardD) {    // out
// 
// !DESCRIPTION:
//      Gets a {\tt EMSC\_Calendar}'s properties
// 
//EOP
// !REQUIREMENTS:

    // must have at least one non-null pointer
    if (type           == ESMC_NULL_POINTER &&
        daysPerMonth   == ESMC_NULL_POINTER && 
        secondsPerDay  == ESMC_NULL_POINTER &&
        secondsPerYear == ESMC_NULL_POINTER &&
        daysPerYear    == ESMC_NULL_POINTER &&
        daysPerYeardN  == ESMC_NULL_POINTER &&
        daysPerYeardD  == ESMC_NULL_POINTER) {
        cout << "ESMC_Calendar::ESMC_CalendarSet():  no valid "
             << "pointer passed in.";
      return (ESMF_FAILURE);
    }

    if (type != ESMC_NULL_POINTER) {
      *type = this->type;
    }
    if (daysPerMonth != ESMC_NULL_POINTER) {
      for (int i=0; i<MONTHS_PER_YEAR; i++) {
        daysPerMonth[i] = this->daysPerMonth[i];
      }
    }
    if (secondsPerDay != ESMC_NULL_POINTER) {
      *secondsPerDay = this->secondsPerDay;
    }
    if (secondsPerYear != ESMC_NULL_POINTER) {
      *secondsPerYear = this->secondsPerYear;
    }
    if (daysPerYear != ESMC_NULL_POINTER) {
      *daysPerYear = this->daysPerYear.d;
    }
    if (daysPerYeardN != ESMC_NULL_POINTER) {
      *daysPerYeardN = this->daysPerYear.dN;
    }
    if (daysPerYeardD != ESMC_NULL_POINTER) {
      *daysPerYeardD = this->daysPerYear.dD;
    }

    return(ESMF_SUCCESS);

}  // end ESMC_CalendarGet
    
//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CalendarConvertToTime - convert calendar date to ESMC_BaseTime
//
// !INTERFACE:
      int ESMC_Calendar::ESMC_CalendarConvertToTime(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMF_IKIND_I8 yr, int mm, int dd, ESMF_IKIND_I8 d,    // in
      ESMC_BaseTime *t) const {                             // out
//
// !DESCRIPTION:
//     Converts a calendar-specific date to core {\tt ESMC\_BaseTime}
//     representation. Conversions based on UTC: time zone offset done by
//     client
//
//     The Gregorian <-> Julian conversion algorithm is from Henry F. Fliegel
//     and Thomas C. Van Flandern, in Communications of the ACM,
//     CACM, volume 11, number 10, October 1968, p. 657.  Julian day refers
//     to the number of days since a reference day.  For the algorithm used,
//     this reference day is November 24, -4713 in the Gregorian calendar.
//     This algorithm is valid from 3/1/-4800 forward and takes into
//     account leap years.  However, it does not take into account the
//     Gregorian Reformation where 10 days were eliminated from the calendar
//     in September 1752.
//
//EOP
// !REQUIREMENTS:   TMG 2.4.5, 2.5.6

    switch (this->type)
    {
        // convert Gregorian Date => Time
        case ESMC_CAL_GREGORIAN:
        {
            //
            // Validate inputs 
            //
            if (yr < -4800 || mm < 1 || mm > 12 || dd < 1) {
              return (ESMF_FAILURE);
            }
            // invalid before 3/1/-4800
            if (yr == -4800 && mm < 3) {
              return (ESMF_FAILURE);
            }

            // TODO: upper bounds date range check dependent on machine
            //  word size, e.g. for signed 64-bit, max date is
            //  10/29/292,277,019,914

            // check day of the month for any month except February
            if (mm != 2 && dd > daysPerMonth[mm-1]) {
              return (ESMF_FAILURE);
            }
            // if February, take leap year into account before checking
            //   day of the month
            if (mm == 2) {
              int leapDay = ESMC_IS_LEAP_YEAR(yr) ? 1 : 0;
              if (dd > (daysPerMonth[1] + leapDay)) {
                return (ESMF_FAILURE);
              }
            }

            // convert Gregorian date to Julian days
            // Gregorian date (yr, mm, dd) => Julian days (jdays)
            int temp            = (mm - 14) / 12;
            ESMF_IKIND_I8 jdays = (1461 * (yr + 4800 + temp)) / 4 +
                             (367 * (mm - 2 - 12 * temp )) / 12 -
                             (3 * ((yr + 4900 + temp) / 100)) / 4 + dd - 32075;

            // convert Julian days to basetime seconds (>= 64 bit)
            t->s = jdays * secondsPerDay;

            break;
        }
        // convert No Leap Date => Time
        case ESMC_CAL_NOLEAP:
        {
            t->s = yr * secondsPerYear;
            for(int month=0; month < mm-1; month++) {
              t->s += daysPerMonth[month] * secondsPerDay;
            }
            t->s += (dd-1) * secondsPerDay + 148600915200LL;
                                          // ^ adjust to match Julian time zero
                                          // = (1/1/0000) - (11/24/-4713)
            break;
        }
        // convert 360 Day Date => Time
        case ESMC_CAL_360DAY:
        {
            t->s  = yr * secondsPerYear
                  + (mm-1) * 30 * secondsPerDay   // each month has 30 days
                  + (dd-1) * secondsPerDay + 146565244800LL;
                                          // ^ adjust to match Julian time zero
                                          // = (1/1/0000) - (11/24/-4713)
            break;
        }
        // convert Julian Date => Time
        case ESMC_CAL_JULIANDAY:
        {
            // convert Julian days to basetime seconds (>= 64 bit)
            t->s = d * secondsPerDay;

            break;
        }
        default:
            break;
    }

    return(ESMF_SUCCESS);

}  // end ESMC_CalendarConvertToTime

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CalendarConvertToDate - convert ESMC_BaseTime to calendar date
//
// !INTERFACE:
      int ESMC_Calendar::ESMC_CalendarConvertToDate(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const ESMC_BaseTime *t,                                      // in
      ESMF_IKIND_I4 *yr, ESMF_IKIND_I8 *yr_i8, int *mm, int *dd,   // out
      ESMF_IKIND_I4 *d, ESMF_IKIND_I8 *d_i8, ESMF_IKIND_R8 *d_r8) const { // out
//
// !DESCRIPTION:
//     Converts a core {\tt ESMC\_BaseTime} representation to a
//     calendar-specific date. Conversions based on UTC: time zone offset
//     done by client
//
//     The Gregorian <-> Julian conversion algorithm is from Henry F. Fliegel
//     and Thomas C. Van Flandern, in Communications of the ACM,
//     CACM, volume 11, number 10, October 1968, p. 657.  Julian day refers
//     to the number of days since a reference day.  For the algorithm used,
//     this reference day is November 24, -4713 in the Gregorian calendar.
//     This algorithm is valid from 3/1/-4900 forward and takes into
//     account leap years.  However, it does not take into account the
//     Gregorian Reformation where 10 days were eliminated from the calendar
//     in September 1752.
//
//EOP
// !REQUIREMENTS:   TMG 2.4.5, 2.5.6

    int rc = ESMF_SUCCESS;

    switch (this->type)
    {
        // convert Time => Gregorian Date
        case ESMC_CAL_GREGORIAN:
        {
            // Convert basetime portion of Time into date
            // Julian day (D) => Gregorian date (yr, mm, dd)
            // The calculation below fails for jday > 106,751,991,167,300
            //    (4*templ = 2^63)

            // convert basetime seconds to Julian days
            ESMF_IKIND_I8 jdays = t->s / secondsPerDay;

            if (d != ESMC_NULL_POINTER) {
              if (jdays > INT_MIN && jdays <= INT_MAX) {
                *d = (ESMF_IKIND_I4) jdays;
                // adjust for negative time (reverse integer division)
                if (t->s % secondsPerDay < 0) (*d)--;
              } else {
                // too large to fit in given int
                rc = ESMF_FAILURE;
              }
            }
            if (d_i8 != ESMC_NULL_POINTER) {
              *d_i8 = jdays;
              // adjust for negative time (reverse integer division)
              if (t->s % secondsPerDay < 0) (*d_i8)--;
            }
            if (d_r8 != ESMC_NULL_POINTER) {
              *d_r8 = (ESMF_IKIND_R8) t->s / (ESMF_IKIND_R8) secondsPerDay;
            }

            // convert Julian days to Gregorian date
            // Julian days (jdays) => Gregorian date (yr, mm, dd)
            if (dd != ESMC_NULL_POINTER || mm != ESMC_NULL_POINTER ||
                yr != ESMC_NULL_POINTER || yr_i8 != ESMC_NULL_POINTER) {
              ESMF_IKIND_I8 templ = jdays + 68569;
              ESMF_IKIND_I8 tempn = (4 * templ) / 146097;
                            templ = templ - (146097 * tempn + 3) / 4;
              ESMF_IKIND_I8 tempi = (4000 * (templ + 1)) / 1461001;
                            templ = templ - (1461 * tempi) / 4 + 31;
              ESMF_IKIND_I8 tempj = (80 * templ) / 2447;
              if (dd != ESMC_NULL_POINTER) {
                *dd = templ - (2447 * tempj) / 80;
              }
              templ = tempj / 11;
              if (mm != ESMC_NULL_POINTER) {
                *mm = tempj + 2 - (12 * templ);
              }

              ESMF_IKIND_I8 year = 100 * (tempn - 49) + tempi + templ;
              if (yr != ESMC_NULL_POINTER) {
                if (year >= INT_MIN && year <= INT_MAX) {
                  *yr = (ESMF_IKIND_I4) year;  // >= 32-bit
                } else {
                  // too large to fit in given int
                  rc = ESMF_FAILURE;
                }
              }
              if (yr_i8 != ESMC_NULL_POINTER) {
                *yr_i8 = year;    // >= 64-bit
              }
            }

            break;
        }
        // convert Time => No Leap Date
        case ESMC_CAL_NOLEAP:
        {
            ESMF_IKIND_I8 tmpS = t->s - 148600915200LL;
                                     // ^ adjust to match Julian time zero
                                     // = (1/1/0000) - (11/24/-4713)

            if (yr != ESMC_NULL_POINTER) {
              ESMF_IKIND_I8 year = tmpS / secondsPerYear;
              if (year > INT_MIN && year <= INT_MAX) {
                  *yr = (ESMF_IKIND_I4) year;  // >= 32-bit
                  // adjust for negative time (reverse integer division)
                  if (tmpS % secondsPerYear < 0) (*yr)--;
              } else {
                  // too large to fit in given int
                  rc = ESMF_FAILURE;
              }
            }
            if (yr_i8 != ESMC_NULL_POINTER) {
              *yr_i8 = tmpS / secondsPerYear;  // >= 64-bit
              // adjust for negative time (reverse integer division)
              if (tmpS % secondsPerYear < 0) (*yr_i8)--;
            }

            int day = (tmpS % secondsPerYear) / secondsPerDay + 1;
            // ensure day range is positive 1-365
            if (day <= 0) day += daysPerYear.d;

            int month;
            for(month=0; day > daysPerMonth[month]; month++) {
              day -= daysPerMonth[month];
            }
            if (mm != ESMC_NULL_POINTER) {
              *mm = month + 1;
            }
            if (dd != ESMC_NULL_POINTER) {
              *dd = day;
            }

            // convert basetime seconds to Julian days
            if (d != ESMC_NULL_POINTER) {
              ESMF_IKIND_I8 day = tmpS / secondsPerDay;
              if (day > INT_MIN && day <= INT_MAX) {
                *d = (ESMF_IKIND_I4) day;   // >= 32-bit
                // adjust for negative time (reverse integer division)
                if (tmpS % secondsPerDay < 0) (*d)--;
              } else {
                // too large to fit in given int
                rc = ESMF_FAILURE;
              }
            }
            if (d_i8 != ESMC_NULL_POINTER) {
              *d_i8 = tmpS / secondsPerDay;   // >= 64-bit
              // adjust for negative time (reverse integer division)
              if (tmpS % secondsPerDay < 0) (*d_i8)--;
            }
            if (d_r8 != ESMC_NULL_POINTER) {
              *d_r8 = (ESMF_IKIND_R8) tmpS / (ESMF_IKIND_R8) secondsPerDay;
            }

            break;
        }
        // convert Time => 360 Day Date
        case ESMC_CAL_360DAY:
        {
            ESMF_IKIND_I8 tmpS = t->s - 146565244800LL;
                                     // ^ adjust to match Julian time zero
                                     // = (1/1/0000) - (11/24/-4713)

            if (yr != ESMC_NULL_POINTER) {
              ESMF_IKIND_I8 year = tmpS / secondsPerYear;
              if (year > INT_MIN && year <= INT_MAX) {
                *yr = (ESMF_IKIND_I4) year;
                // adjust for negative time (reverse integer division)
                if (tmpS % secondsPerYear < 0) (*yr)--;
              } else {
                // too large to fit in given int
                rc = ESMF_FAILURE;
              }
            }
            if (yr_i8 != ESMC_NULL_POINTER) {
              *yr_i8 = tmpS / secondsPerYear;
              // adjust for negative time (reverse integer division)
              if (tmpS % secondsPerYear < 0) (*yr_i8)--;
            }

            int dayOfYear = (tmpS % secondsPerYear) / secondsPerDay + 1;
            // ensure day range is positive 1-360
            if (dayOfYear <= 0) dayOfYear += daysPerYear.d;  

            if (mm != ESMC_NULL_POINTER) {
              *mm = dayOfYear / 30 + 1;  // each month has 30 days
            }
            if (dd != ESMC_NULL_POINTER) {
              *dd = dayOfYear % 30;      // each month has 30 days
            }

            // convert basetime seconds to Julian days
            if (d != ESMC_NULL_POINTER) {
              ESMF_IKIND_I8 day = tmpS / secondsPerDay;
              if (day > INT_MIN && day <= INT_MAX) {
                *d = (ESMF_IKIND_I4) day;   // >= 32-bit
                // adjust for negative time (reverse integer division)
                if (tmpS % secondsPerDay < 0) (*d)--;
              } else {
                // too large to fit in given int
                rc = ESMF_FAILURE;
              }
            }
            if (d_i8 != ESMC_NULL_POINTER) {
              *d_i8 = tmpS / secondsPerDay;   // >= 64-bit
              // adjust for negative time (reverse integer division)
              if (tmpS % secondsPerDay < 0) (*d_i8)--;
            }
            if (d_r8 != ESMC_NULL_POINTER) {
              *d_r8 = (ESMF_IKIND_R8) tmpS / (ESMF_IKIND_R8) secondsPerDay;
            }

            break;
        }
        // convert Time => Julian Date
        case ESMC_CAL_JULIANDAY:
        {
            // convert basetime seconds to Julian days
            if (d != ESMC_NULL_POINTER) {
              ESMF_IKIND_I8 day = t->s / secondsPerDay;
              if (day > INT_MIN && day <= INT_MAX) {
                *d = (ESMF_IKIND_I4) day;    // >= 32-bit
                // adjust for negative time (reverse integer division)
                if (t->s % secondsPerDay < 0) (*d)--;
              } else {
                // too large to fit in given int
                rc = ESMF_FAILURE;
              }
            }
            if (d_i8 != ESMC_NULL_POINTER) {
              *d_i8 = t->s / secondsPerDay;  // >= 64-bit
                // adjust for negative time (reverse integer division)
                if (t->s % secondsPerDay < 0) (*d_i8)--;
            }
            if (d_r8 != ESMC_NULL_POINTER) {
              *d_r8 = (ESMF_IKIND_R8) t->s / (ESMF_IKIND_R8) secondsPerDay;
            }
            break;
        }
        default:
            break;
    }

    return(rc);

}  // end ESMC_CalendarConvertToDate

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CalendarReadRestart - restore Calendar state
//
// !INTERFACE:
      int ESMC_Calendar::ESMC_CalendarReadRestart(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_CalendarType type,        // in
      int          *daysPerMonth,    // in
      ESMF_IKIND_I4 secondsPerDay,   // in
      ESMF_IKIND_I4 secondsPerYear,  // in
      ESMF_IKIND_I4 daysPerYear,     // in
      ESMF_IKIND_I4 daysPerYeardN,   // in
      ESMF_IKIND_I4 daysPerYeardD) { // in
// 
// !DESCRIPTION:
//      Restores {\tt EMSC\_Calendar} state for persistence/checkpointing
// 
//EOP
// !REQUIREMENTS:

    if (daysPerMonth == ESMC_NULL_POINTER) {
      cout << "ESMC_Calendar::ESMC_CalendarReadRestart(): null pointer passed in" << endl;
      return(ESMF_FAILURE);
    }

    this->type = type;
    for (int i=0; i<MONTHS_PER_YEAR; i++)
    {
        this->daysPerMonth[i] = daysPerMonth[i];    
    }
    this->secondsPerDay  = secondsPerDay;
    this->secondsPerYear = secondsPerYear;
    this->daysPerYear.d  = daysPerYear;
    this->daysPerYear.dN = daysPerYeardN;
    this->daysPerYear.dD = daysPerYeardD;

    return(ESMF_SUCCESS);

}  // end ESMC_CalendarReadRestart

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CalendarWriteRestart - save Calendar state
//
// !INTERFACE:
      int ESMC_Calendar::ESMC_CalendarWriteRestart(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_CalendarType *type,              // out
      int           *daysPerMonth,          // out
      ESMF_IKIND_I4 *secondsPerDay,         // out
      ESMF_IKIND_I4 *secondsPerYear,        // out
      ESMF_IKIND_I4 *daysPerYear,           // out
      ESMF_IKIND_I4 *daysPerYeardN,         // out
      ESMF_IKIND_I4 *daysPerYeardD) const { // out
// 
// !DESCRIPTION:
//      Returns {\tt EMSC\_Calendar} state for persistence/checkpointing
// 
//EOP
// !REQUIREMENTS:

    if (type           == ESMC_NULL_POINTER ||
        daysPerMonth   == ESMC_NULL_POINTER ||
        secondsPerDay  == ESMC_NULL_POINTER ||
        secondsPerYear == ESMC_NULL_POINTER ||
        daysPerYear    == ESMC_NULL_POINTER ||
        daysPerYeardN  == ESMC_NULL_POINTER ||
        daysPerYeardD  == ESMC_NULL_POINTER) {
      cout << "ESMC_Calendar::ESMC_CalendarWriteRestart(): "
           <<  "null pointer(s) passed in" << endl;
      return(ESMF_FAILURE);
    }

    *type = this->type;
    for (int i=0; i<MONTHS_PER_YEAR; i++)
    {
        daysPerMonth[i] = this->daysPerMonth[i];    
    }
    *secondsPerDay  = this->secondsPerDay;
    *secondsPerYear = this->secondsPerYear;
    *daysPerYear    = this->daysPerYear.d;
    *daysPerYeardN  = this->daysPerYear.dN;
    *daysPerYeardD  = this->daysPerYear.dD;

    return(ESMF_SUCCESS);

}  // end ESMC_CalendarWriteRestart

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CalendarValidate - validate Calendar state
//
// !INTERFACE:
      int ESMC_Calendar::ESMC_CalendarValidate(const char *options) const {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      validate {\tt EMSC\_Calendar} state
//
//EOP
// !REQUIREMENTS: 

    if (this->type < ESMC_CAL_GREGORIAN ||
        this->type > ESMC_CAL_NOCALENDAR) return(ESMF_FAILURE);

    return(ESMF_SUCCESS);

}  // end ESMC_CalendarValidate

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CalendarPrint - print Calendar state
//
// !INTERFACE:
      int ESMC_Calendar::ESMC_CalendarPrint(const char *options) const {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      print {\tt EMSC\_Calendar} state for testing/debugging
//
//EOP
// !REQUIREMENTS: 

    cout << "Calendar -------------------------------" << endl;
    cout << "type = " << type << endl;
    cout << "daysPerMonth = " << endl;
    for (int i=0; i<MONTHS_PER_YEAR; i++) cout << daysPerMonth[i] << " ";
    cout << endl;
    cout << "secondsPerDay = "  << secondsPerDay  << endl;
    cout << "secondsPerYear = " << secondsPerYear << endl;
    cout << "daysPerYear = "    << daysPerYear.d  << endl;
    cout << "daysPerYeardN = "  << daysPerYear.dN << endl;
    cout << "daysPerYeardD = "  << daysPerYear.dD << endl;
    cout << "end Calendar ---------------------------" << endl << endl;
    
    return(ESMF_SUCCESS);

}  // end ESMC_CalendarPrint

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Calendar - native default C++ constructor
//
// !INTERFACE:
      ESMC_Calendar::ESMC_Calendar(void) {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      Initializes a {\tt ESMC\_Calendar} with defaults
//
//EOP
// !REQUIREMENTS: 

    type = ESMC_CAL_NOCALENDAR;
    for (int i=0; i<MONTHS_PER_YEAR; i++) daysPerMonth[i] = 0;
    secondsPerDay  = 0;
    secondsPerYear = 0;
    daysPerYear.d  = 0;
    daysPerYear.dN = 0;
    daysPerYear.dD = 1;

} // end ESMC_Calendar

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Calendar - native C++ constructor
//
// !INTERFACE:
      ESMC_Calendar::ESMC_Calendar(
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
      ESMC_CalendarType type) {  // in
//
// !DESCRIPTION:
//      Initializes a {\tt ESMC\_TimeInstant} to be of a specific type via
//      {\tt ESMC\_CalendarSet}
//
//EOP
// !REQUIREMENTS: 

    ESMC_CalendarSet(type);

}   // end ESMC_Calendar

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Calendar - native C++ constructor
//
// !INTERFACE:
      ESMC_Calendar::ESMC_Calendar(
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
      int          *daysPerMonth,      // in
      ESMF_IKIND_I4 secondsPerDay,     // in
      ESMF_IKIND_I4 daysPerYear,       // in
      ESMF_IKIND_I4 daysPerYeardN,     // in
      ESMF_IKIND_I4 daysPerYeardD) {   // in
//
// !DESCRIPTION:
//      Initializes a {\tt ESMC\_Time} to be of a custom user-defined type
//      via {\tt ESMC\_CalendarSetGeneric}
//
//EOP
// !REQUIREMENTS: 

    ESMC_CalendarSetGeneric(daysPerMonth,  secondsPerDay, daysPerYear,
                            daysPerYeardN, daysPerYeardD);
}  // end ESMC_Calendar

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ~ESMC_Calendar - native default C++ destructor
//
// !INTERFACE:
      ESMC_Calendar::~ESMC_Calendar(void) {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      Default {\tt ESMC\_Calendar} destructor
//
//EOP
// !REQUIREMENTS: 

} // end ~ESMC_Calendar
