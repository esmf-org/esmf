// $Id: ESMC_Calendar.C,v 1.43 2004/02/02 19:14:07 eschwab Exp $
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
// The code in this file implements the C++ {\tt EMSC\_Calendar} methods
// declared in the companion file {\tt ESMC\_Calendar.h}
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
 static const char *const version = "$Id: ESMC_Calendar.C,v 1.43 2004/02/02 19:14:07 eschwab Exp $";
//-------------------------------------------------------------------------

// initialize static calendar instance counter
// TODO: inherit from ESMC_Base class
int ESMC_Calendar::count=0;

//
//-------------------------------------------------------------------------
//-------------------------------------------------------------------------
//
// This section includes all the ESMC_Calendar routines
//
//
//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CalendarCreate - Allocates and Initializes a Calendar object
//
// !INTERFACE:
      ESMC_Calendar *ESMC_CalendarCreate(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMC_Calendar
//
// !ARGUMENTS:
      int                nameLen,    // in
      const char        *name,       // in
      ESMC_CalendarType  type,       // in
      int               *rc) {       // out - return code

// !DESCRIPTION:
//      Allocates and Initializes a {\tt ESMC\_Calendar} with given type
//
//EOP
// !REQUIREMENTS:

    int returnCode;
    ESMC_Calendar *calendar;

    try {
      calendar = new ESMC_Calendar;
    }
    catch (...) {
      // TODO:  call ESMF log/err handler
      cerr << "ESMC_CalendarCreate() (new) memory allocation failed\n";
      if (rc != ESMC_NULL_POINTER) {
        *rc = ESMF_FAILURE;
      }
      return(ESMC_NULL_POINTER);
    }

    // TODO: use inherited methods from ESMC_Base or share with ESMC_Alarm
    if (name != ESMC_NULL_POINTER) {
      if (nameLen < ESMF_MAXSTR) {
        strncpy(calendar->name, name, nameLen);
        calendar->name[nameLen] = '\0';  // null terminate
      } else {
        // TODO: error, delete and return null calendar?
        if (rc != ESMC_NULL_POINTER) {
          *rc = ESMF_FAILURE;
        }
        return(calendar);
      }
    } else {
      // create default name "CalendarNNN"
      sprintf(calendar->name, "Calendar%3.3d\0", calendar->id);
    }

    if((returnCode = calendar->ESMC_CalendarSet(type)) != ESMF_SUCCESS) {
      if (rc != ESMC_NULL_POINTER) {
        *rc = returnCode;
      }
      return(calendar);
    }

    returnCode = calendar->ESMC_CalendarValidate();
    if (rc != ESMC_NULL_POINTER) {
      *rc = returnCode;
    }

    return(calendar);

 } // end ESMC_CalendarCreate (new)

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CalendarCreate - Allocates and Initializes a Custom Calendar object
//
// !INTERFACE:
      ESMC_Calendar *ESMC_CalendarCreate(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMC_Calendar
//
// !ARGUMENTS:
      int           nameLen,       // in
      const char   *name,          // in
      int          *monthsPerYear, // in  
      int          *daysPerMonth,  // in
      ESMF_KIND_I4 *secondsPerDay, // in
      ESMF_KIND_I4 *daysPerYear,   // in
      ESMF_KIND_I4 *daysPerYearDn, // in
      ESMF_KIND_I4 *daysPerYearDd, // in
      int          *rc) {          // out - return code

// !DESCRIPTION:
//      Allocates and Initializes a custom {\tt ESMC\_Calendar}.
//
//EOP
// !REQUIREMENTS:

    int returnCode;
    ESMC_Calendar *calendar;

    try {
      calendar = new ESMC_Calendar;
    }
    catch (...) {
      // TODO:  call ESMF log/err handler
      cerr << "ESMC_CalendarCreate() (custom) memory allocation failed\n";
      if (rc != ESMC_NULL_POINTER) {
        *rc = ESMF_FAILURE;
      }
      return(ESMC_NULL_POINTER);
    }

    // TODO: use inherited methods from ESMC_Base or share with ESMC_Alarm
    if (name != ESMC_NULL_POINTER) {
      if (nameLen < ESMF_MAXSTR) {
        strncpy(calendar->name, name, nameLen);
        calendar->name[nameLen] = '\0';  // null terminate
      } else {
        // TODO: error, delete and return null calendar?
        if (rc != ESMC_NULL_POINTER) {
          *rc = ESMF_FAILURE;
        }
        return(calendar);
      }
    } else {
      // create default name "CalendarNNN"
      sprintf(calendar->name, "Calendar%3.3d\0", calendar->id);
    }

    returnCode = calendar->ESMC_CalendarSet(monthsPerYear, daysPerMonth,
                                            secondsPerDay, daysPerYear,
                                            daysPerYearDn, daysPerYearDd);
    if (returnCode != ESMF_SUCCESS) {
      if (rc != ESMC_NULL_POINTER) {
        *rc = returnCode;
      }
      return(calendar);
    }

    returnCode = calendar->ESMC_CalendarValidate();
    if (rc != ESMC_NULL_POINTER) {
      *rc = returnCode;
    }

    return(calendar);

 } // end ESMC_CalendarCreate (custom)

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CalendarCreate - Creates a copy of a calendar
//
// !INTERFACE:
      ESMC_Calendar *ESMC_CalendarCreate(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMC_Calendar
//
// !ARGUMENTS:
      ESMC_Calendar *calendar,  // in  - calendar to copy
      int           *rc) {      // out - return code 

// !DESCRIPTION:
//      Creates a new copy of the given calendar.
//
//EOP
// !REQUIREMENTS:

    int returnCode;
    ESMC_Calendar *calendarCopy;

    try {
      // allocate new calendar and pass given calendar to copy constructor.
      calendarCopy = new ESMC_Calendar(*calendar);
    }
    catch (...) {
      // TODO:  call ESMF log/err handler
      cerr << "ESMC_CalendarCreate() (copy) memory allocation failed\n";
      if (rc != ESMC_NULL_POINTER) {
        *rc = ESMF_FAILURE;
      }
      return(ESMC_NULL_POINTER);
    }

    returnCode = calendarCopy->ESMC_CalendarValidate();
    if (rc != ESMC_NULL_POINTER) {
      *rc = returnCode;
    }

    return(calendarCopy);     

 } // end ESMC_CalendarCreate (copy)

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CalendarDestroy - free a Calendar created with Create
//
// !INTERFACE:
      int ESMC_CalendarDestroy(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_Calendar *calendar) {  // in - ESMC_Calendar to destroy
//
// !DESCRIPTION:
//      ESMF routine which destroys a Calendar object previously allocated
//      via an {\tt ESMC\_CalendarCreate} routine. Define for deep classes only.
//
//EOP

  if (calendar != ESMC_NULL_POINTER) {
    //calendar->ESMC_CalendarDestruct(); constructor calls it!
    delete calendar;
    calendar = ESMC_NULL_POINTER;
    return(ESMF_SUCCESS);
  } else {
    return(ESMF_FAILURE);
  }

 } // end ESMC_CalendarDestroy

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CalendarSet - Set a calendar's type
//
// !INTERFACE:
      int ESMC_Calendar::ESMC_CalendarSet(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_CalendarType type) {   // in - set to be Calendar type
//
// !DESCRIPTION:
//      Sets a {\tt EMSC\_Calendar} to be of a specific type
//
//EOP
// !REQUIREMENTS:

    // TODO: ensure initialization if called via F90 interface;
    //       cannot call constructor, because destructor is subsequently
    //       called automatically, returning initialized values to garbage.

    // save current values to restore in case of failure;
    ESMC_Calendar saveCalendar = *this;

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

        case ESMC_CAL_CUSTOM:
            // user defined; need more info; user must call
            //   Set() (custom) instead
            // restore original calendar
            *this = saveCalendar;
            rc = ESMF_FAILURE;
            break;

        default:
            // unknown calendar type; restore original
            *this = saveCalendar; 
            rc = ESMF_FAILURE;
            break;
    }
    return(rc);

}  // end ESMC_CalendarSet (new)

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CalendarSet - custom calendar initializer
//
// !INTERFACE:
      int ESMC_Calendar::ESMC_CalendarSet(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int          *monthsPerYear,    // in
      int          *daysPerMonth,     // in
      ESMF_KIND_I4 *secondsPerDay,    // in
      ESMF_KIND_I4 *daysPerYear,      // in
      ESMF_KIND_I4 *daysPerYeardN,    // in
      ESMF_KIND_I4 *daysPerYeardD) {  // in
// 
// !DESCRIPTION:
//      Initialzes a {\tt EMSC\_Calendar} to be a custom, user-defined type
// 
//EOP
// !REQUIREMENTS:

    // TODO: replace MONTHS_PER_YEAR with monthsPerYear

    // TODO: ensure initialization if called via F90 interface;
    //       cannot call constructor, because destructor is subsequently
    //       called automatically, returning initialized values to garbage.
    for (int i=0; i<MONTHS_PER_YEAR; i++) this->daysPerMonth[i] = 0;
    this->secondsPerDay  = 0;
    this->secondsPerYear = 0;
    this->daysPerYear.d  = 0;
    this->daysPerYear.dN = 0;
    this->daysPerYear.dD = 1;
    
    this->type = ESMC_CAL_CUSTOM;

    if (daysPerMonth != ESMC_NULL_POINTER) {
      for(int i=0; i<MONTHS_PER_YEAR; i++)
      { 
        this->daysPerMonth[i] = daysPerMonth[i];
      }
    }

    // use passed in value if present, otherwise use default

    this->secondsPerDay = (secondsPerDay != ESMC_NULL_POINTER) ?
                                              *secondsPerDay : SECONDS_PER_DAY;
    if (daysPerYear != ESMC_NULL_POINTER) {
      this->daysPerYear.d = *daysPerYear;
    } else {
      this->daysPerYear.d = 0;
      for(int i=0; i<MONTHS_PER_YEAR; i++)
      { 
        this->daysPerYear.d += daysPerMonth[i];
      }
    }

    if (daysPerYeardN != ESMC_NULL_POINTER) {
      this->daysPerYear.dN = *daysPerYeardN;
    }
    if (daysPerYeardD != ESMC_NULL_POINTER) {
      this->daysPerYear.dD = *daysPerYeardD;
    }

    this->secondsPerYear = this->secondsPerDay * this->daysPerYear.d;

    return(ESMF_SUCCESS);

}  // end ESMC_CalendarSet (custom)

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
      ESMC_CalendarType *type,          // out - Calendar type
      int          *monthsPerYear,      // out
      int          *daysPerMonth,       // out
      ESMF_KIND_I4 *secondsPerDay,      // out
      ESMF_KIND_I4 *secondsPerYear,     // out
      ESMF_KIND_I4 *daysPerYear,        // out
      ESMF_KIND_I4 *daysPerYeardN,      // out
      ESMF_KIND_I4 *daysPerYeardD) {    // out
// 
// !DESCRIPTION:
//      Gets a {\tt EMSC\_Calendar}'s properties
// 
//EOP
// !REQUIREMENTS:

    // TODO: replace MONTHS_PER_YEAR with monthsPerYear

    // must have at least one non-null pointer
    if (type           == ESMC_NULL_POINTER &&
        monthsPerYear  == ESMC_NULL_POINTER && 
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
      ESMF_KIND_I8 yy, int mm, int dd, ESMF_KIND_I8 d,    // in
      ESMC_BaseTime *t) const {                           // out
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
            if (yy < -4800 || mm < 1 || mm > 12 || dd < 1) {
              return (ESMF_FAILURE);
            }
            // invalid before 3/1/-4800
            if (yy == -4800 && mm < 3) {
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
              int leapDay = ESMC_IS_LEAP_YEAR(yy) ? 1 : 0;
              if (dd > (daysPerMonth[1] + leapDay)) {
                return (ESMF_FAILURE);
              }
            }

            // convert Gregorian date to Julian days
            // Gregorian date (yy, mm, dd) => Julian days (jdays)
            int temp            = (mm - 14) / 12;
            ESMF_KIND_I8 jdays = (1461 * (yy + 4800 + temp)) / 4 +
                             (367 * (mm - 2 - 12 * temp )) / 12 -
                             (3 * ((yy + 4900 + temp) / 100)) / 4 + dd - 32075;

            // convert Julian days to basetime seconds (>= 64 bit)
            t->s = jdays * secondsPerDay;

            break;
        }
        // convert No Leap Date => Time
        case ESMC_CAL_NOLEAP:
        {
            t->s = yy * secondsPerYear;
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
            t->s  = yy * secondsPerYear
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
      const ESMC_BaseTime *t,                                          // in
      ESMF_KIND_I4 *yy, ESMF_KIND_I8 *yy_i8, int *mm, int *dd,         // out
      ESMF_KIND_I4 *d, ESMF_KIND_I8 *d_i8, ESMF_KIND_R8 *d_r8) const { // out
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

// TODO: validate core values before conversion as they can go out-of-range
//       during arithmetic operations

    int rc = ESMF_SUCCESS;

    switch (this->type)
    {
        // convert Time => Gregorian Date
        case ESMC_CAL_GREGORIAN:
        {
            // Convert basetime portion of Time into date
            // Julian day (D) => Gregorian date (yy, mm, dd)
            // The calculation below fails for jday > 106,751,991,167,300
            //    (4*templ = 2^63)

            // convert basetime seconds to Julian days
            ESMF_KIND_I8 jdays = t->s / secondsPerDay;

            if (d != ESMC_NULL_POINTER) {
              if (jdays > INT_MIN && jdays <= INT_MAX) {
                *d = (ESMF_KIND_I4) jdays;
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
              *d_r8 = (ESMF_KIND_R8) t->s / (ESMF_KIND_R8) secondsPerDay;
            }

            // convert Julian days to Gregorian date
            // Julian days (jdays) => Gregorian date (yy, mm, dd)
            if (dd != ESMC_NULL_POINTER || mm != ESMC_NULL_POINTER ||
                yy != ESMC_NULL_POINTER || yy_i8 != ESMC_NULL_POINTER) {
              ESMF_KIND_I8 templ = jdays + 68569;
              ESMF_KIND_I8 tempn = (4 * templ) / 146097;
                            templ = templ - (146097 * tempn + 3) / 4;
              ESMF_KIND_I8 tempi = (4000 * (templ + 1)) / 1461001;
                            templ = templ - (1461 * tempi) / 4 + 31;
              ESMF_KIND_I8 tempj = (80 * templ) / 2447;
              if (dd != ESMC_NULL_POINTER) {
                *dd = templ - (2447 * tempj) / 80;
              }
              templ = tempj / 11;
              if (mm != ESMC_NULL_POINTER) {
                *mm = tempj + 2 - (12 * templ);
              }

              ESMF_KIND_I8 year = 100 * (tempn - 49) + tempi + templ;
              if (yy != ESMC_NULL_POINTER) {
                if (year >= INT_MIN && year <= INT_MAX) {
                  *yy = (ESMF_KIND_I4) year;  // >= 32-bit
                } else {
                  // too large to fit in given int
                  rc = ESMF_FAILURE;
                }
              }
              if (yy_i8 != ESMC_NULL_POINTER) {
                *yy_i8 = year;    // >= 64-bit
              }
            }

            break;
        }
        // convert Time => No Leap Date
        case ESMC_CAL_NOLEAP:
        {
            ESMF_KIND_I8 tmpS = t->s - 148600915200LL;
                                     // ^ adjust to match Julian time zero
                                     // = (1/1/0000) - (11/24/-4713)

            if (yy != ESMC_NULL_POINTER) {
              ESMF_KIND_I8 year = tmpS / secondsPerYear;
              if (year > INT_MIN && year <= INT_MAX) {
                  *yy = (ESMF_KIND_I4) year;  // >= 32-bit
                  // adjust for negative time (reverse integer division)
                  if (tmpS % secondsPerYear < 0) (*yy)--;
              } else {
                  // too large to fit in given int
                  rc = ESMF_FAILURE;
              }
            }
            if (yy_i8 != ESMC_NULL_POINTER) {
              *yy_i8 = tmpS / secondsPerYear;  // >= 64-bit
              // adjust for negative time (reverse integer division)
              if (tmpS % secondsPerYear < 0) (*yy_i8)--;
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
              ESMF_KIND_I8 day = tmpS / secondsPerDay;
              if (day > INT_MIN && day <= INT_MAX) {
                *d = (ESMF_KIND_I4) day;   // >= 32-bit
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
              *d_r8 = (ESMF_KIND_R8) tmpS / (ESMF_KIND_R8) secondsPerDay;
            }

            break;
        }
        // convert Time => 360 Day Date
        case ESMC_CAL_360DAY:
        {
            ESMF_KIND_I8 tmpS = t->s - 146565244800LL;
                                     // ^ adjust to match Julian time zero
                                     // = (1/1/0000) - (11/24/-4713)

            if (yy != ESMC_NULL_POINTER) {
              ESMF_KIND_I8 year = tmpS / secondsPerYear;
              if (year > INT_MIN && year <= INT_MAX) {
                *yy = (ESMF_KIND_I4) year;
                // adjust for negative time (reverse integer division)
                if (tmpS % secondsPerYear < 0) (*yy)--;
              } else {
                // too large to fit in given int
                rc = ESMF_FAILURE;
              }
            }
            if (yy_i8 != ESMC_NULL_POINTER) {
              *yy_i8 = tmpS / secondsPerYear;
              // adjust for negative time (reverse integer division)
              if (tmpS % secondsPerYear < 0) (*yy_i8)--;
            }

            int dayOfYear = (tmpS % secondsPerYear) / secondsPerDay + 1;
            // ensure day range is positive 1-360
            if (dayOfYear <= 0) dayOfYear += daysPerYear.d;  

            if (mm != ESMC_NULL_POINTER) {
              *mm = (dayOfYear-1) / 30 + 1;  // each month has 30 days
            }
            if (dd != ESMC_NULL_POINTER) {
              *dd = (dayOfYear-1) % 30 + 1;  // each month has 30 days
            }

            // convert basetime seconds to Julian days
            if (d != ESMC_NULL_POINTER) {
              ESMF_KIND_I8 day = tmpS / secondsPerDay;
              if (day > INT_MIN && day <= INT_MAX) {
                *d = (ESMF_KIND_I4) day;   // >= 32-bit
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
              *d_r8 = (ESMF_KIND_R8) tmpS / (ESMF_KIND_R8) secondsPerDay;
            }

            break;
        }
        // convert Time => Julian Date
        case ESMC_CAL_JULIANDAY:
        {
            // convert basetime seconds to Julian days
            if (d != ESMC_NULL_POINTER) {
              ESMF_KIND_I8 day = t->s / secondsPerDay;
              if (day > INT_MIN && day <= INT_MAX) {
                *d = (ESMF_KIND_I4) day;    // >= 32-bit
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
              *d_r8 = (ESMF_KIND_R8) t->s / (ESMF_KIND_R8) secondsPerDay;
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
// !IROUTINE:  ESMC_CalendarReadRestart - restore contents of a Calendar 
//
// !INTERFACE:
      ESMC_Calendar *ESMC_CalendarReadRestart(
//
// !RETURN VALUE:
//    pointer to newly allocated and restored ESMC_Calendar
//
// !ARGUMENTS:
      int          nameLen,  // in
      const char  *name,     // in
      ESMC_IOSpec *iospec,   // in
      int         *rc ) {    // out - return code

//
// !DESCRIPTION:
//      Restore information about a {\tt Calendar}. 
//      For persistence/checkpointing.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

    // TODO:  read calendar state from iospec/name, then allocate/restore
    //        (share code with ESMC_CalendarCreate()).

    return(ESMC_NULL_POINTER);

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
      ESMC_IOSpec *iospec) const {
//
// !DESCRIPTION:
//      Save {\tt Calendar} state for persistence/checkpointing
//
//EOP
// !REQUIREMENTS:

    int rc = ESMF_SUCCESS;

    // TODO:

    return(rc);

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

    name[0] = '\0';
    id = ++count;  // TODO: inherit from ESMC_Base class

    type           = ESMC_CAL_NOCALENDAR;
    monthsPerYear  = 0;
    // TODO: daysPerMonth   = ESMC_NULL_POINTER;
    secondsPerDay  = SECONDS_PER_DAY;
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

    ESMC_Calendar();  // invoke default constructor
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
      int          *monthsPerYear,     // in
      int          *daysPerMonth,      // in
      ESMF_KIND_I4 *secondsPerDay,     // in
      ESMF_KIND_I4 *daysPerYear,       // in
      ESMF_KIND_I4 *daysPerYeardN,     // in
      ESMF_KIND_I4 *daysPerYeardD) {   // in
//
// !DESCRIPTION:
//      Initializes a {\tt ESMC\_Time} to be of a custom user-defined type
//      via {\tt ESMC\_CalendarSet}
//
//EOP
// !REQUIREMENTS: 

    ESMC_Calendar();  // invoke default constructor
    ESMC_CalendarSet(monthsPerYear, daysPerMonth, secondsPerDay, 
                     daysPerYear, daysPerYeardN, daysPerYeardD);
}  // end ESMC_Calendar

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Calendar - native C++ copy constructor
//
// !INTERFACE:
      ESMC_Calendar::ESMC_Calendar(
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
      const ESMC_Calendar &calendar) {  // in - calendar to copy
//
// !DESCRIPTION:
//      Copies members of given calendar.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

    *this = calendar;

 } // end ESMC_Calendar

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

  // TODO: make dynamically allocatable with monthsPerYear
  // delete[] daysPerMonth;

} // end ~ESMC_Calendar
