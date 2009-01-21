// $Id: ESMC_Calendar.C,v 1.86.2.2 2009/01/21 21:25:23 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
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
//
 #define ESMC_FILENAME "ESMC_Calendar.C"

 // higher level, 3rd party or system includes
 #include <stdio.h>
 #include <limits.h>
 #include <string.h>
 #include <ctype.h>
 #include <math.h>
 #include <stdlib.h>

 #include <ESMC_LogErr.h>
 #include <ESMF_LogMacros.inc>

 #include <ESMC_Time.h>
 #include <ESMC_TimeInterval.h>

 // associated class definition file
 #include <ESMC_Calendar.h>

//-------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_Calendar.C,v 1.86.2.2 2009/01/21 21:25:23 cdeluca Exp $";
//-------------------------------------------------------------------------

// initialize static array of calendar type names
const char *const ESMC_Calendar::calendarTypeName[CALENDAR_TYPE_COUNT] =
                                                  { "Gregorian", "Julian",
                                                    "Julian Day", "No Leap",
                                                    "360 Day", "Custom",
                                                    "No Calendar" };

// initialize static internal calendar pointer array
ESMC_Calendar *ESMC_Calendar::internalCalendar[CALENDAR_TYPE_COUNT] =
                                      { ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                        ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                        ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                        ESMC_NULL_POINTER };

// initialize default calendar
ESMC_Calendar *ESMC_Calendar::defaultCalendar = ESMC_NULL_POINTER;

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
//-----------------------------------------------------------------------------
//BOPI
// !IROUTINE:  ESMC_CalendarInitialize - initialize the default Calendar type
//
// !INTERFACE:
      int ESMC_CalendarInitialize(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_CalendarType *calendarType) {  // in - ESMC_CalendarType to be the
                                          //      default
//
// !DESCRIPTION:
//      Friend function which initializes the Time Manager default calendar.
//
//EOPI

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_CalendarInitialize()"

  int rc = ESMC_CalendarSetDefault(calendarType);
  ESMC_LogDefault.ESMC_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &rc);
  return(rc);

 } // end ESMC_CalendarInitialize

//-----------------------------------------------------------------------------
//BOPI
// !IROUTINE:  ESMC_CalendarFinalize - free all internal Calendars
//
// !INTERFACE:
      int ESMC_CalendarFinalize(void) { 
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      Friend function which de-allocates all internal built-in Calendars.
//
//EOPI

  for (int i=0; i<CALENDAR_TYPE_COUNT; i++) {
    delete ESMC_Calendar::internalCalendar[i];
    ESMC_Calendar::internalCalendar[i] = ESMC_NULL_POINTER;
  }
  ESMC_Calendar::defaultCalendar = ESMC_NULL_POINTER;

  return(ESMF_SUCCESS);

 } // end ESMC_CalendarFinalize

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
      int                nameLen,      // in
      const char        *name,         // in
      ESMC_CalendarType  calendarType, // in
      int               *rc) {         // out - return code

// !DESCRIPTION:
//      Allocates and Initializes a {\tt ESMC\_Calendar} with given type
//
//EOP
// !REQUIREMENTS:

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_CalendarCreate(built-in)"

    int returnCode;
    ESMC_Calendar *calendar;

    // default return code
    if (rc != ESMC_NULL_POINTER) *rc = ESMC_RC_NOT_IMPL;

    // make sure calendar type is valid
    if (calendarType < 1 || calendarType > CALENDAR_TYPE_COUNT) {
      char logMsg[ESMF_MAXSTR];
      sprintf(logMsg, "calendarType %d not in valid range of 1 to %d.",
              calendarType, CALENDAR_TYPE_COUNT);
      ESMC_LogDefault.ESMC_LogWrite(logMsg, ESMC_LOG_ERROR);
      return(ESMC_NULL_POINTER);
    }

    try {
      calendar = new ESMC_Calendar;
    }
    catch (...) {
      ESMC_LogDefault.ESMC_LogAllocError(rc);
      return(ESMC_NULL_POINTER);
    }

    // TODO: use inherited methods from ESMC_Base
    if (name != ESMC_NULL_POINTER) {
      if (nameLen < ESMF_MAXSTR) {
        strncpy(calendar->name, name, nameLen);
        calendar->name[nameLen] = '\0';  // null terminate
      } else {
        // truncate
        strncpy(calendar->name, name, ESMF_MAXSTR-1);
        calendar->name[ESMF_MAXSTR-1] = '\0';  // null terminate

        char logMsg[ESMF_MAXSTR];
        sprintf(logMsg, "calendar name %s, length >= ESMF_MAXSTR; truncated.",
                name);
        ESMC_LogDefault.ESMC_LogWrite(logMsg, ESMC_LOG_WARN);
        // TODO: return ESMF_WARNING when defined
        // if (rc != ESMC_NULL_POINTER) *rc = ESMF_WARNING;
      }
    } else {
      // create default name "CalendarNNN"
      sprintf(calendar->name, "Calendar%3.3d\0", calendar->id);
    }

    returnCode = calendar->ESMC_CalendarSet(strlen(calendar->name), 
                                            calendar->name, 
                                            calendarType);

    if (ESMC_LogDefault.ESMC_LogMsgFoundError(returnCode,
                                              ESMF_ERR_PASSTHRU, rc)) {
      return(calendar);
    }

    returnCode = calendar->ESMC_CalendarValidate();
    ESMC_LogDefault.ESMC_LogMsgFoundError(returnCode,
                                          ESMF_ERR_PASSTHRU, rc);

    return(calendar);

 } // end ESMC_CalendarCreate (built-in)

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CalendarCreate - Allocates and Initializes an internal Calendar object
//
// !INTERFACE:
      int ESMC_CalendarCreate(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_CalendarType calendarType) { // in

// !DESCRIPTION:
//      Allocates and Initializes an internal {\tt ESMC\_Calendar} of given type
//
//EOP
// !REQUIREMENTS:

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_CalendarCreate(internal)"

    int returnCode;

    // make sure it is valid
    if (calendarType < 1 || calendarType > CALENDAR_TYPE_COUNT) {
      char logMsg[ESMF_MAXSTR];
      sprintf(logMsg, "calendarType %d not in valid range of 1 to %d.",
              calendarType, CALENDAR_TYPE_COUNT);
      ESMC_LogDefault.ESMC_LogWrite(logMsg, ESMC_LOG_ERROR);
      return(ESMF_FAILURE);
    }

    // select internal calendar static pointer based on specified cal type
    ESMC_Calendar **internalCal =
                            &(ESMC_Calendar::internalCalendar[calendarType-1]);

    // check if valid internal calendar already exists
    if (*internalCal != ESMC_NULL_POINTER) {
      if ((*internalCal)->ESMC_CalendarValidate() == ESMF_SUCCESS) {
        return(ESMF_SUCCESS);
      } else {
        // something malformed exists; delete it and try again
        delete *internalCal;
      }
    }

    // create desired internal calendar
    try {
      *internalCal = new ESMC_Calendar;
    }
    catch (...) {
      ESMC_LogDefault.ESMC_LogAllocError(&returnCode);
      return(ESMF_FAILURE);
    }

    // create default internal name, e.g. "InternalGregorian001"
    sprintf((*internalCal)->name, "Internal%s%3.3d\0",
                         ESMC_Calendar::calendarTypeName[calendarType-1],
                                                    (*internalCal)->id);

    returnCode = (*internalCal)->ESMC_CalendarSet(strlen((*internalCal)->name), 
                                                 (*internalCal)->name, 
                                                 calendarType);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(returnCode,
                                              ESMF_ERR_PASSTHRU, &returnCode)) {
      return(returnCode);
    }

    returnCode = (*internalCal)->ESMC_CalendarValidate();
    ESMC_LogDefault.ESMC_LogMsgFoundError(returnCode,
                                          ESMF_ERR_PASSTHRU, &returnCode);
    return(returnCode);

 } // end ESMC_CalendarCreate (internal)

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
      int          *daysPerMonth,  // in
      int           monthsPerYear, // in  
      ESMC_I4 *secondsPerDay, // in
      ESMC_I4 *daysPerYear,   // in
      ESMC_I4 *daysPerYearDn, // in
      ESMC_I4 *daysPerYearDd, // in
      int          *rc) {          // out - return code

// !DESCRIPTION:
//      Allocates and Initializes a custom {\tt ESMC\_Calendar}.
//
//EOP
// !REQUIREMENTS:

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_CalendarCreate(custom)"

    int returnCode;
    ESMC_Calendar *calendar;

    // default return code
    if (rc != ESMC_NULL_POINTER) *rc = ESMF_FAILURE;

    try {
      calendar = new ESMC_Calendar;
    }
    catch (...) {
      ESMC_LogDefault.ESMC_LogAllocError(rc);
      return(ESMC_NULL_POINTER);
    }

    // TODO: use inherited methods from ESMC_Base
    if (name != ESMC_NULL_POINTER) {
      if (nameLen < ESMF_MAXSTR) {
        strncpy(calendar->name, name, nameLen);
        calendar->name[nameLen] = '\0';  // null terminate
      } else {
        // truncate
        strncpy(calendar->name, name, ESMF_MAXSTR-1);
        calendar->name[ESMF_MAXSTR-1] = '\0';  // null terminate

        char logMsg[ESMF_MAXSTR];
        sprintf(logMsg, "calendar name %s, length >= ESMF_MAXSTR; truncated.",
                name);
        ESMC_LogDefault.ESMC_LogWrite(logMsg, ESMC_LOG_WARN);
        // TODO: return ESMF_WARNING when defined
        // if (rc != ESMC_NULL_POINTER) *rc = ESMF_WARNING;
      }
    } else {
      // create default name "CalendarNNN"
      sprintf(calendar->name, "Calendar%3.3d\0", calendar->id);
    }

    returnCode = calendar->ESMC_CalendarSet(strlen(calendar->name),
                                            calendar->name, 
                                            daysPerMonth, monthsPerYear,
                                            secondsPerDay, daysPerYear,
                                            daysPerYearDn, daysPerYearDd);
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(returnCode,
                                              ESMF_ERR_PASSTHRU, rc)) {
      return(calendar);
    }

    returnCode = calendar->ESMC_CalendarValidate();
    ESMC_LogDefault.ESMC_LogMsgFoundError(returnCode, ESMF_ERR_PASSTHRU, rc);
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

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_CalendarCreate(copy)"

    int returnCode;
    ESMC_Calendar *calendarCopy;

    // default return code
    if (rc != ESMC_NULL_POINTER) *rc = ESMF_FAILURE;

    // can't copy a non-existent object
    if (calendar == ESMC_NULL_POINTER) {
      ESMC_LogDefault.ESMC_LogWrite("Can't copy a non-existent calendar",
                                    ESMC_LOG_ERROR);
      return(ESMC_NULL_POINTER);
    }

    try {
      // allocate new calendar and pass given calendar to copy constructor.
      calendarCopy = new ESMC_Calendar(*calendar);
    }
    catch (...) {
      ESMC_LogDefault.ESMC_LogAllocError(rc);
      return(ESMC_NULL_POINTER);
    }

    returnCode = calendarCopy->ESMC_CalendarValidate();
    ESMC_LogDefault.ESMC_LogMsgFoundError(returnCode, ESMF_ERR_PASSTHRU, rc);
                            
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
      ESMC_Calendar **calendar) {  // in - ESMC_Calendar to destroy
//
// !DESCRIPTION:
//      ESMF routine which destroys a Calendar object previously allocated
//      via an {\tt ESMC\_CalendarCreate} routine. Define for deep classes only.
//
//EOP

  // TODO: calendar->ESMC_CalendarDestruct(); constructor calls it!
  delete *calendar;    // ok to delete null pointer
  *calendar = ESMC_NULL_POINTER;
  return(ESMF_SUCCESS);

 } // end ESMC_CalendarDestroy

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CalendarSetDefault - set the default Calendar
//
// !INTERFACE:
      int ESMC_CalendarSetDefault(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_Calendar **calendar) {  // in - ESMC_Calendar to be the default
//
// !DESCRIPTION:
//      Friend function which sets the Time Manager default calendar.
//
//EOP

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_CalendarSetDefault(calendar)"

  // ensure we have a valid calendar
  if (calendar == ESMC_NULL_POINTER) {
    ESMC_LogDefault.ESMC_LogWrite("calendar pointer-pointer is NULL.",
                                  ESMC_LOG_ERROR);
    return(ESMF_FAILURE);
  }
  if (*calendar == ESMC_NULL_POINTER) {
    ESMC_LogDefault.ESMC_LogWrite("calendar pointer is NULL.",
                                  ESMC_LOG_ERROR);
    return(ESMF_FAILURE);
  }

  int rc = (*calendar)->ESMC_CalendarValidate();
  if (ESMC_LogDefault.ESMC_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &rc)) {
    return(rc);
  }

  // set the default calendar
  ESMC_Calendar::defaultCalendar = *calendar;

  return(ESMF_SUCCESS);

 } // end ESMC_CalendarSetDefault

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CalendarSetDefault - set the default Calendar type
//
// !INTERFACE:
      int ESMC_CalendarSetDefault(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_CalendarType *calendarType) {  // in - ESMC_CalendarType to be the
                                          //      default
//
// !DESCRIPTION:
//      Friend function which sets the Time Manager default calendar.
//
//EOP

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_CalendarSetDefault(calendarType)"

  ESMC_CalendarType calType = (calendarType == ESMC_NULL_POINTER) ?
                                            ESMC_CAL_NOCALENDAR : *calendarType;

  // create internal calendar if necessary
  int rc = ESMC_CalendarCreate(calType);
  if (rc != ESMF_SUCCESS) {
    char logMsg[ESMF_MAXSTR];
    sprintf(logMsg, "ESMC_CalendarCreate(%s) failed.",
            ESMC_Calendar::calendarTypeName[calType]);
    ESMC_LogDefault.ESMC_LogWrite(logMsg, ESMC_LOG_ERROR);
    return (rc);
  }

  // set the default calendar
  ESMC_Calendar::defaultCalendar = 
                             ESMC_Calendar::internalCalendar[calType-1];

  return(ESMF_SUCCESS);

 } // end ESMC_CalendarSetDefault

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
      int               nameLen,         // in
      const char       *name,            // in
      ESMC_CalendarType calendarType) {  // in - set to be Calendar type
//
// !DESCRIPTION:
//      Sets a {\tt EMSC\_Calendar} to be of a specific type
//
//EOP
// !REQUIREMENTS:

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_CalendarSet(built-in)"

    int rc = ESMF_SUCCESS; // return code 

    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", &rc);
      return(rc);
    }

    // save current values to restore in case of failure;
    ESMC_Calendar saveCalendar = *this;

    // TODO: use inherited methods from ESMC_Base
    if (name != ESMC_NULL_POINTER) {
      if (name != this->name) {  // skip if called internally via Create()
        if (nameLen < ESMF_MAXSTR) {
          strncpy(this->name, name, nameLen);
          this->name[nameLen] = '\0';  // null terminate
        } else {
          // truncate
          strncpy(this->name, name, ESMF_MAXSTR-1);
          this->name[ESMF_MAXSTR-1] = '\0';  // null terminate

          char logMsg[ESMF_MAXSTR];
          sprintf(logMsg, "calendar name %s, length >= ESMF_MAXSTR; truncated.",
                  name);
          ESMC_LogDefault.ESMC_LogWrite(logMsg, ESMC_LOG_WARN);
          // TODO: return ESMF_WARNING when defined
          // rc = ESMF_WARNING;
        }
      }
    }

    this->calendarType = calendarType;

    switch (calendarType)
    {
        case ESMC_CAL_GREGORIAN:
        case ESMC_CAL_JULIAN:
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
            for (int i=0; i<monthsPerYear; i++) daysPerMonth[i] = 0;
            secondsPerDay  = SECONDS_PER_DAY;
            secondsPerYear = 0;
            daysPerYear.d  = 0;
            daysPerYear.dN = 0;
            daysPerYear.dD = 1;
            rc = ESMF_SUCCESS;
            break;

        case ESMC_CAL_360DAY:
            // 12 months of 30 days each
            for (int i=0; i<monthsPerYear; i++) daysPerMonth[i] = 30;
            secondsPerDay  = SECONDS_PER_DAY;
            secondsPerYear = SECONDS_PER_DAY * 360;
            daysPerYear.d  = 360;
            daysPerYear.dN = 0;
            daysPerYear.dD = 1;
            rc = ESMF_SUCCESS;
            break;

        case ESMC_CAL_NOCALENDAR:
            // no calendar needed, convert base time up to days only
            for (int i=0; i<monthsPerYear; i++) daysPerMonth[i] = 0;
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
            ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_WRONG,
                 ", must call CalendarSet(custom) for custom calendars.", &rc);
            break;

        default:
            // unknown calendar type; restore original
            *this = saveCalendar; 
            char logMsg[ESMF_MAXSTR];
            sprintf(logMsg, "; unknown calendar type %d.", calendarType);
            ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, logMsg,
                                                  &rc);
            break;
    }
    return(rc);

}  // end ESMC_CalendarSet (built-in)

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CalendarSet - Set up a custom calendar
//
// !INTERFACE:
      int ESMC_Calendar::ESMC_CalendarSet(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int           nameLen,          // in
      const char   *name,             // in
      int          *daysPerMonth,     // in
      int           monthsPerYear,    // in
      ESMC_I4 *secondsPerDay,    // in
      ESMC_I4 *daysPerYear,      // in
      ESMC_I4 *daysPerYeardN,    // in
      ESMC_I4 *daysPerYeardD) {  // in
// 
// !DESCRIPTION:
//      Initialzes a {\tt EMSC\_Calendar} to be a custom, user-defined type
// 
//EOP
// !REQUIREMENTS:

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_CalendarSet(custom)"

    int rc = ESMF_SUCCESS; // return code 

    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", &rc);
      return(rc);
    }

    // save current values to restore in case of failure;
    ESMC_Calendar saveCalendar = *this;

    // TODO: use inherited methods from ESMC_Base
    if (name != ESMC_NULL_POINTER) {
      if (name != this->name) {  // skip if called internally via Create()
        if (nameLen < ESMF_MAXSTR) {
          strncpy(this->name, name, nameLen);
          this->name[nameLen] = '\0';  // null terminate
        } else {
          // error, restore previous state and return ESMF_FAILURE
          *this = saveCalendar;
          // truncate
          strncpy(this->name, name, ESMF_MAXSTR-1);
          this->name[ESMF_MAXSTR-1] = '\0';  // null terminate

          char logMsg[ESMF_MAXSTR];
          sprintf(logMsg, "calendar name %s, length >= ESMF_MAXSTR; truncated.",
                  name);
          ESMC_LogDefault.ESMC_LogWrite(logMsg, ESMC_LOG_WARN);
          // TODO: return ESMF_WARNING when defined
          // rc = ESMF_WARNING;
        }
      }
    }

    this->calendarType = ESMC_CAL_CUSTOM;

    // TODO: replace MONTHS_PER_YEAR with dynamic daysPerMonth[monthsPerYear]
    if (monthsPerYear <= MONTHS_PER_YEAR && monthsPerYear >= 0) {
      this->monthsPerYear = monthsPerYear;
    } else {
      // error, restore previous state
      *this = saveCalendar;
      char logMsg[ESMF_MAXSTR];
      sprintf(logMsg, "; monthsPerYear %d negative or > MONTHS_PER_YEAR %d.",
                      this->daysPerYear.d, MONTHS_PER_YEAR);
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD, logMsg, &rc);
      return(rc);
    }

    this->daysPerYear.d = 0;
    if (daysPerMonth != ESMC_NULL_POINTER) {
      for(int i=0; i<this->monthsPerYear; i++)
      { 
        this->daysPerMonth[i] = daysPerMonth[i];
        this->daysPerYear.d += daysPerMonth[i];
      }
    }

    if (secondsPerDay != ESMC_NULL_POINTER) {
      this->secondsPerDay = *secondsPerDay;
    }
    if (daysPerYear != ESMC_NULL_POINTER) {
      this->daysPerYear.d = *daysPerYear;
    }
    if (daysPerYeardN != ESMC_NULL_POINTER) {
      this->daysPerYear.dN = *daysPerYeardN;
    }
    if (daysPerYeardD != ESMC_NULL_POINTER) {
      this->daysPerYear.dD = *daysPerYeardD;
    }

    this->secondsPerYear = this->secondsPerDay * this->daysPerYear.d;

    if ((rc = ESMC_CalendarValidate()) != ESMF_SUCCESS) {
      // error, restore previous state
      *this = saveCalendar;
      ESMC_LogDefault.ESMC_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &rc);
    }

    return(rc);

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
      int                nameLen,         // in
      int               *tempNameLen,     // out
      char              *tempName,        // out
      ESMC_CalendarType *calendarType,    // out
      int               *daysPerMonth,    // out
      int                sizeofDaysPerMonth, // in
      int               *monthsPerYear,   // out
      ESMC_I4      *secondsPerDay,   // out
      ESMC_I4      *secondsPerYear,  // out
      ESMC_I4      *daysPerYear,     // out
      ESMC_I4      *daysPerYeardN,   // out
      ESMC_I4      *daysPerYeardD) { // out
// 
// !DESCRIPTION:
//      Gets a {\tt EMSC\_Calendar}'s properties
// 
//EOP
// !REQUIREMENTS:

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_CalendarGet()"

    int rc = ESMF_SUCCESS; // return code 

    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", &rc);
      return(rc);
    }

    // TODO: replace MONTHS_PER_YEAR with monthsPerYear

    // must have at least one non-null pointer or a non-zero nameLen
    if (calendarType   == ESMC_NULL_POINTER &&
        daysPerMonth   == ESMC_NULL_POINTER && 
        monthsPerYear  == ESMC_NULL_POINTER && 
        secondsPerDay  == ESMC_NULL_POINTER &&
        secondsPerYear == ESMC_NULL_POINTER &&
        daysPerYear    == ESMC_NULL_POINTER &&
        daysPerYeardN  == ESMC_NULL_POINTER &&
        daysPerYeardD  == ESMC_NULL_POINTER &&
        nameLen == 0) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_BAD, 
                  ", no valid argument passed in.", &rc);
      return(rc);
    }

    // TODO: use inherited methods from ESMC_Base
    if (nameLen > 0) {
      if (strlen(this->name) < nameLen) {
        // copy all of it
        strcpy(tempName, this->name);
      } else {
        // TODO: copy what will fit and return ESMF_FAILURE ?
        strncpy(tempName, this->name, nameLen-1);
        tempName[nameLen] = '\0';  // null terminate

        char logMsg[ESMF_MAXSTR];
        sprintf(logMsg, "For calendar name %s, "
                "length >= given character array; truncated.", this->name);
        ESMC_LogDefault.ESMC_LogWrite(logMsg, ESMC_LOG_WARN);
        // TODO: return ESMF_WARNING when defined
        // rc = ESMF_WARNING;
      }
      // report how many characters were copied
      *tempNameLen = strlen(tempName);
    }

    if (calendarType != ESMC_NULL_POINTER) {
      *calendarType = this->calendarType;
    }
    if (daysPerMonth != ESMC_NULL_POINTER) {
      if (sizeofDaysPerMonth < this->monthsPerYear) return(ESMF_FAILURE);
      for (int i=0; i<this->monthsPerYear; i++) {
        daysPerMonth[i] = this->daysPerMonth[i];
      }
    }
    if (monthsPerYear != ESMC_NULL_POINTER) {
      *monthsPerYear = this->monthsPerYear;
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
      ESMC_I8 yy, int mm, int dd, ESMC_I8 d,    // in
      ESMC_BaseTime *t) const {                           // out
//
// !DESCRIPTION:
//     Converts a calendar-specific date to core {\tt ESMC\_BaseTime}
//     representation. Conversions based on UTC: time zone offset done by
//     client
//
//     The Gregorian <-> Julian day conversion algorithm is from
//     Henry F. Fliegel and Thomas C. Van Flandern, in Communications of
//     the ACM, CACM, volume 11, number 10, October 1968, p. 657.
//     Julian day refers to the number of days since a reference day.
//     For the algorithm used, this reference day is November 24, -4713
//     in the Proleptic Gregorian calendar, which is equivalent to
//     January 1, -4712 in the Proleptic Julian calendar.
// 
//     When converting from a Julian day to a Gregorian date (implemented
//     in {\tt ESMC\_CalendarConvertToDate()}), this algorithm is valid from
//     3/1/-4900 Gregorian forward.  When converting from a Gregorian date to
//     a Julian day (implemented in this method), the algorithm is valid from
//     3/1/-4800 forward.  In both cases, the algorithm correctly takes into
//     account leap years, those that are divisable by 4 and not 100, or those
//     divisible by 400.
//
//     The Fliegel algorithm implements the Gregorian calendar as continuously
//     proleptic from October 15, 1582 backward to March 1, -4800/-4900.
//     Hence the algorithm does not take into account the Gregorian Reformation
//     (when the Gregorian calendar officially began) where 10 days were
//     eliminated from the calendar in October 1582.  Thursday, October 4, 1582
//     was officially the last day of the Julian calendar; the following day,
//     Friday, was decreed to be October 15, 1582, the first day of the
//     Gregorian calendar.
//
//     The Julian <-> Julian day conversion algorithm is from D.A. Hatcher,
//     Q.JlR. astr. Soc. (1984) 25, 53-55.  It is valid from 3/1/-4712 forward.
//
//     See also:  http://www.hermetic.ch/cal\_stud/jdn.htm
//                http://www.numerical-recipes.com/julian.html
//
//EOP
// !REQUIREMENTS:   TMG 2.4.5, 2.5.6

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_CalendarConvertToTime()"

    int rc = ESMF_SUCCESS; // return code 

    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", &rc);
      return(rc);
    }

    switch (this->calendarType)
    {
        // convert Gregorian Date => Time
        case ESMC_CAL_GREGORIAN:
        {
            //
            // Validate inputs 
            //
            if (yy < -4800 || mm < 1 || mm > 12 || dd < 1) {
              char logMsg[ESMF_MAXSTR];
              sprintf(logMsg, "; Gregorian %d/%d/%lld (1-12/>=1/>=-4800).",
                      mm, dd, yy); 
              ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_OUTOFRANGE,
                                                    logMsg, &rc);
              return(rc);
            }
            // invalid before 3/1/-4800
            if (yy == -4800 && mm < 3) {
              char logMsg[ESMF_MAXSTR];
              sprintf(logMsg, "; Gregorian %d/%d/%lld is before 3/1/-4800.",
                      mm, dd, yy); 
              ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_OUTOFRANGE,
                                                    logMsg, &rc);
              return(rc);
            }

            // TODO: upper bounds date range check dependent on machine
            //  word size, e.g. for signed 64-bit, max date is
            //  10/29/292,277,019,914

            // check day of the month for any month except February
            if (mm != 2 && dd > daysPerMonth[mm-1]) {
              char logMsg[ESMF_MAXSTR];
              sprintf(logMsg, "; Gregorian: for month %d, dd=%d > %d days "
                      "in the month.", mm, dd, daysPerMonth[mm-1]);
              ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_OUTOFRANGE,
                                                    logMsg, &rc);
              return(rc);
            }
            // if February, take leap year into account before checking
            //   day of the month
            if (mm == 2) {
              int leapDay = ESMC_CalendarIsLeapYear(yy) ? 1 : 0;
              if (dd > (daysPerMonth[1] + leapDay)) {
                char logMsg[ESMF_MAXSTR];
                sprintf(logMsg, "; Gregorian: for February %lld, dd=%d > %d "
                        "days in the month.", yy, dd,
                        (daysPerMonth[1]+leapDay));
                ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_OUTOFRANGE,
                                                    logMsg, &rc);
                return(rc);
              }
            }

            // convert Gregorian date to Julian days
            // Gregorian date (yy, mm, dd) => Julian days (jdays)
            int temp            = (mm - 14) / 12;
            ESMC_I8 jdays = (1461 * (yy + 4800 + temp)) / 4 +
                             (367 * (mm - 2 - 12 * temp )) / 12 -
                             (3 * ((yy + 4900 + temp) / 100)) / 4 + dd - 32075;

            // convert Julian days to basetime seconds (>= 64 bit)
            t->ESMC_FractionSetw(jdays * secondsPerDay);

            break;
        }

        // convert Julian Date => Time
        case ESMC_CAL_JULIAN:
        {
            //
            // Validate inputs 
            //
            if (yy < -4712 || mm < 1 || mm > 12 || dd < 1) {
              char logMsg[ESMF_MAXSTR];
              sprintf(logMsg, "; Julian %d/%d/%lld (1-12/>=1/>=-4712).",
                      mm, dd, yy); 
              ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_OUTOFRANGE,
                                                    logMsg, &rc);
              return(rc);
            }
            // invalid before 3/1/-4712
            if (yy == -4712 && mm < 3) {
              char logMsg[ESMF_MAXSTR];
              sprintf(logMsg, "; Julian %d/%d/%lld is before 3/1/-4712.",
                      mm, dd, yy); 
              ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_OUTOFRANGE,
                                                    logMsg, &rc);
              return(rc);
            }

            // TODO: upper bounds date range check dependent on machine
            //  word size, e.g. for signed 64-bit, max date is
            //  4/24/292,271,018,333.

            // check day of the month for any month except February
            if (mm != 2 && dd > daysPerMonth[mm-1]) {
              char logMsg[ESMF_MAXSTR];
              sprintf(logMsg, "; Julian: for month %d, dd=%d > %d days "
                      "in the month.", mm, dd, daysPerMonth[mm-1]);
              ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_OUTOFRANGE,
                                                    logMsg, &rc);
              return(rc);
            }
            // if February, take leap year into account before checking
            //   day of the month
            if (mm == 2) {
              int leapDay = ESMC_CalendarIsLeapYear(yy) ? 1 : 0;
              if (dd > (daysPerMonth[1] + leapDay)) {
                char logMsg[ESMF_MAXSTR];
                sprintf(logMsg, "; Julian: for February %lld, dd=%d > %d "
                        "days in the month.", yy, dd,
                        (daysPerMonth[1]+leapDay));
                ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_OUTOFRANGE,
                                                    logMsg, &rc);
                return(rc);
              }
            }

            // Algorithm is from D.A. Hatcher, Q. Jl R. astr. Soc. (1984),
            //  volume 25, pp. 53-55.
            ESMC_I8 yyprime = yy - ((12-mm) / 10);
            int          mmprime = (mm+9) % 12;
            ESMC_I8 y = (ESMC_I8) (365.25 * (yyprime + 4712));
            int          d = (int)((30.6 * mmprime) + 0.5);
            ESMC_I8 jdays = y + d + dd + 59;

            // convert Julian days to basetime seconds (>= 64 bit)
            t->ESMC_FractionSetw(jdays * secondsPerDay);

            break;
        }

        // convert No Leap Date => Time
        case ESMC_CAL_NOLEAP:
        {
            // Validate inputs. TODO: determine lowpoint year, month & day
            if (mm < 1 || mm > 12 || dd < 1) {
              char logMsg[ESMF_MAXSTR];
              sprintf(logMsg, "; NoLeap mm=%d (1-12), dd=%d (>=1).",
                      mm, dd);
              ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_OUTOFRANGE,
                                                    logMsg, &rc);
              return(rc);
            }
            // check day of the month
            if (dd > daysPerMonth[mm-1]) {
              char logMsg[ESMF_MAXSTR];
              sprintf(logMsg, "; NoLeap: for month %d, dd=%d > %d days "
                              "in the month.", mm, dd, daysPerMonth[mm-1]);
              ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_OUTOFRANGE,
                                                    logMsg, &rc);
              return(rc);
            }
            // TODO: upper bounds date range check dependent on machine
            //  word size

            t->ESMC_FractionSetw(yy * secondsPerYear);
            for(int month=0; month < mm-1; month++) {
              t->ESMC_FractionSetw(t->ESMC_FractionGetw() +
                                   daysPerMonth[month] * secondsPerDay);
            }
            t->ESMC_FractionSetw(t->ESMC_FractionGetw() +
                                 (dd-1) * secondsPerDay);
                      // TODO: ? (dd-1) * secondsPerDay + 148600915200LL);
                                          // ^ adjust to match Julian time zero
                                          // = (1/1/0000) - (11/24/-4713)
            break;
        }
        // convert 360 Day Date => Time
        case ESMC_CAL_360DAY:
        {
            // Validate inputs. TODO: determine lowpoint year, month & day
            if (mm < 1 || mm > 12 || dd < 1 || dd > 30) {
              char logMsg[ESMF_MAXSTR];
              sprintf(logMsg, "; 360 Day: mm=%d (1-12), dd=%d (1-30).", mm, dd);
              ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_OUTOFRANGE,
                                                    logMsg, &rc);
              return(rc);
            }
            // TODO: upper bounds date range check dependent on machine
            //  word size

            t->ESMC_FractionSetw(yy * secondsPerYear
                  + (mm-1) * 30 * secondsPerDay   // each month has 30 days
                  + (dd-1) * secondsPerDay);
       // TODO: ? + (dd-1) * secondsPerDay + 146565244800LL);
                                          // ^ adjust to match Julian time zero
                                          // = (1/1/0000) - (11/24/-4713)
            break;
        }
        // convert Julian Date => Time
        case ESMC_CAL_JULIANDAY:
        {
            // TODO: lower/upper bounds date range check dependent on machine
            //  word size

            // convert Julian days to basetime seconds (>= 64 bit)
            t->ESMC_FractionSetw(d * secondsPerDay);

            break;
        }
        case ESMC_CAL_CUSTOM:
            // TODO:
            break;
        case ESMC_CAL_NOCALENDAR:
            // need real calendar type
            ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_WRONG,
                                                  ", need real calendar.", &rc);
            return(rc);
            break;
        default:
            // unknown calendar type
            char logMsg[ESMF_MAXSTR];
            sprintf(logMsg, "; unknown calendar type %d.", this->calendarType);
            ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, logMsg,
                                                  &rc);
            return(rc);
            break;
    }

    return(rc);

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
      ESMC_BaseTime *t,                                                // in/out
      ESMC_I4 *yy, ESMC_I8 *yy_i8, int *mm, int *dd,         // out
      ESMC_I4 *d, ESMC_I8 *d_i8, ESMC_R8 *d_r8) const { // out
//
// !DESCRIPTION:
//     Converts a core {\tt ESMC\_BaseTime} representation to a
//     calendar-specific date. Conversions based on UTC: time zone offset
//     done by client
//
//     The Gregorian <-> Julian day conversion algorithm is from
//     Henry F. Fliegel and Thomas C. Van Flandern, in Communications of
//     the ACM, CACM, volume 11, number 10, October 1968, p. 657.
//     Julian day refers to the number of days since a reference day.
//     For the algorithm used, this reference day is November 24, -4713
//     in the Proleptic Gregorian calendar, which is equivalent to
//     January 1, -4712 in the Proleptic Julian calendar.
// 
//     When converting from a Julian day to a Gregorian date (implemented
//     in this method), this algorithm is valid from 3/1/-4900 Gregorian
//     forward.  When converting from a Gregorian date to a Julian day
//     (implemented in method {\tt ESMC\_CalendarConvertToTime()}), the
//     algorithm is valid from 3/1/-4800 forward.  In both cases, the
//     algorithm correctly takes into account leap years, those that are
//     divisable by 4 and not 100, or those divisible by 400.
//
//     The Fliegel algorithm implements the Gregorian calendar as continuously
//     proleptic from October 15, 1582 backward to March 1, -4800/-4900.
//     Hence the algorithm does not take into account the Gregorian Reformation
//     (when the Gregorian calendar officially began) where 10 days were
//     eliminated from the calendar in October 1582.  Thursday, October 4, 1582
//     was officially the last day of the Julian calendar; the following day,
//     Friday, was decreed to be October 15, 1582, the first day of the
//     Gregorian calendar.
//
//     The Julian <-> Julian day conversion algorithm is from D.A. Hatcher,
//     Q.JlR. astr. Soc. (1984) 25, 53-55.  It is valid from 3/1/-4712 forward.
//
//     See also:  http://www.hermetic.ch/cal\_stud/jdn.htm\#comp
//                http://www.numerical-recipes.com/julian.html
//
//EOP
// !REQUIREMENTS:   TMG 2.4.5, 2.5.6

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_CalendarConvertToDate()"

// TODO: validate core values before conversion as they can go out-of-range
//       during arithmetic operations

    int rc = ESMF_SUCCESS;

    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", &rc);
      return(rc);
    }

    switch (this->calendarType)
    {
        // convert Time => Gregorian Date
        case ESMC_CAL_GREGORIAN:
        {
            // Convert basetime portion of Time into date
            // Julian day (D) => Gregorian date (yy, mm, dd)
            // The calculation below fails for jday > 106,751,991,167,300
            //    (4*templ = 2^63)

            // convert basetime seconds to Julian days
            ESMC_I8 jdays = t->ESMC_FractionGetw() / secondsPerDay;

            // Validate input
            // From Fliegel algorithm:  lower limit of 3/1/-4900 Gregorian
            //    equals -68569 Julian days
            if (jdays < -68569) {
              char logMsg[ESMF_MAXSTR];
              sprintf(logMsg, "; Julian Day: d=%lld < -68569, out-of-range "
                              "for valid conversion to Gregorian date.", jdays);
              ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_OUTOFRANGE,
                                                    logMsg, &rc);
              return(rc);
            }

            if (d != ESMC_NULL_POINTER) {
              if (jdays > INT_MIN && jdays <= INT_MAX) {
                *d = (ESMC_I4) jdays;
                // adjust for negative time (reverse integer division)
                if (t->ESMC_FractionGetw() % secondsPerDay < 0) (*d)--;
              } else {
                // too large to fit in given int
                char logMsg[ESMF_MAXSTR];
                sprintf(logMsg, "; Julian days value %lld won't fit in given "
                                "d integer.", jdays);
                ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
                                                      logMsg, &rc);
              }
            }
            if (d_i8 != ESMC_NULL_POINTER) {
              *d_i8 = jdays;
              // adjust for negative time (reverse integer division)
              if (t->ESMC_FractionGetw() % secondsPerDay < 0) (*d_i8)--;
            }
            if (d_r8 != ESMC_NULL_POINTER) {
              *d_r8 = (ESMC_R8) t->ESMC_FractionGetw() /
                                     (ESMC_R8) secondsPerDay;
            }

            // convert Julian days to Gregorian date
            // Julian days (jdays) => Gregorian date (yy, mm, dd)

            int day, month; 
            ESMC_I8 year;
            if (dd != ESMC_NULL_POINTER || mm    != ESMC_NULL_POINTER ||
                yy != ESMC_NULL_POINTER || yy_i8 != ESMC_NULL_POINTER) {

              ESMC_I8 templ = jdays + 68569;
              ESMC_I8 tempn = (4 * templ) / 146097;
                           templ = templ - (146097 * tempn + 3) / 4;
              ESMC_I8 tempi = (4000 * (templ + 1)) / 1461001;
                           templ = templ - (1461 * tempi) / 4 + 31;
              ESMC_I8 tempj = (80 * templ) / 2447;

              day   = templ - (2447 * tempj) / 80;
              templ = tempj / 11;
              month = tempj + 2 - (12 * templ);
              year  = 100 * (tempn - 49) + tempi + templ;

              if (dd != ESMC_NULL_POINTER) {
                *dd = day;
              }
              if (mm != ESMC_NULL_POINTER) {
                *mm = month;
              }
              if (yy != ESMC_NULL_POINTER) {
                if (year >= INT_MIN && year <= INT_MAX) {
                  *yy = (ESMC_I4) year;  // >= 32-bit
                } else {
                  // too large to fit in given int
                  char logMsg[ESMF_MAXSTR];
                  sprintf(logMsg, "; year value %lld won't fit in given "
                                  "yy integer.", year);
                  ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
                                                        logMsg, &rc);
                }
              }
              if (yy_i8 != ESMC_NULL_POINTER) {
                *yy_i8 = year;    // >= 64-bit
              }
            }

            // TODO: share this code with ESMC_CAL_JULIAN below
            // remove smallest requested date unit from given time for
            // subsequent getting of remaining hours, minutes, seconds units
            if (dd   != ESMC_NULL_POINTER || d    != ESMC_NULL_POINTER ||
                d_i8 != ESMC_NULL_POINTER || d_r8 != ESMC_NULL_POINTER) {
              t->ESMC_FractionSetw(t->ESMC_FractionGetw() % secondsPerDay);
            } else if (mm != ESMC_NULL_POINTER) {
              t->ESMC_FractionSetw(t->ESMC_FractionGetw() % secondsPerDay
                                   + ((day-1) * secondsPerDay));
            } else if (yy != ESMC_NULL_POINTER || yy_i8 != ESMC_NULL_POINTER) {
              // TODO: use native C++ Set(), not F90 entry point
              ESMC_Calendar *cal = (ESMC_Calendar *) this;
              ESMC_Time begnningOfYear; 
              begnningOfYear.ESMC_TimeSet((ESMC_I4 *)ESMC_NULL_POINTER,
                                           &year, ESMC_NULL_POINTER,
                                           ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                           ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                           ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                           ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                           ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                           ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                           ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                           ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                           ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                           ESMC_NULL_POINTER, &cal);
              ESMC_TimeInterval secondsOfTheYear;
              secondsOfTheYear = *t - begnningOfYear;
              ESMC_I8 seconds;
              secondsOfTheYear.ESMC_TimeIntervalGet(ESMC_NULL_POINTER,
                                                    ESMC_NULL_POINTER,
                                                    ESMC_NULL_POINTER,
                                                    ESMC_NULL_POINTER,
                                                    ESMC_NULL_POINTER,
                                                    ESMC_NULL_POINTER,
                                                    ESMC_NULL_POINTER,
                                                    ESMC_NULL_POINTER,
                                                    ESMC_NULL_POINTER,
                                                    &seconds);
              t->ESMC_FractionSetw(seconds);
            }

            break;
        }
        // convert Time => Julian Date
        case ESMC_CAL_JULIAN:
        {
            // convert basetime seconds to Julian days
            ESMC_I8 jdays = t->ESMC_FractionGetw() / secondsPerDay;

            // Validate input
            // From Hatcher algorithm:  lower limit of 2/29/-4712 Julian
            //    equals 59 Julian days
            if (jdays < 59) {
              char logMsg[ESMF_MAXSTR];
              sprintf(logMsg, "; Julian Day: d=%lld < 59, out-of-range "
                              "for valid conversion to Julian date.", jdays);
              ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_OUTOFRANGE,
                                                    logMsg, &rc);
              return(rc);
            }

            if (d != ESMC_NULL_POINTER) {
              if (jdays > INT_MIN && jdays <= INT_MAX) {
                *d = (ESMC_I4) jdays;
                // adjust for negative time (reverse integer division)
                if (t->ESMC_FractionGetw() % secondsPerDay < 0) (*d)--;
              } else {
                // too large to fit in given int
                char logMsg[ESMF_MAXSTR];
                sprintf(logMsg, "; Julian days value %lld won't fit in given "
                                "d integer.", jdays);
                ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
                                                      logMsg, &rc);
              }
            }
            if (d_i8 != ESMC_NULL_POINTER) {
              *d_i8 = jdays;
              // adjust for negative time (reverse integer division)
              if (t->ESMC_FractionGetw() % secondsPerDay < 0) (*d_i8)--;
            }
            if (d_r8 != ESMC_NULL_POINTER) {
              *d_r8 = (ESMC_R8) t->ESMC_FractionGetw() /
                                     (ESMC_R8) secondsPerDay;
            }

            // Julian day (D) => Julian date (yy, mm, dd)

            int day, month; 
            ESMC_I8 year;
            if (dd != ESMC_NULL_POINTER || mm    != ESMC_NULL_POINTER ||
                yy != ESMC_NULL_POINTER || yy_i8 != ESMC_NULL_POINTER) {

              // Algorithm is from D.A. Hatcher, Q. Jl R. astr. Soc. (1984),
              //  Volume 25, No. 1, pp. 53-55.
                  year   = (ESMC_I8)(jdays / 365.25) - 4712;
              int dprime = (int)fmod((jdays - 59.25), 365.25);
                  month  = ((int)((dprime + 0.5) / 30.6) + 2) % 12 + 1;
                  day    =  (int)(fmod((dprime + 0.5), 30.6)) + 1;

              if (mm != ESMC_NULL_POINTER) {
                *mm = month;
              }
              if (dd != ESMC_NULL_POINTER) {
                *dd = day;
              }
              if (yy != ESMC_NULL_POINTER) {
                if (year >= INT_MIN && year <= INT_MAX) {
                  *yy = (ESMC_I4) year;  // >= 32-bit
                } else {
                  // too large to fit in given int
                  char logMsg[ESMF_MAXSTR];
                  sprintf(logMsg, "; year value %lld won't fit in given "
                                  "yy integer.", year);
                  ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
                                                        logMsg, &rc);
                }
              }
              if (yy_i8 != ESMC_NULL_POINTER) {
                *yy_i8 = year;    // >= 64-bit
              }
            }

            // TODO: share this code with ESMC_CAL_GREGORIAN above
            // remove smallest requested date unit from given time for
            // subsequent getting of remaining hours, minutes, seconds units
            if (dd   != ESMC_NULL_POINTER || d    != ESMC_NULL_POINTER ||
                d_i8 != ESMC_NULL_POINTER || d_r8 != ESMC_NULL_POINTER) {
              t->ESMC_FractionSetw(t->ESMC_FractionGetw() % secondsPerDay);
            } else if (mm != ESMC_NULL_POINTER) {
              t->ESMC_FractionSetw(t->ESMC_FractionGetw() % secondsPerDay
                                   + ((day-1) * secondsPerDay));
            } else if (yy != ESMC_NULL_POINTER || yy_i8 != ESMC_NULL_POINTER) {
              // TODO: use native C++ Set(), not F90 entry point
              ESMC_Calendar *cal = (ESMC_Calendar *) this;
              ESMC_Time begnningOfYear; 
              begnningOfYear.ESMC_TimeSet((ESMC_I4 *)ESMC_NULL_POINTER,
                                           &year, ESMC_NULL_POINTER,
                                           ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                           ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                           ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                           ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                           ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                           ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                           ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                           ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                           ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                           ESMC_NULL_POINTER, &cal);
              ESMC_TimeInterval secondsOfTheYear;
              secondsOfTheYear = *t - begnningOfYear;
              ESMC_I8 seconds;
              secondsOfTheYear.ESMC_TimeIntervalGet(ESMC_NULL_POINTER,
                                                    ESMC_NULL_POINTER,
                                                    ESMC_NULL_POINTER,
                                                    ESMC_NULL_POINTER,
                                                    ESMC_NULL_POINTER,
                                                    ESMC_NULL_POINTER,
                                                    ESMC_NULL_POINTER,
                                                    ESMC_NULL_POINTER,
                                                    ESMC_NULL_POINTER,
                                                    &seconds);
              t->ESMC_FractionSetw(seconds);
            }
            break;
        }
        // convert Time => No Leap Date
        case ESMC_CAL_NOLEAP:
        {
            ESMC_I8 tmpS = t->ESMC_FractionGetw();
 // TODO: ? ESMC_I8 tmpS = t->ESMC_FractionGetw() - 148600915200LL;
                                     // ^ adjust to match Julian time zero
                                     // = (1/1/0000) - (11/24/-4713)

            if (yy != ESMC_NULL_POINTER) {
              ESMC_I8 year = tmpS / secondsPerYear;
              if (year > INT_MIN && year <= INT_MAX) {
                  *yy = (ESMC_I4) year;  // >= 32-bit
                  // adjust for negative time (reverse integer division)
                  if (tmpS % secondsPerYear < 0) (*yy)--;
              } else {
                  // too large to fit in given int
                  char logMsg[ESMF_MAXSTR];
                  sprintf(logMsg, "; year value %lld won't fit in given "
                                  "yy integer.", year);
                  ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
                                                        logMsg, &rc);
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
              ESMC_I8 day = tmpS / secondsPerDay;
              if (day > INT_MIN && day <= INT_MAX) {
                *d = (ESMC_I4) day;   // >= 32-bit
                // adjust for negative time (reverse integer division)
                if (tmpS % secondsPerDay < 0) (*d)--;
              } else {
                // too large to fit in given int
                char logMsg[ESMF_MAXSTR];
                sprintf(logMsg, "; Julian days value %lld won't fit in given "
                                "d integer.", day);
                ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
                                                      logMsg, &rc);
              }
            }
            if (d_i8 != ESMC_NULL_POINTER) {
              *d_i8 = tmpS / secondsPerDay;   // >= 64-bit
              // adjust for negative time (reverse integer division)
              if (tmpS % secondsPerDay < 0) (*d_i8)--;
            }
            if (d_r8 != ESMC_NULL_POINTER) {
              *d_r8 = (ESMC_R8) tmpS / (ESMC_R8) secondsPerDay;
            }

            // remove smallest requested date unit from given time for
            // subsequent getting of remaining hours, minutes, seconds units
            if (dd   != ESMC_NULL_POINTER || d    != ESMC_NULL_POINTER ||
                d_i8 != ESMC_NULL_POINTER || d_r8 != ESMC_NULL_POINTER) {
              t->ESMC_FractionSetw(t->ESMC_FractionGetw() % secondsPerDay);
            } else if (mm != ESMC_NULL_POINTER) {
              t->ESMC_FractionSetw(t->ESMC_FractionGetw() % secondsPerDay
                                   + ((day-1) * secondsPerDay));
            } else if (yy != ESMC_NULL_POINTER || yy_i8 != ESMC_NULL_POINTER) {
              t->ESMC_FractionSetw(t->ESMC_FractionGetw() % secondsPerYear);
            }

            break;
        }
        // convert Time => 360 Day Date
        case ESMC_CAL_360DAY:
        {
            ESMC_I8 tmpS = t->ESMC_FractionGetw();
 // TODO: ? ESMC_I8 tmpS = t->ESMC_FractionGetw() - 146565244800LL;
                                     // ^ adjust to match Julian time zero
                                     // = (1/1/0000) - (11/24/-4713)

            if (yy != ESMC_NULL_POINTER) {
              ESMC_I8 year = tmpS / secondsPerYear;
              if (year > INT_MIN && year <= INT_MAX) {
                *yy = (ESMC_I4) year;
                // adjust for negative time (reverse integer division)
                if (tmpS % secondsPerYear < 0) (*yy)--;
              } else {
                // too large to fit in given int
                char logMsg[ESMF_MAXSTR];
                sprintf(logMsg, "; year value %lld won't fit in given "
                                "yy integer.", year);
                ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
                                                      logMsg, &rc);
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
              ESMC_I8 day = tmpS / secondsPerDay;
              if (day > INT_MIN && day <= INT_MAX) {
                *d = (ESMC_I4) day;   // >= 32-bit
                // adjust for negative time (reverse integer division)
                if (tmpS % secondsPerDay < 0) (*d)--;
              } else {
                // too large to fit in given int
                char logMsg[ESMF_MAXSTR];
                sprintf(logMsg, "; Julian days value %lld won't fit in given "
                                "d integer.", day);
                ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
                                                      logMsg, &rc);
              }
            }
            if (d_i8 != ESMC_NULL_POINTER) {
              *d_i8 = tmpS / secondsPerDay;   // >= 64-bit
              // adjust for negative time (reverse integer division)
              if (tmpS % secondsPerDay < 0) (*d_i8)--;
            }
            if (d_r8 != ESMC_NULL_POINTER) {
              *d_r8 = (ESMC_R8) tmpS / (ESMC_R8) secondsPerDay;
            }

            // remove smallest requested date unit from given time for
            // subsequent getting of remaining hours, minutes, seconds units
            if (dd   != ESMC_NULL_POINTER || d    != ESMC_NULL_POINTER ||
                d_i8 != ESMC_NULL_POINTER || d_r8 != ESMC_NULL_POINTER) {
              t->ESMC_FractionSetw(t->ESMC_FractionGetw() % secondsPerDay);
            } else if (mm != ESMC_NULL_POINTER) {
              t->ESMC_FractionSetw(t->ESMC_FractionGetw() % secondsPerDay
                                   + (((dayOfYear-1) % 30) * secondsPerDay));
            } else if (yy != ESMC_NULL_POINTER || yy_i8 != ESMC_NULL_POINTER) {
              t->ESMC_FractionSetw(t->ESMC_FractionGetw() % secondsPerYear);
            }

            break;
        }
        // convert Time => Julian Date
        case ESMC_CAL_JULIANDAY:
        {
            // convert basetime seconds to Julian days
            if (d != ESMC_NULL_POINTER) {
              ESMC_I8 day = t->ESMC_FractionGetw() / secondsPerDay;
              if (day > INT_MIN && day <= INT_MAX) {
                *d = (ESMC_I4) day;    // >= 32-bit
                // adjust for negative time (reverse integer division)
                if (t->ESMC_FractionGetw() % secondsPerDay < 0) (*d)--;
              } else {
                // too large to fit in given int
                char logMsg[ESMF_MAXSTR];
                sprintf(logMsg, "; Julian days value %lld won't fit in given "
                                "d integer.", day);
                ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_SIZE,
                                                      logMsg, &rc);
              }
            }
            if (d_i8 != ESMC_NULL_POINTER) {
              *d_i8 = t->ESMC_FractionGetw() / secondsPerDay;  // >= 64-bit
                // adjust for negative time (reverse integer division)
                if (t->ESMC_FractionGetw() % secondsPerDay < 0) (*d_i8)--;
            }
            if (d_r8 != ESMC_NULL_POINTER) {
              *d_r8 = (ESMC_R8) t->ESMC_FractionGetw() /
                                     (ESMC_R8) secondsPerDay;
            }

            // if days specified, remove them from given time for
            // subsequent getting of remaining hours, minutes, seconds units
            if (d != ESMC_NULL_POINTER || d_i8 != ESMC_NULL_POINTER ||
                d_r8 != ESMC_NULL_POINTER) {
              t->ESMC_FractionSetw(t->ESMC_FractionGetw() % secondsPerDay);
            }

            break;
        }
        case ESMC_CAL_CUSTOM:
            // TODO:
            break;
        case ESMC_CAL_NOCALENDAR:
            // need real calendar type
            ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_WRONG,
                                                  ", need real calendar.", &rc);
            break;
        default:
            // unknown calendar type
            char logMsg[ESMF_MAXSTR];
            sprintf(logMsg, "; unknown calendar type %d.", this->calendarType);
            ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_VALUE, logMsg,
                                                  &rc);
    }

    return(rc);

}  // end ESMC_CalendarConvertToDate

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CalendarIncrement - increment a Time by a TimeInterval
//
// !INTERFACE:
      ESMC_Time ESMC_Calendar::ESMC_CalendarIncrement(
//
// !RETURN VALUE:
//    ESMC_Time sum
//
// !ARGUMENTS:
      const ESMC_Time *time,                            // in
      const ESMC_TimeInterval &timeinterval) const {    // in

//
// !DESCRIPTION:
//     Increments a given {\tt ESMC\_Time} by the given
//     {\tt ESMC\_TimeInterval}, taking into account calendar intervals of
//     years, months, and or days as defined on the given time's calendar.
//
//EOP
// !REQUIREMENTS:   TMG 2.4.5

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_CalendarIncrement()"

// TODO: share common code with Decrement method
// TODO: check for overflow/underflow, return 0 with LogErr message

    int rc = ESMF_SUCCESS;

    ESMC_Time zero;

    if (this == ESMC_NULL_POINTER || time == ESMC_NULL_POINTER) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer or time argument is NULL.", ESMC_NULL_POINTER);
      return(zero);
    }

    // intialize result to given time to prepare for the case of
    //   only a non-calendar (h,m,s) increment
    //    (copies calendar & timezone properties)
    ESMC_Time sum = *time;

    // prepare for increment with any non-calendar units (h,m,s)
    ESMC_TimeInterval nonCalTi = timeinterval;

    switch (calendarType)
    {
        case ESMC_CAL_GREGORIAN:
        case ESMC_CAL_JULIAN:
        case ESMC_CAL_NOLEAP:
        case ESMC_CAL_360DAY:
        {
            ESMC_I8 yy_i8;
            int mm, dd, timeZone;
            ESMC_I4 h, m, s;
            ESMC_Calendar *cal;

            // TODO:  This algorithm operates on yy, mm, dd units the way
            //    a person would (easier to comprehend).  But could
            //    also do by determining the absolute value of the relative
            //    interval (convert to days, then seconds) and then adding it
            //    to time.  Could be faster because it would eliminate the 
            //    arithmetic-intensive steps of converting from basetime
            //    to yy,mm,dd and back again (at least for Gregorian's
            //    Fliegel algorithm)

            // TODO: fractional seconds, fractional calendar interval units

            // do calendar increment only if non-zero calendar interval units
            //    years or months are specified
            if (timeinterval.yy != 0 || timeinterval.mm != 0) {

                // get calendar units from given time, while saving time-of-day
                //   units and calendar & timezone properties
                rc = time->ESMC_TimeGet(ESMC_NULL_POINTER, &yy_i8, &mm, &dd,
                                        ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                        &h, &m, &s,
                                        ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                        ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                        ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                        ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                        ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                        ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                        ESMC_NULL_POINTER, &cal,
                                        ESMC_NULL_POINTER, &timeZone);
                                   // TODO: use native C++ interface when
                                   //   ready
                if (ESMC_LogDefault.ESMC_LogMsgFoundError(rc, 
                            ESMF_ERR_PASSTHRU, ESMC_NULL_POINTER)) return(sum);
            
                // do the calendar increment!
                mm += timeinterval.mm % monthsPerYear;  // months increment
                if (mm > monthsPerYear) {  // check for years carryover
                  mm -= monthsPerYear;
                  yy_i8++;
                } else if (mm < 1) {  // check for years carryunder (borrow)
                  mm += monthsPerYear;
                  yy_i8--;
                }
                yy_i8 += timeinterval.mm / monthsPerYear; 
                                               // years part of months increment
                yy_i8 += timeinterval.yy;      // years increment

                // clip day-of-the-month if necessary
                int daysInMonth = daysPerMonth[mm-1];
                if (mm == 2 && ESMC_CalendarIsLeapYear(yy_i8)) daysInMonth++;
                                                               // Feb. 29 days
                if (dd > daysInMonth) dd = daysInMonth;

                // convert resulting calendar sum back to base time, while also
                // restoring time-of-day units and properties from given time
                rc = sum.ESMC_TimeSet(ESMC_NULL_POINTER, &yy_i8, &mm, &dd,
                                      ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                      &h, &m, &s,
                                      ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                      ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                      ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                      ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                      ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                      ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                      ESMC_NULL_POINTER, &cal,
                                      ESMC_NULL_POINTER, &timeZone);
                                 // TODO: use native C++ interface when
                                 //   ready
                if (ESMC_LogDefault.ESMC_LogMsgFoundError(rc, 
                            ESMF_ERR_PASSTHRU, ESMC_NULL_POINTER)) return(sum);
            }
            break;
        }
        default:
            break;
    }

    // convert any relative days increment to absolute time based
    //   on this calendar and add to non-calendar units increment
    //   (applies to all calendars since secondsPerDay is defined for all)
    if (timeinterval.d != 0) {
        ESMC_TimeInterval daysTi(timeinterval.d * secondsPerDay);
        nonCalTi.ESMC_BaseTime::operator+=(daysTi);
    }

    // perform the remaining increment with the non-calendar and
    //   any relative days parts
    sum.ESMC_BaseTime::operator+=(nonCalTi);

    return(sum);

}  // end ESMC_CalendarIncrement

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CalendarDecrement - decrement a Time by a TimeInterval
//
// !INTERFACE:
      ESMC_Time ESMC_Calendar::ESMC_CalendarDecrement(
//
// !RETURN VALUE:
//    ESMC_Time diff
//
// !ARGUMENTS:
      const ESMC_Time *time,                            // in
      const ESMC_TimeInterval &timeinterval) const {    // in

//
// !DESCRIPTION:
//     Decrements a given {\tt ESMC\_Time} by the given
//     {\tt ESMC\_TimeInterval}, taking into account calendar intervals of
//     years, months, and or days as defined on the given time's calendar.
//
//EOP
// !REQUIREMENTS:   TMG 2.4.5

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_CalendarDecrement()"

    int rc = ESMF_SUCCESS;

    ESMC_Time zero;

    if (this == ESMC_NULL_POINTER || time == ESMC_NULL_POINTER) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer or time argument is NULL.", ESMC_NULL_POINTER);
      return(zero);
    }

// TODO: share common code with Increment method
// TODO: check for overflow/underflow, return 0 with LogErr message

    // intialize result to given time to prepare for the case of
    //   only a non-calendar (h,m,s) decrement
    //    (copies calendar & timezone properties)
    ESMC_Time diff = *time;

    // prepare for decrement with any non-calendar units (h,m,s)
    ESMC_TimeInterval nonCalTi = timeinterval;

    switch (calendarType)
    {
        case ESMC_CAL_GREGORIAN:
        case ESMC_CAL_JULIAN:
        case ESMC_CAL_NOLEAP:
        case ESMC_CAL_360DAY:
        {
            ESMC_I8 yy_i8;
            int mm, dd, timeZone;
            ESMC_I4 h, m, s;
            ESMC_Calendar *cal;

            // TODO:  This algorithm operates on yy, mm, dd units the way
            //    a person would (easier to comprehend).  But could
            //    also do by determining the absolute value of the relative
            //    interval (convert to days, then seconds) and then subtracting
            //    it from time.  Could be faster because it would eliminate the 
            //    arithmetic-intensive steps of converting from basetime
            //    to yy,mm,dd and back again (at least for Gregorian's
            //    Fliegel algorithm)

            // TODO: fractional seconds, fractional calendar interval units

            // do calendar decrement only if non-zero calendar interval units
            //    years or months are specified
            if (timeinterval.yy != 0 || timeinterval.mm != 0) {

                // get calendar units from given time, while saving time-of-day
                //   units and calendar & timezone properties
                rc = time->ESMC_TimeGet(ESMC_NULL_POINTER, &yy_i8, &mm, &dd,
                                        ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                        &h, &m, &s,
                                        ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                        ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                        ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                        ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                        ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                        ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                        ESMC_NULL_POINTER, &cal,
                                        ESMC_NULL_POINTER, &timeZone);
                                   // TODO: use native C++ interface when
                                   //   ready
                if (ESMC_LogDefault.ESMC_LogMsgFoundError(rc, 
                            ESMF_ERR_PASSTHRU, ESMC_NULL_POINTER)) return(diff);

                // do the calendar decrement!
                mm -= timeinterval.mm % monthsPerYear;  // months decrement
                if (mm < 1) {  // check for year carryunder (borrow)
                  mm += monthsPerYear;
                  yy_i8--;
                } else if (mm > monthsPerYear) {  // check for year carryover
                  mm -= monthsPerYear;
                  yy_i8++;
                }
                yy_i8 -= timeinterval.mm / monthsPerYear; 
                                               // years part of months decrement
                yy_i8 -= timeinterval.yy;      // years decrement

                // clip day-of-the-month if necessary
                int daysInMonth = daysPerMonth[mm-1];
                if (mm == 2 && ESMC_CalendarIsLeapYear(yy_i8)) daysInMonth++;
                                                               // Feb. 29 days
                if (dd > daysInMonth) dd = daysInMonth;

                // convert resulting calendar diff back to base time, while also
                // restoring time-of-day units and properties from given time
                rc = diff.ESMC_TimeSet(ESMC_NULL_POINTER, &yy_i8, &mm, &dd,
                                       ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                       &h, &m, &s,
                                       ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                       ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                       ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                       ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                       ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                       ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                       ESMC_NULL_POINTER, &cal,
                                       ESMC_NULL_POINTER, &timeZone);
                                  // TODO: use native C++ interface when
                                  //   ready
                if (ESMC_LogDefault.ESMC_LogMsgFoundError(rc, 
                            ESMF_ERR_PASSTHRU, ESMC_NULL_POINTER)) return(diff);
            }
            break;
        }
        default:
            break;
    }

    // convert any relative days increment to absolute time based
    //   on this calendar and add to non-calendar units increment
    //   (applies to all calendars since secondsPerDay is defined for all)
    if (timeinterval.d != 0) {
        ESMC_TimeInterval daysTi(timeinterval.d * secondsPerDay);
        nonCalTi.ESMC_BaseTime::operator+=(daysTi);
    }

    // perform the remaining decrement with the non-calendar and
    //   any relative days parts
    diff.ESMC_BaseTime::operator-=(nonCalTi);

    return(diff);

}  // end ESMC_CalendarDecrement

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CalendarIsLeapYear - Determine if given year is a leap year
//
// !INTERFACE:
      bool ESMC_Calendar::ESMC_CalendarIsLeapYear(
//
// !RETURN VALUE:
//    bool is leap year or not
//
// !ARGUMENTS:
      ESMC_I8 yy_i8,      // in  - year
      int  *rc) const {        // out - error return code
//
// !DESCRIPTION:
//     Determines whether given year is a leap year within *this* calendar.
//
//EOP
// !REQUIREMENTS:   TMG x.x.x

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_CalendarIsLeapYear()"

    if (rc != ESMC_NULL_POINTER) *rc = ESMF_SUCCESS;

    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", rc);
      return(false);
    }

    switch (calendarType)
    {
      case ESMC_CAL_GREGORIAN:
        // leap year is divisable by 400 or divisable by 4 and not 100.
        return( (yy_i8 % 400 == 0) || ((yy_i8 % 4 == 0)&&(yy_i8 % 100 != 0)) );
        break;
      
      case ESMC_CAL_JULIAN:
        // leap year is divisable by 4.
        return(yy_i8 % 4 == 0);
        break;

      default:
        // all other calendars don't have leap years.  TODO: Custom Calendar ?
        return(false);
        break;
    }

}  // end ESMC_CalendarIsLeapYear

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Calendar(==) - Calendar equality comparison
//
// !INTERFACE:
      bool ESMC_Calendar::operator==(
//
// !RETURN VALUE:
//    bool result
//
// !ARGUMENTS:
      const ESMC_Calendar &calendar) const {   // in - ESMC_Calendar to compare
//
// !DESCRIPTION:
//      Compare for equality the current object's (this) {\tt ESMC\_Calendar}
//      with given {\tt ESMC\_Calendar}, return result.
//
//EOP
// !REQUIREMENTS:

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_Calendar::operator==(calendar)"

    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", ESMC_NULL_POINTER);
      return(false);
    }

    return(calendarType == calendar.calendarType);

}  // end ESMC_Calendar::operator==

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Calendar(==) - Calendar equality comparison
//
// !INTERFACE:
      bool ESMC_Calendar::operator==(
//
// !RETURN VALUE:
//    bool result
//
// !ARGUMENTS:
      const ESMC_CalendarType &calendarType) const {   // in - ESMC_CalendarType to compare
//
// !DESCRIPTION:
//      Compare for equality the current object's (this) {\tt ESMC\_Calendar}
//      type with given {\tt ESMC\_CalendarType}, return result.
//
//EOP
// !REQUIREMENTS:

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_Calendar::operator==(calendarType)"

    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", ESMC_NULL_POINTER);
      return(false);
    }

    return(this->calendarType == calendarType);

}  // end ESMC_Calendar::operator==

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Calendar(!=) - Calendar inequality comparison
//
// !INTERFACE:
      bool ESMC_Calendar::operator!=(
//
// !RETURN VALUE:
//    bool result
//
// !ARGUMENTS:
      const ESMC_Calendar &calendar) const {   // in - ESMC_Calendar to compare
//
// !DESCRIPTION:
//      Compare for inequality the current object's (this) {\tt ESMC\_Calendar}
//      with given {\tt ESMC\_Calendar}, return result.
//
//EOP
// !REQUIREMENTS:

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_Calendar::operator!=(calendar)"

    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", ESMC_NULL_POINTER);
      return(false);
    }

    return(calendarType != calendar.calendarType);

}  // end ESMC_Calendar::operator!=

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Calendar(!=) - Calendar inequality comparison
//
// !INTERFACE:
      bool ESMC_Calendar::operator!=(
//
// !RETURN VALUE:
//    bool result
//
// !ARGUMENTS:
      const ESMC_CalendarType &calendarType) const {   // in - ESMC_CalendarType to compare
//
// !DESCRIPTION:
//      Compare for inequality the current object's (this) {\tt ESMC\_Calendar}
//      type with given {\tt ESMC\_CalendarType}, return result.
//
//EOP
// !REQUIREMENTS:

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_Calendar::operator!=(calendarType)"

    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", ESMC_NULL_POINTER);
      return(false);
    }

    return(this->calendarType != calendarType);

}  // end ESMC_Calendar::operator!=

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

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_CalendarReadRestart()"

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

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_CalendarWriteRestart()"

    int rc = ESMF_SUCCESS;

    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", &rc);
      return(rc);
    }

    // TODO:

    return(rc);

}  // end ESMC_CalendarWriteRestart

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CalendarValidate - validate Calendar state
//
// !INTERFACE:
      int ESMC_Calendar::ESMC_CalendarValidate(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {   // in - validate options
//    none
//
// !DESCRIPTION:
//      validate {\tt EMSC\_Calendar} state
//
//EOP
// !REQUIREMENTS: 

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_CalendarValidate()"

    int rc = ESMF_SUCCESS;

    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", &rc);
      return(rc);
    }

    if (this->calendarType < 1 ||
        this->calendarType > CALENDAR_TYPE_COUNT) {
      char logMsg[ESMF_MAXSTR];
      sprintf(logMsg, "; calendarType %d (1-%d).", this->calendarType,
              CALENDAR_TYPE_COUNT);
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_ARG_OUTOFRANGE,
                                            logMsg, &rc);
    }

    if (this->monthsPerYear > MONTHS_PER_YEAR || this->monthsPerYear < 0) {
      char logMsg[ESMF_MAXSTR];
      sprintf(logMsg, "; monthsPerYear %d negative or > MONTHS_PER_YEAR %d.",
                      this->daysPerYear.d, MONTHS_PER_YEAR);
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_BAD, logMsg, &rc);
    }

    int daysPerYear = 0;
    for(int i=0; i<this->monthsPerYear; i++)
    {
      daysPerYear += this->daysPerMonth[i];
      if (this->daysPerMonth[i] < 0) {
        char logMsg[ESMF_MAXSTR];
        sprintf(logMsg, "; daysPerMonth[%d] %d < 0", i, this->daysPerMonth[i]);
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_BAD, logMsg, &rc);
      }
    }

    if (daysPerYear != this->daysPerYear.d) {
      char logMsg[ESMF_MAXSTR];
      sprintf(logMsg, "; daysPerYear %d != sum of daysPerMonth[].",
                      this->daysPerYear.d);
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_BAD, logMsg, &rc);
    }

    if (this->secondsPerDay < 0) {
      char logMsg[ESMF_MAXSTR];
      sprintf(logMsg, "; secondsPerDay %d < 0", this->secondsPerDay);
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_BAD, logMsg, &rc);
    }

    if (this->daysPerYear.dN < 0) {
      char logMsg[ESMF_MAXSTR];
      sprintf(logMsg, "; daysPerYear.dN %d < 0", this->daysPerYear.dN);
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_BAD, logMsg, &rc);
    }
    if (this->daysPerYear.dD <= 0) {
        char logMsg[ESMF_MAXSTR];
        sprintf(logMsg, "; daysPerYear.dD %d <= 0", this->daysPerYear.dD);
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_OBJ_BAD, logMsg, &rc);
    }

    return(rc);

}  // end ESMC_CalendarValidate

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CalendarPrint - print Calendar state
//
// !INTERFACE:
      int ESMC_Calendar::ESMC_CalendarPrint(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options,            // in - print options
      const ESMC_Time *time) const {  // in - optional time context in which
                                      //      to print
                                      //      (e.g. Leap Year => Feb. 29 days)
//
// !DESCRIPTION:
//      print {\tt EMSC\_Calendar} state for testing/debugging
//
//EOP
// !REQUIREMENTS: 

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_CalendarPrint()"

    int rc = ESMF_SUCCESS; // return code 

    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", &rc);
      return(rc);
    }

    // determine leap year, if requested
    // TODO:  replace with leap year method for ESMC_Time (which calls
    //        ESMC_CalendarIsLeapYear() )
    bool isLeapYear = false;
    if (time != ESMC_NULL_POINTER) {
      ESMC_I8 yy_i8;
      rc = time->ESMC_TimeGet(ESMC_NULL_POINTER, &yy_i8);
                              // TODO: use native C++ interface when ready
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(rc,
          ESMF_ERR_PASSTHRU, ESMC_NULL_POINTER)) return(rc);
      // TODO:  ensure *this* calendar and time's calendar are the same ?
      isLeapYear = ESMC_CalendarIsLeapYear(yy_i8);
    }

    printf("Calendar -------------------------------\n");

    // print out individually selected components
    // TODO: enable multiple simultaneous options (token parsing)
    //       (currently mutually exclusive)
    if (options != ESMC_NULL_POINTER) {

      // make options case insensitive
      // TODO: put this into function to share
      char opts[ESMF_MAXSTR];
      int i;
      for(i=0; i<strlen(options) && i<ESMF_MAXSTR-1; i++) {
        opts[i] = tolower(options[i]);
      }
      opts[i] = '\0';

      if (strncmp(opts, "name", 4) == 0) {
        printf("name = %s\n", name);
      }
      else if (strncmp(opts, "calendartype", 12) == 0) {
        // TODO:  make lookup table: int -> string
        printf("calendarType = %s\n", calendarTypeName[calendarType-1]);
      }
      else if (strncmp(opts, "dayspermonth", 12) == 0) {
        printf("daysPerMonth = ");
        for (int i=0; i<this->monthsPerYear; i++) {
          if (i == 1 && isLeapYear) {
            printf("%d ", daysPerMonth[i]+1);  // leap year
          } else {
            printf("%d ", daysPerMonth[i]);    // non leap year
          }
        }
      }
      else if (strncmp(opts, "monthsperyear", 13) == 0) {
        printf("monthsPerYear = %d\n", monthsPerYear);
      }
      else if (strncmp(opts, "secondsperday", 13) == 0) {
        printf("secondsPerDay = %d\n", secondsPerDay);
      }
      else if (strncmp(opts, "secondsperyear", 14) == 0) {
        printf("secondsPerYear = %d\n", secondsPerYear);
      }
      else if (strncmp(opts, "daysperyear", 11) == 0) {
        printf("daysPerYear = %d\n",   daysPerYear.d);
        printf("daysPerYeardN = %d\n", daysPerYear.dN);
        printf("daysPerYeardD = %d\n", daysPerYear.dD);
      }

    } else {
      // default:  print out all properties

      printf("name = %s\n", name);
      printf("calendarType = %s\n", calendarTypeName[calendarType-1]);

      printf("daysPerMonth = "); 
      for (int i=0; i<this->monthsPerYear; i++) {
          if (i == 1 && isLeapYear) {
            printf("%d ", daysPerMonth[i]+1);  // leap year
          } else {
            printf("%d ", daysPerMonth[i]);    // non leap year
          }
      }
      printf("\n");
      printf("monthsPerYear = %d\n",  monthsPerYear);
      printf("secondsPerDay = %d\n",  secondsPerDay);
      printf("secondsPerYear = %d\n", secondsPerYear);
      printf("daysPerYear = %d\n",    daysPerYear.d);
      printf("daysPerYeardN = %d\n",  daysPerYear.dN);
      printf("daysPerYeardD = %d\n",  daysPerYear.dD);
    }

    printf("end Calendar ---------------------------\n\n");
    
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
//      Initializes a {\tt ESMC\_Calendar} with defaults for either
//      C++ or F90, since {\tt ESMC\_Calendar} is a deep, dynamically
//      allocated class.
//
//EOP
// !REQUIREMENTS: 

    name[0] = '\0';
    id = ++count;  // TODO: inherit from ESMC_Base class
    // copy = false;  // TODO: see notes in constructors and destructor below

    calendarType   = ESMC_CAL_NOCALENDAR;
    // TODO: make daysPerMonth[] dynamically allocatable with monthsPerYear
    //       daysPerMonth = ESMC_NULL_POINTER;
    monthsPerYear  = MONTHS_PER_YEAR;
    for (int i=0; i<monthsPerYear; i++) daysPerMonth[i] = 0;
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
      const char       *name,            // in
      ESMC_CalendarType calendarType) {  // in
//
// !DESCRIPTION:
//      Initializes a {\tt ESMC\_TimeInstant} to be of a specific type via
//      {\tt ESMC\_CalendarSet}
//
//EOP
// !REQUIREMENTS: 

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_Calendar(built-in) constructor"

    int rc = ESMF_SUCCESS;

    ESMC_Calendar();  // invoke default constructor
    rc = ESMC_CalendarSet(strlen(name), name, calendarType);
    ESMC_LogDefault.ESMC_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU,
                                          ESMC_NULL_POINTER);

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
      const char   *name,              // in
      int          *daysPerMonth,      // in
      int           monthsPerYear,     // in
      ESMC_I4 *secondsPerDay,     // in
      ESMC_I4 *daysPerYear,       // in
      ESMC_I4 *daysPerYeardN,     // in
      ESMC_I4 *daysPerYeardD) {   // in
//
// !DESCRIPTION:
//      Initializes a {\tt ESMC\_Time} to be of a custom user-defined type
//      via {\tt ESMC\_CalendarSet}
//
//EOP
// !REQUIREMENTS: 

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_Calendar(custom) constructor"

    int rc = ESMF_SUCCESS;

    ESMC_Calendar();  // invoke default constructor
    rc = ESMC_CalendarSet(strlen(name), name, 
                          daysPerMonth, monthsPerYear, secondsPerDay, 
                          daysPerYear, daysPerYeardN, daysPerYeardD);
    ESMC_LogDefault.ESMC_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU,
                                          ESMC_NULL_POINTER);
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
    // copy = true;   // TODO: Unique copy ? (id = ++count) (review operator==
                      //       and operator!=)  Must do same in assignment
                      //       overloaded method and interface from F90.
                      //       Also, inherit from ESMC_Base class.

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

  // TODO: Decrement static count for one less object; but don't decrement
  //       for copies.  Must create and set a copy flag property to detect.
  //       Also must set copy flag in copy constructor and overloaded
  //       assignment method, and provide interface from F90.
  // if (!copy) count--;

  // TODO: make dynamically allocatable with monthsPerYear
  // delete[] daysPerMonth;

} // end ~ESMC_Calendar
