// $Id: ESMC_Calendar.C,v 1.65 2004/05/24 20:27:33 eschwab Exp $
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
//
 #define ESMF_FILENAME "ESMC_Calendar.C"

 // higher level, 3rd party or system includes
 #include <iostream.h>
 #include <limits.h>
 #include <string.h>
 #include <ctype.h>

 #include <ESMC_LogErr.h>
 #include <ESMF_LogMacros.inc>

 #include <ESMC_Time.h>
 #include <ESMC_TimeInterval.h>

 // associated class definition file
 #include <ESMC_Calendar.h>

//-------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_Calendar.C,v 1.65 2004/05/24 20:27:33 eschwab Exp $";
//-------------------------------------------------------------------------

// array of calendar type names
static const char *const calendarTypeName[CALENDAR_TYPE_COUNT] =
                                               { "Gregorian", "Julian Day", 
                                                 "No Leap", "360 Day", "Custom",
                                                 "No Calendar" };

// initialize static internal calendar pointer array
ESMC_Calendar *ESMC_Calendar::internalCalendar[CALENDAR_TYPE_COUNT] =
                                      { ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                        ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                        ESMC_NULL_POINTER, ESMC_NULL_POINTER };

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
    if (rc != ESMC_NULL_POINTER) *rc = ESMF_FAILURE;

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
      if ((*internalCal)->ESMC_CalendarValidate()) {
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
                                   calendarTypeName[calendarType-1],
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
            calendarTypeName[calType]);
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

    int rc; // return code 

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
          return(ESMF_FAILURE);
        }
      }
    }

    this->calendarType = calendarType;

    switch (calendarType)
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
            rc = ESMF_FAILURE;
            break;

        default:
            // unknown calendar type; restore original
            *this = saveCalendar; 
            rc = ESMF_FAILURE;
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
          return(ESMF_FAILURE);
        }
      }
    }

    this->calendarType = ESMC_CAL_CUSTOM;

    // TODO: replace MONTHS_PER_YEAR with dynamic daysPerMonth[monthsPerYear]
    if (monthsPerYear <= MONTHS_PER_YEAR) {
      this->monthsPerYear = monthsPerYear;
    } else {
      // error, restore previous state and return ESMF_FAILURE
      *this = saveCalendar;
      return(ESMF_FAILURE);
    }

    if (daysPerMonth != ESMC_NULL_POINTER) {
      for(int i=0; i<this->monthsPerYear; i++)
      { 
        this->daysPerMonth[i] = daysPerMonth[i];
      }
    }

    if (secondsPerDay != ESMC_NULL_POINTER) {
      this->secondsPerDay = *secondsPerDay;
    }
    if (daysPerYear != ESMC_NULL_POINTER) {
      this->daysPerYear.d = *daysPerYear;
    } else {
      this->daysPerYear.d = 0;
      for(int i=0; i<this->monthsPerYear; i++)
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
      int                nameLen,         // in
      int               *tempNameLen,     // out
      char              *tempName,        // out
      ESMC_CalendarType *calendarType,    // out
      int               *daysPerMonth,    // out
      int                sizeofDaysPerMonth, // in
      int               *monthsPerYear,   // out
      ESMF_KIND_I4      *secondsPerDay,   // out
      ESMF_KIND_I4      *secondsPerYear,  // out
      ESMF_KIND_I4      *daysPerYear,     // out
      ESMF_KIND_I4      *daysPerYeardN,   // out
      ESMF_KIND_I4      *daysPerYeardD) { // out
// 
// !DESCRIPTION:
//      Gets a {\tt EMSC\_Calendar}'s properties
// 
//EOP
// !REQUIREMENTS:

    // TODO: replace MONTHS_PER_YEAR with monthsPerYear

    // must have at least one non-null pointer
    if (calendarType   == ESMC_NULL_POINTER &&
        daysPerMonth   == ESMC_NULL_POINTER && 
        monthsPerYear  == ESMC_NULL_POINTER && 
        secondsPerDay  == ESMC_NULL_POINTER &&
        secondsPerYear == ESMC_NULL_POINTER &&
        daysPerYear    == ESMC_NULL_POINTER &&
        daysPerYeardN  == ESMC_NULL_POINTER &&
        daysPerYeardD  == ESMC_NULL_POINTER) {
        cout << "ESMC_Calendar::ESMC_CalendarGet():  no valid "
             << "pointer passed in." << endl;
      return (ESMF_FAILURE);
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
        return(ESMF_FAILURE);
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

    switch (this->calendarType)
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
            // Validate inputs. TODO: determine lowpoint year, month & day
            if (mm < 1 || mm > 12 || dd < 1) {
              return (ESMF_FAILURE);
            }
            // check day of the month
            if (dd > daysPerMonth[mm-1]) {
              return (ESMF_FAILURE);
            }
            // TODO: upper bounds date range check dependent on machine
            //  word size

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
            // Validate inputs. TODO: determine lowpoint year, month & day
            if (mm < 1 || mm > 12 || dd < 1 || dd > 30) {
              return (ESMF_FAILURE);
            }
            // TODO: upper bounds date range check dependent on machine
            //  word size

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
            // Validate input
            // From Fliegel algorithm:  lower limit of 3/1/-4800 Gregorian
            //    equals -32044 Julian days
            if (d < -32044) return(ESMF_FAILURE);
            // TODO: upper bounds date range check dependent on machine
            //  word size

            // convert Julian days to basetime seconds (>= 64 bit)
            t->s = d * secondsPerDay;

            break;
        }
        case ESMC_CAL_CUSTOM:
            // TODO:
            break;
        case ESMC_CAL_NOCALENDAR:
            // need real calendar type
            return(ESMF_FAILURE);
            break;
        default:
            // unknown calendar type
            return(ESMF_FAILURE);
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
        case ESMC_CAL_CUSTOM:
            // TODO:
            break;
        case ESMC_CAL_NOCALENDAR:
            // need real calendar type
            rc = ESMF_FAILURE;
            break;
        default:
            // unknown calendar type
            rc = ESMF_FAILURE;
            break;
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

// TODO: share common code with Decrement method
// TODO: check for overflow/underflow, return 0 with LogErr message

    // intialize result to given time to prepare for the case of
    //   only a non-calendar (h,m,s) increment
    //    (copies calendar & timezone properties)
    ESMC_Time sum = *time;

    // prepare for increment with any non-calendar units (h,m,s)
    ESMC_TimeInterval nonCalTi = timeinterval;

    switch (calendarType)
    {
        case ESMC_CAL_GREGORIAN:
        // case ESMC_CAL_JULIAN:   TODO:  uncomment when implemented
        case ESMC_CAL_NOLEAP:
        case ESMC_CAL_360DAY:
        {
            ESMF_KIND_I8 yy_i8;
            int mm, dd, timeZone;
            ESMF_KIND_I4 h, m, s;
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
                time->ESMC_TimeGet(ESMC_NULL_POINTER, &yy_i8, &mm, &dd,
                                   ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                   &h, &m, &s,
                                   ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                   ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                   ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                   ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                   ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                   ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                   ESMC_NULL_POINTER, &cal, ESMC_NULL_POINTER,
                                   &timeZone);
                                   // TODO: use native C++ interface when
                                   //   ready
            
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
                if (mm == 2 && calendarType == ESMC_CAL_GREGORIAN &&
                    ESMC_IS_LEAP_YEAR(yy_i8)) daysInMonth++;  // Feb. 29 days
                if (dd > daysInMonth) dd = daysInMonth;

                // convert resulting calendar sum back to base time, while also
                // restoring time-of-day units and properties from given time
                sum.ESMC_TimeSet(ESMC_NULL_POINTER, &yy_i8, &mm, &dd,
                                 ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                 &h, &m, &s,
                                 ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                 ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                 ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                 ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                 ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                 ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                 ESMC_NULL_POINTER, &cal, ESMC_NULL_POINTER,
                                 &timeZone);
                                 // TODO: use native C++ interface when
                                 //   ready
            }
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
        // case ESMC_CAL_JULIAN:   TODO:  uncomment when implemented
        case ESMC_CAL_NOLEAP:
        case ESMC_CAL_360DAY:
        {
            ESMF_KIND_I8 yy_i8;
            int mm, dd, timeZone;
            ESMF_KIND_I4 h, m, s;
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
                time->ESMC_TimeGet(ESMC_NULL_POINTER, &yy_i8, &mm, &dd,
                                   ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                   &h, &m, &s,
                                   ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                   ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                   ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                   ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                   ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                   ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                   ESMC_NULL_POINTER, &cal, ESMC_NULL_POINTER,
                                   &timeZone);
                                   // TODO: use native C++ interface when
                                   //   ready

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
                if (mm == 2 && calendarType == ESMC_CAL_GREGORIAN &&
                    ESMC_IS_LEAP_YEAR(yy_i8)) daysInMonth++;  // Feb. 29 days
                if (dd > daysInMonth) dd = daysInMonth;

                // convert resulting calendar diff back to base time, while also
                // restoring time-of-day units and properties from given time
                diff.ESMC_TimeSet(ESMC_NULL_POINTER, &yy_i8, &mm, &dd,
                                  ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                  &h, &m, &s,
                                  ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                  ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                  ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                  ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                  ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                  ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                  ESMC_NULL_POINTER, &cal, ESMC_NULL_POINTER,
                                  &timeZone);
                                  // TODO: use native C++ interface when
                                  //   ready
            }
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
//      with given {\tt ESMC\_Calendar}, return result
//
//EOP
// !REQUIREMENTS:

    return(calendarType == calendar.calendarType);

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
//      with given {\tt ESMC\_Calendar}, return result
//
//EOP
// !REQUIREMENTS:

    return(calendarType != calendar.calendarType);

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

    if (this->calendarType < 1 ||
        this->calendarType > CALENDAR_TYPE_COUNT) return(ESMF_FAILURE);

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
        cout << "name = " << name << endl;
      }
      else if (strncmp(opts, "calendartype", 12) == 0) {
        // TODO:  make lookup table: int -> string
        cout << "calendarType = " << calendarType << endl;
      }
      else if (strncmp(opts, "dayspermonth", 12) == 0) {
        cout << "daysPerMonth = ";
        for (int i=0; i<this->monthsPerYear; i++) {
          cout << daysPerMonth[i] << " ";
        }
      }
      else if (strncmp(opts, "monthsperyear", 13) == 0) {
        cout << "monthsPerYear = "  << monthsPerYear  << endl;
      }
      else if (strncmp(opts, "secondsperday", 13) == 0) {
        cout << "secondsPerDay = "  << secondsPerDay  << endl;
      }
      else if (strncmp(opts, "secondsperyear", 14) == 0) {
        cout << "secondsPerYear = " << secondsPerYear << endl;
      }
      else if (strncmp(opts, "daysperyear", 11) == 0) {
        cout << "daysPerYear = "    << daysPerYear.d  << endl;
        cout << "daysPerYeardN = "  << daysPerYear.dN << endl;
        cout << "daysPerYeardD = "  << daysPerYear.dD << endl;
      }

    } else {
      // default:  print out all properties

      cout << "name = " << name << endl;

      // TODO:  make lookup table: int -> string
      cout << "calendarType = " << calendarType << endl;

      cout << "daysPerMonth = "; 
      for (int i=0; i<this->monthsPerYear; i++) cout << daysPerMonth[i] << " ";
      cout << endl;
      cout << "monthsPerYear = "  << monthsPerYear  << endl;
      cout << "secondsPerDay = "  << secondsPerDay  << endl;
      cout << "secondsPerYear = " << secondsPerYear << endl;
      cout << "daysPerYear = "    << daysPerYear.d  << endl;
      cout << "daysPerYeardN = "  << daysPerYear.dN << endl;
      cout << "daysPerYeardD = "  << daysPerYear.dD << endl;
    }

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

    ESMC_Calendar();  // invoke default constructor
    ESMC_CalendarSet(strlen(name), name, calendarType);

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
    ESMC_CalendarSet(strlen(name), name, 
                     daysPerMonth, monthsPerYear, secondsPerDay, 
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
