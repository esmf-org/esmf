// $Id: ESMC_Time.C,v 1.10 2003/03/29 01:41:21 eschwab Exp $
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

 // associated class definition file
 #include <ESMC_Time.h>

//-------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_Time.C,v 1.10 2003/03/29 01:41:21 eschwab Exp $";
//-------------------------------------------------------------------------

//
//-------------------------------------------------------------------------
//-------------------------------------------------------------------------
//
// This section includes all the ESMC_Time routines
//
//

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
      ESMF_IKIND_I8 S,              // in - integer seconds
      int Sn,             // in - fractional seconds, numerator
      int Sd,             // in - fractional seconds, denominator
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
//      Initialzes a {\tt Time} with values given in variable arg list
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
      int *YR,                  // in - integer year
      int *MM,                  // in - integer month
      int *DD,                  // in - integer day of the month
      int *D,                   // in - integer days
      int *H,                   // in - integer hours
      int *M,                   // in - integer minutes
      ESMF_IKIND_I8 *S,         // in - long integer seconds 
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
//      Initialzes a {\tt Time} with values given in variable arg list
//
//EOP
// !REQUIREMENTS:  

    // TODO: validate inputs (individual and combos), set basetime values

    // initialize basetime values
    this->S  = 0;
    this->Sn = 0;
    this->Sd = 1;

    // set calendar type
    if (cal != 0) {
      Calendar = cal;
    }
    else {
      // TODO: log err
      cout << "ESMC_Time::ESMC_TimeInit(): calendar not set" << endl;
      return (ESMF_FAILURE);
    }

    // set timezone
    if (tz != 0) {
      Timezone = *tz;
    }

    // convert date to base time according to calendar type
    // TODO: create two calendar conversion method entry points ?
    if (YR != 0 && MM !=0 && DD != 0) {
      Calendar->ESMC_CalendarConvertToTime(*YR, *MM, *DD, 0, this);
    }
    if (D != 0) {
      Calendar->ESMC_CalendarConvertToTime(0, 0, 0, *D, this);
    }
    
    
    if (H != 0) {
      this->S += *H * SECONDS_PER_HOUR;
    }
    if (M != 0) {
      this->S += *M * SECONDS_PER_MINUTE;
    }
    if (S != 0) {
      this->S += *S;
    }

    return(ESMF_SUCCESS);

 }  // end ESMC_TimeInit

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
//      Gets a {\tt Time}'s values in user-specified format
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
//      Sets a {\tt Time}'s values in user-specified values
//
//EOP
// !REQUIREMENTS:  

    // TODO
    return(ESMF_SUCCESS);

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
      ESMC_Calendar *calendar) const {    // out - Time's calendar
//
// !DESCRIPTION:
//      Gets a {\tt Time}'s associated calendar
//
//EOP
// !REQUIREMENTS:  

    calendar = Calendar;
    return(ESMF_SUCCESS);

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

    if (calendar != 0) {
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

    if (Calendar != 0)
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

    if (timezone != 0) {
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

    // TODO
    return(ESMF_SUCCESS);

 }  // end ESMC_TimeGet

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

    // TODO
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

    // TODO
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

    // TODO
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

    // TODO
    return(ESMF_SUCCESS);

 }  // end ESMC_TimeGetRealTime

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Read - restore Time state
//
// !INTERFACE:
      int ESMC_Time::ESMC_Read(
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

    if (cal == 0) {
      cout << "ESMC_Time::ESMC_Read(): null pointer passed in" << endl;
      return(ESMF_FAILURE);
    }

    // use base class Read() first
    rc = ESMC_BaseTime::ESMC_Read(S, Sn, Sd);

    Calendar = cal;  // TODO?: this only restores calendar pointer; component
                     // must be sure to restore corresponding calendar first
    Timezone = timezone;
  
    return(rc);

 }  // end ESMC_Read

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Write - return Time state
//
// !INTERFACE:
      int ESMC_Time::ESMC_Write(
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

    if (S == 0 || Sn == 0 || Sd == 0 || cal == 0 || timezone == 0) {
      cout << "ESMC_Time::ESMC_Write(): null pointer(s) passed in" << endl;
      return(ESMF_FAILURE);
    }

    // use base class Write() first
    rc = ESMC_BaseTime::ESMC_Write(S, Sn, Sd);

    cal = Calendar;  // TODO?: this only saves calendar pointer; component
                     // must be sure to save corresponding calendar
    *timezone = Timezone;
  
    return(rc);

 }  // end ESMC_Write

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Validate - validate Time state
//
// !INTERFACE:
      int ESMC_Time::ESMC_Validate(
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

 }  // end ESMC_Validate

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Print - print Time state
//
// !INTERFACE:
      int ESMC_Time::ESMC_Print(
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

    cout << "Time:" << endl;
    ESMC_BaseTime::ESMC_Print(options);
    if (Calendar != 0) Calendar->ESMC_Print(options);
    cout << "Timezone = " << Timezone << endl;

    return(ESMF_SUCCESS);

 }  // end ESMC_Print

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

   // ESMC_TimeInit(0, 0, 1, 0, 0); // use variable args Init TODO

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
      ESMF_IKIND_I8 S,              // in - integer seconds
      int Sn,             // in - fractional seconds, numerator
      int Sd,             // in - fractional seconds, denominator
      ESMC_Calendar *Cal,   // in - associated calendar
      int Tz) {             // in - timezone
//
// !DESCRIPTION:
//      Initializes a {\tt ESMC\_Time} via {\tt ESMC\_TimeInit}
//
//EOP
// !REQUIREMENTS:  

    // ESMC_TimeInit(S, Sn, Sd, Cal, Tz); // use variable args Init TODO

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
