// $Id: ESMC_Calendar.C,v 1.14 2003/04/15 16:47:42 eschwab Exp $
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
// The code in this file implements the C++ {\tt Calendar} methods declared
// in the companion file {\tt ESMC_Calendar.h}
//
//-------------------------------------------------------------------------

 // higher level, 3rd party or system includes
 #include <iostream.h>

 // associated class definition file
 #include <ESMC_Calendar.h>

//-------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_Calendar.C,v 1.14 2003/04/15 16:47:42 eschwab Exp $";
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
// !IROUTINE:  ESMC_CalendarInit - shallow class initializer 1
//
// !INTERFACE:
      int ESMC_Calendar::ESMC_CalendarInit(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_CalendarType type) {   // in - initialize to be Calendar type
//
// !DESCRIPTION:
//      Initialzes a {\tt Calendar} to be of a specific type
//
//EOP
// !REQUIREMENTS:

    // TODO: ensure initialization if called via F90 interface;
    //       cannot call constructor, because destructor is subsequently
    //       called automatically, returning initialized values to garbage.
    for (int i=0; i<MONTHS_PER_YEAR; i++) DaysPerMonth[i] = 0;
    SecondsPerDay  = 0;
    SecondsPerYear = 0;
    DaysPerYear.D  = 0;
    DaysPerYear.Dn = 0;
    DaysPerYear.Dd = 1;
    
    int rc; // return code 

    Type = type;

    switch (Type)
    {
        case ESMC_CAL_GREGORIAN:
        case ESMC_CAL_NOLEAP:
            // specific leap year is property of a Time instant, not Calendar ??
            //    OR calculated on-the-fly during Time instant calculations ??
            //    Calendar type only determines whether leap year is used
            DaysPerMonth[0]  = 31; DaysPerMonth[1]  = 28;
            DaysPerMonth[2]  = 31; DaysPerMonth[3]  = 30;
            DaysPerMonth[4]  = 31; DaysPerMonth[5]  = 30;
            DaysPerMonth[6]  = 31; DaysPerMonth[7]  = 31;
            DaysPerMonth[8]  = 30; DaysPerMonth[9]  = 31;
            DaysPerMonth[10] = 30; DaysPerMonth[11] = 31;
            SecondsPerDay  = SECONDS_PER_DAY;
            SecondsPerYear = SECONDS_PER_DAY * 365;
            DaysPerYear.D  = 365;
            DaysPerYear.Dn = 0;
            DaysPerYear.Dd = 1;
            rc = ESMF_SUCCESS;
            break;

        case ESMC_CAL_JULIAN:
            // Days is the highest resolution of time, i.e. there is no
            //   concept of months or years ??
            for (int i=0; i<MONTHS_PER_YEAR; i++) DaysPerMonth[i] = 0;
            SecondsPerDay  = SECONDS_PER_DAY;
            SecondsPerYear = 0;
            DaysPerYear.D  = 0;
            DaysPerYear.Dn = 0;
            DaysPerYear.Dd = 1;
            rc = ESMF_SUCCESS;
            break;

        case ESMC_CAL_360DAY:
            // 12 months of 30 days each
            for (int i=0; i<MONTHS_PER_YEAR; i++) DaysPerMonth[i] = 30;
            SecondsPerDay  = SECONDS_PER_DAY;
            SecondsPerYear = SECONDS_PER_DAY * 360;
            DaysPerYear.D  = 360;
            DaysPerYear.Dn = 0;
            DaysPerYear.Dd = 1;
            rc = ESMF_SUCCESS;
            break;

        case ESMC_CAL_NOCALENDAR:
            // no calendar needed, convert base time up to days only
            for (int i=0; i<MONTHS_PER_YEAR; i++) DaysPerMonth[i] = 0;
            SecondsPerDay  = 0;
            SecondsPerYear = 0;
            DaysPerYear.D  = 0;
            DaysPerYear.Dn = 0;
            DaysPerYear.Dd = 1;
            rc = ESMF_SUCCESS;
            break;

        case ESMC_CAL_GENERIC:
            // user defined; need more info; user must call
            //   InitGeneric() instead
            rc = ESMF_FAILURE;
            break;

        default:
            // unknown calendar type
            rc = ESMF_FAILURE;
            break;
    }
    return(rc);

}  // end ESMC_CalendarInit

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CalendarInitGeneric - shallow class initializer 1
//
// !INTERFACE:
      int ESMC_Calendar::ESMC_CalendarInitGeneric(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int *DaysPerMonth,    // in
      int SecondsPerDay,    // in
      int DaysPerYear,      // in
      int DaysPerYearDn,    // in
      int DaysPerYearDd) {  // in
// 
// !DESCRIPTION:
//      Initialzes a {\tt Calendar} to be a custom, user-defined type
// 
//EOP
// !REQUIREMENTS:

    // TODO: ensure initialization if called via F90 interface;
    //       cannot call constructor, because destructor is subsequently
    //       called automatically, returning initialized values to garbage.
    for (int i=0; i<MONTHS_PER_YEAR; i++) this->DaysPerMonth[i] = 0;
    this->SecondsPerDay  = 0;
    this->SecondsPerYear = 0;
    this->DaysPerYear.D  = 0;
    this->DaysPerYear.Dn = 0;
    this->DaysPerYear.Dd = 1;
    
    if (DaysPerMonth == 0) {
      cout << "ESMC_Calendar::ESMC_CalendarInitGeneric():  DaysPerMonth "
           << "pointer passed in is zero.";
      return(ESMF_FAILURE);
    }

    Type = ESMC_CAL_GENERIC;

    for(int i=0; i<MONTHS_PER_YEAR; i++)
    { 
           this->DaysPerMonth[i] = DaysPerMonth[i];
    }
    this->SecondsPerDay  = SecondsPerDay;
    this->DaysPerYear.D  = DaysPerYear;
    this->DaysPerYear.Dn = DaysPerYearDn;
    this->DaysPerYear.Dd = DaysPerYearDd;
    this->SecondsPerYear = SecondsPerDay * DaysPerYear;

    return(ESMF_SUCCESS);

}  // end ESMC_CalendarInitGeneric

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
      int YR, int MM, int DD, int D,          // in
      ESMC_BaseTime *T) const {               // out
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
//     The algorithm is valid through all future dates, assuming standard
//     leap-year corrections are applied (every 4 years, 100 years, and
//     400 years).
//
//EOP
// !REQUIREMENTS:   TMG 2.4.5, 2.5.6

    switch (Type)
    {
        // convert Gregorian Date => Time
        case ESMC_CAL_GREGORIAN:
        {
            int temp;
            int jdays;

            // convert Gregorian date to Julian days
            // Gregorian date (YR, MM, DD) => Julian days (jdays)
            temp = (MM-14)/12;
            jdays = (1461 * (YR + 4800 + temp)) / 4 +
                    (367 * (MM - 2 - 12 * temp ))/12 -
                    (3 * ( (YR + 4900 + temp)/100))/4 + DD - 32075;

            // convert Julian days to basetime seconds (>= 64 bit)
            T->S = (ESMF_IKIND_I8) jdays * (ESMF_IKIND_I8) SecondsPerDay;
            break;
        }
        // convert No Leap Date => Time
        // TODO:  assumes time zero = 1/1/0000 00:00:00, make same as
        //        Gregorian/Julian time zero = 11/24/-4713 00:00:00 ?
        case ESMC_CAL_NOLEAP:
        {
            T->S = (ESMF_IKIND_I8) (YR-1) * (ESMF_IKIND_I8) SecondsPerYear;

            for(int month=0; month < MM-1; month++) {
              T->S += DaysPerMonth[month] * SecondsPerDay;
            }
            T->S += (DD-1) * SecondsPerDay;
            
            break;
        }
        // convert Julian Date => Time
        case ESMC_CAL_JULIAN:
        {
            // convert Julian days to basetime seconds (>= 64 bit)
            T->S = (ESMF_IKIND_I8) D * (ESMF_IKIND_I8) SecondsPerDay;
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
      const ESMC_BaseTime *T,                      // in
      int *YR, int *MM, int *DD, int *D) const {   // out
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
//     The algorithm is valid through all future dates, assuming standard
//     leap-year corrections are applied (every 4 years, 100 years, and
//     400 years).
//
//EOP
// !REQUIREMENTS:   TMG 2.4.5, 2.5.6

    switch (Type)
    {
        // convert Time => Gregorian Date
        case ESMC_CAL_GREGORIAN:
        {
            int tempi, tempj, templ, tempn;
            int jdays;

            // Convert basetime portion of Time into date
            // Julian day (D) => Gregorian date (YR, MM, DD)
            // The calculation below fails for jday >= 536,802,343.
            //    (4*templ = 2^31)

            // convert basetime seconds to Julian days
            jdays = T->S / (ESMF_IKIND_I8) SecondsPerDay;

            // convert Julian days to Gregorian date
            // Julian days (jdays) => Gregorian date (YR, MM, DD)
            templ = jdays + 68569;
            tempn = ( 4 * templ ) / 146097;
            templ = templ - ( 146097 * tempn + 3 ) / 4;
            tempi = ( 4000 * ( templ + 1) ) / 1461001;
            templ = templ - ( 1461 * tempi ) / 4 + 31;
            tempj = ( 80 * templ ) / 2447;
            *DD = templ - ( 2447 * tempj ) / 80;
            templ = tempj / 11;
            *MM = tempj + 2 - ( 12 * templ );
            *YR = 100 * ( tempn - 49 ) + tempi + templ;

            break;
        }
        // convert Time => No Leap Date
        // TODO:  assumes time zero = 1/1/0000 00:00:00, make same as
        //        Gregorian/Julian time zero = 11/24/-4713 00:00:00 ?
        case ESMC_CAL_NOLEAP:
        {
            *YR     = (T->S / SecondsPerYear) + 1;
            int day = ((T->S % SecondsPerYear) / SecondsPerDay) + 1;

            int month;
            for(month=0; day > DaysPerMonth[month]; month++) {
              day -= DaysPerMonth[month];
            }
            *MM = month + 1;
            *DD = day;
            
            break;
        }
        // convert Time => Julian Date
        case ESMC_CAL_JULIAN:
        {
            // convert basetime seconds to Julian days
            *D = T->S / (ESMF_IKIND_I8) SecondsPerDay;
            break;
        }
        default:
            break;
    }

    return(ESMF_SUCCESS);

}  // end ESMC_CalendarConvertToDate

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CalendarRead - restore Calendar state
//
// !INTERFACE:
      int ESMC_Calendar::ESMC_CalendarRead(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_CalendarType type,    // in
      int *daysPerMonth,         // in
      int secondsPerDay,         // in
      int daysPerYear,           // in
      int daysPerYearDn,         // in
      int daysPerYearDd)       { // in
// 
// !DESCRIPTION:
//      Restores {\tt Calendar} state for persistence/checkpointing
// 
//EOP
// !REQUIREMENTS:

    if (daysPerMonth == ESMC_NULL_POINTER) {
      cout << "ESMC_Calendar::ESMC_CalendarRead(): null pointer passed in"
           << endl;
      return(ESMF_FAILURE);
    }

    Type = type;
    for (int i=0; i<MONTHS_PER_YEAR; i++)
    {
        DaysPerMonth[i] = daysPerMonth[i];    
    }
    SecondsPerDay  = secondsPerDay;
    DaysPerYear.D  = daysPerYear;
    DaysPerYear.Dn = daysPerYearDn;
    DaysPerYear.Dd = daysPerYearDd;
    SecondsPerYear = secondsPerDay * daysPerYear;

    return(ESMF_SUCCESS);

}  // end ESMC_CalendarRead

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CalendarWrite - save Calendar state
//
// !INTERFACE:
      int ESMC_Calendar::ESMC_CalendarWrite(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_CalendarType *type,    // out
      int *daysPerMonth,          // out
      int *secondsPerDay,         // out
      int *daysPerYear,           // out
      int *daysPerYearDn,         // out
      int *daysPerYearDd) const { // out
// 
// !DESCRIPTION:
//      Returns {\tt Calendar} state for persistence/checkpointing
// 
//EOP
// !REQUIREMENTS:

    if (type          == ESMC_NULL_POINTER ||
        daysPerMonth  == ESMC_NULL_POINTER ||
        secondsPerDay == ESMC_NULL_POINTER ||
        daysPerYear   == ESMC_NULL_POINTER ||
        daysPerYearDn == ESMC_NULL_POINTER ||
        daysPerYearDd == ESMC_NULL_POINTER) {
      cout << "ESMC_Calendar::ESMC_CalendarWrite(): null pointer(s) passed in"
           << endl;
      return(ESMF_FAILURE);
    }

    *type = Type;
    for (int i=0; i<MONTHS_PER_YEAR; i++)
    {
        daysPerMonth[i] = DaysPerMonth[i];    
    }
    *secondsPerDay = SecondsPerDay;
    *daysPerYear   = DaysPerYear.D;
    *daysPerYearDn = DaysPerYear.Dn;
    *daysPerYearDd = DaysPerYear.Dd;

    return(ESMF_SUCCESS);

}  // end ESMC_CalendarWrite

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
//      validate {\tt Calendar} state
//
//EOP
// !REQUIREMENTS: 

    // TODO
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
//      print {\tt Calendar} state for testing/debugging
//
//EOP
// !REQUIREMENTS: 

    cout << "Calendar -------------------------------" << endl;
    cout << "Type = " << Type << endl;
    cout << "DaysPerMonth = " << endl;
    for (int i=0; i<MONTHS_PER_YEAR; i++) cout << DaysPerMonth[i] << " ";
    cout << endl;
    cout << "SecondsPerDay = "  << SecondsPerDay  << endl;
    cout << "SecondsPerYear = " << SecondsPerYear << endl;
    cout << "DaysPerYear = "    << DaysPerYear.D  << endl;
    cout << "DaysPerYearDn = "  << DaysPerYear.Dn << endl;
    cout << "DaysPerYearDd = "  << DaysPerYear.Dd << endl;
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
//      Initializes a {\tt ESMC\_Calendar} with defaults via
//      {\tt ESMC\_CalendarInit}
//
//EOP
// !REQUIREMENTS: 

    Type = ESMC_CAL_NOCALENDAR;
    for (int i=0; i<MONTHS_PER_YEAR; i++) DaysPerMonth[i] = 0;
    SecondsPerDay  = 0;
    SecondsPerYear = 0;
    DaysPerYear.D  = 0;
    DaysPerYear.Dn = 0;
    DaysPerYear.Dd = 1;

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
      ESMC_CalendarType Type) {  // in
//
// !DESCRIPTION:
//      Initializes a {\tt ESMC\_TimeInstant} to be of a specific type via
//      {\tt ESMC\_CalendarInit}
//
//EOP
// !REQUIREMENTS: 

    ESMC_CalendarInit(Type);

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
      int *DaysPerMonth,     // in
      int SecondsPerDay,     // in
      int DaysPerYear,       // in
      int DaysPerYearDn,     // in
      int DaysPerYearDd) {   // in
//
// !DESCRIPTION:
//      Initializes a {\tt ESMC\_Time} to be of a custom user-defined type
//      via {\tt ESMC\_CalendarInitGeneric}
//
//EOP
// !REQUIREMENTS: 

    ESMC_CalendarInitGeneric(DaysPerMonth,  SecondsPerDay, DaysPerYear,
                             DaysPerYearDn, DaysPerYearDd);
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
