// $Id: ESMC_Calendar.C,v 1.5 2003/03/27 01:55:51 eschwab Exp $
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
 #include <stdio.h>

 // associated class definition file
 #include <ESMC_Calendar.h>

//-------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_Calendar.C,v 1.5 2003/03/27 01:55:51 eschwab Exp $";
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
      ESMC_CalendarType_e type) {   // in - initialize to be Calendar type
//
// !DESCRIPTION:
//      Initialzes a {\tt Calendar} to be of a specific type
//
//EOP
// !REQUIREMENTS:

    int rc; // return code 

//cout << "ESMC_Calendar::ESMC_CalendarInit(): Type = " << type << endl;
    Type = type;
    //this->Type = Type;
//cout << "HERE C0" << endl;

    switch (Type)
    {
        case ESMC_CAL_GREGORIAN:
        case ESMC_CAL_NOLEAP:
//cout << "HERE C1" << endl;
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
            DaysPerYear.D  = 365;
            DaysPerYear.Dn = 0;
            DaysPerYear.Dd = 1;
            rc = ESMF_SUCCESS;
//cout << "HERE C2" << endl;
            break;

        case ESMC_CAL_JULIAN:
            // Days is the highest resolution of time, i.e. there is no
            //   concept of months or years ??
            SecondsPerDay  = SECONDS_PER_DAY;
            DaysPerYear.D  = 0;
            DaysPerYear.Dn = 0;
            DaysPerYear.Dd = 1;
            rc = ESMF_SUCCESS;
            break;

        case ESMC_CAL_360DAY:
            // 12 months of 30 days each
            for (int i=1; i<=MONTHSPERYEAR; i++) DaysPerMonth[i] = 30;
            SecondsPerDay  = 86400;
            DaysPerYear.D  = 360;
            DaysPerYear.Dn = 0;
            DaysPerYear.Dd = 1;
            rc = ESMF_SUCCESS;
            break;

        case ESMC_CAL_NOCALENDAR:
            // no calendar needed, convert base time up to days only
            SecondsPerDay  = 0;
            DaysPerYear.D  = 0;
            DaysPerYear.Dn = 0;
            DaysPerYear.Dd = 1;
            rc = ESMF_SUCCESS;
            break;

        case ESMC_CAL_GENERIC:
//cout << "HERE C3" << endl;
            // user defined; need more info; user must call
            //   InitGeneric() instead
            rc = ESMF_FAILURE;
            break;

        default:
            // unknown calendar type
            rc = ESMF_FAILURE;
            break;
    }
//cout << "ESMC_Calendar::ESMC_CalendarInit() returning" << endl;
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

    Type = ESMC_CAL_GENERIC;

    for(int i=1; i<=MONTHSPERYEAR; i++)
    { 
           this->DaysPerMonth[i] = DaysPerMonth[i];
    }
    this->SecondsPerDay  = SecondsPerDay;
    this->DaysPerYear.D  = DaysPerYear;
    this->DaysPerYear.Dn = DaysPerYearDn;
    this->DaysPerYear.Dd = DaysPerYearDd;

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
      ESMC_BaseTime *T) {                     // out
//
// !DESCRIPTION:
//     Converts a calendar-specific date to core {\tt ESMC\_BaseTime}
//     representation. Conversions based on UTC: time zone offset done by
//     client
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

            // convert date portion of Time instant into time portion of
            //  Time instant
            // Convert to Julian first
            // Gregorian date (YR, MM, DD) => Julian day (D)
            temp = (MM-14)/12;
            jdays = (1461 * (YR + 4800 + temp)) / 4 + (367 * (MM - 2 - 12 *
                    temp ))/12 - (3 * ( (YR + 4900 + temp)/100))/4 + DD - 32075;
            T->S = jdays * SecondsPerDay;
            break;
        }
        // convert Julian Date => Time
        case ESMC_CAL_JULIAN:
        {
            T->S = D * SecondsPerDay;
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
      ESMC_BaseTime *T,                      // in
      int *YR, int *MM, int *DD, int *D) {   // out
//
// !DESCRIPTION:
//     Converts a core {\tt ESMC\_BaseTime} representation to a
//     calendar-specific date. Conversions based on UTC: time zone offset
//     done by client
//
//EOP
// !REQUIREMENTS:   TMG 2.4.5, 2.5.6

    ESMF_IKIND_I8 TimeS;

    TimeS = T->S;

    switch (Type)
    {
        // convert Time => Gregorian Date
        case ESMC_CAL_GREGORIAN:
        {
            int tempi, tempj, templ, tempn;
            int jdays;

            // convert time portion of Time instant into date portion of
            //     Time instant
            // Julian day (D) => Gregorian date (YR, MM, DD)
            // The calculation below fails for jday >= 536,802,343.
            //    (4*templ = 2^31)
            jdays = TimeS / SecondsPerDay;    // convert to Julian first
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
        // convert Time => Julian Date
        case ESMC_CAL_JULIAN:
        {
            *D = TimeS / SecondsPerDay;
            break;
        }
        default:
            break;
    }

    return(ESMF_SUCCESS);

}  // end ESMC_CalendarConvertToDate

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BasePrint - return Calendar state
//
// !INTERFACE:
      int ESMC_Calendar::ESMC_BasePrint(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_CalendarType_e *Type,  // out
      int *DaysPerMonth,          // out
      int *SecondsPerDay,         // out
      int *DaysPerYear,           // out
      int *DaysPerYearDn,         // out
      int *DaysPerYearDd) const { // out
// 
// !DESCRIPTION:
//      Returns {\tt Calendar} state for persistence/checkpointing
// 
//EOP
// !REQUIREMENTS:

    if (Type != NULL && DaysPerMonth != NULL &&
        SecondsPerDay != NULL && DaysPerYear != NULL &&
        DaysPerYearDn != NULL && DaysPerYearDd != NULL)
    {
        *Type = this->Type;
        for (int i=1; i<= MONTHSPERYEAR; i++)
        {
            DaysPerMonth[i] = this->DaysPerMonth[i];    
        }
        *SecondsPerDay = this->SecondsPerDay;
        *DaysPerYear   = this->DaysPerYear.D;
        *DaysPerYearDn = this->DaysPerYear.Dn;
        *DaysPerYearDd = this->DaysPerYear.Dd;

        return(ESMF_SUCCESS);
    }
    else return(ESMF_FAILURE);

}  // end ESMC_BasePrint

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BasePrint - print Calendar state
//
// !INTERFACE:
      int ESMC_Calendar::ESMC_BasePrint(void) const {
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

    printf("Type = %d\n", Type);
    printf("DaysPerMonth = ");
    for (int i=1; i<= MONTHSPERYEAR; i++) printf("%d ", DaysPerMonth[i]);
    printf("\n");
    printf("SecondsPerDay = %d\n", SecondsPerDay);
    printf("DaysPerYear = %d\n", DaysPerYear.D);
    printf("DaysPerYearDn = %d\n", DaysPerYear.Dn);
    printf("DaysPerYearDd = %d\n", DaysPerYear.Dd);
    
    return(ESMF_SUCCESS);

}  // end ESMC_BasePrint

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

    // default calendar type is none ??
    ESMC_CalendarInit(ESMC_CAL_NOCALENDAR);

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
      ESMC_CalendarType_e Type) {  // in
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
