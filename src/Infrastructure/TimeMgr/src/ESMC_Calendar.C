// $Id: ESMC_Calendar.C,v 1.2 2002/10/15 23:29:54 eschwab Exp $
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
// The code in this file implements the C++ Calendar methods declared
// in the companion file ESMC_Calendar.h
//
//-------------------------------------------------------------------------

 // higher level, 3rd party or system includes
 #include <stdio.h>
 #include<ESMC_Util.h>

 // associated class definition file
 #include <ESMC_Calendar.h>

//-------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_Calendar.C,v 1.2 2002/10/15 23:29:54 eschwab Exp $";
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
// !IROUTINE:  ESMC_CalInit - shallow class initializer 1
//
// !INTERFACE:
      int ESMC_Calendar::ESMC_CalInit(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_CalendarType_e Type) {   // in - initialize to be Calendar type
//
// !DESCRIPTION:
//      Initialzes a Calendar to be of a specific type
//
//EOP
// !REQUIREMENTS:

	int rc; // return code 

	this->Type = Type;

	switch (Type)
	{
		case ESMC_GREGORIAN:
		case ESMC_NOLEAP:
			// specific leap year is property of a TimeInstant, not Calendar ??
			//    OR calculated on-the-fly during TimeInstant calculations ??
			//    Calendar type only determines whether leap year is used
			DaysPerMonth[1]  = 31; DaysPerMonth[2]  = 28;
			DaysPerMonth[3]  = 31; DaysPerMonth[4]  = 30;
			DaysPerMonth[5]  = 31; DaysPerMonth[6]  = 30;
			DaysPerMonth[7]  = 31; DaysPerMonth[8]  = 31;
			DaysPerMonth[9]  = 30; DaysPerMonth[10] = 31;
			DaysPerMonth[11] = 30; DaysPerMonth[12] = 31;
			SecondsPerDay  = 86400;
			DaysPerYear.D  = 365;
			DaysPerYear.Dn = 0;
			DaysPerYear.Dd = 1;
			rc = ESMC_SUCCESS;
			break;

		case ESMC_JULIAN:
			// Days is the highest grouping of time, i.e. there is no
			//   concept of months or years ??
			SecondsPerDay  = 86400;
			DaysPerYear.D  = 0;
			DaysPerYear.Dn = 0;
			DaysPerYear.Dd = 1;
			rc = ESMC_SUCCESS;
			break;

		case ESMC_360DAY:
			// 12 months of 30 days each
			for (int i=1; i<=MonthsPerYear; i++) DaysPerMonth[i] = 30;
			SecondsPerDay  = 86400;
			DaysPerYear.D  = 360;
			DaysPerYear.Dn = 0;
			DaysPerYear.Dd = 1;
			rc = ESMC_SUCCESS;
			break;

		case ESMC_NOCALENDAR:
			// no calendar needed, not for any planetary body ??
			SecondsPerDay  = 0;
			DaysPerYear.D  = 0;
			DaysPerYear.Dn = 0;
			DaysPerYear.Dd = 1;
			rc = ESMC_SUCCESS;
			break;

		case ESMC_GENERIC:
			// need more info -- must call InitGeneric() instead
			rc = ESMC_FAILURE;
			break;

		default:
			// unknown calendar type
			rc = ESMC_FAILURE;
			break;
	}
	return(rc);

}  // end ESMC_CalInit

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CalInitGeneric - shallow class initializer 1
//
// !INTERFACE:
      int ESMC_Calendar::ESMC_CalInitGeneric(
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
//      Initialzes a Calendar to be a custom, user-defined type
// 
//EOP
// !REQUIREMENTS:

	Type = ESMC_GENERIC;

	for(int i=1; i<=MonthsPerYear; i++)
	{ 
       	this->DaysPerMonth[i] = DaysPerMonth[i];
	}
    this->SecondsPerDay  = SecondsPerDay;
    this->DaysPerYear.D  = DaysPerYear;
    this->DaysPerYear.Dn = DaysPerYearDn;
    this->DaysPerYear.Dd = DaysPerYearDd;

	return(ESMC_SUCCESS);

}  // end ESMC_CalInitGeneric

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CalConvertToTime - convert calendar date to ESMC_Time
//
// !INTERFACE:
      int ESMC_Calendar::ESMC_CalConvertToTime(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int YR, int MM, int DD, int32 D,          // in
      int H, int M, int S, int MS, int32 US,    // in
      int32 NS, int32 Sn, int32 Sd,             // in
      double d, double h, double m, double s,   // in
      double ms, double us, double ns,          // in
      ESMC_Time *T) {                           // out
//
// !DESCRIPTION:
//     Converts a calendar-specific date to core ESMC_Time representation
//     Conversions based on UTC: time zone offset done by client
//
//EOP
// !REQUIREMENTS:   TMG 2.4.5, 2.5.6

	switch (Type)
	{
		// convert Gregorian Date => Time
		case ESMC_GREGORIAN:
		{
			int temp;
			int32 jdays;

			// convert date portion of TimeInstant into time portion of
			//  TimeInstant
			// Convert to Julian first
			// Gregorian date (YR, MM, DD) => Julian day (D)
    		temp = (MM-14)/12;
    		jdays = (1461 * (YR + 4800 + temp)) / 4 + (367 * (MM - 2 - 12 *
       				temp ))/12 - (3 * ( (YR + 4900 + temp)/100))/4 + DD - 32075;
			T->ESMC_TimeWrite_S(jdays * SecondsPerDay);
			break;
		}
		// convert Julian Date => Time
		case ESMC_JULIAN:
		{
			T->ESMC_TimeWrite_S(D * SecondsPerDay);
			break;
		}
		default:
			break;
	}

	return(ESMC_SUCCESS);

}  // end ESMC_CalConvertToTime

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CalConvertToDate - convert ESMC_Time to calendar date
//
// !INTERFACE:
      int ESMC_Calendar::ESMC_CalConvertToDate(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_Time *T,                                   // in
      int *YR, int *MM, int *DD, int32 *D, int *H,    // out
      int *M, int *S, int *MS, int32 *US, int32 *NS,  // out
      int32 *Sn, int32 *Sd, double *d, double *h,     // out
      double *m, double *s, double *ms) {             // out
//
// !DESCRIPTION:
//     Converts a core ESMC_Time representation to a calendar-specific date
//     Conversions based on UTC: time zone offset done by client
//
//EOP
// !REQUIREMENTS:   TMG 2.4.5, 2.5.6

	int64 TimeS;

	T->ESMC_TimeRead_S(&TimeS);

	switch (Type)
	{
		// convert Time => Gregorian Date
		case ESMC_GREGORIAN:
		{
			int tempi, tempj, templ, tempn;
			int32 jdays;

			// convert time portion of TimeInstant into date portion of
			//     TimeInstant
			// Julian day (D) => Gregorian date (YR, MM, DD)
			// The calculation below fails for jday >= 536,802,343.
			//    (4*templ = 2^31)
			jdays = TimeS / SecondsPerDay;	// convert to Julian first
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
		case ESMC_JULIAN:
		{
			*D = TimeS / SecondsPerDay;
			break;
		}
		default:
			break;
	}

	return(ESMC_SUCCESS);

}  // end ESMC_CalConvertToDate

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CalPrint - return Calendar state
//
// !INTERFACE:
      int ESMC_Calendar::ESMC_CalPrint(
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
//      Returns Calendar state for persistence/checkpointing
// 
//EOP
// !REQUIREMENTS:

	if (Type != NULL && DaysPerMonth != NULL &&
		SecondsPerDay != NULL && DaysPerYear != NULL &&
		DaysPerYearDn != NULL && DaysPerYearDd != NULL)
	{
		*Type = this->Type;
		for (int i=1; i<= MonthsPerYear; i++)
		{
			DaysPerMonth[i] = this->DaysPerMonth[i];	
		}
		*SecondsPerDay = this->SecondsPerDay;
		*DaysPerYear   = this->DaysPerYear.D;
		*DaysPerYearDn = this->DaysPerYear.Dn;
		*DaysPerYearDd = this->DaysPerYear.Dd;

		return(ESMC_SUCCESS);
	}
	else return(ESMC_FAILURE);

}  // end ESMC_CalPrint

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_CalPrint - print Calendar state
//
// !INTERFACE:
      int ESMC_Calendar::ESMC_CalPrint(void) const {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      print Calendar state for testing/debugging
//
//EOP
// !REQUIREMENTS: 

	printf("Type = %d\n", Type);
	printf("DaysPerMonth = ");
	for (int i=1; i<= MonthsPerYear; i++) printf("%d ", DaysPerMonth[i]);
	printf("\n");
	printf("SecondsPerDay = %d\n", SecondsPerDay);
	printf("DaysPerYear = %d\n", DaysPerYear.D);
	printf("DaysPerYearDn = %d\n", DaysPerYear.Dn);
	printf("DaysPerYearDd = %d\n", DaysPerYear.Dd);
	
	return(ESMC_SUCCESS);

}  // end ESMC_CalPrint

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
//      Initializes a ESMC_Calendar with defaults via ESMC_CalInit
//
//EOP
// !REQUIREMENTS: 

	// default calendar type is none ??
	ESMC_CalInit(ESMC_NOCALENDAR);

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
//      Initializes a ESMC_TimeInstant to be of a specific type via
//      ESMC_CalInit
//
//EOP
// !REQUIREMENTS: 

	ESMC_CalInit(Type);

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
//      Initializes a ESMC_TimeInstant to be of a custom user-defined type
//      via ESMC_CalInitGeneric
//
//EOP
// !REQUIREMENTS: 

	ESMC_CalInitGeneric(DaysPerMonth,  SecondsPerDay, DaysPerYear,
			            DaysPerYearDn, DaysPerYearDd);
}  // end ESMC_Calendar

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ~~ESMC_Calendar - native default C++ destructor
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
//      Default ESMC_Calendar destructor
//
//EOP
// !REQUIREMENTS: 

} // end ~ESMC_Calendar
