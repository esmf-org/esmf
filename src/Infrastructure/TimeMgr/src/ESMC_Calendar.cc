#include <ESMC_TimeInstant.h>
#include <ESMC_Calendar.h>
#include <ESMC_Util.h>
#include <stdio.h>

//-------------------------------------------------------------------------
// Class ESMC_CalendarMethods
//-------------------------------------------------------------------------

ESMC_Calendar::ESMC_Calendar(void)
{
	// default calendar type is none ??
	Init(ESMC_NOCALENDAR);
}

ESMC_Calendar::ESMC_Calendar(ESMC_CalendarType_e Type)
{
	Init(Type);
}

ESMC_Calendar::ESMC_Calendar(int *DaysPerMonth,
						     int SecondsPerDay,
						     int DaysPerYear,
						     int DaysPerYearDn,
						     int DaysPerYearDd)
{
	InitGeneric(DaysPerMonth,  SecondsPerDay, DaysPerYear,
			    DaysPerYearDn, DaysPerYearDd);
}

ESMC_Calendar::~ESMC_Calendar(void)
{
}

int ESMC_Calendar::Init(ESMC_CalendarType_e Type)
{
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
}

int ESMC_Calendar::InitGeneric(int *DaysPerMonth,
							   int SecondsPerDay,
							   int DaysPerYear,
							   int DaysPerYearDn,
							   int DaysPerYearDd)

{
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
}

// for persistence/checkpointing
int ESMC_Calendar::Dump(ESMC_CalendarType_e *Type, 
             int *DaysPerMonth,  int *SecondsPerDay,
             int *DaysPerYear,   int *DaysPerYearDn, int *DaysPerYearDd)
{
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
}

// for testing/debugging
int ESMC_Calendar::Dump(void)
{
	printf("Type = %d\n", Type);
	printf("DaysPerMonth = ");
	for (int i=1; i<= MonthsPerYear; i++) printf("%d ", DaysPerMonth[i]);
	printf("\n");
	printf("SecondsPerDay = %d\n", SecondsPerDay);
	printf("DaysPerYear = %d\n", DaysPerYear.D);
	printf("DaysPerYearDn = %d\n", DaysPerYear.Dn);
	printf("DaysPerYearDd = %d\n", DaysPerYear.Dd);
	
	return(ESMC_SUCCESS);
}

// conversions based on UTC: time zone offset done by client
int ESMC_Calendar::ConvertToTime(int YR, int MM, int DD, int32 D,
                                 int H, int M, int S, int MS, int32 US,
								 int32 NS, int32 Sn, int32 Sd,
					  			 double d, double h, double m, double s,
								 double ms, double us, double ns,
								 ESMC_Time *T)
{
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
			T->Write_S(jdays * SecondsPerDay);
			break;
		}
		// convert Julian Date => Time
		case ESMC_JULIAN:
		{
			T->Write_S(D * SecondsPerDay);
			break;
		}
		default:
			break;
	}

	return(ESMC_SUCCESS);
}

// conversions based on UTC: time zone offset done by client
int ESMC_Calendar::ConvertToDate(ESMC_Time *T,
								 int *YR, int *MM, int *DD, int32 *D, int *H,
 								 int *M, int *S, int *MS, int32 *US, int32 *NS,
								 int32 *Sn, int32 *Sd, double *d, double *h,
								 double *m, double *s, double *ms)
{
	int64 TimeS;

	T->Read_S(&TimeS);

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
}
