// $Id: ESMC_Calendar.h,v 1.3 2002/10/07 18:56:36 eschwab Exp $
#ifndef ESMC_CALENDAR_H
#define ESMC_CALENDAR_H

class ESMC_TimeInstant;

#define MonthsPerYear 12

// (TMG 2.3.1, 2.3.2, 2.3.3, 2.3.4, 2.3.5)
typedef enum ESMC_CalendarType {ESMC_GREGORIAN=1, ESMC_JULIAN,  ESMC_NOLEAP,
                                ESMC_360DAY,      ESMC_GENERIC, ESMC_NOCALENDAR}
             ESMC_CalendarType_e;

class ESMC_Calendar
{
//-------------------------------------------------------------------------
//BOP
//
// !CLASS: ESMC_Calendar
//
// !SUPERCLASSES:
//
// !AGGREGATE CLASSES:
//
// !ASSOCIATE CLASSES:
//
// !FRIEND CLASSES:
//
// !PUBLIC DATA MEMBERS:
//
// !PUBLIC MEMBER FUNCTIONS:
  public:

    ESMC_Calendar(void);
	ESMC_Calendar(ESMC_CalendarType_e Type);
	ESMC_Calendar(int *DaysPerMonth, int SecondsPerDay,
               	  int DaysPerYear,   int DaysPerYearDn, int DaysPerYearDd);
    ~ESMC_Calendar(void);

    int Init(ESMC_CalendarType_e Type);
    int InitGeneric(int *DaysPerMonth, int SecondsPerDay,
                    int DaysPerYear,   int DaysPerYearDn, int DaysPerYearDd);

	// for testing/debugging
    int Dump(ESMC_CalendarType_e *Type,
			 int *DaysPerMonth,  int *SecondsPerDay,
             int *DaysPerYear,   int *DaysPerYearDn, int *DaysPerYearDd);

	// for testing/debugging
    int Dump(void);

    // conversions based on UTC: time zone offset done by client
	//  (TMG 2.4.5, 2.5.6)
    int ConvertToTime(int YR, int MM, int DD, int32 D, int H, int M, int S,
					  int MS, int32 US, int32 NS, int32 Sn, int32 Sd,
					  double d, double h, double m, double s, double ms,
					  double us, double ns, ESMC_Time *T);
    int ConvertToDate(ESMC_Time *T,
					  int *YR, int *MM, int *DD, int32 *D, int *H, int *M,
					  int *S, int *MS, int32 *US, int32 *NS, int32 *Sn,
					  int32 *Sd, double *d, double *h, double *m, double *s,
					  double *ms);

// !DESCRIPTION:
//    - Instantiate as few as possilbe; ideally no more than one calendar
//      type per application (for reference only, like a wall calendar)
//      But may have multiples for convienience such as one per component.
//    - Generic enough to define for any planetary body
//    - if SecondsPerDay != 86400, then how are minutes and hours defined ??
//      Assume always minute=60 seconds; hour=3600 seconds
//
// !BUGS:
//
// !SEE ALSO:
//
// !REVISION HISTORY:
//
//  10Jun02   Earl Schwab  Initial code.
//
//EOP
//-------------------------------------------------------------------------
  private:

    ESMC_CalendarType_e Type;    // Calendar type

    int DaysPerMonth[MonthsPerYear+1];
    int SecondsPerDay;
    struct DaysPerYear_s
    {
        int D;        // integer number of days per year
        int Dn;       // fractional number of days per year (numerator)
        int Dd;       //                                    (denominator)
    } DaysPerYear;    // e.g. for Venus, D=0, Dn=926, Dd=1000

};

#endif // ESMC_CALENDAR_H
