// $Id: ESMC_TimeInstant.h,v 1.2 2002/10/10 18:48:37 eschwab Exp $
//
// ESMF TimeInstant C++ definition include file
//
// < Something here from legal about the status of the code, like:
//  This code developed by NASA/NCAR/ESMF whatever, and is covered by
//  the terms of the GNU public license.  See license file for more details. >
//
// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMC_TIME_INSTANT_H
#define ESMC_TIME_INSTANT_H

//-------------------------------------------------------------------------
//
// !PURPOSE:
//
// The code in this file defines the C++ TimeInstant members and method
// signatures (prototypes).  The companion file ESMC_TimeInstant.C contains
// the full code (bodies) for the TimeInstant methods.
//
// A TimeInstant inherits from the Time base class and is designed to represent
// a specific point in time which is dependent upon a calendar type.
//
// TimeInstant inherits from the base class Time.  As such, it gains the core
// representation of time as well as its associated methods.   TimeInstant
// further specializes Time by adding shortcut methods to set and get a
// TimeInstant in a natural way with appropriate unit combinations, as per the
// requirements.  A TimeInstant is calendar-dependent, since its largest units
// of time are months and years.  TimeInstant also defines special methods for
// getting the day of the year, day of the week, middle of the month, and
// synchronizing with a real-time clock.  TimeInstant adds its associated
// Calendar and local Timezone attributes to Time.
//
// A time value of zero (both whole and numerator) will correspond
// to the Julian date of zero UTC.  This will ease conversions 
// between Julian and Gregorian calendars.
//
// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//
//-------------------------------------------------------------------------

// put any constants or macros which apply to the whole component in this file
#include <ESMC_TimeMgr.h>
#include <ESMC_Types.h>

//-------------------------------------------------------------------------
//BOP
// !CLASS: ESMC_TimeInstant

// this include section corresponds to the USES: section in F90 modules
#include <ESMC_Time.h>           // inherited Time class
#include <ESMC_Calendar.h>       // associated Calendar class

// ! PUBLIC TYPES:

// class configuration type:  not needed for TimeInstant

// class definition type
class ESMC_TimeInstant : public ESMC_Time  // inherits ESMC_Time & Base classes
{
	friend class ESMC_Calendar;

  private:   // corresponds to F90 module 'type ESMF_TimeInstant' members 
    ESMC_Calendar *Calendar;    // associated calendar
	int Timezone;				// Offset from GMT

// !PUBLIC MEMBER FUNCTIONS:

  public:
	// native C++ constructors/destructors
    ESMC_TimeInstant(void);
	ESMC_TimeInstant(int64 S, int32 Sn, int32 Sd, ESMC_Calendar *Cal,
					 int Timezone);
    ~ESMC_TimeInstant(void);

    // TimeInstant is a shallow class, so only Init methods are needed
	int ESMC_TimeInstInit(int64 S, int32 Sn, int32 Sd,
                  ESMC_Calendar *Cal, int Timezone);
    int ESMC_TimeInstInit(const char *TimeList, ...);

    // TimeInstant doesn't need configuration, hence GetConfig/SetConfig
    // methods are not required

    // accessor methods

    // all get/set routines perform signed conversions, where applicable;
    //   direct, one-to-one access to core time elements is provided by the
    //   ESMC_Time base class

    int ESMC_TimeInstGetCalendar(ESMC_Calendar *Calendar);	// (TMG 2.5.1)
    int ESMC_TimeInstSetCalendar(ESMC_Calendar  Calendar);

    int ESMC_TimeInstGetTimeZone(int *Timezone);  // (TMG 2.5.1)
    int ESMC_TimeInstSetTimeZone(int  Timezone);

    // shortcut interfaces (TMG 2.1, 2.4.1, 2.5.1)
    int ESMC_TimeInstGetYR_MM_DD_S(int32 *YR, int *MM, int *DD, int *S);
    int ESMC_TimeInstSetYR_MM_DD_S(int32  YR, int  MM, int  DD, int  S);

    int ESMC_TimeInstGetYR_MM_DD_H_M_S(int32 *YR, int *MM, int *DD,
                                       int   *H,  int *M,  int *S);
    int ESMC_TimeInstSetYR_MM_DD_H_M_S(int32  YR, int  MM, int  DD,
                                       int    H,  int  M,  int  S);

    // generic interface -- via variable argument lists
    //   can map to F90 named-optional-arguments interface
	// (TMG 2.1, 2.5.1)
    int ESMC_TimeInstGet(const char *TimeList, ...);
    // e.g. ESMC_TimeInstGet("YR:MM:DD", (int *)YR,(int *)MM, (int *)DD);

    int ESMC_TimeInstSet(const char *TimeList, ...);
    // e.g. ESMC_TimeInstSet("s" , (double) s);

    int ESMC_TimeInstGetDayOfYear(double *DayOfYear);	// (TMG 2.5.2)

    int ESMC_TimeInstGetDayOfWeek(int *DayOfWeek);	    // (TMG 2.5.3)

    int ESMC_TimeInstGetMiddleOfMonth(ESMC_TimeInstant *MiddleOfMonth);
                                                        // (TMG 2.5.4)
    // return in string format (TMG 2.4.7)
    int ESMC_TimeInstGetString(char *Ts);

    // standalone method, not class method
    //  (see ESMC_Clock::SyncToWallClock() )
    int ESMC_TimeInstGetRealTime(ESMC_TimeInstant *RealTime);	// (TMG 2.5.7)

    // required methods inherited and overridden from the ESMC_Base class

    // internal validation
	int ESMC_TimeInstValidate(const char *options);

	// for persistence/checkpointing
	int ESMC_TimeInstPrint(int64 *S, int32 *Sn, int32 *Sd);

	// for testing/debugging
	int ESMC_TimeInstPrint(void);

// < list the rest of the public interface methods here >

//EOP
//-------------------------------------------------------------------------

  private:
//
// < list private interface methods here >
//

};   // end class ESMC_TimeInstant

#endif // ESMC_TIME_INSTANT_H
