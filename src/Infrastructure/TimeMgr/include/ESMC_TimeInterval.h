// $Id: ESMC_TimeInterval.h,v 1.3 2002/10/15 03:27:37 eschwab Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the GPL.
//
// ESMF TimeInterval C++ definition include file
//
// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
//
 // these lines prevent this file from being read more than once if it
 // ends up being included multiple times

#ifndef ESMC_TIME_INTERVAL_H
#define ESMC_TIME_INTERVAL_H

//-------------------------------------------------------------------------

 // Put any constants or macros which apply to the whole component in this file.
 // Anything public or esmf-wide should be up higher at the top level
 // include files.
#include <ESMC_TimeMgr.h>
#include <ESMC_Types.h>
#include <ESMC_Fraction.h>

//-------------------------------------------------------------------------
//BOP
//
// !CLASS:  ESMC_TimeInterval - represents a time interval
//
// !DESCRIPTION:
//   A TimeInterval inherits from the Time base class and is designed to
//   represent time deltas which are independent of any calendar.
//
//   TimeInterval inherits from the base class Time.  As such, it gains the core
//   representation of time as well as its associated methods.   TimeInterval
//   further specializes Time by adding shortcut methods to set and get a
//   TimeInterval in natural way with appropriate unit combinations, as per the
//   requirements.  The largest unit of time for a TimeInterval is a day, so a
//   TimeInterval is independent of any calendar.  This is in contrast with a
//   TimeInstant, which is calendar-dependent, since its largest units of time
//   are months and years.  TimeInterval also defines methods for multiplication
//   and division of TimeIntervals by integers, reals, fractions and other
//   TimeIntervals.  TimeInterval defines methods for absolute value and
//   negative absolute value for use with both positive or negative time
//   intervals.  TimeInterval does not add any new attributes to Time.
//   Calendar intervals are dependent on a calendar and so represent a
//   specialized case of a TimeInterval.  A derived class CalendarInterval
//   will be defined to inherit from TimeInterval and specialize it for
//   use with Calendars.
//
//   Notes:
//       - For arithmetic consistency both whole seconds and the numerator of
//         fractional seconds must carry the same sign (both positve or both 
//         negative), except, of course, for zero values.
//       - fractional math should be handled by an open-source package if
//         available (see ESMC\_Time.h also)
//       - Calendar intervals are dependent on a calendar and so represent
//         a specialized case of a TimeInterval.  A derived class
//         CalendarInterval will be defined to inherit from TimeInterval
//         and specialize it for use with Calendars.
//
//-------------------------------------------------------------------------

// !USES:
//#include <ESMC_Base.h>           // inherited Base class ??
 #include <ESMC_Time.h>           // inherited Time class

// !PUBLIC TYPES:
 class ESMC_TimeInterval;

// !PRIVATE TYPES:
 // class configuration type:  not needed for TimeInterval

 // class definition type
class ESMC_TimeInterval : public ESMC_Time {  // inherits ESMC_Time & Base
                                              // classes ??
  private:
	// inherited from ESMC_Time

// !PUBLIC MEMBER FUNCTIONS:

  public:

    // TimeInterval is a shallow class, so only Init methods are needed
	int ESMC_TimeIntvInit(int64 S, int32 Sn, int32 Sd);
    int ESMC_TimeIntvInit(const char *TimeList, ...);

    // TimeInstant doesn't need configuration, hence GetConfig/SetConfig
    // methods are not required

	// accessor methods

    // all get/set routines perform signed conversions, where applicable;
    //   direct, one-to-one access to core time elements is provided by the
    //   ESMC_Time base class

    // generic interface -- via variable argument lists
    //   can map to F90 named-optional-arguments interface
    //   adv:     flexible -- can specify any combination w/o source changes
    //            elegant -- only list those time items needed
    //   disadv:  parsing overhead, but limited to just those items specified
    //            no arg type checking -- user may pass-in bad args.
	//   

	// (TMG 1.1)
    int ESMC_TimeIntvGet(const char *TimeList, ...);
    // e.g. Get("D:S",(int *)D, (int *)S);

    int ESMC_TimeIntvSet(const char *TimeList, ...);
    // e.g. Set("s" , (double) s);

    // -- AND/OR -- individual/combo get/set
    //   adv:    fastest -- no parsing
    //             args type checked
    //   disadv: limited combinations, must modify source to add more

    // shortcut interfaces (TMG 1.1, 1.2, 1.5.1)
    int ESMC_TimeIntvGet_S_nd(int64 *S, int32 *Sn, int32 *Sd);
    int ESMC_TimeIntvSet_S_nd(int64  S, int32  Sn, int32  Sd);

    int ESMC_TimeIntvGet_D_S(int32 *D, int *S);
    int ESMC_TimeIntvSet_D_S(int32  D, int  S);

    int ESMC_TimeIntvGet_D_H_M_S_MS(int32 *D, int *H, int *M, int *S, int *MS);
    int ESMC_TimeIntvSet_D_H_M_S_MS(int32  D, int  H, int  M, int  S, int  MS);

    // division (TMG 1.5.5)
     // return fraction _nd ??
    ESMC_Fraction& operator/(ESMC_TimeInterval &);

    // subdivision (TMG 1.5.6, 5.3, 7.2)
    ESMC_TimeInterval& operator/=(int &);
    ESMC_TimeInterval& operator/ (int &);

    // multiplication (TMG 1.5.7, 7.2)
    ESMC_TimeInterval& operator*=(int &);
    ESMC_TimeInterval& operator* (int &);
    ESMC_TimeInterval& operator*=(ESMC_Fraction &);
    ESMC_TimeInterval& operator* (ESMC_Fraction &);
    ESMC_TimeInterval& operator*=(double &);
    ESMC_TimeInterval& operator* (double &);

    // return in string format (TMG 1.5.9)
    int ESMC_TimeIntvGetString(char *Ts);

    // return positive value (TMG 1.5.8)
    ESMC_TimeInterval *ESMC_TimeIntvGetAbsValue(ESMC_TimeInterval *);

    // return negative value (TMG 1.5.8)
    ESMC_TimeInterval *ESMC_TimeIntvGetNegAbsValue(ESMC_TimeInterval *);

    // required methods inherited and overridden from the ESMC_Base class

    // internal validation
    int ESMC_TimeIntvValidate(const char *options) const;

    // for persistence/checkpointing
    int ESMC_TimeIntvPrint(int64 *S, int32 *Sn, int32 *Sd) const;

    // for testing/debugging
    int ESMC_TimeIntvPrint(void) const;

    // native C++ constructors/destructors
    ESMC_TimeInterval(void);
	ESMC_TimeInterval(int64 S, int32 Sn, int32 Sd);
    ~ESMC_TimeInterval(void);

 // < declare the rest of the public interface methods here >

// !PRIVATE MEMBER FUNCTIONS:
//
  private:
//
 // < declare private interface methods here >
//
//EOP
//-------------------------------------------------------------------------

};

#endif // ESMC_TIME_INTERVAL_H
