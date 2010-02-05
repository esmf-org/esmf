// $Id: ESMCI_TimeInterval.h,v 1.14.2.1 2010/02/05 20:00:07 svasquez Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
// ESMF TimeInterval C++ definition include file
//
// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
//
 // these lines prevent this file from being read more than once if it
 // ends up being included multiple times

#ifndef ESMCI_TIME_INTERVAL_H
#define ESMCI_TIME_INTERVAL_H

//-------------------------------------------------------------------------

 // Put any constants or macros which apply to the whole component in this file.
 // Anything public or esmf-wide should be up higher at the top level
 // include files.
#include "ESMC_Start.h"
#include "ESMF_TimeMgr.inc"
#include "ESMCI_Fraction.h"

//-------------------------------------------------------------------------
//BOP
//
// !CLASS:  ESMCI::TimeInterval - represents a time interval
//
// !DESCRIPTION:
//   A {\tt TimeInterval} inherits from the {\tt BaseTime} base class and is
//   designed to represent time deltas. These can either be independent of
//   any calendar or dependent on a calendar and thought of as a calendar
//   interval. 
//
//   {\tt TimeInterval} inherits from the base class {\tt BaseTime}.  As such,
//   it gains the core representation of time as well as its associated methods.
//   {\tt TimeInterval} further specializes {\tt BaseTime} by adding shortcut
//   methods to set and get a {\tt TimeInterval} in a natural way with 
//   appropriate unit combinations, as per the requirements.  The largest
//   calendar-independent resolution of time for a {\tt TimeInterval} is in
//   hours.  A {\tt TimeInterval} can also be used as a {\tt Calendar} interval.
//   Then it becomes calendar-dependent, since its largest resolution of time
//   will be in days, months and years.  Days are calendar-specific to allow
//   for non-Earth specific calendars.  {\tt TimeInterval} also defines methods
//   for multiplication and division of {\tt TimeIntervals} by integers, reals,
//   fractions and other {\tt TimeIntervals}.  {\tt TimeInterval} defines
//   methods for absolute value and negative absolute value for use with both
//   positive or negative time intervals. 
//
//   Notes:
//       - For arithmetic consistency both whole seconds and the numerator of
//         fractional seconds must carry the same sign (both positve or both 
//         negative), except, of course, for zero values.
//
//-------------------------------------------------------------------------

// !USES:
#include "ESMC_Base.h"           // inherited Base class
#include "ESMCI_BaseTime.h"       // inherited BaseTime class
#include "ESMCI_Time.h"
#include "ESMCI_Calendar.h"
#include "ESMC_TimeInterval.h"    // for use of enumerated types

namespace ESMCI {

// PUBLIC TYPES:
class TimeInterval;

// !PRIVATE TYPES:

 // class definition type
class TimeInterval : public BaseTime { 
                                             // inherits BaseTime
                                             // TODO: (& ESMC_Base class when
                                             // fully aligned with F90 equiv)
  private:
    Time startTime;  // for absolute calendar intervals
    Time endTime;    // for absolute calendar intervals
    Calendar *calendar;  // for calendar intervals on a 
                              //   specific calendar
    ESMC_I8 yy;      // for relative Calendar intervals:  number of years
    ESMC_I8 mm;      // for relative Calendar intervals:  number of months
    ESMC_I8 d;       // for relative Calendar intervals:  integer number of days
    ESMC_R8 d_r8;    // for relative Calendar intervals:  real number of days

// !PUBLIC MEMBER FUNCTIONS:

  public:

    // accessor methods

    // all get/set routines perform signed conversions, where applicable;
    //   direct, one-to-one access to core time elements is provided by the
    //   BaseTime base class

    // Get/Set methods to support the F90 optional arguments interface
    int set(ESMC_I4 *yy=0, ESMC_I8 *yy_i8=0,
                             ESMC_I4 *mm=0, ESMC_I8 *mm_i8=0,
                             ESMC_I4 *d=0,  ESMC_I8 *d_i8=0,
                             ESMC_I4 *h=0,  ESMC_I4 *m=0,
                             ESMC_I4 *s=0,  ESMC_I8 *s_i8=0,
                             ESMC_I4 *ms=0, ESMC_I4 *us=0,
                             ESMC_I4 *ns=0,
                             ESMC_R8 *d_r8=0,  ESMC_R8 *h_r8=0,
                             ESMC_R8 *m_r8=0,  ESMC_R8 *s_r8=0,
                             ESMC_R8 *ms_r8=0, ESMC_R8 *us_r8=0,
                             ESMC_R8 *ns_r8=0,
                             ESMC_I4 *sN=0, ESMC_I8 *sN_i8=0,
                             ESMC_I4 *sD=0, ESMC_I8 *sD_i8=0,
                             Time *startTime=0, Time *endTime=0,
                             Calendar **calendar=0,
                             ESMC_CalendarType *calendarType=0);

    int get(ESMC_I4 *yy=0, ESMC_I8 *yy_i8=0,
                             ESMC_I4 *mm=0, ESMC_I8 *mm_i8=0,
                             ESMC_I4 *d=0,  ESMC_I8 *d_i8=0,
                             ESMC_I4 *h=0,  ESMC_I4 *m=0,
                             ESMC_I4 *s=0,  ESMC_I8 *s_i8=0,
                             ESMC_I4 *ms=0, ESMC_I4 *us=0,
                             ESMC_I4 *ns=0,
                             ESMC_R8 *d_r8=0,  ESMC_R8 *h_r8=0,
                             ESMC_R8 *m_r8=0,  ESMC_R8 *s_r8=0,
                             ESMC_R8 *ms_r8=0, ESMC_R8 *us_r8=0,
                             ESMC_R8 *ns_r8=0,
                             ESMC_I4 *sN=0, ESMC_I8 *sN_i8=0,
                             ESMC_I4 *sD=0, ESMC_I8 *sD_i8=0,
                             Time *startTime=0, Time *endTime=0,
                             Calendar **calendar=0,
                             ESMC_CalendarType *calendarType=0,
                             Time *startTimeIn=0, Time *endTimeIn=0,
                             Calendar **calendarIn=0,
                             ESMC_CalendarType *calendarTypeIn=0,
                             int timeStringLen=0, int *tempTimeStringLen=0,
                             char *tempTimeString=0,
                             int timeStringLenISOFrac=0,
                             int *tempTimeStringLenISOFrac=0,
                             char *tempTimeStringISOFrac=0) const;

    // native C++ interface -- via variable argument lists
    //   corresponds to F90 named-optional-arguments interface

    //int set(const char *timeList, ...);
    // e.g. Set("s" , (ESMC_R8) s);

    // (TMG 1.1)
    //int get(const char *timeList, ...) const;
    // e.g. Get("D:S",(int *)D, (int *)S);

    // return positive value (TMG 1.5.8)
    TimeInterval absValue(void) const;

    // return negative value (TMG 1.5.8)
    TimeInterval negAbsValue(void) const;

    // subdivision (TMG 1.5.6, 5.3, 7.2)
    TimeInterval  operator/ (const ESMC_I4 &) const;
    TimeInterval& operator/=(const ESMC_I4 &);
    TimeInterval  operator/ (const ESMC_R8 &) const;
    TimeInterval& operator/=(const ESMC_R8 &);

    // division (TMG 1.5.5)
    Fraction div(const TimeInterval &) const;
    ESMC_R8 operator/(const TimeInterval &) const;

    // modulus
    TimeInterval  operator% (const TimeInterval &) const;
    TimeInterval& operator%=(const TimeInterval &);

    // multiplication (TMG 1.5.7, 7.2)
    TimeInterval  operator* (const ESMC_I4 &) const;
    TimeInterval& operator*=(const ESMC_I4 &);
    TimeInterval  operator* (const Fraction &) const;
    TimeInterval& operator*=(const Fraction &);
    TimeInterval  operator* (const ESMC_R8 &) const;
    TimeInterval& operator*=(const ESMC_R8 &);

    // addition, subtraction
    TimeInterval operator+(const TimeInterval &) const;
    TimeInterval operator-(const TimeInterval &) const;

    // unary negation
    TimeInterval operator-(void) const;

    // comparison methods (TMG 1.5.3, 7.2)
    bool operator==(const TimeInterval &) const;
    bool operator!=(const TimeInterval &) const;
    bool operator< (const TimeInterval &) const;
    bool operator> (const TimeInterval &) const;
    bool operator<=(const TimeInterval &) const;
    bool operator>=(const TimeInterval &) const;

    // copy or assign from ESMCI::Fraction expressions, supports Time1-Time2
    // operator in Time.
    // TODO:  should be implicit ?
    TimeInterval& operator=(const Fraction &);

    // required methods inherited and overridden from the ESMC_Base class

    // for persistence/checkpointing
    int readRestart(int nameLen, const char *name=0, ESMC_IOSpec *iospec=0);
    int writeRestart(ESMC_IOSpec *iospec=0) const;

    // internal validation (TMG 7.1.1)
    int validate(const char *options=0) const;

    // for testing/debugging
    int print(const char *options=0) const;

    // native C++ constructors/destructors
    TimeInterval(void);
    TimeInterval(ESMC_I8 s, ESMC_I8 sN=0, ESMC_I8 sD=1,
                 ESMC_I8 yy=0, ESMC_I8 mm=0, ESMC_I8 d=0, ESMC_R8 d_r8=0.0,
                 Time *startTime=0, Time *endTime=0,
                 Calendar *calendar=0,
                 ESMC_CalendarType calendarType=(ESMC_CalendarType)0);
    int set(ESMC_I8 s, ESMC_I8 sN=0, ESMC_I8 sD=1,
            ESMC_I8 yy=0, ESMC_I8 mm=0, ESMC_I8 d=0, ESMC_R8 d_r8=0.0,
            Time *startTime=0, Time *endTime=0,
            Calendar *calendar=0,
            ESMC_CalendarType calendarType=(ESMC_CalendarType)0);
                                   // used internally instead of constructor
                                   // to cover case of initial entry from F90,
                                   // to avoid automatic destructor invocation
                                   // when leaving scope to return to F90.

    ~TimeInterval(void);

 // < declare the rest of the public interface methods here >

    // commutative complements to TimeInterval class member overloaded
    //   "*" operators
    friend TimeInterval operator* (const ESMC_I4 &,  const TimeInterval &);
    friend TimeInterval operator* (const Fraction &, const TimeInterval &);
    friend TimeInterval operator* (const ESMC_R8 &,  const TimeInterval &);

// !PRIVATE MEMBER FUNCTIONS:
//
  private:
    // return in string format (TMG 1.5.9)
    int getString(char *timeString, const char *options=0) const;

    // common method for overloaded comparison operators
    bool compare(const TimeInterval &, ESMC_ComparisonType) const;

    // common method for positive and negative absolute value
    TimeInterval absValue(ESMC_AbsValueType) const;

    // reduce time interval to smallest and least number of units
    int reduce(void);

    friend class Time;
    friend class Calendar;
                                                        // (TMG 2.5.5)
//
 // < declare private interface methods here >
//
//EOP
//-------------------------------------------------------------------------

};     // end class TimeInterval

}      // end namespace ESMCI

#endif // ESMCI_TIME_INTERVAL_H
