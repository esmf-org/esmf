// $Id: ESMC_TimeInterval.h,v 1.47.2.3 2009/01/21 21:25:23 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research,
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

#ifndef ESMC_TIME_INTERVAL_H
#define ESMC_TIME_INTERVAL_H

//-------------------------------------------------------------------------

 // Put any constants or macros which apply to the whole component in this file.
 // Anything public or esmf-wide should be up higher at the top level
 // include files.
#include "ESMC_Start.h"
#include "ESMF_TimeMgr.inc"
#include "ESMC_Fraction.h"
class ESMC_TimeInterval;

//-------------------------------------------------------------------------
//BOP
//
// !CLASS:  ESMC_TimeInterval - represents a time interval
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
//   appropriate unit combinations, as per the requirements.  Usually, the
//   largest resolution of time for a {\tt TimeInterval} is in days, making it
//   independent of any calendar.  A {\tt TimeInterval} can also be used as a
//   {\tt Calendar} interval. Then it becomes calendar-dependent, since its
//   largest resolution of time will be in months and years.  
//   {\tt TimeInterval} also defines methods for multiplication and division
//   of {\tt TimeIntervals} by integers, reals, fractions and other
//   {\tt TimeIntervals}.  {\tt TimeInterval} defines methods for absolute
//   value and negative absolute value for use with both positive or
//   negative time intervals. 
//
//   Notes:
//       - For arithmetic consistency both whole seconds and the numerator of
//         fractional seconds must carry the same sign (both positve or both 
//         negative), except, of course, for zero values.
//       - fractional math should be handled by an open-source package if
//         available (see {\tt ESMC\_BaseTime.h} also)
//
//-------------------------------------------------------------------------

// !USES:
#include "ESMC_Base.h"           // inherited Base class
#include "ESMC_BaseTime.h"       // inherited BaseTime class
#include "ESMC_Time.h"

enum ESMC_ComparisonType {ESMC_EQ, ESMC_NE,
                          ESMC_LT, ESMC_GT,
                          ESMC_LE, ESMC_GE};

enum ESMC_AbsValueType {ESMC_POSITIVE_ABS, ESMC_NEGATIVE_ABS};

// !PUBLIC TYPES:
 class ESMC_TimeInterval;

// !PRIVATE TYPES:

 // class definition type
class ESMC_TimeInterval : public ESMC_BaseTime { 
                                             // inherits ESMC_BaseTime
                                             // TODO: (& ESMC_Base class when
                                             // fully aligned with F90 equiv)
  private:
    ESMC_Time startTime;  // for absolute calendar intervals
    ESMC_Time endTime;    // for absolute calendar intervals
    ESMC_Calendar *calendar;  // for calendar intervals on a 
                              //   specific calendar
    ESMC_I8 yy;      // for relative Calendar intervals:  number of years
    ESMC_I8 mm;      // for relative Calendar intervals:  number of months
    ESMC_I8 d;       // for relative Calendar intervals:  number of days

// !PUBLIC MEMBER FUNCTIONS:

  public:

    // accessor methods

    // all get/set routines perform signed conversions, where applicable;
    //   direct, one-to-one access to core time elements is provided by the
    //   ESMC_BaseTime base class

    // Get/Set methods to support the F90 optional arguments interface
    int ESMC_TimeIntervalSet(ESMC_I4 *yy=0, ESMC_I8 *yy_i8=0,
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
                             ESMC_I4 *sN=0, ESMC_I4 *sD=0,
                             ESMC_Time *startTime=0, ESMC_Time *endTime=0,
                             ESMC_Calendar **calendar=0,
                             ESMC_CalendarType *calendarType=0);

    int ESMC_TimeIntervalGet(ESMC_I4 *yy=0, ESMC_I8 *yy_i8=0,
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
                             ESMC_I4 *sN=0, ESMC_I4 *sD=0,
                             ESMC_Time *startTime=0, ESMC_Time *endTime=0,
                             ESMC_Calendar **calendar=0,
                             ESMC_CalendarType *calendarType=0,
                             ESMC_Time *startTimeIn=0, ESMC_Time *endTimeIn=0,
                             ESMC_Calendar **calendarIn=0,
                             ESMC_CalendarType *calendarTypeIn=0,
                             int timeStringLen=0, int *tempTimeStringLen=0,
                             char *tempTimeString=0,
                             int timeStringLenISOFrac=0,
                             int *tempTimeStringLenISOFrac=0,
                             char *tempTimeStringISOFrac=0) const;

    // native C++ interface -- via variable argument lists
    //   corresponds to F90 named-optional-arguments interface

    int ESMC_TimeIntervalSet(const char *timeList, ...);
    // e.g. Set("s" , (ESMC_R8) s);

    // (TMG 1.1)
    int ESMC_TimeIntervalGet(const char *timeList, ...) const;
    // e.g. Get("D:S",(int *)D, (int *)S);

    // return positive value (TMG 1.5.8)
    ESMC_TimeInterval ESMC_TimeIntervalAbsValue(void) const;

    // return negative value (TMG 1.5.8)
    ESMC_TimeInterval ESMC_TimeIntervalNegAbsValue(void) const;

    // subdivision (TMG 1.5.6, 5.3, 7.2)
    ESMC_TimeInterval  operator/ (const ESMC_I4 &) const;
    ESMC_TimeInterval& operator/=(const ESMC_I4 &);
    ESMC_TimeInterval  operator/ (const ESMC_R8 &) const;
    ESMC_TimeInterval& operator/=(const ESMC_R8 &);

    // division (TMG 1.5.5)
    ESMC_Fraction ESMC_TimeIntervalDiv(const ESMC_TimeInterval &) const;
    ESMC_R8 operator/(const ESMC_TimeInterval &) const;

    // modulus
    ESMC_TimeInterval  operator% (const ESMC_TimeInterval &) const;
    ESMC_TimeInterval& operator%=(const ESMC_TimeInterval &);

    // multiplication (TMG 1.5.7, 7.2)
    ESMC_TimeInterval  operator* (const ESMC_I4 &) const;
    ESMC_TimeInterval& operator*=(const ESMC_I4 &);
    ESMC_TimeInterval  operator* (const ESMC_Fraction &) const;
    ESMC_TimeInterval& operator*=(const ESMC_Fraction &);
    ESMC_TimeInterval  operator* (const ESMC_R8 &) const;
    ESMC_TimeInterval& operator*=(const ESMC_R8 &);

    // addition, subtraction
    ESMC_TimeInterval operator+(const ESMC_TimeInterval &) const;
    ESMC_TimeInterval operator-(const ESMC_TimeInterval &) const;

    // unary negation
    ESMC_TimeInterval operator-(void) const;

    // comparison methods (TMG 1.5.3, 7.2)
    bool operator==(const ESMC_TimeInterval &) const;
    bool operator!=(const ESMC_TimeInterval &) const;
    bool operator< (const ESMC_TimeInterval &) const;
    bool operator> (const ESMC_TimeInterval &) const;
    bool operator<=(const ESMC_TimeInterval &) const;
    bool operator>=(const ESMC_TimeInterval &) const;

    // copy or assign from ESMC_Fraction expressions, supports Time1-Time2
    // operator in ESMC_Time.
    // TODO:  should be implicit ?
    ESMC_TimeInterval& operator=(const ESMC_Fraction &);

    // required methods inherited and overridden from the ESMC_Base class

    // for persistence/checkpointing
    int ESMC_TimeIntervalReadRestart(int nameLen, const char *name=0,
                                     ESMC_IOSpec *iospec=0);
    int ESMC_TimeIntervalWriteRestart(ESMC_IOSpec *iospec=0) const;

    // internal validation (TMG 7.1.1)
    int ESMC_TimeIntervalValidate(const char *options=0) const;

    // for testing/debugging
    int ESMC_TimeIntervalPrint(const char *options=0) const;

    // native C++ constructors/destructors
    ESMC_TimeInterval(void);
    ESMC_TimeInterval(ESMC_I8 s, int sN=0, int sD=1,
                      ESMC_I8 yy=0, ESMC_I8 mm=0, ESMC_I8 d=0,
                      ESMC_Time *startTime=0, ESMC_Time *endTime=0,
                      ESMC_Calendar *calendar=0,
                      ESMC_CalendarType calendarType=(ESMC_CalendarType)0);
    int ESMC_TimeIntervalSet(ESMC_I8 s, int sN=0, int sD=1,
                      ESMC_I8 yy=0, ESMC_I8 mm=0, ESMC_I8 d=0,
                      ESMC_Time *startTime=0, ESMC_Time *endTime=0,
                      ESMC_Calendar *calendar=0,
                      ESMC_CalendarType calendarType=(ESMC_CalendarType)0);
                                   // used internally instead of constructor
                                   // to cover case of initial entry from F90,
                                   // to avoid automatic destructor invocation
                                   // when leaving scope to return to F90.

    ~ESMC_TimeInterval(void);

 // < declare the rest of the public interface methods here >

    // commutative complements to ESMC_TimeInterval class member overloaded
    //   "*" operators
    friend ESMC_TimeInterval
                   operator* (const ESMC_I4 &, const ESMC_TimeInterval &);
    friend ESMC_TimeInterval
                   operator* (const ESMC_Fraction &, const ESMC_TimeInterval &);
    friend ESMC_TimeInterval
                   operator* (const ESMC_R8 &, const ESMC_TimeInterval &);

// !PRIVATE MEMBER FUNCTIONS:
//
  private:
    // return in string format (TMG 1.5.9)
    int ESMC_TimeIntervalGetString(char *timeString, 
                                   const char *options=0) const;

    // common method for overloaded comparison operators
    bool ESMC_TimeIntervalCompare(const ESMC_TimeInterval &,
                                  ESMC_ComparisonType) const;

    // common method for positive and negative absolute value
    ESMC_TimeInterval ESMC_TimeIntervalAbsValue(ESMC_AbsValueType) const;

    // reduce time interval to smallest and least number of units
    int ESMC_TimeIntervalReduce(void);

    friend class ESMC_Time;
    friend class ESMC_Calendar;
                                                        // (TMG 2.5.5)
//
 // < declare private interface methods here >
//
//EOP
//-------------------------------------------------------------------------

};

#endif // ESMC_TIME_INTERVAL_H
