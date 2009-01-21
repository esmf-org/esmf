// $Id: ESMC_Time.h,v 1.47.2.3 2009/01/21 21:25:23 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
// ESMF Time C++ definition include file
//
// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
//
 // these lines prevent this file from being read more than once if it
 // ends up being included multiple times

#ifndef ESMC_TIME_H
#define ESMC_TIME_H

//-------------------------------------------------------------------------

 // put any constants or macros which apply to the whole component in this file.
 // anything public or esmf-wide should be up higher at the top level
 // include files.
#include "ESMC_Start.h"
#include "ESMF_TimeMgr.inc"

//-------------------------------------------------------------------------
//BOP
// !CLASS: ESMC_Time - represents a specific point in time
//
// !DESCRIPTION:
//
// The code in this file defines the C++ {\tt Time} members and method
// signatures (prototypes).  The companion file {\tt ESMC\_Time.C} contains
// the full code (bodies) for the {\tt Time] methods.
//
// A {\tt Time} inherits from the {\tt BaseTime} base class and is designed
// to represent a specific point in time which is dependent upon a calendar
// type.
//
// {\tt Time} inherits from the base class {\tt BaseTime}.  As such, it gains
// the core representation of time as well as its associated methods.
// {\tt Time} further specializes {\tt BaseTime} by adding shortcut methods
// to set and get a {\tt Time} in a natural way with appropriate unit
// combinations, as per the requirements.  A {\tt Time} is calendar-dependent,
// since its largest units of time are months and years.  {\tt Time} also
// defines special methods for getting the day of the year, day of the week,
// middle of the month, and synchronizing with a real-time clock.  {\tt Time}
// adds its associated {\tt Calendar} and local Timezone attributes to
// {\tt BaseTime}.
//
//-------------------------------------------------------------------------
//
// !USES:
#include "ESMC_Base.h"           // inherited Base class
#include "ESMC_BaseTime.h"       // inherited BaseTime class
#include "ESMC_Calendar.h"       // associated Calendar class
 class ESMC_TimeInterval;

// !PUBLIC TYPES:
 class ESMC_Time;

// !PRIVATE TYPES:
 // class configuration type:  not needed for Time

 // class definition type
 class ESMC_Time : public ESMC_BaseTime { // inherits ESMC_BaseTime 
                                          // TODO: (& ESMC_Base class when
                                          // fully aligned with F90 equiv)
  private:   // corresponds to F90 module 'type ESMF_Time' members 
    ESMC_Calendar *calendar;    // associated calendar
    int timeZone;               // Offset from UTC

// !PUBLIC MEMBER FUNCTIONS:

  public:
    // Time doesn't need configuration, hence GetConfig/SetConfig
    // methods are not required

    // accessor methods
    // all get/set routines perform signed conversions, where applicable

    // Get/Set methods to support the F90 optional arguments interface
    int ESMC_TimeSet(ESMC_I4 *yy=0, ESMC_I8 *yy_i8=0,
                     int *mm=0, int *dd=0,
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
                     ESMC_Calendar **calendar=0, 
                     ESMC_CalendarType *calendarType=0, 
                     int *timeZone=0);

    int ESMC_TimeGet(ESMC_I4 *yy=0, ESMC_I8 *yy_i8=0,
                     int *mm=0, int *dd=0,
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
                     ESMC_Calendar **calendar=0, 
                     ESMC_CalendarType *calendarType=0, 
                     int *timeZone=0,
                     int timeStringLen=0, int *tempTimeStringLen=0,
                     char *tempTimeString=0,
                     int timeStringLenISOFrac=0,
                     int *tempTimeStringLenISOFrac=0,
                     char *tempTimeStringISOFrac=0,
                     int *dayOfWeek=0,
                     ESMC_Time *midMonth=0,
                     ESMC_I4 *dayOfYear=0,
                     ESMC_R8 *dayOfYear_r8=0,
                     ESMC_TimeInterval *dayOfYear_intvl=0) const;

    // native C++ interface -- via variable argument lists
    //   corresponds to F90 named-optional-arguments interface

    // (TMG 2.1, 2.5.1, 2.5.6)
    int ESMC_TimeGet(const char *timeList, ...) const;
    // e.g. ESMC_TimeGet("YY:MM:DD", (int *)YY,(int *)MM, (int *)DD);

    int ESMC_TimeSet(ESMC_Calendar *calendar, int timeZone,
                     const char *timeList, ...);
    int ESMC_TimeSet(const char *timeList, ...);
    // e.g. ESMC_TimeSet("s" , (ESMC_R8) s);

    bool ESMC_TimeIsLeapYear(int *rc=0) const;
    bool ESMC_TimeIsSameCalendar(const ESMC_Time *time, int *rc=0) const;

    // to support ESMC_Clock::SyncToWallClock() and TMG 2.5.7
    int ESMC_TimeSyncToRealTime(void);

    // override BaseTime +/- operators in order to copy ESMC_Time-only
    // properties (calendar & timeZone) to the result
    ESMC_Time operator+(const ESMC_TimeInterval &) const; // time + timeinterval
    ESMC_Time operator-(const ESMC_TimeInterval &) const; // time - timeinterval
    ESMC_Time& operator+=(const ESMC_TimeInterval &); // time += timeinterval
    ESMC_Time& operator-=(const ESMC_TimeInterval &); // time -= timeinterval

    // override 2nd BaseTime (-) operator because 1st (-) operator is overridden
    // (compiler can't find 2nd (-) operator at ESMC_Fraction!)
    ESMC_TimeInterval operator-(const ESMC_Time&) const;  // time1 - time2

    // TODO: ? override BaseTime arithmetic operators with same operators
    //         which use the BaseTime operators and then specialize
    //         with logic to validate (range check) the new value against
    //         the associated calendar.

    // required methods inherited and overridden from the ESMC_Base class

    // for persistence/checkpointing
    int ESMC_TimeReadRestart(int nameLen, const char *name=0,
                             ESMC_IOSpec *iospec=0);
    int ESMC_TimeWriteRestart(ESMC_IOSpec *iospec=0) const;

    // internal validation
    int ESMC_TimeValidate(const char *options=0) const;  // (TMG 7.1.1)

    // for testing/debugging
    int ESMC_TimePrint(const char *options=0) const;

    // native C++ constructors/destructors
    ESMC_Time(void);
    ESMC_Time(ESMC_I8 s, int sN=0, int sD=1, ESMC_Calendar *calendar=0,
              ESMC_CalendarType calendarType=(ESMC_CalendarType)0,
              int timeZone=0);
    int ESMC_TimeSet(ESMC_I8 s, int sN=0, int sD=1,
                     ESMC_Calendar *calendar=0,
                     ESMC_CalendarType calendarType=(ESMC_CalendarType)0,
                     int timeZone=0);
                                   // used internally instead of constructor
                                   // to cover case of initial entry from F90,
                                   // to avoid automatic destructor invocation
                                   // when leaving scope to return to F90.
    ~ESMC_Time(void);

 // < declare the rest of the public interface methods here >

// !PRIVATE MEMBER FUNCTIONS:
//
  private:
    // return in string format (TMG 2.4.7)
    int ESMC_TimeGetString(char *timeString, const char *options=0) const;

    int ESMC_TimeGetDayOfWeek(int *dayOfWeek) const;    // (TMG 2.5.3)
    int ESMC_TimeGetMidMonth(ESMC_Time *midMonth) const;

    int ESMC_TimeGetDayOfYear(ESMC_I4 *dayOfYear) const;
    int ESMC_TimeGetDayOfYear(ESMC_R8 *dayOfYear) const; // (TMG 2.5.2)
    int ESMC_TimeGetDayOfYear(ESMC_TimeInterval *dayOfYear) const;

    friend class ESMC_TimeInterval;
    friend class ESMC_Calendar;
                                                        // (TMG 2.5.5)
//
 // < declare private interface methods here >
//
//EOP
//-------------------------------------------------------------------------

 };   // end class ESMC_Time

 #endif // ESMC_TIME_H
