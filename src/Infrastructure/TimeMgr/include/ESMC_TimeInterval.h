// $Id: ESMC_TimeInterval.h,v 1.7 2003/03/29 01:41:19 eschwab Exp $
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
#include <ESMF_TimeMgr.inc>
#include <ESMC_Fraction.h>

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
//   {\tt Calendar} interval by associating it with a calendar type.  Then it
//   becomes calendar-dependent, since its largest resolution of time will be
//   in months and years.  In order to support calendar intervals,
//   {\tt TimeInterval} adds a calendar type attribute to {\tt BaseTime}.
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
 #include <ESMC_Base.h>           // inherited Base class
 #include <ESMC_BaseTime.h>       // inherited BaseTime class
 #include <ESMC_Calendar.h>       // associated Calendar class

// !PUBLIC TYPES:
 class ESMC_TimeInterval;

// !PRIVATE TYPES:

 // class definition type
class ESMC_TimeInterval : public ESMC_BaseTime { 
                                             // inherits ESMC_BaseTime & Base
                                             // classes
  private:
    // set for Calendar intervals only
    ESMC_Calendar *Calendar;    // associated calendar for Calendar intervals

// !PUBLIC MEMBER FUNCTIONS:

  public:

    // TimeInterval is a shallow class, so only Init methods are needed
    // TODO:  use default argument for calendar (NULL)
    int ESMC_TimeIntervalInit(ESMC_Calendar *Cal, const char *TimeList, ...);

    // Init method to support the F90 optional arguments interface
    int ESMC_TimeIntervalInit(int *YY, int *MO, int *D, int *H, int *M,
                              ESMF_IKIND_I8 *S, int *MS, int *US, int *NS,
                              double *d_, double *h_, double *m_, double *s_,
                              double *ms_, double *us_, double *ns_,
                              int *Sn, int *Sd, ESMC_Calendar *cal);

    // accessor methods

    // all get/set routines perform signed conversions, where applicable;
    //   direct, one-to-one access to core time elements is provided by the
    //   ESMC_BaseTime base class

    // generic interface -- via variable argument lists
    //   can map to F90 named-optional-arguments interface

    // (TMG 1.1)
    int ESMC_TimeIntervalGet(const char *TimeList, ...);
    // e.g. Get("D:S",(int *)D, (int *)S);

    int ESMC_TimeIntervalSet(const char *TimeList, ...);
    // e.g. Set("s" , (double) s);

    int ESMC_TimeIntervalGetCalendar(ESMC_Calendar *Cal);
    int ESMC_TimeIntervalSetCalendar(ESMC_Calendar *Cal);

    bool ESMC_TimeIntervalIsSameCal(ESMC_TimeInterval *);

    // return in string format (TMG 1.5.9)
    int ESMC_TimeIntervalGetString(char *Ts);

    // return positive value (TMG 1.5.8)
    ESMC_TimeInterval *ESMC_TimeIntervalGetAbsValue(ESMC_TimeInterval *);

    // return negative value (TMG 1.5.8)
    ESMC_TimeInterval *ESMC_TimeIntervalGetNegAbsVal(ESMC_TimeInterval *);

    // division (TMG 1.5.5)
    double& operator/(ESMC_TimeInterval &);
    ESMC_Fraction& operatorDIV(ESMC_TimeInterval &);  // TODO  DIV

    // subdivision (TMG 1.5.6, 5.3, 7.2)
    ESMC_TimeInterval& operator/=(int &);
    ESMC_TimeInterval& operator/ (int &);
    ESMC_TimeInterval& operator/=(double &);
    ESMC_TimeInterval& operator/ (double &);

    // multiplication (TMG 1.5.7, 7.2)
    ESMC_TimeInterval& operator*=(int &);
    ESMC_TimeInterval& operator* (int &);
    ESMC_TimeInterval& operator*=(ESMC_Fraction &);
    ESMC_TimeInterval& operator* (ESMC_Fraction &);
    ESMC_TimeInterval& operator*=(double &);
    ESMC_TimeInterval& operator* (double &);

    // required methods inherited and overridden from the ESMC_Base class

    // for persistence/checkpointing
    int ESMC_Read(ESMF_IKIND_I8 S, int Sn, int Sd,
                  ESMC_Calendar *cal);
    int ESMC_Write(ESMF_IKIND_I8 *S, int *Sn, int *Sd,
                   ESMC_Calendar *cal) const;

    // internal validation
    int ESMC_Validate(const char *options=0) const;

    // for testing/debugging
    int ESMC_Print(const char *options=0) const;

    // native C++ constructors/destructors
    ESMC_TimeInterval(void);
    ESMC_TimeInterval(ESMF_IKIND_I8 S, int Sn, int Sd);
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
