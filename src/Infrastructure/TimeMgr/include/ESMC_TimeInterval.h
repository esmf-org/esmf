// $Id: ESMC_TimeInterval.h,v 1.18 2003/05/02 22:08:30 eschwab Exp $
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
 #include <ESMC_Base.h>           // inherited Base class
 #include <ESMC_BaseTime.h>       // inherited BaseTime class

// !PUBLIC TYPES:
 class ESMC_TimeInterval;

// !PRIVATE TYPES:

 // class definition type
class ESMC_TimeInterval : public ESMC_BaseTime { 
                                             // inherits ESMC_BaseTime
                                             // TODO: (& ESMC_Base class when
                                             // fully aligned with F90 equiv)
  private:
    ESMF_IKIND_I8 YY;      // for Calendar intervals:  number of years
    ESMF_IKIND_I8 MO;      // for Calendar intervals:  number of months
    ESMF_IKIND_I8 D;       // for Calendar intervals:  number of days

// !PUBLIC MEMBER FUNCTIONS:

  public:

    // TimeInterval is a shallow class, so only Init methods are needed
    int ESMC_TimeIntervalInit(const char *timeList, ...);

    // Init method to support the F90 optional arguments interface
    int ESMC_TimeIntervalInit(ESMF_IKIND_I8 *YY=0, ESMF_IKIND_I8 *MO=0,
                              ESMF_IKIND_I8 *D=0,
                              int *H=0, int *M=0, ESMF_IKIND_I8 *S=0,
                              int *MS=0, int *US=0, int *NS=0,
                              double *d_=0, double *h_=0, double *m_=0,
                              double *s_=0, double *ms_=0, double *us_=0,
                              double *ns_=0, int *Sn=0, int *Sd=0);

    // accessor methods

    // all get/set routines perform signed conversions, where applicable;
    //   direct, one-to-one access to core time elements is provided by the
    //   ESMC_BaseTime base class

    // generic interface -- via variable argument lists
    //   can map to F90 named-optional-arguments interface

    // (TMG 1.1)
    int ESMC_TimeIntervalGet(const char *TimeList, ...) const;
    // e.g. Get("D:S",(int *)D, (int *)S);

    int ESMC_TimeIntervalSet(const char *TimeList, ...);
    // e.g. Set("s" , (double) s);

    // Get/Set methods to support the F90 optional arguments interface
    int ESMC_TimeIntervalGet(ESMF_IKIND_I8 *YY=0, ESMF_IKIND_I8 *MO=0,
                             ESMF_IKIND_I8 *D=0,
                             int *H=0, int *M=0, ESMF_IKIND_I8 *S=0,
                             int *MS=0, int *US=0, int *NS=0,
                             double *d_=0, double *h_=0, double *m_=0,
                             double *s_=0, double *ms_=0, double *us_=0,
                             double *ns_=0, int *Sn=0, int *Sd=0) const;

    int ESMC_TimeIntervalSet(ESMF_IKIND_I8 *YY=0, ESMF_IKIND_I8 *MO=0,
                             ESMF_IKIND_I8 *D=0,
                             int *H=0, int *M=0, ESMF_IKIND_I8 *S=0,
                             int *MS=0, int *US=0, int *NS=0,
                             double *d_=0, double *h_=0, double *m_=0,
                             double *s_=0, double *ms_=0, double *us_=0,
                             double *ns_=0, int *Sn=0, int *Sd=0);

    // return in string format (TMG 1.5.9)
    int ESMC_TimeIntervalGetString(char *timeString) const;

    // return positive value (TMG 1.5.8)
    ESMC_TimeInterval ESMC_TimeIntervalAbsValue(void) const;

    // return negative value (TMG 1.5.8)
    ESMC_TimeInterval ESMC_TimeIntervalNegAbsVal(void) const;

    // division (TMG 1.5.5)
    ESMC_Fraction ESMC_TimeIntervalDiv(const ESMC_TimeInterval &) const;
    double operator/(const ESMC_TimeInterval &) const;

    // subdivision (TMG 1.5.6, 5.3, 7.2)
    ESMC_TimeInterval  operator/ (const int &) const;
    ESMC_TimeInterval& operator/=(const int &);
    ESMC_TimeInterval  operator/ (const double &) const;
    ESMC_TimeInterval& operator/=(const double &);

    // multiplication (TMG 1.5.7, 7.2)
    ESMC_TimeInterval  operator* (const int &) const;
    ESMC_TimeInterval& operator*=(const int &);
    ESMC_TimeInterval  operator* (const ESMC_Fraction &) const;
    ESMC_TimeInterval& operator*=(const ESMC_Fraction &);
    ESMC_TimeInterval  operator* (const double &) const;
    ESMC_TimeInterval& operator*=(const double &);

    // copy or assign from ESMC_BaseTime expressions
    // TODO:  should be implicit ?
    ESMC_TimeInterval& operator=(const ESMC_BaseTime &);

    // required methods inherited and overridden from the ESMC_Base class

    // for persistence/checkpointing
    int ESMC_TimeIntervalRead(ESMF_IKIND_I8 S, int Sn, int Sd,
                              ESMF_IKIND_I8 YY, ESMF_IKIND_I8 MO,
                              ESMF_IKIND_I8 D);
    int ESMC_TimeIntervalWrite(ESMF_IKIND_I8 *S, int *Sn, int *Sd,
                               ESMF_IKIND_I8 *YY, ESMF_IKIND_I8 *MO,
                               ESMF_IKIND_I8 *D) const;

    // internal validation (TMG 7.1.1)
    int ESMC_TimeIntervalValidate(const char *options=0) const;

    // for testing/debugging
    int ESMC_TimeIntervalPrint(const char *options=0) const;

    // native C++ constructors/destructors
    ESMC_TimeInterval(void);
    ESMC_TimeInterval(ESMF_IKIND_I8 S, int Sn, int Sd,
                      ESMF_IKIND_I8 YY, ESMF_IKIND_I8 MO, ESMF_IKIND_I8 D);

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
