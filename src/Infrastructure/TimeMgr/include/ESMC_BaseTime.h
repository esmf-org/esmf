// $Id: ESMC_BaseTime.h,v 1.4 2003/03/22 05:43:10 eschwab Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the GPL.
//
// ESMF BaseTime C++ definition include file
//
// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
//
 // these lines prevent this file from being read more than once if it
 // ends up being included multiple times

#ifndef ESMC_BASETIME_H
#define ESMC_BASETIME_H

//-------------------------------------------------------------------------

 // Put any constants or macros which apply to the whole component in this file.
 // Anything public or esmf-wide should be up higher at the top level
 // include files.
 #include <ESMF_TimeMgr.inc>
 #include <pthread.h>

//-------------------------------------------------------------------------
//BOP
// !CLASS:  ESMC_BaseTime - Base time class
//
// !DESCRIPTION:
//
// The code in this file defines the C++ {\tt ESMC\_BaseTime} members and
// declares method signatures (prototypes).  The companion file
// {\tt ESMC\_BaseTime.C} contains the definitions (full code bodies) for
// the {\tt ESMC\_BaseTime} methods.
//
// The {\tt BaseTime} class is a base class which encapulates the core
// representation and functionality of time for time intervals and time
// instances.
//
// The {\tt BaseTime} class is designed with a minimum number of elements
// to represent any required time.  The design is based on the idea used
// in the real-time POSIX 1003.1b-1993 standard.  That is, to represent
// time simply as a pair of integers: one for seconds (whole) and one for
// nanoseconds (fractional).  These can then be converted at the interface
// level to any desired format.
//
// For ESMF, this idea is modified and extended, in order to handle the
// requirements for a large time range (> 200,000 years) and to exactly
// represent any rational fraction, not just nanoseconds.  To handle the
// large time range, a 64-bit or greater integer is used for whole seconds.
// Any rational fractional second is expressed using two additional integers:
// a numerator and a denominator.  Both the whole seconds and fractional
// numerator are signed to handle negative time intervals and instants.
// For arithmetic consistency both must carry the same sign (both positve
// or both negative), except, of course, for zero values.  The fractional
// seconds element (numerator) is \htmlref{normalized}{glos:Normalized}
// (bounded) with respect to whole seconds. If the absolute value of the
// numerator becomes greater than or equal to the denominator, the whole
// seconds is incremented or decremented accordingly and the numerator is
// reset to the remainder.  Conversions are performed upon demand by
// interface methods within the derived classes {\tt TimeInterval} and
// {\tt Time}.  This is done because different applications require different
// representations of time intervals and time instances.
//
// The {\tt BaseTime} class defines increment and decrement methods for basic
// time interval calculations between time instants.  It is done here rather
// than in the calendar class because it can be done with simple arithmetic
// that is calendar-independent.  Upon demand by a user, the results are
// converted to user-units via methods in the derived classes {\tt TimeInterval}
// and {\tt Time} and the associated {\tt Calendar} class.
//
// Comparison methods are also defined in the {\tt BaseTime} class.  These
// perform equality/inequality, less than, and greater than comparisons
// between any two {\tt TimeIntervals} or {\tt Time}.  These methods capture
// the common comparison logic between {\tt TimeIntervals} and {\tt Time} and
// hence are defined here for sharing.
//
// Methods or possibly a separate class will be included to handle fractional
// arithmetic.  An pre-developed open source package would be preferable.
//
// For ease in calendar conversions, a time value of zero (both whole and
// numerator) will correspond to the Julian date of zero.
//
// The {\tt BaseTime} class is only a base class not to be instantiated by any
// application. It is only used by the derived classes {\tt TimeInterval} and
// {\tt Time}.
//
// Core representation meets TMG 1.3, 1.4, 2.2, 5.4
//
// TMG 1.5.2, 2.4.2: Copy from one object to another is handled
// via the class default memberwise assignment method, which uses
// the overloaded equals (=) operator.  E.g.  *obj1 = *obj2 which
// can be called from a C wrapper function mapping F90 to C++
//
// TMG 1.5.1, 2.4.1, 3.1, 4.1: Each class has an Init() function,
// which is used in lieu of a constructor since it can
// return an error code.
//
//-------------------------------------------------------------------------
//
// !USES:
#include <ESMC_Base.h>  // all classes inherit from the ESMC Base class.

// !PUBLIC TYPES:
 class ESMC_BaseTime;

// !PRIVATE TYPES:
 // class configuration type:  not needed for ESMC_BaseTime

 // class definition type
class ESMC_BaseTime {

  protected:

    ESMF_IKIND_I8 S;    // Integer seconds (signed)
    int Sn;     // Integer fractional seconds (exact) n/d; numerator (signed)
    int Sd;     // Integer fractional seconds (exact) n/d; denominator

    pthread_mutex_t BaseTimeMutex; // for thread safety (TMG 7.5)

// !PUBLIC MEMBER FUNCTIONS:

  public:

    // BaseTime is a shallow class, so only Init methods are needed
    int ESMC_BaseTimeInit(ESMF_IKIND_I8 S, int Sn, int Sd);

    // ESMC_BaseTime doesn't need configuration, hence GetConfig/SetConfig
    // methods are not required

    // individual get/set accessor methods which perform signed conversion
    //  (TMG 1.1, 1.2, 2.1)

    // Integer Days
    int ESMC_BaseTimeGet_D(int *D) const;
    int ESMC_BaseTimeSet_D(int  D);

    // Integer Hours
    int ESMC_BaseTimeGet_H(int *H) const;
    int ESMC_BaseTimeSet_H(int  H);

    // Integer Minutes
    int ESMC_BaseTimeGet_M(int *M) const;
    int ESMC_BaseTimeSet_M(int  M);

    // Integer Seconds
    int ESMC_BaseTimeGet_S(int *S) const;
    int ESMC_BaseTimeSet_S(int  S);

    // Integer Millseconds
    int ESMC_BaseTimeGet_MS(int *MS) const;
    int ESMC_BaseTimeSet_MS(int  MS);

    // Integer Microseconds
    int ESMC_BaseTimeGet_US(int *US) const;
    int ESMC_BaseTimeSet_US(int  US);

    // Integer Nanoseconds
    int ESMC_BaseTimeGet_NS(int *NS) const;
    int ESMC_BaseTimeSet_NS(int  NS);

    // Floating point days
    int ESMC_BaseTimeGet_d(double *d) const;
    int ESMC_BaseTimeSet_d(double  d);

    // Floating point hours
    int ESMC_BaseTimeGet_h(double *h) const;
    int ESMC_BaseTimeSet_h(double  h);

    // Floating point minutes
    int ESMC_BaseTimeGet_m(double *m) const;
    int ESMC_BaseTimeSet_m(double  m);

    // Floating point seconds
    int ESMC_BaseTimeGet_s(double *s) const;
    int ESMC_BaseTimeSet_s(double  s);

    // Floating point milliseconds
    int ESMC_BaseTimeGet_ms(double *ms) const;
    int ESMC_BaseTimeSet_ms(double  ms);

    // Floating point microseconds
    int ESMC_BaseTimeGet_us(double *us) const;
    int ESMC_BaseTimeSet_us(double  us);

    // Floating point nanoseconds
    int ESMC_BaseTimeGet_ns(double *ns) const;
    int ESMC_BaseTimeSet_ns(double  ns);

    // comparison methods (TMG 1.5.3, 2.4.3, 7.2)
    bool operator==(const ESMC_BaseTime &) const; 
    bool operator!=(const ESMC_BaseTime &) const; 
    bool operator< (const ESMC_BaseTime &) const; 
    bool operator> (const ESMC_BaseTime &) const; 
    bool operator<=(const ESMC_BaseTime &) const; 
    bool operator>=(const ESMC_BaseTime &) const; 

    // increment, decrement methods (TMG 1.5.4, 2.4.4, 2.4.5, 2.4.6, 5.1, 5.2,
    //                                   7.2)
    ESMC_BaseTime  operator+ (const ESMC_BaseTime &) const;
    ESMC_BaseTime  operator- (const ESMC_BaseTime &) const;
    ESMC_BaseTime& operator+=(const ESMC_BaseTime &);
    ESMC_BaseTime& operator-=(const ESMC_BaseTime &);

    // required methods inherited and overridden from the ESMC_Base class

    // internal validation
    int ESMC_BaseValidate(const char *options) const;

    // for testing/debugging
    int ESMC_BasePrint(const char *options) const;

    // for persistence/checkpointing
    int ESMC_BasePrint(ESMF_IKIND_I8 *S, int *Sn, int *Sd) const;

    // native C++ constructors/destructors
    ESMC_BaseTime(void);
    ESMC_BaseTime(ESMF_IKIND_I8 S, int Sn, int Sd);
    ~ESMC_BaseTime(void);

 // < declare the rest of the public interface methods here >

    friend class ESMC_Calendar;

// !PRIVATE MEMBER FUNCTIONS:
//
  private:
//
 // < declare private interface methods here >
//
//EOP
//-------------------------------------------------------------------------

};    // end class <ESMC_BaseTime>

#endif // ESMC_BASETIME_H
