// $Id: ESMC_Time.h,v 1.4 2002/10/15 03:27:37 eschwab Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the GPL.
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

 // Put any constants or macros which apply to the whole component in this file.
 // Anything public or esmf-wide should be up higher at the top level
 // include files.
 #include <ESMC_TimeMgr.h>
 #include <ESMC_Types.h>
 #include <pthread.h>

//-------------------------------------------------------------------------
//BOP
// !CLASS:  ESMC_Time - Base time class
//
// !DESCRIPTION:
//
// The code in this file defines the C++ ESMC_Time members and declares method
// signatures (prototypes).  The companion file ESMC_Time.C contains
// the definitions (full code bodies) for the ESMC_Time methods.
//
// The Time class is a base class which encapulates the core representation and
// functionality of time for time intervals and time instances.
//
// The Time class is designed with a minimum number of elements to represent
// any required time.  The design is based on the idea used in the real-time
// POSIX 1003.1b-1993 standard.  That is, to represent time simply as a pair of
// integers: one for seconds (whole) and one for nanoseconds (fractional).
// These can then be converted at the interface level to any desired format.
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
// interface methods within the derived classes TimeInterval and TimeInstant.
// This is done because different applications require different
// representations of time intervals and time instances.
//
// The Time class defines increment and decrement methods for basic time
// interval calculations between time instants.  It is done here rather than in
// the calendar class because it can be done with simple arithmetic that is
// calendar-independent.  Upon demand by a user, the results are converted to
// user-units via methods in the derived classes TimeInterval and TimeInstant
// and the associated Calendar class.
//
// Comparison methods are also defined in the Time class.  These perform
// equality/inequality, less than, and greater than comparisons between any two
// TimeIntervals or TimeInstants.  These methods capture the common comparison
// logic between TimeIntervals and TimeInstants and hence are defined here for
// sharing.
//
// Methods or possibly a separate class will be included to handle fractional
// arithmetic.  An pre-developed open source package would be preferable.
//
// For ease in calendar conversions, a time value of zero (both whole and
// numerator) will correspond to the Julian date of zero.
//
// The Time class is only a base class not to be instantiated by any
// application. It is only used by the derived classes TimeInterval and
// TimeInstant.
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
//#include <ESMC_Base.h>  // all classes inherit from the ESMC Base class.

// !PUBLIC TYPES:
 class ESMC_Time;

// !PRIVATE TYPES:
 // class configuration type:  not needed for ESMC_Time

 // class definition type
class ESMC_Time {

  protected:

    int64 S;    // Integer seconds (signed)
    int32 Sn;	// Integer fractional seconds (exact) n/d; numerator (signed)
    int32 Sd;  	// Integer fractional seconds (exact) n/d; denominator

    pthread_mutex_t TimeMutex; // for thread safety (TMG 7.5)

// !PUBLIC MEMBER FUNCTIONS:

  public:

    // Time is a shallow class, so only Init methods are needed
    int ESMC_TimeInit(int64 S, int32 Sn, int32  Sd);

    // ESMC_Time doesn't need configuration, hence GetConfig/SetConfig
    // methods are not required

    // accessor methods

    // direct, one-to-one access to each core element (no conversions)
    int ESMC_TimeRead_S (int64 *S);
    int ESMC_TimeWrite_S(int64  S);

    int ESMC_TimeRead_Sn (int32 *Sn);
    int ESMC_TimeWrite_Sn(int32  Sn);

    int ESMC_TimeRead_Sd (int32 *Sd);
    int ESMC_TimeWrite_Sd(int32  Sd);

    // individual get/set accessor methods which perform signed conversion
	//  (TMG 1.1, 1.2, 2.1)
	int ESMC_TimeGet_D(int *D);
	int ESMC_TimeSet_D(int  D);

    int ESMC_TimeGet_H(int *H);
    int ESMC_TimeSet_H(int  H);

    int ESMC_TimeGet_M(int *M);
    int ESMC_TimeSet_M(int  M);

    int ESMC_TimeGet_S(int *S);
    int ESMC_TimeSet_S(int  S);

    int ESMC_TimeGet_Sn(int *Sn);
    int ESMC_TimeSet_Sn(int  Sn);

    int ESMC_TimeGet_Sd(int *Sd);
    int ESMC_TimeSet_Sd(int  Sd);

    int ESMC_TimeGet_MS(int *MS);
    int ESMC_TimeSet_MS(int  MS);

    int ESMC_TimeGet_US(int *MS);
    int ESMC_TimeSet_US(int  MS);

    int ESMC_TimeGet_NS(int *NS);
    int ESMC_TimeSet_NS(int  NS);

    int ESMC_TimeGet_d(double *d);
    int ESMC_TimeSet_d(double  d);

    int ESMC_TimeGet_h(double *h);
    int ESMC_TimeSet_h(double  h);

    int ESMC_TimeGet_m(double *m);
    int ESMC_TimeSet_m(double  m);

    int ESMC_TimeGet_s(double *s);
    int ESMC_TimeSet_s(double  s);

    int ESMC_TimeGet_ms(double *ms);
    int ESMC_TimeSet_ms(double  ms);

    int ESMC_TimeGet_us(double *us);
    int ESMC_TimeSet_us(double  us);

    int ESMC_TimeGet_ns(double *ns);
    int ESMC_TimeSet_ns(double  ns);

    // comparison methods (TMG 1.5.3, 2.4.3, 7.2)
    bool operator==(const ESMC_Time &) const; 
    bool operator!=(const ESMC_Time &) const; 
    bool operator< (const ESMC_Time &) const; 
    bool operator> (const ESMC_Time &) const; 
    bool operator<=(const ESMC_Time &) const; 
    bool operator>=(const ESMC_Time &) const; 

    // increment, decrement methods (TMG 1.5.4, 2.4.4, 2.4.5, 2.4.6, 5.1, 5.2,
    //                                   7.2)
    ESMC_Time& operator+=(ESMC_Time &);
    ESMC_Time  operator+ (ESMC_Time &);
    ESMC_Time& operator-=(ESMC_Time &);
    ESMC_Time  operator- (ESMC_Time &);

    // required methods inherited and overridden from the ESMC_Base class

    // internal validation
	int ESMC_TimeValidate(const char *options) const;

	// for persistence/checkpointing
	int ESMC_TimePrint(int64 *S, int32 *Sn, int32 *Sd) const;

	// for testing/debugging
	int ESMC_TimePrint(void) const;

	// native C++ constructors/destructors
    ESMC_Time(void);
    ESMC_Time(int64 S, int32 Sn, int32 Sd);
    ~ESMC_Time(void);

 // < declare the rest of the public interface methods here >

// !PRIVATE MEMBER FUNCTIONS:
//
  private:
//
 // < declare private interface methods here >
//
//EOP
//-------------------------------------------------------------------------

};    // end class <ESMC_Time>

#endif // ESMC_TIME_H
