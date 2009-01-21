// $Id: ESMC_BaseTime.h,v 1.32.2.4 2009/01/21 21:25:23 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
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
#include "ESMC_Start.h"
#include "ESMF_TimeMgr.inc"

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
// For arithmetic consistency both must carry the same sign (both positive
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
// The separate class ESMC_Fraction is inherited to handle fractional
// arithmetic.  ESMC_BaseTime encapsulates common time-specific knowledge,
// whereas ESMC_Fraction is time-knowledge independent; it simply performs
// generic fractional arithmetic, manipulations and comparisons.
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
// TMG 1.5.1, 2.4.1, 3.1, 4.1: Each class has an Set() function,
// which is used in lieu of a constructor since it can
// return an error code.
//
//-------------------------------------------------------------------------
//
// !USES:
#include "ESMC_Base.h"   // all classes inherit from the ESMC Base class.
#include "ESMC_IOSpec.h" // IOSpec class for ReadRestart()/WriteRestart()
#include "ESMC_Fraction.h"

// !PUBLIC TYPES:
 class ESMC_BaseTime;

// !PRIVATE TYPES:
 // class configuration type:  not needed for ESMC_BaseTime

 // class definition type
class ESMC_BaseTime : public ESMC_Fraction { // it is a fraction !
//class ESMC_BaseTime : public ESMC_Base { // TODO: inherit from ESMC_Base class
                                           // when fully aligned with F90 equiv

  protected:

    // TODO:  move ESMC_Calendar* here to define seconds per day ? then could
    //        add D (Julian Days) to Get()/Set() below, and remove secondsPerDay
    //        from Get()

//    pthread_mutex_t BaseTimeMutex; // for thread safety (TMG 7.5)

// !PUBLIC MEMBER FUNCTIONS:

  public:

    // Get/Set methods (primarily to support F90 interface)
    int ESMC_BaseTimeSet(ESMC_I4 *h=0, ESMC_I4 *m=0,
                         ESMC_I4 *s=0, ESMC_I8 *s_i8=0,
                         ESMC_I4 *ms=0, ESMC_I4 *us=0,
                         ESMC_I4 *ns=0,
                         ESMC_R8 *h_r8=0, ESMC_R8 *m_r8=0,
                         ESMC_R8 *s_r8=0,
                         ESMC_R8 *ms_r8=0, ESMC_R8 *us_r8=0,
                         ESMC_R8 *ns_r8=0,
                         ESMC_I4 *sN=0, ESMC_I4 *sD=0);

    int ESMC_BaseTimeSet(ESMC_I8 S, int sN, int sD);

    int ESMC_BaseTimeGet(const ESMC_BaseTime *timeToConvert,
                         ESMC_I4 *h=0, ESMC_I4 *m=0,
                         ESMC_I4 *s=0, ESMC_I8 *s_i8=0,
                         ESMC_I4 *ms=0, ESMC_I4 *us=0,
                         ESMC_I4 *ns=0,
                         ESMC_R8 *h_r8=0, ESMC_R8 *m_r8=0,
                         ESMC_R8 *s_r8=0,
                         ESMC_R8 *ms_r8=0, ESMC_R8 *us_r8=0,
                         ESMC_R8 *ns_r8=0,
                         ESMC_I4 *sN=0, ESMC_I4 *sD=0) const;

    // ESMC_BaseTime doesn't need configuration, hence GetConfig/SetConfig
    // methods are not required

    // TODO: should be implicit, but then won't support
    //   F90 ESMF_Time & ESMF_TimeInterval via ESMC_BaseTime_F.C interface
    //   for increment/decrement
    ESMC_BaseTime& operator=(const ESMC_Fraction &);

    // required methods inherited and overridden from the ESMC_Base class

    // for persistence/checkpointing
    int ESMC_BaseTimeReadRestart(int nameLen, const char *name=0,
                                 ESMC_IOSpec *iospec=0);
    int ESMC_BaseTimeWriteRestart(ESMC_IOSpec *iospec=0) const;

    // internal validation
    int ESMC_BaseTimeValidate(const char *options=0) const;

    // for testing/debugging
    int ESMC_BaseTimePrint(const char *options=0) const;

    // native C++ constructors/destructors
    ESMC_BaseTime(void);
    ESMC_BaseTime(ESMC_I8 S, ESMC_I4 sN, ESMC_I4 sD);
    ~ESMC_BaseTime(void);

 // < declare the rest of the public interface methods here >

// !PRIVATE MEMBER FUNCTIONS:
//
  private:

    friend class ESMC_Calendar;

//
 // < declare private interface methods here >
//
//EOP
//-------------------------------------------------------------------------

};    // end class ESMC_BaseTime

#endif // ESMC_BASETIME_H
