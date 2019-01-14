// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2019, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
// ESMC BaseTime method code (body) file
//
//-------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ {\tt ESMCI\_BaseTime} methods
// declared in the companion file {\tt ESMCI\_BaseTime.h}
//
//-------------------------------------------------------------------------
#define ESMC_FILENAME "ESMCI_BaseTime.C"

// associated class definition file
#include "ESMCI_BaseTime.h"

#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <math.h>    // modf()
/*
#include <iostream>
#include <stdlib>
using std::cout;
using std::endl;
*/
    
#include "ESMCI_LogErr.h"

//-------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-------------------------------------------------------------------------

namespace ESMCI{

//
//-------------------------------------------------------------------------
//-------------------------------------------------------------------------
//
// This section includes all the BaseTime routines
//
//

//-------------------------------------------------------------------------
// Class ESMCI::BaseTime Methods
//-------------------------------------------------------------------------

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMCI::BaseTime::set - set sub-day values of a basetime
//
// !INTERFACE:
      int BaseTime::set(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_I4 *h,       // in - integer hours
      ESMC_I4 *m,       // in - integer minutes
      ESMC_I4 *s,       // in - integer seconds (>= 32 bit)
      ESMC_I8 *s_i8,    // in - integer seconds (large, >= 64 bit)
      ESMC_I4 *ms,      // in - integer milliseconds
      ESMC_I4 *us,      // in - integer microseconds
      ESMC_I4 *ns,      // in - integer nanoseconds
      ESMC_R8 *h_r8,    // in - floating point hours
      ESMC_R8 *m_r8,    // in - floating point minutes
      ESMC_R8 *s_r8,    // in - floating point seconds
      ESMC_R8 *ms_r8,   // in - floating point milliseconds
      ESMC_R8 *us_r8,   // in - floating point microseconds
      ESMC_R8 *ns_r8,   // in - floating point nanoseconds
      ESMC_I4 *sN,      // in - fractional seconds numerator
      ESMC_I8 *sN_i8,   // in - fractional seconds numerator  (large, >= 64 bit)
      ESMC_I4 *sD,      // in - fractional seconds denominator
      ESMC_I8 *sD_i8) { // in - fractional seconds denominator(large, >= 64 bit)
//
// !DESCRIPTION:
//      Sets sub-day (non-calendar dependent) values of a {\tt ESMCI\_BaseTime}.
//      Primarily to support F90 interface.
//
//EOP
// !REQUIREMENTS:  

    //
    // whole seconds
    //

    // integer units
    if (h != ESMC_NULL_POINTER) {
      Fraction time(((ESMC_I8) *h) * SECONDS_PER_HOUR);
      *this += time;
    }
    if (m != ESMC_NULL_POINTER) {
      Fraction time(((ESMC_I8) *m) * SECONDS_PER_MINUTE);
      *this += time;
    }
    if (s != ESMC_NULL_POINTER) {
      Fraction time((ESMC_I8)*s);
      *this += time;  // >= 32-bit
    } else if (s_i8 != ESMC_NULL_POINTER) {
      Fraction time(*s_i8);
      *this += time;  // >= 64-bit
    }

    // floating point units
    //   avoid error introduced by floating point multiply by setting a time
    //   in the given units, then performing an integer multiply by
    //   the unit conversion factor.
    if (h_r8 != ESMC_NULL_POINTER) {
      Fraction time(*h_r8);
      *this += time * SECONDS_PER_HOUR;
    }
    if (m_r8 != ESMC_NULL_POINTER) {
      Fraction time(*m_r8);
      *this += time * SECONDS_PER_MINUTE;
    }
    if (s_r8 != ESMC_NULL_POINTER) {
      Fraction time(*s_r8);
      *this += time;
    }

    //
    // fractional seconds
    //

    // integer units
    if (ms != ESMC_NULL_POINTER) {
      Fraction fractional_time(0, *ms, 1000);
      *this += fractional_time;
    }
    if (us != ESMC_NULL_POINTER) {
      Fraction fractional_time(0, *us, 1000000);
      *this += fractional_time;
    }
    if (ns != ESMC_NULL_POINTER) {
      Fraction fractional_time(0, *ns, 1000000000);
      *this += fractional_time;
    }

    // floating point units
    //   avoid error introduced by floating point divide by setting a time
    //   in the given units, then performing an integer divide by
    //   the unit conversion factor.
    if (ms_r8 != ESMC_NULL_POINTER) {
      Fraction fractional_time(*ms_r8);
      *this += fractional_time / 1000;
    }
    if (us_r8 != ESMC_NULL_POINTER) {
      Fraction fractional_time(*us_r8);
      *this += fractional_time / 1000000;
    }
    if (ns_r8 != ESMC_NULL_POINTER) {
      Fraction fractional_time(*ns_r8);
      *this += fractional_time / 1000000000;
    }

    // integer numerator and denominator
    if (sN != ESMC_NULL_POINTER && sD != ESMC_NULL_POINTER) {
      Fraction fractional_time((ESMC_I8)0, (ESMC_I8)*sN, (ESMC_I8)*sD);
      *this += fractional_time;
    } else if (sN_i8 != ESMC_NULL_POINTER && sD_i8 != ESMC_NULL_POINTER) {
      Fraction fractional_time((ESMC_I8)0, *sN_i8, *sD_i8);
      *this += fractional_time;
    } else if (sN != ESMC_NULL_POINTER && sD_i8 != ESMC_NULL_POINTER) {
      Fraction fractional_time((ESMC_I8)0, (ESMC_I8)*sN, *sD_i8);
      *this += fractional_time;
    } else if (sN_i8 != ESMC_NULL_POINTER && sD != ESMC_NULL_POINTER) {
      Fraction fractional_time((ESMC_I8)0, *sN_i8, (ESMC_I8)*sD);
      *this += fractional_time;
    }

    return(ESMF_SUCCESS);

}  // end ESMCI::BaseTime::set

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMCI::BaseTime::set - direct core value initializer
//
// !INTERFACE:
      int BaseTime::set(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_I8 s,      // in - integer seconds
      ESMC_I8 sN,     // in - fractional seconds, numerator
      ESMC_I8 sD ) {  // in - fractional seconds, denominator
//
// !DESCRIPTION:
//      Initialzes a {\tt ESMCI::BaseTime} with given values
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::BaseTime::set()"

    // s, sN must be either both positive or both negative;
    //    sD always positive and >= 1
    if ( !(((s >= 0 && sN >= 0) || (s <= 0 && sN <= 0)) && sD >= 1) ) {
      char logMsg[ESMF_MAXSTR];
      sprintf(logMsg, "s=%lld and sN=%lld not both positive or both negative, "
                      "or sD=%lld negative or less than one.", s, sN, sD); 
      ESMC_LogDefault.Write(logMsg, ESMC_LOGMSG_WARN,ESMC_CONTEXT);
      return(ESMF_FAILURE);
    }

    Fraction::set(s, sN, sD);

    return(ESMF_SUCCESS);

}  // end ESMCI::BaseTime::set

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMCI::BaseTime::get - get units of a basetime
//
// !INTERFACE:
      int BaseTime::get(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const BaseTime *timeToConvert, // in  - the time to convert
                                          //     (divide) into requested units
      ESMC_I4 *h,              // out - integer hours
      ESMC_I4 *m,              // out - integer minutes
      ESMC_I4 *s,              // out - integer seconds (>= 32-bit)
      ESMC_I8 *s_i8,           // out - integer seconds (large, >= 64-bit)
      ESMC_I4 *ms,             // out - integer milliseconds
      ESMC_I4 *us,             // out - integer microseconds
      ESMC_I4 *ns,             // out - integer nanoseconds
      ESMC_R8 *h_r8,           // out - floating point hours
      ESMC_R8 *m_r8,           // out - floating point minutes
      ESMC_R8 *s_r8,           // out - floating point seconds
      ESMC_R8 *ms_r8,          // out - floating point milliseconds
      ESMC_R8 *us_r8,          // out - floating point microseconds
      ESMC_R8 *ns_r8,          // out - floating point nanoseconds
      ESMC_I4 *sN,             // out - fractional seconds numerator
      ESMC_I8 *sN_i8,          // out - fractional seconds numerator
                               //                            (large, >= 64-bit)
      ESMC_I4 *sD,             // out - fractional seconds denominator
      ESMC_I8 *sD_i8) const {  // out - fractional seconds denominator
                               //                            (large, >= 64-bit)

//
// !DESCRIPTION:
//      Get non-calendar dependent values of a {\tt ESMC\_BaseTime}
//      converted to user units.  Primarily to support F90 interface
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::BaseTime::get()"

    int rc = ESMF_SUCCESS;

    // validate input
    if (timeToConvert == ESMC_NULL_POINTER) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
        "; timeToConvert is NULL", ESMC_CONTEXT, &rc);
      return(rc);
    }

    // make local copy for manipulation
    BaseTime remainingTime = *timeToConvert;
    remainingTime.simplify(); // ensure maximum whole seconds
    BaseTime saveRemainingTime = remainingTime;  // for float units below
    ESMC_I8 remainingSeconds = remainingTime.getw();

    // get integer numerator and denominator
    if (sN != ESMC_NULL_POINTER) {
      ESMC_I8 numerator = remainingTime.getn();
      if (numerator < INT_MIN || numerator > INT_MAX) {
        char logMsg[ESMF_MAXSTR];
        sprintf(logMsg, "For sN=%lld out-of-range with respect to "
                        "machine limits (INT_MIN=%d to INT_MAX=%d), "
                        "use sN_i8.", numerator, INT_MIN, INT_MAX);
        ESMC_LogDefault.Write(logMsg, ESMC_LOGMSG_WARN,ESMC_CONTEXT);
        return (ESMF_FAILURE);
      }
      *sN = numerator;
    }
    if (sN_i8 != ESMC_NULL_POINTER) {
      *sN_i8 = remainingTime.getn();
    }
    if (sD != ESMC_NULL_POINTER) {
      ESMC_I8 denominator = remainingTime.getd();
      if (denominator < INT_MIN || denominator > INT_MAX) {
        char logMsg[ESMF_MAXSTR];
        sprintf(logMsg, "For sD=%lld out-of-range with respect to "
                        "machine limits (INT_MIN=%d to INT_MAX=%d), "
                        "use sD_i8.", denominator, INT_MIN, INT_MAX);
        ESMC_LogDefault.Write(logMsg, ESMC_LOGMSG_WARN, ESMC_CONTEXT);
        return (ESMF_FAILURE);
      }
      *sD = denominator;
    }
    if (sD_i8 != ESMC_NULL_POINTER) {
      *sD_i8 = remainingTime.getd();
    }

    if (h != ESMC_NULL_POINTER) {
      ESMC_I8 hours = remainingSeconds / SECONDS_PER_HOUR;
      if (hours < INT_MIN || hours > INT_MAX) {
        char logMsg[ESMF_MAXSTR];
        sprintf(logMsg, "For s=%lld, hours=%lld out-of-range with respect to "
                        "machine limits (INT_MIN=%d to INT_MAX=%d).",
                        remainingSeconds, hours, INT_MIN, INT_MAX);
        ESMC_LogDefault.Write(logMsg, ESMC_LOGMSG_WARN, ESMC_CONTEXT);
        return (ESMF_FAILURE);
      }
      *h = hours;
      remainingSeconds %= SECONDS_PER_HOUR;  // remove hours
    }
    if (m != ESMC_NULL_POINTER) {
      ESMC_I8 minutes = remainingSeconds / SECONDS_PER_MINUTE;
      if (minutes < INT_MIN || minutes > INT_MAX) {
        char logMsg[ESMF_MAXSTR];
        sprintf(logMsg, "For s=%lld, minutes=%lld out-of-range with respect to "
                        "machine limits (INT_MIN=%d to INT_MAX=%d).",
                        remainingSeconds, minutes, INT_MIN, INT_MAX);
        ESMC_LogDefault.Write(logMsg, ESMC_LOGMSG_WARN,ESMC_CONTEXT);
        return (ESMF_FAILURE);
      }
      *m = minutes;
      remainingSeconds %= SECONDS_PER_MINUTE;  // remove minutes
    }
    if (s != ESMC_NULL_POINTER) {
      if (remainingSeconds < INT_MIN || remainingSeconds > INT_MAX) {
        char logMsg[ESMF_MAXSTR];
        sprintf(logMsg, "s=%lld out-of-range with respect to "
                        "machine limits (INT_MIN=%d to INT_MAX=%d), "
                        "use s_i8.", remainingSeconds, INT_MIN, INT_MAX);
        ESMC_LogDefault.Write(logMsg, ESMC_LOGMSG_WARN,ESMC_CONTEXT);
        return (ESMF_FAILURE);
      }
      *s = (ESMC_I4) remainingSeconds;    // >= 32 bit
    }
    if (s_i8 != ESMC_NULL_POINTER) {
      *s_i8 = remainingSeconds;   // >= 64 bit
    }
    if (s != ESMC_NULL_POINTER || s_i8 != ESMC_NULL_POINTER) {
      remainingSeconds = 0;  // remove seconds
    }

    // integer fractional seconds

    // reset whole seconds part of remaining time
    remainingTime.setw(remainingSeconds);

    if (ms != ESMC_NULL_POINTER) {
      // convert remaining time to milliseconds
      Fraction msRemainingTime = remainingTime;
      int rc = msRemainingTime.convert(1000);
      if (ESMC_LogDefault.MsgFoundError(rc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        &rc))
        return(rc);
      *ms = msRemainingTime.getn();

      // remove total milliseconds from remainingTime
      Fraction milliseconds(0, *ms, 1000);
      remainingTime -= milliseconds;
    }
    if (us != ESMC_NULL_POINTER) {
      // convert remaining time to microseconds
      Fraction usRemainingTime = remainingTime;
      int rc = usRemainingTime.convert(1000000);
      if (ESMC_LogDefault.MsgFoundError(rc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        &rc))
        return(rc);
      *us = usRemainingTime.getn();

      // remove total microseconds from remainingTime
      Fraction microseconds(0, *us, 1000000);
      remainingTime -= microseconds;
    }
    if (ns != ESMC_NULL_POINTER) {
      // convert remaining time to nanoseconds
      Fraction nsRemainingTime = remainingTime;
      int rc = nsRemainingTime.convert(1000000000);
      if (ESMC_LogDefault.MsgFoundError(rc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
        &rc))
        return(rc);
      *ns = nsRemainingTime.getn();
    }

    //
    // Floating point units
    //
    //  Since floating point values can carry the full value of the time in
    //  question, the full original remainingTime is converted for each
    //  requested floating point unit; multiple requested units are *not*
    //  bounded by next higher unit, whether integer or floating point, i.e.
    //  units *not* removed (subtracted) after getting it.  Could make a case
    //  for bounding floating point values by higher, but not lower, integer
    //  units, but would need more logic/testing/docs to do this.  TODO ?
    //

    // reset original full remainingTime for floating point conversion
    remainingTime = saveRemainingTime;

    // avoid error introduced by floating point divide or multiply by
    // performing an integer divide or multiply first, then converting to
    // floating point.
    if (h_r8 != ESMC_NULL_POINTER) {
      BaseTime hours = remainingTime;
      hours /= SECONDS_PER_HOUR;
      *h_r8 = hours.getr();
    }
    if (m_r8 != ESMC_NULL_POINTER) {
      BaseTime minutes = remainingTime;
      minutes /= SECONDS_PER_MINUTE;
      *m_r8 = minutes.getr();
    }
    if (s_r8 != ESMC_NULL_POINTER) {
      *s_r8 = remainingTime.getr();
    }
    if (ms_r8 != ESMC_NULL_POINTER) {
      // convert remaining time to milliseconds
      BaseTime milliseconds = remainingTime;
      milliseconds *= 1000;
      *ms_r8 = milliseconds.getr();
    }
    if (us_r8 != ESMC_NULL_POINTER) {
      // convert remaining time to microseconds
      BaseTime microseconds = remainingTime;
      microseconds *= 1000000;
      *us_r8 = microseconds.getr();
    }
    if (ns_r8 != ESMC_NULL_POINTER) {
      // convert remaining time to nanoseconds
      BaseTime nanoseconds = remainingTime;
      nanoseconds *= 1000000000;
      *ns_r8 = nanoseconds.getr();
    }

    return(rc);

}  // end ESMCI::BaseTime::get

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMCI::BaseTime(=) - assignment operator
//
// !INTERFACE:
      BaseTime& BaseTime::operator=(
//
// !RETURN VALUE:
//    ESMCI::BaseTime& result
//
// !ARGUMENTS:
      const Fraction &fraction) {   // in - Fraction
//
// !DESCRIPTION:
//      Assign current object's (this) {\tt ESMC\_Fraction} with given
//      {\tt ESMC\_Fraction}.  
//EOP
// !REQUIREMENTS:  

    // TODO: should be implicit, but then won't support
    //   F90 ESMF_Time & ESMF_TimeInterval via ESMC_BaseTime_F.C interface
    //   for increment/decrement

    // use = operator in Fraction class
    Fraction::operator=(fraction);

    return(*this);

}  // end ESMCI::BaseTime::operator=

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMCI::BaseTime::readRestart - restore BaseTime state
//
// !INTERFACE:
      int BaseTime::readRestart(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int          nameLen,   // in
      const char  *name) {    // in
//
// !DESCRIPTION:
//      restore {\tt BaseTime} state for persistence/checkpointing.
//
//EOP
// !REQUIREMENTS:

    int rc = ESMF_SUCCESS;

    // TODO:  read base time state from name, then restore
    //        (share code with ESMCI::BaseTime::set()).

    return(rc);

}  // end ESMCI::BaseTime::readRestart

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMCI::BaseTime::writeRestart - save BaseTime state
//
// !INTERFACE:
      int BaseTime::writeRestart(void) const {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      Save {\tt BaseTime} state for persistence/checkpointing
//
//EOP
// !REQUIREMENTS: 

    int rc = ESMF_SUCCESS;

    // TODO:

    return(rc);

}  // end ESMCI::BaseTime::writeRestart

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMCI::BaseTime::validate - validate BaseTime state
//
// !INTERFACE:
      int BaseTime::validate(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {     // in - options
//
// !DESCRIPTION:
//      validate {\tt ESMCI::BaseTime} state
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::BaseTime::validate()"

    return(Fraction::validate());

}  // end ESMCI::BaseTime::validate

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMCI::BaseTime::print - print BaseTime state
//
// !INTERFACE:
      int BaseTime::print(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {    // in - print options
//
// !DESCRIPTION:
//      print {\tt ESMCI::BaseTime} state for testing/debugging
//
//EOP
// !REQUIREMENTS:  

    printf("BaseTime -------------------------------\n");
    printf("s = %lld\n", getw());
    printf("sN = %lld\n",  getn());
    printf("sD = %lld\n",  getd());
    printf("end BaseTime ---------------------------\n\n");

    return(ESMF_SUCCESS);

}  // end ESMCI::BaseTime::print

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMCI::BaseTime - native default C++ constructor
//
// !INTERFACE:
      BaseTime::BaseTime(void) {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      Initializes a {\tt ESMCI::BaseTime} with defaults
//
//EOP
// !REQUIREMENTS:  

    Fraction(0,0,1);

}  // end ESMCI::BaseTime

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMCI::BaseTime - native C++ constructor
//
// !INTERFACE:
      BaseTime::BaseTime(
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
      ESMC_I8 s,              // in - integer seconds
      ESMC_I8 sN,             // in - fractional seconds, numerator
      ESMC_I8 sD) :           // in - fractional seconds, denominator
//
// !DESCRIPTION:
//      Initializes a {\tt ESMC\_BaseTime}
//
//EOP
// !REQUIREMENTS:  

    Fraction(s, sN, sD) {  // use base class constructor

}  // end ESMCI::BaseTime

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMCI::~BaseTime - native default C++ destructor
//
// !INTERFACE:
      BaseTime::~BaseTime(void) {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      Default {\tt ESMCI::BaseTime} destructor
//
//EOP
// !REQUIREMENTS:  

} // end ~BaseTime

} // end namespace BaseTime
