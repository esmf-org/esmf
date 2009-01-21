// $Id: ESMC_BaseTime.C,v 1.45.2.2 2009/01/21 21:25:23 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research,
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
// The code in this file implements the C++ {\tt ESMC\_BaseTime} methods
// declared in the companion file {\tt ESMC\_BaseTime.h}
//
//-------------------------------------------------------------------------
//
 #define ESMC_FILENAME "ESMC_BaseTime.C"

 #include <stdio.h>
 #include <stdlib.h>
 #include <limits.h>
 /*
 #include <iostream>
 #include <stdlib>
 using std::cout;
 using std::endl;
 */
 #include <ESMC_LogErr.h>
 #include <ESMF_LogMacros.inc>

 // associated class definition file
 #include <ESMC_BaseTime.h>

//-------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_BaseTime.C,v 1.45.2.2 2009/01/21 21:25:23 cdeluca Exp $";
//-------------------------------------------------------------------------

//
//-------------------------------------------------------------------------
//-------------------------------------------------------------------------
//
// This section includes all the BaseTime routines
//
//

//-------------------------------------------------------------------------
// Class ESMC_BaseTime Methods
//-------------------------------------------------------------------------

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BaseTimeSet - set sub-day values of a basetime
//
// !INTERFACE:
      int ESMC_BaseTime::ESMC_BaseTimeSet(
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
      ESMC_I4 *sD) {    // in - fractional seconds denominator
//
// !DESCRIPTION:
//      Sets sub-day (non-calendar dependent) values of a {\tt ESMC\_BaseTime}.
//      Primarily to support F90 interface.
//
//EOP
// !REQUIREMENTS:  

    //
    // whole seconds
    //

    // integer units
    if (h != ESMC_NULL_POINTER) {
      ESMC_Fraction time(((ESMC_I8) *h) * SECONDS_PER_HOUR);
      *this += time;
    }
    if (m != ESMC_NULL_POINTER) {
      ESMC_Fraction time(((ESMC_I8) *m) * SECONDS_PER_MINUTE);
      *this += time;
    }
    if (s != ESMC_NULL_POINTER) {
      ESMC_Fraction time(*s);
      *this += time;  // >= 32-bit
    } else if (s_i8 != ESMC_NULL_POINTER) {
      ESMC_Fraction time(*s_i8);
      *this += time;  // >= 64-bit
    }

    // floating point units
    // TODO: include fractional part
    if (h_r8 != ESMC_NULL_POINTER) {
      ESMC_Fraction time((ESMC_I8) (*h_r8 * SECONDS_PER_HOUR));
      *this += time;
    }
    if (m_r8 != ESMC_NULL_POINTER) {
      ESMC_Fraction time((ESMC_I8) (*m_r8 * SECONDS_PER_MINUTE));
      *this += time;
    }
    if (s_r8 != ESMC_NULL_POINTER) {
      ESMC_Fraction time((ESMC_I8) *s_r8);
      *this += time;
    }

    //
    // fractional seconds
    //

    // integer units
    if (ms != ESMC_NULL_POINTER) {
      ESMC_Fraction fractional_time(0, *ms, 1000);
      *this += fractional_time;
    }
    if (us != ESMC_NULL_POINTER) {
      ESMC_Fraction fractional_time(0, *us, 1000000);
      *this += fractional_time;
    }
    if (ns != ESMC_NULL_POINTER) {
      ESMC_Fraction fractional_time(0, *ns, 1000000000);
      *this += fractional_time;
    }

    // floating point units
    // TODO: include fractional part
    if (ms_r8 != ESMC_NULL_POINTER) {
      ESMC_Fraction fractional_time(0, (ESMC_I4) *ms_r8, 1000);
      *this += fractional_time;
    }
    if (us_r8 != ESMC_NULL_POINTER) {
      ESMC_Fraction fractional_time(0, (ESMC_I4) *us_r8, 1000000);
      *this += fractional_time;
    }
    if (ns_r8 != ESMC_NULL_POINTER) {
      ESMC_Fraction fractional_time(0, (ESMC_I4) *ns_r8, 1000000000);
      *this += fractional_time;
    }

    // integer numerator and denominator
    if (sN != ESMC_NULL_POINTER && sD != ESMC_NULL_POINTER) {
      ESMC_Fraction fractional_time(0, *sN, *sD);
      *this += fractional_time;
    }

    return(ESMF_SUCCESS);

}  // end ESMC_BaseTimeSet

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BaseTimeSet - direct core value initializer
//
// !INTERFACE:
      int ESMC_BaseTime::ESMC_BaseTimeSet(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_I8 s,      // in - integer seconds
      ESMC_I4 sN,     // in - fractional seconds, numerator
      ESMC_I4 sD ) {  // in - fractional seconds, denominator
//
// !DESCRIPTION:
//      Initialzes a {\tt ESMC\_BaseTime} with given values
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_BaseTimeSet()"

    // s, sN must be either both positive or both negative;
    //    sD always positive and >= 1
    if ( !(((s >= 0 && sN >= 0) || (s <= 0 && sN <= 0)) && sD >= 1) ) {
      char logMsg[ESMF_MAXSTR];
      sprintf(logMsg, "s=%lld and sN=%d not both positive or both negative, "
                      "or sD=%d negative or less than one.", s, sN, sD); 
      ESMC_LogDefault.ESMC_LogWrite(logMsg, ESMC_LOG_ERROR);
      return(ESMF_FAILURE);
    }

    ESMC_FractionSet(s, sN, sD);

    return(ESMF_SUCCESS);

}  // end ESMC_BaseTimeSet

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BaseTimeGet - get units of a basetime
//
// !INTERFACE:
      int ESMC_BaseTime::ESMC_BaseTimeGet(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const ESMC_BaseTime *timeToConvert, // in  - the time to convert
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
      ESMC_I4 *sD) const {     // out - fractional seconds denominator

//
// !DESCRIPTION:
//      Get non-calendar dependent values of a {\tt ESMC\_BaseTime}
//      converted to user units.  Primarily to support F90 interface
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_BaseTimeGet()"

    int rc = ESMF_SUCCESS;

    // validate input
    if (timeToConvert == ESMC_NULL_POINTER) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
                                            "; timeToConvert is NULL", &rc);
      return(rc);
    }

    // make local copy for manipulation
    ESMC_BaseTime remainingTime = *timeToConvert;
    remainingTime.ESMC_FractionSimplify(); // ensure maximum whole seconds
    ESMC_BaseTime saveRemainingTime = remainingTime;  // for float units below
    ESMC_I8 remainingSeconds = remainingTime.ESMC_FractionGetw();

    // get integer numerator and denominator
    if (sN != ESMC_NULL_POINTER) {
      *sN = remainingTime.ESMC_FractionGetn();
    }
    if (sD != ESMC_NULL_POINTER) {
      *sD = remainingTime.ESMC_FractionGetd();
    }

    if (h != ESMC_NULL_POINTER) {
      ESMC_I8 hours = remainingSeconds / SECONDS_PER_HOUR;
      if (hours < INT_MIN || hours > INT_MAX) {
        char logMsg[ESMF_MAXSTR];
        sprintf(logMsg, "For s=%lld, hours=%lld out-of-range with respect to "
                        "machine limits (INT_MIN=%d to INT_MAX=%d).",
                        remainingSeconds, hours, INT_MIN, INT_MAX);
        ESMC_LogDefault.ESMC_LogWrite(logMsg, ESMC_LOG_ERROR);
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
        ESMC_LogDefault.ESMC_LogWrite(logMsg, ESMC_LOG_ERROR);
        return (ESMF_FAILURE);
      }
      *m = minutes;
      remainingSeconds %= SECONDS_PER_MINUTE;  // remove minutes
    }
    if (s != ESMC_NULL_POINTER) {
      if (remainingSeconds < INT_MIN || remainingSeconds > INT_MAX) {
        char logMsg[ESMF_MAXSTR];
        sprintf(logMsg, "s=%lld out-of-range with respect to "
                        "machine limits (INT_MIN=%d to INT_MAX=%d).",
                        remainingSeconds, INT_MIN, INT_MAX);
        ESMC_LogDefault.ESMC_LogWrite(logMsg, ESMC_LOG_ERROR);
        return (ESMF_FAILURE);
      }
      *s = remainingSeconds;    // >= 32 bit
    }
    if (s_i8 != ESMC_NULL_POINTER) {
      *s_i8 = remainingSeconds;   // >= 64 bit
    }
    if (s != ESMC_NULL_POINTER || s_i8 != ESMC_NULL_POINTER) {
      remainingSeconds = 0;  // remove seconds
    }

    // fractional seconds

    // reset whole seconds part of remaining time
    remainingTime.ESMC_FractionSetw(remainingSeconds);

    if (ms != ESMC_NULL_POINTER) {
      // convert remaining time to milliseconds
      ESMC_Fraction msRemainingTime = remainingTime;
      int rc = msRemainingTime.ESMC_FractionConvert(1000);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &rc))
        return(rc);
      *ms = msRemainingTime.ESMC_FractionGetn();

      // remove total milliseconds from remainingTime
      ESMC_Fraction milliseconds(0, *ms, 1000);
      remainingTime -= milliseconds;
    }
    if (us != ESMC_NULL_POINTER) {
      // convert remaining time to microseconds
      ESMC_Fraction usRemainingTime = remainingTime;
      int rc = usRemainingTime.ESMC_FractionConvert(1000000);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &rc))
        return(rc);
      *us = usRemainingTime.ESMC_FractionGetn();

      // remove total microseconds from remainingTime
      ESMC_Fraction microseconds(0, *us, 1000000);
      remainingTime -= microseconds;
    }
    if (ns != ESMC_NULL_POINTER) {
      // convert remaining time to nanoseconds
      ESMC_Fraction nsRemainingTime = remainingTime;
      int rc = nsRemainingTime.ESMC_FractionConvert(1000000000);
      if (ESMC_LogDefault.ESMC_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &rc))
        return(rc);
      *ns = nsRemainingTime.ESMC_FractionGetn();
    }

    //
    // floating point units
    //
    // TODO:  include fractional part 

    // reset remainingSeconds for floating point conversion
    remainingTime = saveRemainingTime;
    remainingSeconds = remainingTime.ESMC_FractionGetw();

    if (h_r8 != ESMC_NULL_POINTER) {
      *h_r8 = (ESMC_R8) remainingSeconds / SECONDS_PER_HOUR;
      remainingSeconds %= SECONDS_PER_HOUR;    // remove hours
    }
    if (m_r8 != ESMC_NULL_POINTER) {
      *m_r8 = (ESMC_R8) remainingSeconds / SECONDS_PER_MINUTE;
      remainingSeconds %= SECONDS_PER_MINUTE;  // remove minutes
    }
    if (s_r8 != ESMC_NULL_POINTER) {
      *s_r8 = (ESMC_R8) remainingSeconds;
      remainingSeconds = 0;   // remove seconds
    }

    // reset whole seconds part of remaining time
    remainingTime.ESMC_FractionSetw(remainingSeconds);

    if (ms_r8 != ESMC_NULL_POINTER) {
      // convert remaining time to milliseconds
      ESMC_Fraction msRemainingTime = remainingTime;

      // TODO: use ESMC_FractionConvert() when n/d changed to ESMC_I8 ?

      // get total milliseconds
      ESMC_R8 w = msRemainingTime.ESMC_FractionGetw();
      ESMC_R8 n = msRemainingTime.ESMC_FractionGetn();
      ESMC_R8 d = msRemainingTime.ESMC_FractionGetd();
      *ms_r8 = w * 1000 + (n * 1000) / d;

      // remove total milliseconds from remainingTime
      ESMC_Fraction milliseconds(0, (ESMC_I4)*ms_r8, 1000);
      remainingTime -= milliseconds;
    }
    if (us_r8 != ESMC_NULL_POINTER) {
      // convert remaining time to microseconds
      ESMC_Fraction usRemainingTime = remainingTime;

      // TODO: use ESMC_FractionConvert() when n/d changed to ESMC_I8 ?

      // get total microseconds
      ESMC_R8 w = usRemainingTime.ESMC_FractionGetw();
      ESMC_R8 n = usRemainingTime.ESMC_FractionGetn();
      ESMC_R8 d = usRemainingTime.ESMC_FractionGetd();
      *us_r8 = w * 1000000 + (n * 1000000) / d;

      // remove total microseconds from remainingTime
      ESMC_Fraction microseconds(0, (ESMC_I4)*us_r8, 1000000);
      remainingTime -= microseconds;
    }
    if (ns_r8 != ESMC_NULL_POINTER) {
      // convert remaining time to nanoseconds
      ESMC_Fraction nsRemainingTime = remainingTime;

      // TODO: use ESMC_FractionConvert() when n/d changed to ESMC_I8 ?

      // get total nanoseconds
      ESMC_R8 w = nsRemainingTime.ESMC_FractionGetw();
      ESMC_R8 n = nsRemainingTime.ESMC_FractionGetn();
      ESMC_R8 d = nsRemainingTime.ESMC_FractionGetd();
      *ns_r8 = w * 1000000000 + (n * 1000000000) / d;
    }

    return(rc);

}  // end ESMC_BaseTimeGet

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BaseTime(=) - assignment operator
//
// !INTERFACE:
      ESMC_BaseTime& ESMC_BaseTime::operator=(
//
// !RETURN VALUE:
//    ESMC_BaseTime& result
//
// !ARGUMENTS:
      const ESMC_Fraction &fraction) {   // in - ESMC_Fraction
//
// !DESCRIPTION:
//      Assign current object's (this) {\tt ESMC\_Fraction} with given
//      {\tt ESMC\_Fraction}.  
//EOP
// !REQUIREMENTS:  

    // TODO: should be implicit, but then won't support
    //   F90 ESMF_Time & ESMF_TimeInterval via ESMC_BaseTime_F.C interface
    //   for increment/decrement

    // use = operator in ESMC_Fraction class
    ESMC_Fraction::operator=(fraction);

    return(*this);

}  // end ESMC_BaseTime::operator=

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BaseTimeReadRestart - restore BaseTime state
//
// !INTERFACE:
      int ESMC_BaseTime::ESMC_BaseTimeReadRestart(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int          nameLen,   // in
      const char  *name,      // in
      ESMC_IOSpec *iospec) {  // in
//
// !DESCRIPTION:
//      restore {\tt BaseTime} state for persistence/checkpointing.
//
//EOP
// !REQUIREMENTS:

    int rc = ESMF_SUCCESS;

    // TODO:  read base time state from iospec/name, then restore
    //        (share code with ESMC_BaseTimeSet()).

    return(rc);

}  // end ESMC_BaseTimeReadRestart

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BaseTimeWriteRestart - save BaseTime state
//
// !INTERFACE:
      int ESMC_BaseTime::ESMC_BaseTimeWriteRestart(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_IOSpec *iospec) const {
//
// !DESCRIPTION:
//      Save {\tt BaseTime} state for persistence/checkpointing
//
//EOP
// !REQUIREMENTS: 

    int rc = ESMF_SUCCESS;

    // TODO:

    return(rc);

}  // end ESMC_BaseTimeWriteRestart

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BaseTimeValidate - validate BaseTime state
//
// !INTERFACE:
      int ESMC_BaseTime::ESMC_BaseTimeValidate(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {     // in - options
//
// !DESCRIPTION:
//      validate {\tt ESMC\_BaseTime} state
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_BaseTimeValidate()"

    return(ESMC_FractionValidate());

}  // end ESMC_BaseTimeValidate

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BaseTimePrint - print BaseTime state
//
// !INTERFACE:
      int ESMC_BaseTime::ESMC_BaseTimePrint(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {    // in - print options
//
// !DESCRIPTION:
//      print {\tt ESMC\_BaseTime} state for testing/debugging
//
//EOP
// !REQUIREMENTS:  

    printf("BaseTime -------------------------------\n");
    printf("s = %lld\n", ESMC_FractionGetw());
    printf("sN = %d\n",  ESMC_FractionGetn());
    printf("sD = %d\n",  ESMC_FractionGetd());
    printf("end BaseTime ---------------------------\n\n");

    return(ESMF_SUCCESS);

}  // end ESMC_BaseTimePrint

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BaseTime - native default C++ constructor
//
// !INTERFACE:
      ESMC_BaseTime::ESMC_BaseTime(void) {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      Initializes a {\tt ESMC\_BaseTime} with defaults
//
//EOP
// !REQUIREMENTS:  

    ESMC_Fraction(0,0,1);

}  // end ESMC_BaseTime

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BaseTime - native C++ constructor
//
// !INTERFACE:
      ESMC_BaseTime::ESMC_BaseTime(
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
      ESMC_I8 s,              // in - integer seconds
      ESMC_I4 sN,             // in - fractional seconds, numerator
      ESMC_I4 sD) :           // in - fractional seconds, denominator
//
// !DESCRIPTION:
//      Initializes a {\tt ESMC\_BaseTime}
//
//EOP
// !REQUIREMENTS:  

    ESMC_Fraction(s, sN, sD) {  // use base class constructor

}  // end ESMC_BaseTime

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ~ESMC_BaseTime - native default C++ destructor
//
// !INTERFACE:
      ESMC_BaseTime::~ESMC_BaseTime(void) {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      Default {\tt ESMC\_BaseTime} destructor
//
//EOP
// !REQUIREMENTS:  

} // end ~ESMC_BaseTime
