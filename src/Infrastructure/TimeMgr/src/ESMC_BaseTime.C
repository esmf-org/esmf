// $Id: ESMC_BaseTime.C,v 1.32 2004/05/24 20:27:33 eschwab Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the GPL.
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
 #define ESMF_FILENAME "ESMC_BaseTime.C"

 #include <iostream.h>
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
 static const char *const version = "$Id: ESMC_BaseTime.C,v 1.32 2004/05/24 20:27:33 eschwab Exp $";
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
      ESMF_KIND_I4 *h,       // out - integer hours
      ESMF_KIND_I4 *m,       // out - integer minutes
      ESMF_KIND_I4 *s,       // out - integer seconds (>= 32 bit)
      ESMF_KIND_I8 *s_i8,    // out - integer seconds (large, >= 64 bit)
      ESMF_KIND_I4 *ms,      // out - integer milliseconds
      ESMF_KIND_I4 *us,      // out - integer microseconds
      ESMF_KIND_I4 *ns,      // out - integer nanoseconds
      ESMF_KIND_R8 *h_r8,    // out - floating point hours
      ESMF_KIND_R8 *m_r8,    // out - floating point minutes
      ESMF_KIND_R8 *s_r8,    // out - floating point seconds
      ESMF_KIND_R8 *ms_r8,   // out - floating point milliseconds
      ESMF_KIND_R8 *us_r8,   // out - floating point microseconds
      ESMF_KIND_R8 *ns_r8,   // out - floating point nanoseconds
      ESMF_KIND_I4 *sN,      // out - fractional seconds numerator
      ESMF_KIND_I4 *sD) {    // out - fractional seconds denominator
//
// !DESCRIPTION:
//      Sets sub-day (non-calendar dependent) values of a {\tt ESMC\_BaseTime}.
//      Primarily to support F90 interface.
//
//EOP
// !REQUIREMENTS:  

    // TODO: fractional seconds

    if (h != ESMC_NULL_POINTER) {
      this->s += ((ESMF_KIND_I8) *h) * SECONDS_PER_HOUR;
    }
    if (m != ESMC_NULL_POINTER) {
      this->s += ((ESMF_KIND_I8) *m) * SECONDS_PER_MINUTE;
    }
    if (s != ESMC_NULL_POINTER) {
      this->s += *s;    // >= 32-bit
    } else if (s_i8 != ESMC_NULL_POINTER) {
      this->s += *s_i8;   // >= 64-bit
    }

    //
    // floating point units
    //

    if (h_r8 != ESMC_NULL_POINTER) {
      this->s += (ESMF_KIND_I8) (*h_r8 * SECONDS_PER_HOUR);
    }
    if (m_r8 != ESMC_NULL_POINTER) {
      this->s += (ESMF_KIND_I8) (*m_r8 * SECONDS_PER_MINUTE);
    }
    if (s_r8 != ESMC_NULL_POINTER) {
      this->s += (ESMF_KIND_I8) *s_r8;
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
      ESMF_KIND_I8 s,      // in - integer seconds
      ESMF_KIND_I4 sN,     // in - fractional seconds, numerator
      ESMF_KIND_I4 sD ) {  // in - fractional seconds, denominator
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
      sprintf(logMsg, "s=%d and sN=%d not both positive or both negative, "
                      "or sD=%d negative or less than one.", s, sN, sD); 
      ESMC_LogDefault.ESMC_LogWrite(logMsg, ESMC_LOG_ERROR);
      return(ESMF_FAILURE);
    }

    this->s  = s;
    this->sN = sN;
    this->sD = sD;

    // normalize (TODO: share logic with += ? )
    ESMF_KIND_I4 w;
    if (labs((w = this->sN/this->sD)) >= 1) {
      this->s += w;
      this->sN = this->sN % this->sD;
    }

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
      ESMF_KIND_I8 timeToConvert,  // in  - the time to convert (divide) into 
                                   //         requested units
      ESMF_KIND_I4 *h,             // out - integer hours
      ESMF_KIND_I4 *m,             // out - integer minutes
      ESMF_KIND_I4 *s,             // out - integer seconds (>= 32-bit)
      ESMF_KIND_I8 *s_i8,          // out - integer seconds (large, >= 64-bit)
      ESMF_KIND_I4 *ms,            // out - integer milliseconds
      ESMF_KIND_I4 *us,            // out - integer microseconds
      ESMF_KIND_I4 *ns,            // out - integer nanoseconds
      ESMF_KIND_R8 *h_r8,          // out - floating point hours
      ESMF_KIND_R8 *m_r8,          // out - floating point minutes
      ESMF_KIND_R8 *s_r8,          // out - floating point seconds
      ESMF_KIND_R8 *ms_r8,         // out - floating point milliseconds
      ESMF_KIND_R8 *us_r8,         // out - floating point microseconds
      ESMF_KIND_R8 *ns_r8,         // out - floating point nanoseconds
      ESMF_KIND_I4 *sN,            // out - fractional seconds numerator
      ESMF_KIND_I4 *sD) const {    // out - fractional seconds denominator

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

    // TODO: fractional seconds

    ESMF_KIND_I8 remainingTime = timeToConvert;

    if (h != ESMC_NULL_POINTER) {
      ESMF_KIND_I8 hours = remainingTime / SECONDS_PER_HOUR;
      if (hours < INT_MIN || hours > INT_MAX) {
        char logMsg[ESMF_MAXSTR];
        sprintf(logMsg, "For s=%ld, hours=%ld out-of-range with respect to "
                        "machine limits (INT_MIN=%ld to INT_MAX=%ld).",
                        remainingTime, hours, INT_MIN, INT_MAX);
        ESMC_LogDefault.ESMC_LogWrite(logMsg, ESMC_LOG_ERROR);
        return (ESMF_FAILURE);
      }
      *h = hours;
      remainingTime %= SECONDS_PER_HOUR;  // remove hours
    }
    if (m != ESMC_NULL_POINTER) {
      ESMF_KIND_I8 minutes = remainingTime / SECONDS_PER_MINUTE;
      if (minutes < INT_MIN || minutes > INT_MAX) {
        char logMsg[ESMF_MAXSTR];
        sprintf(logMsg, "For s=%ld, minutes=%ld out-of-range with respect to "
                        "machine limits (INT_MIN=%ld to INT_MAX=%ld).",
                        remainingTime, minutes, INT_MIN, INT_MAX);
        ESMC_LogDefault.ESMC_LogWrite(logMsg, ESMC_LOG_ERROR);
        return (ESMF_FAILURE);
      }
      *m = minutes;
      remainingTime %= SECONDS_PER_MINUTE;  // remove minutes
    }
    if (s != ESMC_NULL_POINTER) {
      if (remainingTime < INT_MIN || remainingTime > INT_MAX) {
        char logMsg[ESMF_MAXSTR];
        sprintf(logMsg, "s=%ld out-of-range with respect to "
                        "machine limits (INT_MIN=%ld to INT_MAX=%ld).",
                        remainingTime, INT_MIN, INT_MAX);
        ESMC_LogDefault.ESMC_LogWrite(logMsg, ESMC_LOG_ERROR);
        return (ESMF_FAILURE);
      }
      *s = remainingTime;    // >= 32 bit
    }
    if (s_i8 != ESMC_NULL_POINTER) {
      *s_i8 = remainingTime;   // >= 64 bit
    }

    //
    // floating point units
    //

    // reset remainingTime for floating point conversion
    remainingTime = timeToConvert;

    if (h_r8 != ESMC_NULL_POINTER) {
      *h_r8 = (ESMF_KIND_R8) remainingTime / (ESMF_KIND_R8) SECONDS_PER_HOUR;
      remainingTime %= SECONDS_PER_HOUR;  // remove hours
    }
    if (m_r8 != ESMC_NULL_POINTER) {
      *m_r8 = (ESMF_KIND_R8) remainingTime / (ESMF_KIND_R8) SECONDS_PER_MINUTE;
      remainingTime %= SECONDS_PER_MINUTE;  // remove minutes
    }
    if (s_r8 != ESMC_NULL_POINTER) {
      *s_r8 = (ESMF_KIND_R8) remainingTime;
    }

    return(rc);

}  // end ESMC_BaseTimeGet

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BaseTime(==) - BaseTime equality comparison
//
// !INTERFACE:
      bool ESMC_BaseTime::operator==(
//
// !RETURN VALUE:
//    bool result
//
// !ARGUMENTS:
      const ESMC_BaseTime &baseTime) const {   // in - ESMC_BaseTime to compare
//
// !DESCRIPTION:
//      Compare for equality the current object's (this) {\tt ESMC\_BaseTime}
//      with given {\tt ESMC\_BaseTime}, return result
//
//EOP
// !REQUIREMENTS:  

    return(s == baseTime.s);
    // TODO: compare equal sN/sD fractions when sD differs

}  // end ESMC_BaseTime::operator==

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BaseTime(!=) - BaseTime inequality comparison
//
// !INTERFACE:
      bool ESMC_BaseTime::operator!=(
//
// !RETURN VALUE:
//    bool result
//
// !ARGUMENTS:
      const ESMC_BaseTime &baseTime) const {   // in - ESMC_BaseTime to compare
//
// !DESCRIPTION:
//      Compare for inequality the current object's (this)
//      {\tt ESMC\_BaseTime} with given {\tt ESMC\_BaseTime}, return result
//
//EOP
// !REQUIREMENTS:  

    return(s != baseTime.s);
    // TODO:  compare unequal fractions

}  // end ESMC_BaseTime::operator!=

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BaseTime(<) - BaseTime less than comparison
//
// !INTERFACE:
      bool ESMC_BaseTime::operator<(
//
// !RETURN VALUE:
//    bool result
//
// !ARGUMENTS:
      const ESMC_BaseTime &baseTime) const {   // in - ESMC_BaseTime to compare
//
// !DESCRIPTION:
//      Compare for less than the current object's (this)
//      {\tt ESMC\_BaseTime} with given {\tt ESMC\_BaseTime}, return result
//
//EOP
// !REQUIREMENTS:  

    return(s < baseTime.s);
    // TODO:  compare fractions

}  // end ESMC_BaseTime::operator<

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BaseTime(>) - BaseTime greater than comparison
//
// !INTERFACE:
      bool ESMC_BaseTime::operator>(
//
// !RETURN VALUE:
//    bool result
//
// !ARGUMENTS:
      const ESMC_BaseTime &baseTime) const {   // in - ESMC_BaseTime to compare
//
// !DESCRIPTION:
//      Compare for greater than the current object's (this)
//      {\tt ESMC\_BaseTime} with given {\tt ESMC\_BaseTime}, return result
//
//EOP
// !REQUIREMENTS:  

    return(s > baseTime.s);
    // TODO:  compare fractions

}  // end ESMC_BaseTime::operator>

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BaseTime(<=) - BaseTime less or equal than comparison
//
// !INTERFACE:
      bool ESMC_BaseTime::operator<=(
//
// !RETURN VALUE:
//    bool result
//
// !ARGUMENTS:
      const ESMC_BaseTime &baseTime) const {   // in - ESMC_BaseTime to compare
//
// !DESCRIPTION:
//      Compare for less than or equal the current object's (this)
//      {\tt ESMC\_BaseTime} with given {\tt ESMC\_BaseTime}, return result
//
//EOP
// !REQUIREMENTS:  

    return(s <= baseTime.s);
    // TODO:  compare fractions

}  // end ESMC_BaseTime::operator<=

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BaseTime(>=) - BaseTime greater than or equal comparison
//
// !INTERFACE:
      bool ESMC_BaseTime::operator>=(
//
// !RETURN VALUE:
//    bool result
//
// !ARGUMENTS:
      const ESMC_BaseTime &baseTime) const {   // in - ESMC_BaseTime to compare
//
// !DESCRIPTION:
//      Compare for greater than or equal the current object's (this)
//      {\tt ESMC\_BaseTime} with given {\tt ESMC\_BaseTime}, return result
//
//EOP
// !REQUIREMENTS:  

    return(s >= baseTime.s);
    // TODO:  compare fractions

}  // end ESMC_BaseTime::operator>=

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BaseTime(+) - increment BaseTime
//
// !INTERFACE:
      ESMC_BaseTime ESMC_BaseTime::operator+(
//
// !RETURN VALUE:
//    ESMC_BaseTime result
//
// !ARGUMENTS:
      const ESMC_BaseTime &baseTime) const {   // in - ESMC_BaseTime increment
//
// !DESCRIPTION:
//      Increment current object's (this) {\tt ESMC\_BaseTime} with given
//      {\tt ESMC\_BaseTime}, return result
//
//EOP
// !REQUIREMENTS:  

    ESMC_BaseTime sum = *this;

    // assume positive values for now ??
    // fractional part addition -- LCD (assume same denominator for now) ??
    sum.sN += baseTime.sN;

    // normalize (share logic with ESMC_BaseTimeSet() ?? )
    ESMF_KIND_I4 w;
    if (labs((w = sum.sN/sum.sD)) >= 1) {
      sum.s += w;
      sum.sN = sum.sN % sum.sD;
    }

    // whole part addition
    sum.s += baseTime.s;

    return(sum);

}  // end ESMC_BaseTime::operator+

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BaseTime(-) - decrement BaseTime
//
// !INTERFACE:
      ESMC_BaseTime ESMC_BaseTime::operator-(
//
// !RETURN VALUE:
//    ESMC_BaseTime result
//
// !ARGUMENTS:
      const ESMC_BaseTime &baseTime) const {   // in - ESMC_BaseTime decrement
//
// !DESCRIPTION:
//      Decrement current object's (this) {\tt ESMC\_BaseTime} with given
//      {\tt ESMC\_BaseTime}, return result
//
//EOP
// !REQUIREMENTS:  

    ESMC_BaseTime diff = *this;

    // assume positive values for now ??
    // assume this > Time and both normalized for now ??
    // fractional part subtraction -- LCD (assume same denominator for now) ??

    // fractional part subtraction
    if (diff.sN < baseTime.sN) {
      // borrow
      diff.sN += diff.sD;
      diff.s--;
    }
    diff.sN -= baseTime.sN;

    // whole part subtraction 
    diff.s -= baseTime.s;

    return(diff);

}  // end ESMC_BaseTime::operator-

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BaseTime(+=) - increment BaseTime
//
// !INTERFACE:
      ESMC_BaseTime& ESMC_BaseTime::operator+=(
//
// !RETURN VALUE:
//    ESMC_BaseTime& result
//
// !ARGUMENTS:
      const ESMC_BaseTime &baseTime) {   // in - ESMC_BaseTime increment
//
// !DESCRIPTION:
//      Increment current object's (this) {\tt ESMC\_BaseTime} with given
//      {\tt ESMC\_BaseTime}
//EOP
// !REQUIREMENTS:  

    // assume positive values for now ??
    // fractional part addition -- LCD (assume same denominator for now) ??
    sN += baseTime.sN;

    // normalize (share logic with ESMC_BaseTimeSet() ?? )
    ESMF_KIND_I4 w;
    if (labs((w = sN/sD)) >= 1) {
      s += w;
      sN = sN % sD;
    }

    // whole part addition
    s += baseTime.s;

    return(*this);

}  // end ESMC_BaseTime::operator+=

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BaseTime(-=) - decrement BaseTime
//
// !INTERFACE:
      ESMC_BaseTime& ESMC_BaseTime::operator-=(
//
// !RETURN VALUE:
//    ESMC_BaseTime& result
//
// !ARGUMENTS:
      const ESMC_BaseTime &baseTime) {   // in - ESMC_BaseTime decrement
//
// !DESCRIPTION:
//      Decrement current object's (this) {\tt ESMC\_BaseTime} with given
//      {\tt ESMC\_BaseTime}
//
//EOP
// !REQUIREMENTS:  

    // assume positive values for now ??
    // assume this > baseTime and both normalized for now ??
    // fractional part subtraction -- LCD (assume same denominator for now) ??

    // fractional part subtraction
    if (sN < baseTime.sN) {
      // borrow
      sN += sD;
      s--;
    }
    sN -= baseTime.sN;

    // whole part subtraction 
    s -= baseTime.s;

    return(*this);

}  // end ESMC_BaseTime::operator-=

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
      const ESMC_BaseTime &baseTime) {   // in - ESMC_BaseTime
//
// !DESCRIPTION:
//      Assign current object's (this) {\tt ESMC\_BaseTime} with given
//      {\tt ESMC\_BaseTime}.  
//EOP
// !REQUIREMENTS:  

    // TODO: should be implicit, but then won't support
    //   ESMC_Time & ESMC_TimeInterval ?
    s  = baseTime.s;
    sN = baseTime.sN;
    sD = baseTime.sD;

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

    // must have positive denominator
    if (sD <= 0) {
      char logMsg[ESMF_MAXSTR];
      sprintf(logMsg, "sD=%d must be positive.", sD); 
      ESMC_LogDefault.ESMC_LogWrite(logMsg, ESMC_LOG_ERROR);
      return(ESMF_FAILURE);
    }

    return(ESMF_SUCCESS);

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

    cout << "BaseTime -------------------------------" << endl;
    cout << "s = "  << s  << endl;
    cout << "sN = " << sN << endl;
    cout << "sD = " << sD << endl;
    cout << "end BaseTime ---------------------------" << endl << endl;

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

    s  = 0;
    sN = 0;
    sD = 1;

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
      ESMF_KIND_I8 s,              // in - integer seconds
      ESMF_KIND_I4 sN,             // in - fractional seconds, numerator
      ESMF_KIND_I4 sD) {           // in - fractional seconds, denominator
//
// !DESCRIPTION:
//      Initializes a {\tt ESMC\_BaseTime}
//
//EOP
// !REQUIREMENTS:  

    this->s  = s;
    this->sN = sN;
    this->sD = sD;

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
