// $Id: ESMC_BaseTime.C,v 1.24 2003/08/29 05:31:58 eschwab Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMC BaseTime method code (body) file

//-------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ {\tt ESMC\_BaseTime} methods declared
// in the companion file {\tt ESMC\_BaseTime.h}
//
//-------------------------------------------------------------------------
//
 #include <iostream.h>
 #include <stdlib.h>
 /*
 #include <iostream>
 #include <stdlib>
 using std::cout;
 using std::endl;
 */

 // associated class definition file
 #include <ESMC_BaseTime.h>

//-------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_BaseTime.C,v 1.24 2003/08/29 05:31:58 eschwab Exp $";
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
      ESMF_IKIND_I4 *h,       // out - integer hours
      ESMF_IKIND_I4 *m,       // out - integer minutes
      ESMF_IKIND_I4 *s,       // out - integer seconds (>= 32 bit)
      ESMF_IKIND_I8 *s_i8,    // out - integer seconds (large, >= 64 bit)
      ESMF_IKIND_I4 *ms,      // out - integer milliseconds
      ESMF_IKIND_I4 *us,      // out - integer microseconds
      ESMF_IKIND_I4 *ns,      // out - integer nanoseconds
      ESMF_IKIND_R8 *h_r8,    // out - floating point hours
      ESMF_IKIND_R8 *m_r8,    // out - floating point minutes
      ESMF_IKIND_R8 *s_r8,    // out - floating point seconds
      ESMF_IKIND_R8 *ms_r8,   // out - floating point milliseconds
      ESMF_IKIND_R8 *us_r8,   // out - floating point microseconds
      ESMF_IKIND_R8 *ns_r8,   // out - floating point nanoseconds
      ESMF_IKIND_I4 *sN,      // out - fractional seconds numerator
      ESMF_IKIND_I4 *sD) {    // out - fractional seconds denominator
//
// !DESCRIPTION:
//      Sets sub-day (non-calendar dependent) values of a {\tt ESMC\_BaseTime}.
//      Primarily to support F90 interface.
//
//EOP
// !REQUIREMENTS:  

    // TODO: fractional seconds

    if (h != ESMC_NULL_POINTER) {
      this->s += *h * SECONDS_PER_HOUR;
    }
    if (m != ESMC_NULL_POINTER) {
      this->s += *m * SECONDS_PER_MINUTE;
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
      this->s += (ESMF_IKIND_I8) (*h_r8 * SECONDS_PER_HOUR);
    }
    if (m_r8 != ESMC_NULL_POINTER) {
      this->s += (ESMF_IKIND_I8) (*m_r8 * SECONDS_PER_MINUTE);
    }
    if (s_r8 != ESMC_NULL_POINTER) {
      this->s += (ESMF_IKIND_I8) *s_r8;
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
      ESMF_IKIND_I8 s,      // in - integer seconds
      ESMF_IKIND_I4 sN,     // in - fractional seconds, numerator
      ESMF_IKIND_I4 sD ) {  // in - fractional seconds, denominator
//
// !DESCRIPTION:
//      Initialzes a {\tt ESMC\_BaseTime} with given values
//
//EOP
// !REQUIREMENTS:  

    // s, sN must be either both positive or both negative;
    //    sD always positive and >= 1
    if ( ((s >= 0 && sN >= 0) || (s <= 0 && sN <= 0)) && sD >= 1 ) {
        this->s  = s;
        this->sN = sN;
        this->sD = sD;

        // normalize (share logic with += ?? )
        ESMF_IKIND_I4 w;
        if (labs((w = this->sN/this->sD)) >= 1) {
          this->s += w;
          this->sN = this->sN % this->sD;
        }

        return(ESMF_SUCCESS);
    }
    else return(ESMF_FAILURE);

}  // end ESMC_BaseTimeSet

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BaseTimeGet - get sub-day units of a basetime
//
// !INTERFACE:
      int ESMC_BaseTime::ESMC_BaseTimeGet(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMF_IKIND_I4 secondsPerDay,  // in  - seconds per day
      ESMF_IKIND_I4 *h,             // out - integer hours
      ESMF_IKIND_I4 *m,             // out - integer minutes
      ESMF_IKIND_I4 *s,             // out - integer seconds (>= 32-bit)
      ESMF_IKIND_I8 *s_i8,          // out - integer seconds (large, >= 64-bit)
      ESMF_IKIND_I4 *ms,            // out - integer milliseconds
      ESMF_IKIND_I4 *us,            // out - integer microseconds
      ESMF_IKIND_I4 *ns,            // out - integer nanoseconds
      ESMF_IKIND_R8 *h_r8,          // out - floating point hours
      ESMF_IKIND_R8 *m_r8,          // out - floating point minutes
      ESMF_IKIND_R8 *s_r8,          // out - floating point seconds
      ESMF_IKIND_R8 *ms_r8,         // out - floating point milliseconds
      ESMF_IKIND_R8 *us_r8,         // out - floating point microseconds
      ESMF_IKIND_R8 *ns_r8,         // out - floating point nanoseconds
      ESMF_IKIND_I4 *sN,            // out - fractional seconds numerator
      ESMF_IKIND_I4 *sD) const {    // out - fractional seconds denominator

//
// !DESCRIPTION:
//      Get sub-day (non-calendar dependent) values of a {\tt ESMC\_BaseTime}
//      converted to user units.  Primarily to support F90 interface
//
//EOP
// !REQUIREMENTS:  

    int rc = ESMF_SUCCESS;

    // TODO: fractional seconds

    // for sub-day units, start with number of seconds into the date
    ESMF_IKIND_I4 remainder = this->s % secondsPerDay;

    if (h != ESMC_NULL_POINTER) {
      *h = remainder / SECONDS_PER_HOUR;
      remainder %= SECONDS_PER_HOUR;
    }
    if (m != ESMC_NULL_POINTER) {
      *m = remainder / SECONDS_PER_MINUTE;
      remainder %= SECONDS_PER_MINUTE;
    }
    if (s != ESMC_NULL_POINTER) {
      *s = remainder;    // >= 32 bit
    }
    if (s_i8 != ESMC_NULL_POINTER) {
      *s_i8 = remainder;   // >= 64 bit
    }

    //
    // floating point units
    //

    // for sub-day units, start with number of seconds into the date
    remainder = this->s % secondsPerDay;

    if (h_r8 != ESMC_NULL_POINTER) {
      *h_r8 = (ESMF_IKIND_R8) remainder / (ESMF_IKIND_R8) SECONDS_PER_HOUR;
      remainder %= SECONDS_PER_HOUR;
    }
    if (m_r8 != ESMC_NULL_POINTER) {
      *m_r8 = (ESMF_IKIND_R8) remainder / (ESMF_IKIND_R8) SECONDS_PER_MINUTE;
      remainder %= SECONDS_PER_MINUTE;
    }
    if (s_r8 != ESMC_NULL_POINTER) {
      *s_r8 = (ESMF_IKIND_R8) remainder;
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
    ESMF_IKIND_I4 w;
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
    ESMF_IKIND_I4 w;
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
      ESMF_IKIND_I8 s,    // in - integer seconds
      ESMF_IKIND_I4 sN,   // in - fractional seconds, numerator
      ESMF_IKIND_I4 sD) { // in - fractional seconds, denominator
//
// !DESCRIPTION:
//      restore {\tt ESMC\_BaseTime} state for persistence/checkpointing
//
//EOP
// !REQUIREMENTS:  

    this->s  = s;
    this->sN = sN;
    this->sD = sD;

    return(ESMF_SUCCESS);

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
      ESMF_IKIND_I8 *s,    // out - integer seconds
      ESMF_IKIND_I4 *sN,             // out - fractional seconds, numerator
      ESMF_IKIND_I4 *sD) const {     // out - fractional seconds, denominator
//
// !DESCRIPTION:
//      return {\tt ESMC\_BaseTime} state for persistence/checkpointing
//
//EOP
// !REQUIREMENTS:  

    if (s  == ESMC_NULL_POINTER || sN == ESMC_NULL_POINTER ||
        sD == ESMC_NULL_POINTER) {
      cout << "ESMC_BaseTime::ESMC_BaseTimeWriteRestart(): null pointer(s) passed in " << endl;
      return(ESMF_FAILURE);
    }

    *s  = this->s;
    *sN = this->sN;
    *sD = this->sD;

    return(ESMF_SUCCESS);

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

    // must have positive denominator
    if (sD <= 0) return(ESMF_FAILURE);

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
      ESMF_IKIND_I8 s,              // in - integer seconds
      ESMF_IKIND_I4 sN,             // in - fractional seconds, numerator
      ESMF_IKIND_I4 sD) {           // in - fractional seconds, denominator
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
