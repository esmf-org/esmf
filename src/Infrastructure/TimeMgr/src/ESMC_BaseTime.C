// $Id: ESMC_BaseTime.C,v 1.18 2003/04/28 23:07:37 eschwab Exp $
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
// The code in this file implements the C++ {\tt BaseTime} methods declared
// in the companion file {\tt ESMC_BaseTime.h}
//
//-------------------------------------------------------------------------
//
 // insert any higher level, 3rd party or system includes here
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
 static const char *const version = "$Id: ESMC_BaseTime.C,v 1.18 2003/04/28 23:07:37 eschwab Exp $";
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
// !IROUTINE:  ESMC_BaseTimeInit - shallow class initializer 1
//
// !INTERFACE:
      int ESMC_BaseTime::ESMC_BaseTimeInit(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMF_IKIND_I8 S,    // in - integer seconds
      int Sn,             // in - fractional seconds, numerator
      int Sd ) {          // in - fractional seconds, denominator
//
// !DESCRIPTION:
//      Initialzes a {\tt BaseTime} with given values
//
//EOP
// !REQUIREMENTS:  

    // S, Sn must be either both positive or both negative;
    //    Sd always positive and >= 1
    if ( ((S >= 0 && Sn >= 0) || (S <= 0 && Sn <= 0)) && Sd >= 1 )
    {
        this->S = S;
        this->Sn = Sn;
        this->Sd = Sd;

        // normalize (share logic with += ?? )
        int w;
        if (labs((w = this->Sn/this->Sd)) >= 1)
        {
             this->S += w;
             this->Sn = this->Sn % this->Sd;
        }

        return(ESMF_SUCCESS);
    }
    else return(ESMF_FAILURE);

}  // end ESMC_BaseTimeInit

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
      int secondsPerDay,        // in  - seconds per day
      int *H,                   // out - integer hours
      int *M,                   // out - integer minutes
      ESMF_IKIND_I8 *S,         // out - integer seconds
      int *MS,                  // out - integer milliseconds
      int *US,                  // out - integer microseconds
      int *NS,                  // out - integer nanoseconds
      double *h_,               // out - floating point hours
      double *m_,               // out - floating point minutes
      double *s_,               // out - floating point seconds
      double *ms_,              // out - floating point milliseconds
      double *us_,              // out - floating point microseconds
      double *ns_,              // out - floating point nanoseconds
      int *Sn,                  // out - fractional seconds numerator
      int *Sd) const {          // out - fractional seconds denominator

//
// !DESCRIPTION:
//      Get sub-day (non-calendar dependent) values of a {\tt BaseTime}
//      converted to user units.  Primarily to support F90 interface
//
//EOP
// !REQUIREMENTS:  

    // TODO: fractional seconds

    // for sub-day units, start with number of seconds into the date
    ESMF_IKIND_I8 remainder = this->S % secondsPerDay;

    if (H != ESMC_NULL_POINTER) {
      *H = remainder / SECONDS_PER_HOUR;
      remainder %= SECONDS_PER_HOUR;
    }
    if (M != ESMC_NULL_POINTER) {
      *M = remainder / SECONDS_PER_MINUTE;
      remainder %= SECONDS_PER_MINUTE;
    }
    if (S != ESMC_NULL_POINTER) {
      *S = remainder;
    }

    //
    // floating point units
    //

    // for sub-day units, start with number of seconds into the date
    remainder = this->S % secondsPerDay;

    if (h_ != ESMC_NULL_POINTER) {
      *h_ = (double) remainder / (double) SECONDS_PER_HOUR;
      remainder %= SECONDS_PER_HOUR;
    }
    if (m_ != ESMC_NULL_POINTER) {
      *m_ = (double) remainder / (double) SECONDS_PER_MINUTE;
      remainder %= SECONDS_PER_MINUTE;
    }
    if (s_ != ESMC_NULL_POINTER) {
      *s_ = (double) remainder;
    }

    return(ESMF_SUCCESS);

}  // end ESMC_BaseTimeGet

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
      int *H,                   // out - integer hours
      int *M,                   // out - integer minutes
      ESMF_IKIND_I8 *S,         // out - integer seconds
      int *MS,                  // out - integer milliseconds
      int *US,                  // out - integer microseconds
      int *NS,                  // out - integer nanoseconds
      double *h_,               // out - floating point hours
      double *m_,               // out - floating point minutes
      double *s_,               // out - floating point seconds
      double *ms_,              // out - floating point milliseconds
      double *us_,              // out - floating point microseconds
      double *ns_,              // out - floating point nanoseconds
      int *Sn,                  // out - fractional seconds numerator
      int *Sd) {                // out - fractional seconds denominator
//
// !DESCRIPTION:
//      Sets sub-day (non-calendar dependent) values of a {\tt BaseTime}.
//      Primarily to support F90 interface.
//
//EOP
// !REQUIREMENTS:  

    // TODO: fractional seconds

    if (H != ESMC_NULL_POINTER) {
      this->S += *H * SECONDS_PER_HOUR;
    }
    if (M != ESMC_NULL_POINTER) {
      this->S += *M * SECONDS_PER_MINUTE;
    }
    if (S != ESMC_NULL_POINTER) {
      this->S += *S;
    }

    //
    // floating point units
    //

    if (h_ != ESMC_NULL_POINTER) {
      this->S += (ESMF_IKIND_I8) (*h_ * SECONDS_PER_HOUR);
    }
    if (m_ != ESMC_NULL_POINTER) {
      this->S += (ESMF_IKIND_I8) (*m_ * SECONDS_PER_MINUTE);
    }
    if (s_ != ESMC_NULL_POINTER) {
      this->S += (ESMF_IKIND_I8) *s_;
    }

    return(ESMF_SUCCESS);

}  // end ESMC_BaseTimeSet

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
//      Compare for equality the current object's (this) {\tt BaseTime} with
//      given {\tt BaseTime}, return result
//
//EOP
// !REQUIREMENTS:  

    return(S == baseTime.S);
    // TODO: compare equal Sn/Sd fractions when Sd differs

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
//      Compare for inequality the current object's (this) {\tt BaseTime} with
//      given {\tt BaseTime}, return result
//
//EOP
// !REQUIREMENTS:  

    return(S != baseTime.S);
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
//      Compare for less than the current object's (this) {\tt BaseTime} with
//      given {\tt BaseTime}, return result
//
//EOP
// !REQUIREMENTS:  

    return(S < baseTime.S);
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
//      Compare for greater than the current object's (this) {\tt BaseTime} with
//      given {\tt BaseTime}, return result
//
//EOP
// !REQUIREMENTS:  

    return(S > baseTime.S);
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
//      {\tt BaseTime} with given {\tt BaseTime}, return result
//
//EOP
// !REQUIREMENTS:  

    return(S <= baseTime.S);
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
//      {\tt BaseTime} with given {\tt BaseTime}, return result
//
//EOP
// !REQUIREMENTS:  

    return(S >= baseTime.S);
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
//      Increment current object's (this) {\tt BaseTime} with given
//      {\tt BaseTime}, return result
//
//EOP
// !REQUIREMENTS:  

    ESMC_BaseTime sum = *this;

    // assume positive values for now ??
    // fractional part addition -- LCD (assume same denominator for now) ??
    sum.Sn += baseTime.Sn;

    // normalize (share logic with ESMC_BaseTimeInit() ?? )
    int w;
    if (labs((w = sum.Sn/sum.Sd)) >= 1)
    {
         sum.S += w;
         sum.Sn = sum.Sn % sum.Sd;
    }

    // whole part addition
    sum.S += baseTime.S;

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
//      Decrement current object's (this) {\tt BaseTime} with given
//      {\tt BaseTime}, return result
//
//EOP
// !REQUIREMENTS:  

    ESMC_BaseTime diff = *this;

    // assume positive values for now ??
    // assume this > Time and both normalized for now ??
    // fractional part subtraction -- LCD (assume same denominator for now) ??

    // fractional part subtraction
    if (diff.Sn < baseTime.Sn)
    {
        // borrow
        diff.Sn += diff.Sd;
        diff.S--;
    }
    diff.Sn -= baseTime.Sn;

    // whole part subtraction 
    diff.S -= baseTime.S;

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
//      Increment current object's (this) {\tt BaseTime} with given
//      {\tt BaseTime}
//EOP
// !REQUIREMENTS:  

    // assume positive values for now ??
    // fractional part addition -- LCD (assume same denominator for now) ??
    Sn += baseTime.Sn;

    // normalize (share logic with ESMC_BaseTimeInit() ?? )
    int w;
    if (labs((w = Sn/Sd)) >= 1)
    {
         S += w;
         Sn = Sn % Sd;
    }

    // whole part addition
    S += baseTime.S;

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
//      Decrement current object's (this) {\tt BaseTime} with given
//      {\tt BaseTime}
//
//EOP
// !REQUIREMENTS:  

    // assume positive values for now ??
    // assume this > baseTime and both normalized for now ??
    // fractional part subtraction -- LCD (assume same denominator for now) ??

    // fractional part subtraction
    if (Sn < baseTime.Sn)
    {
        // borrow
        Sn += Sd;
        S--;
    }
    Sn -= baseTime.Sn;

    // whole part subtraction 
    S -= baseTime.S;

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
//      Assign current object's (this) {\tt BaseTime} with given
//      {\tt BaseTime}.  Supports ESMC_Time & ESMC_TimeInterval
//EOP
// !REQUIREMENTS:  

    // TODO: should be implicit, but then won't support
    //   ESMC_Time & ESMC_TimeInterval ?
    S  = baseTime.S;
    Sn = baseTime.Sn;
    Sd = baseTime.Sd;

    return(*this);

}  // end ESMC_BaseTime::operator=

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BaseTimeRead - restore BaseTime state
//
// !INTERFACE:
      int ESMC_BaseTime::ESMC_BaseTimeRead(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMF_IKIND_I8 S,    // in - integer seconds
      int Sn,             // in - fractional seconds, numerator
      int Sd) {           // in - fractional seconds, denominator
//
// !DESCRIPTION:
//      restore {\tt BaseTime} state for persistence/checkpointing
//
//EOP
// !REQUIREMENTS:  

    this->S  = S;
    this->Sn = Sn;
    this->Sd = Sd;

    return(ESMF_SUCCESS);

}  // end ESMC_BaseTimeRead

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BaseTimeWrite - save BaseTime state
//
// !INTERFACE:
      int ESMC_BaseTime::ESMC_BaseTimeWrite(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMF_IKIND_I8 *S,    // out - integer seconds
      int *Sn,             // out - fractional seconds, numerator
      int *Sd) const {     // out - fractional seconds, denominator
//
// !DESCRIPTION:
//      return {\tt BaseTime} state for persistence/checkpointing
//
//EOP
// !REQUIREMENTS:  

    if (S  == ESMC_NULL_POINTER || Sn == ESMC_NULL_POINTER ||
        Sd == ESMC_NULL_POINTER) {
      cout << "ESMC_BaseTime::ESMC_BaseTimeWrite(): null pointer(s) passed in "
           << endl;
      return(ESMF_FAILURE);
    }

    *S = this->S;
    *Sn = this->Sn;
    *Sd = this->Sd;

    return(ESMF_SUCCESS);

}  // end ESMC_BaseTimeWrite

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
//      validate {\tt BaseTime} state
//
//EOP
// !REQUIREMENTS:  

// Code goes here TODO

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
//      print {\tt BaseTime} state for testing/debugging
//
//EOP
// !REQUIREMENTS:  

    cout << "BaseTime -------------------------------" << endl;
    cout << "S = "  << S  << endl;
    cout << "Sn = " << Sn << endl;
    cout << "Sd = " << Sd << endl;
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
//      Initializes a {\tt ESMC\_BaseTime} with defaults via
//      {\tt ESMC\_BaseTimeInit}
//
//EOP
// !REQUIREMENTS:  

    S  = 0;
    Sn = 0;
    Sd = 1;

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
      ESMF_IKIND_I8 S,              // in - integer seconds
      int Sn,             // in - fractional seconds, numerator
      int Sd) {           // in - fractional seconds, denominator
//
// !DESCRIPTION:
//      Initializes a {\tt ESMC\_BaseTime} via {\tt ESMC\_BaseTimeInit}
//
//EOP
// !REQUIREMENTS:  

    this->S  = S;
    this->Sn = Sn;
    this->Sd = Sd;

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
