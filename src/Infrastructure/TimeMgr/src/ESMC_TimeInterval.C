// $Id: ESMC_TimeInterval.C,v 1.18 2003/04/28 23:20:17 eschwab Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the GPL.
//
// ESMC TimeInterval method code (body) file
//
//-------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ {\tt TimeInterval} methods declared
// in the companion file {\tt ESMC_TimeInterval.h}
//
//-------------------------------------------------------------------------

 // higher level, 3rd party or system includes
 #include <iostream.h>
 #include <math.h>
 #include <float.h>

 // associated class definition file
 #include <ESMC_TimeInterval.h>

//-------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_TimeInterval.C,v 1.18 2003/04/28 23:20:17 eschwab Exp $";
//-------------------------------------------------------------------------

//
//-------------------------------------------------------------------------
//-------------------------------------------------------------------------
//
// This section includes all the ESMC_TimeInterval routines
//
//

//-------------------------------------------------------------------------
// Class ESMC_TimeInterval Methods
//-------------------------------------------------------------------------

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeIntervalInit - initializer for native C++ use
//
// !INTERFACE:
      int ESMC_TimeInterval::ESMC_TimeIntervalInit(
//
// !RETURN VALUE:
//    int error return code
// 
// !ARGUMENTS:
      const char *timeList,    // in - initializer specifier string
      ...) {                   // in - specifier values (variable args)
//
// !DESCRIPTION:
//      Initialzes a {\tt TimeInterval} with values given in variable arg list.
//      Supports native C++ use.
//
//EOP
// !REQUIREMENTS:

    // TODO
    return(ESMF_SUCCESS);

 }  // end ESMC_TimeIntervalInit

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeIntervalInit - initializer to support F90 interface
//
// !INTERFACE:
      int ESMC_TimeInterval::ESMC_TimeIntervalInit(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMF_IKIND_I8 *YY,        // in - integer number of interval years
      ESMF_IKIND_I8 *MO,        // in - integer number of interval months
      ESMF_IKIND_I8 *D,         // in - integer number of interval days
      int *H,                   // in - integer hours
      int *M,                   // in - integer minutes
      ESMF_IKIND_I8 *S,         // in - integer seconds 
      int *MS,                  // in - integer milliseconds
      int *US,                  // in - integer microseconds
      int *NS,                  // in - integer nanoseconds
      double *d_,               // in - floating point days
      double *h_,               // in - floating point hours
      double *m_,               // in - floating point minutes
      double *s_,               // in - floating point seconds
      double *ms_,              // in - floating point milliseconds
      double *us_,              // in - floating point microseconds
      double *ns_,              // in - floating point nanoseconds
      int *Sn,                  // in - fractional seconds numerator
      int *Sd) {                // in - fractional seconds denominator
//
// !DESCRIPTION:
//      Initialzes a {\tt TimeInterval} with values given in F90
//      variable arg list.
//
//EOP
// !REQUIREMENTS:  

    // TODO: ensure initialization if called via F90 interface;
    //       cannot call constructor, because destructor is subsequently
    //       called automatically, returning initialized values to garbage.
    this->S  = 0;
    this->Sn = 0;
    this->Sd = 1;
    this->YY = 0;
    this->MO = 0;
    this->D  = 0;
    
    // TODO: validate inputs (individual and combos), set basetime values
    //       e.g. integer and float specifiers are mutually exclusive

    ESMC_TimeIntervalSet(YY, MO, D, H, M, S, MS, US, NS,
                         d_, h_, m_, s_, ms_, us_, ns_, Sn, Sd);

    return(ESMF_SUCCESS);

 }  // end ESMC_TimeIntervalInit

#if 0
//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeIntervalInit - shallow class initializer 1
//
// !INTERFACE:
      int ESMC_TimeInterval::ESMC_TimeIntervalInit(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMF_IKIND_I8 S,     // in - integer seconds
      int Sn,              // in - fractional seconds, numerator
      int Sd,              // in - fractional seconds, denominator
      ESMF_IKIND_I8 YY,    // in - calendar interval number of years
      ESMF_IKIND_I8 MO)    // in - calendar interval number of months
      ESMF_IKIND_I8 D)  {  // in - calendar interval number of days
//
// !DESCRIPTION:
//      Initialzes a {\tt TimeInterval} with given values
//
//EOP
// !REQUIREMENTS:

    // use base class Init()
    if (ESMC_BaseTime::ESMC_BaseTimeInit(S, Sn, Sd) == ESMF_SUCCESS)
    {
      this->YY = YY;
      this->MO = MO;
      this->D  = D;

      return(ESMF_SUCCESS);
    }
    else return(ESMF_FAILURE);

}  // end ESMC_TimeIntervalInit
#endif

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeIntervalGet - Get a TimeInterval value
//
// !INTERFACE:
      int ESMC_TimeInterval::ESMC_TimeIntervalGet(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *TimeList,    // in  - time interval value specifier string
      ...) const {             // out - specifier values (variable args)
//
// !DESCRIPTION:
//      Gets a {\tt TimeInterval}'s values in user-specified format.
//      This version supports native C++ use.
//
//EOP
// !REQUIREMENTS:  

    // TODO
    return(ESMF_SUCCESS);

 }  // end ESMC_TimeIntervalGet

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeIntervalSet - Set a TimeInterval value
//
// !INTERFACE:
      int ESMC_TimeInterval::ESMC_TimeIntervalSet(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *TimeList,    // in - time interval value specifier string
      ...) {                   // in - specifier values (variable args)
//
// !DESCRIPTION:
//      Sets a {\tt TimeInterval}'s values in user-specified values.
//      This version supports native C++ use.
//
//EOP
// !REQUIREMENTS:  

    // TODO
    return(ESMF_SUCCESS);

 }  // end ESMC_TimeIntervalSet

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeIntervalGet - Get a TimeInterval value; supports F90 interface
//
// !INTERFACE:
      int ESMC_TimeInterval::ESMC_TimeIntervalGet(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMF_IKIND_I8 *YY,        // out - integer number of interval years
      ESMF_IKIND_I8 *MO,        // out - integer number of interval months
      ESMF_IKIND_I8 *D,         // out - integer number of interval days
      int *H,                   // out - integer hours
      int *M,                   // out - integer minutes
      ESMF_IKIND_I8 *S,         // out - integer seconds 
      int *MS,                  // out - integer milliseconds
      int *US,                  // out - integer microseconds
      int *NS,                  // out - integer nanoseconds
      double *d_,               // out - floating point days
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
//      Gets a {\tt TimeInterval}'s values in user-specified format.
//      This version supports the F90 interface.
//
//EOP
// !REQUIREMENTS:  

    // TODO: fractional, sub-seconds

    // calendar interval
    if (YY != ESMC_NULL_POINTER) {
      *YY = this->YY;
    }
    if (MO != ESMC_NULL_POINTER) {
      *MO = this->MO;
    }
      // TODO: use when Calendar Intervals implemented
//    if (D != ESMC_NULL_POINTER) {
//      *D = this->D;
//    }

    // TODO: use Calendar-defined SecondsPerDay; for now assume 86400
    // get number of seconds in a day
    int secPerDay = SECONDS_PER_DAY;
//    if (Calendar != ESMC_NULL_POINTER) {
//      secPerDay = Calendar->SecondsPerDay;
//    }

    if (D != ESMC_NULL_POINTER) {
      *D = this->S / secPerDay;
    }
    if (d_ != ESMC_NULL_POINTER) {
      *d_ = (double) this->S / (double) secPerDay;
    }

    // use base class to get sub-day values
    ESMC_BaseTimeGet(secPerDay, H, M, S, MS, US, NS,
                     h_, m_, s_, ms_, us_, ns_, Sn, Sd);

    return(ESMF_SUCCESS);

 }  // end ESMC_TimeIntervalGet

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeIntervalSet - Set a TimeInterval value; supports F90 interface
//
// !INTERFACE:
      int ESMC_TimeInterval::ESMC_TimeIntervalSet(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMF_IKIND_I8 *YY,        // in - integer number of interval years
      ESMF_IKIND_I8 *MO,        // in - integer number of interval months
      ESMF_IKIND_I8 *D,         // in - integer number of interval days
      int *H,                   // in - integer hours
      int *M,                   // in - integer minutes
      ESMF_IKIND_I8 *S,         // in - integer seconds 
      int *MS,                  // in - integer milliseconds
      int *US,                  // in - integer microseconds
      int *NS,                  // in - integer nanoseconds
      double *d_,               // in - floating point days
      double *h_,               // in - floating point hours
      double *m_,               // in - floating point minutes
      double *s_,               // in - floating point seconds
      double *ms_,              // in - floating point milliseconds
      double *us_,              // in - floating point microseconds
      double *ns_,              // in - floating point nanoseconds
      int *Sn,                  // in - fractional seconds numerator
      int *Sd) {                // in - fractional seconds denominator
//
// !DESCRIPTION:
//      Sets a {\tt TimeInterval}'s values in user-specified format.
//      This version supports the F90 interface.
//
//EOP
// !REQUIREMENTS:  

    // TODO: validate inputs (individual and combos), set basetime values
    //       e.g. integer and float specifiers are mutually exclusive

    // TODO: fractional, sub-seconds

    // TODO: share code with ESMC_TimeIntervalInit ?

    // calendar interval
    if (YY != ESMC_NULL_POINTER) {
      this->YY = *YY;
    }
    if (MO != ESMC_NULL_POINTER) {
      this->MO = *MO;
    }
    if (D != ESMC_NULL_POINTER) {
      this->D = *D;
    }

    // TODO: use Calendar-defined SecondsPerDay; for now assume 86400
    // get number of seconds in a day
    int secPerDay = SECONDS_PER_DAY;
//    if (Calendar != ESMC_NULL_POINTER) {
//      secPerDay = Calendar->SecondsPerDay;
//    }

    if (D != ESMC_NULL_POINTER) {
      this->S += *D * secPerDay;
    }
    if (d_ != ESMC_NULL_POINTER) {
      this->S += (ESMF_IKIND_I8) (*d_ * secPerDay);
    }

    // use base class set for sub-day values
    ESMC_BaseTimeSet(H, M, S, MS, US, NS, h_, m_, s_, ms_, us_, ns_, Sn, Sd);

    return(ESMF_SUCCESS);

 }  // end ESMC_TimeIntervalSet

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeIntervalGetString - Get a Time Interval value in string format
//
// !INTERFACE:
      int ESMC_TimeInterval::ESMC_TimeIntervalGetString(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      char *timeString) const {    // out - time interval value in string format
//
// !DESCRIPTION:
//      Gets a {\tt TimeInterval}'s value in character format
//
//EOP
// !REQUIREMENTS:  

    // TODO
    return(ESMF_SUCCESS);

 }  // end ESMC_TimeIntervalGetString

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeIntervalAbsValue - Get a Time Interval's absolute value
//
// !INTERFACE:
      ESMC_TimeInterval
                     ESMC_TimeInterval::ESMC_TimeIntervalAbsValue(void) const{
//
// !RETURN VALUE:
//    ESMC_TimeInterval result
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      Gets a {\tt TimeInterval}'s absolute value
//
//EOP
// !REQUIREMENTS:  TMG 1.5.8

    // initialize result to subject time interval
    ESMC_TimeInterval absValue = *this;

    // check individual components (should be all positive or all negative)
    //   note: can't use abs() or labs() since these will be (long long)
    //         (64-bit) on some platforms
    if (S  < 0) absValue.S  *= -1;
    if (Sn < 0) absValue.Sn *= -1;
    if (YY < 0) absValue.YY *= -1;
    if (MO < 0) absValue.MO *= -1;
//   TODO: use when Calendar Intervals implemented
//    if (D  < 0) absValue.D  *= -1;

    return(absValue);

 }  // end ESMC_TimeIntervalAbsValue

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeIntervalNegAbsVal - Get a Time Interval's negative absolute value
//
// !INTERFACE:
      ESMC_TimeInterval
                     ESMC_TimeInterval::ESMC_TimeIntervalNegAbsVal(void) const{
//
// !RETURN VALUE:
//    ESMC_TimeInterval result
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      Gets a {\tt TimeInterval}'s negative absolute value
//
//EOP
// !REQUIREMENTS:  TMG 1.5.8

    // initialize result to subject time interval
    ESMC_TimeInterval negAbsVal = *this;

    // check individual components (should be all positive or all negative)
    //   note: can't use abs() or labs() since these will be (long long)
    //         (64-bit) on some platforms
    if (S  > 0) negAbsVal.S  *= -1;
    if (Sn > 0) negAbsVal.Sn *= -1;
    if (YY > 0) negAbsVal.YY *= -1;
    if (MO > 0) negAbsVal.MO *= -1;
//   TODO: use when Calendar Intervals implemented
//    if (D  > 0) negAbsVal.D  *= -1;

    return(negAbsVal);

 }  // end ESMC_TimeIntervalNegAbsVal

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeIntervalDiv - Divide two time intervals, return fraction result
//
// !INTERFACE:
      ESMC_Fraction ESMC_TimeInterval::ESMC_TimeIntervalDiv(
//
// !RETURN VALUE:
//    ESMC_Fraction result
//
// !ARGUMENTS:
      const ESMC_TimeInterval &timeInterval) const {  // in - ESMC_TimeInterval
                                                      //        to divide by
//
// !DESCRIPTION:
//    Returns this time interval divided by given time interval as a fractional
//    quotient.
//
// !REQUIREMENTS:  

    ESMC_Fraction quotient;

    // TODO:

    return(quotient);

}  // end ESMC_TimeInterval::ESMC_TimeIntervalDiv

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeInterval(/) - Divide two time intervals, return double precision result
//
// !INTERFACE:
      double ESMC_TimeInterval::operator/(
//
// !RETURN VALUE:
//    double result
//
// !ARGUMENTS:
      const ESMC_TimeInterval &timeInterval) const {  // in - ESMC_TimeInterval
                                                      //        to divide by
//
// !DESCRIPTION:
//    Returns this time interval divided by given time interval as a double
//    precision quotient.
//
// !REQUIREMENTS:  

    double quotient;

    // TODO: fractional & calendar interval parts

    if (timeInterval.S != 0) {
      quotient = (double) this->S / (double) timeInterval.S;
    }
    // TODO:  else throw exception ?

    return(quotient);

}  // end ESMC_TimeInterval::operator/

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeInterval(/) - Divide time interval by an integer, return time interval result
//
// !INTERFACE:
      ESMC_TimeInterval ESMC_TimeInterval::operator/(
//
// !RETURN VALUE:
//    ESMC_TimeInterval result
//
// !ARGUMENTS:
      const int &divisor) const {   // in - integer divisor
//
// !DESCRIPTION:
//    Divides a {\tt TimeInterval} by an integer divisor, returns quotient as a
//    {\tt TimeInterval}
//
// !REQUIREMENTS:  

    ESMC_TimeInterval quotient;

    // TODO: fractional & calendar interval parts

    if (divisor != 0) {
      quotient.S = this->S / divisor;
    }
    // TODO:  else throw exception ?

    return(quotient);

}  // end ESMC_TimeInterval::operator/

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeInterval(/=) - Divide time interval by an integer
//
// !INTERFACE:
      ESMC_TimeInterval& ESMC_TimeInterval::operator/=(
//
// !RETURN VALUE:
//    ESMC_TimeInterval& result
//
// !ARGUMENTS:
      const int &divisor) {   // in - integer divisor
//
// !DESCRIPTION:
//    Divides a {\tt TimeInterval} by an integer divisor
//
// !REQUIREMENTS:  

    // TODO: fractional & calendar interval parts

    if (divisor != 0) {
      this->S /= divisor;
    }
    // TODO:  else throw exception ?

    return(*this);

}  // end ESMC_TimeInterval::operator/=

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeInterval(/) - Divide time interval by a double precision, return time interval result
//
// !INTERFACE:
      ESMC_TimeInterval ESMC_TimeInterval::operator/(
//
// !RETURN VALUE:
//    ESMC_TimeInterval result
//
// !ARGUMENTS:
      const double &divisor) const {   // in - double precision divisor
//
// !DESCRIPTION:
//    Divides a {\tt TimeInterval} by an double divisor, returns quotient as a
//    {\tt TimeInterval}
//
// !REQUIREMENTS:  

    ESMC_TimeInterval quotient;

    // TODO: fractional & calendar interval parts

    if (fabs(divisor) > FLT_EPSILON) {
      quotient.S = (ESMF_IKIND_I8) ((double) this->S / divisor);
    }
    // TODO:  else throw exception ?

    return(quotient);

}  // end ESMC_TimeInterval::operator/

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeInterval(/=) - Divide time interval by a double precision
//
// !INTERFACE:
      ESMC_TimeInterval& ESMC_TimeInterval::operator/=(
//
// !RETURN VALUE:
//    ESMC_TimeInterval& result
//
// !ARGUMENTS:
      const double &divisor) {   // in - double precision divisor
//
// !DESCRIPTION:
//    Divides a {\tt TimeInterval} by a double precision divisor
//
// !REQUIREMENTS:  

    // TODO: fractional & calendar interval parts

    if (fabs(divisor) > FLT_EPSILON) {
      this->S = (ESMF_IKIND_I8) ((double) this->S / divisor);
    }
    // TODO:  else throw exception ?

    return(*this);

}  // end ESMC_TimeInterval::operator/=

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeInterval(*) - Multiply a time interval by an integer
//
// !INTERFACE:
      ESMC_TimeInterval ESMC_TimeInterval::operator*(
//
// !RETURN VALUE:
//    ESMC_TimeInterval result
//
// !ARGUMENTS:
      const int &multiplier) const {   // in - integer multiplier
//
// !DESCRIPTION:
//     Multiply a {\tt TimeInterval} by an integer, return product as a
//    {\tt TimeInterval}
//
// !REQUIREMENTS:  

    ESMC_TimeInterval product;

    // TODO: fractional & calendar interval parts

    product.S = this->S * multiplier;

    return(product);

}  // end ESMC_TimeInterval::operator*

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeInterval(*=) - Multiply a time interval by an integer
//
// !INTERFACE:
      ESMC_TimeInterval& ESMC_TimeInterval::operator*=(
//
// !RETURN VALUE:
//    ESMC_TimeInterval& result
//
// !ARGUMENTS:
      const int &multiplier) {   // in - integer multiplier
//
// !DESCRIPTION:
//     Multiply a {\tt TimeInterval} by an integer
//
// !REQUIREMENTS:  

    // TODO: fractional & calendar interval parts

    this->S *= multiplier;

    return(*this);

}  // end ESMC_TimeInterval::operator*=

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeInterval(*) - Multiply a time interval by an fraction
//
// !INTERFACE:
      ESMC_TimeInterval ESMC_TimeInterval::operator*(
//
// !RETURN VALUE:
//    ESMC_TimeInterval result
//
// !ARGUMENTS:
      const ESMC_Fraction &multiplier) const {   // in - fraction multiplier
//
// !DESCRIPTION:
//     Multiply a {\tt TimeInterval} by an fraction, return product as a
//    {\tt TimeInterval}
//
// !REQUIREMENTS:  

    ESMC_TimeInterval product;

    // TODO: whole, fractional & calendar interval parts

    return(product);

}  // end ESMC_TimeInterval::operator*

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeInterval(*=) - Multiply a time interval by an fraction
//
// !INTERFACE:
      ESMC_TimeInterval& ESMC_TimeInterval::operator*=(
//
// !RETURN VALUE:
//    ESMC_TimeInterval& result
//
// !ARGUMENTS:
      const ESMC_Fraction &multiplier) {   // in - fraction multiplier
//
// !DESCRIPTION:
//     Multiply a {\tt TimeInterval} by a fraction
//
// !REQUIREMENTS:  

    // TODO: whole, fractional & calendar interval parts

    return(*this);

}  // end ESMC_TimeInterval::operator*=

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeInterval(*) - Multiply a time interval by a double precision
//
// !INTERFACE:
      ESMC_TimeInterval ESMC_TimeInterval::operator*(
//
// !RETURN VALUE:
//    ESMC_TimeInterval result
//
// !ARGUMENTS:
      const double &multiplier) const {   // in - double precision multiplier
//
// !DESCRIPTION:
//     Multiply a {\tt TimeInterval} by an double precision,
//     return product as a {\tt TimeInterval}
//
// !REQUIREMENTS:  

    ESMC_TimeInterval product;

    // TODO: fractional & calendar interval parts

    product.S = (ESMF_IKIND_I8) ((double) this->S * multiplier);

    return(product);

}  // end ESMC_TimeInterval::operator*

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeInterval(*=) - Multiply a time interval by a double precision
//
// !INTERFACE:
      ESMC_TimeInterval& ESMC_TimeInterval::operator*=(
//
// !RETURN VALUE:
//    ESMC_TimeInterval& result
//
// !ARGUMENTS:
      const double &multiplier) {   // in - double precision multiplier
//
// !DESCRIPTION:
//     Multiply a {\tt TimeInterval} by a double precision
//
// !REQUIREMENTS:  

    // TODO: fractional & calendar interval parts

    this->S = (ESMF_IKIND_I8) ((double) this->S * multiplier);

    return(*this);

}  // end ESMC_TimeInterval::operator*=

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeInterval(=) - copy or assign from BaseTime expression
//
// !INTERFACE:
      ESMC_TimeInterval& ESMC_TimeInterval::operator=(
//
// !RETURN VALUE:
//    ESMC_TimeInterval& result
//
// !ARGUMENTS:
      const ESMC_BaseTime &baseTime) {   // in - ESMC_BaseTime to copy
//
// !DESCRIPTION:
//    Assign {\tt BaseTime} expression to this time intervals.
//    Supports inherited operators from {\tt ESMC\_BaseTime}
//
// !REQUIREMENTS:  

    // invoke base class assignment operator
    // TODO:  should be implicit ?
    ESMC_BaseTime::operator=(baseTime);

    return(*this);

}  // end ESMC_TimeInterval::operator=

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeIntervalRead - restore TimeInterval state
//
// !INTERFACE:
      int ESMC_TimeInterval::ESMC_TimeIntervalRead(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMF_IKIND_I8 S,     // in - integer seconds
      int Sn,              // in - fractional seconds, numerator
      int Sd,              // in - fractional seconds, denominator
      ESMF_IKIND_I8 YY,    // in - calendar interval number of years
      ESMF_IKIND_I8 MO,    // in - calendar interval number of months
      ESMF_IKIND_I8 D) {   // in - calendar interval number of days
//
// !DESCRIPTION:
//      restore {\tt TimeInterval} state for persistence/checkpointing
//
//EOP
// !REQUIREMENTS:  

    int rc;

    // use base class Read() first
    rc = ESMC_BaseTime::ESMC_BaseTimeRead(S, Sn, Sd);

    // restore exclusive Time Interval properties
    this->YY = YY;
    this->MO = MO;
    this->D  = D;

    return(rc);

 }  // end ESMC_TimeIntervalRead

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeIntervalWrite - return TimeInterval state
//
// !INTERFACE:
      int ESMC_TimeInterval::ESMC_TimeIntervalWrite(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMF_IKIND_I8 *S,           // out - integer seconds
      int *Sn,                    // out - fractional seconds, numerator
      int *Sd,                    // out - fractional seconds, denominator
      ESMF_IKIND_I8 *YY,          // out - calendar interval number of years
      ESMF_IKIND_I8 *MO,          // out - calendar interval number of months
      ESMF_IKIND_I8 *D) const {   // out - calendar interval number of days
//
// !DESCRIPTION:
//      return {\tt TimeInterval} state for persistence/checkpointing
//
//EOP
// !REQUIREMENTS:  

    int rc;

    if (S  == ESMC_NULL_POINTER || Sn == ESMC_NULL_POINTER ||
        Sd == ESMC_NULL_POINTER || YY == ESMC_NULL_POINTER ||
        MO == ESMC_NULL_POINTER || D  == ESMC_NULL_POINTER) {
      cout << "ESMC_TimeInterval::ESMC_TimeIntervalWrite(): null pointer(s) "
           << "passed in" << endl;
      return(ESMF_FAILURE);
    }

    // use base class Write() first
    rc = ESMC_BaseTime::ESMC_BaseTimeWrite(S, Sn, Sd);

    //  return exclusive Time Interval properties
    *YY = this->YY;
    *MO = this->MO;
    *D  = this->D;

    return(rc);

 }  // end ESMC_TimeIntervalWrite

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeIntervalValidate - validate TimeInterval state
//
// !INTERFACE:
      int ESMC_TimeInterval::ESMC_TimeIntervalValidate(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {    // in - validate options
//
// !DESCRIPTION:
//      validate {\tt TimeInterval} state for testing/debugging
//
//EOP
// !REQUIREMENTS:  

    // TODO
    return(ESMF_SUCCESS);

 }  // end ESMC_TimeIntervalValidate

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeIntervalPrint - print TimeInterval state
//
// !INTERFACE:
      int ESMC_TimeInterval::ESMC_TimeIntervalPrint(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {    // in - print options
//
// !DESCRIPTION:
//      print {\tt TimeInterval} state for testing/debugging
//
//EOP
// !REQUIREMENTS:  

    cout << "TimeInterval ---------------------------" << endl;
    ESMC_BaseTime::ESMC_BaseTimePrint(options);
    cout << "YY = " << YY << endl;
    cout << "MO = " << MO << endl;
    cout << "D  = " << D << endl;
    cout << "end TimeInterval -----------------------" << endl << endl;

    return(ESMF_SUCCESS);

 }  // end ESMC_TimeIntervalPrint

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeInterval - native default C++ constructor
//
// !INTERFACE:
      ESMC_TimeInterval::ESMC_TimeInterval(void) {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      Initializes an {\tt ESMC\_TimeInterval} with defaults via
//      {\tt ESMC\_BaseTime} base class
//
//EOP
// !REQUIREMENTS:

//   ESMC_BaseTime(0, 0, 1) { // TODO: F90 issue with base class constructor?
   S  = 0;
   Sn = 0;
   Sd = 0;
   YY = 0;
   MO = 0;
   D  = 0;

} // end ESMC_TimeInterval

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeInterval - native C++ constructor
//
// !INTERFACE:
     ESMC_TimeInterval::ESMC_TimeInterval(
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
      ESMF_IKIND_I8 S,     // in - integer seconds
      int Sn,              // in - fractional seconds, numerator
      int Sd,              // in - fractional seconds, denominator
      ESMF_IKIND_I8 YY,    // in - calendar interval number of years
      ESMF_IKIND_I8 MO,    // in - calendar interval number of months
      ESMF_IKIND_I8 D) :   // in - calendar interval number of days
//
// !DESCRIPTION:
//      Initializes a {\tt ESMC\_TimeInterval} via {\tt ESMC\_BaseTime}
//      base class
//
//EOP
// !REQUIREMENTS:

    ESMC_BaseTime(S, Sn, Sd) {  // pass to base class constructor

    this->YY = YY;
    this->MO = MO;
    this->D  = D;

} // end ESMC_TimeInterval

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ~ESMC_TimeInterval - native default C++ destructor
//
// !INTERFACE:
      ESMC_TimeInterval::~ESMC_TimeInterval(void) {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      Default {\tt ESMC\_TimeInterval} destructor
//
//EOP
// !REQUIREMENTS:

}  // end ~ESMC_TimeInterval
