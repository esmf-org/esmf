// $Id: ESMC_TimeInterval.C,v 1.41 2004/01/26 21:29:17 eschwab Exp $
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
// The code in this file implements the C++ {\tt ESMC\_TimeInterval} methods
// declared in the companion file {\tt ESMC_TimeInterval.h}.
//
//-------------------------------------------------------------------------

 // higher level, 3rd party or system includes
 #include <iostream.h>
 #include <math.h>
 #include <limits.h>
 #include <float.h>
 #include <string.h>

 // associated class definition file
 #include <ESMC_TimeInterval.h>

//-------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_TimeInterval.C,v 1.41 2004/01/26 21:29:17 eschwab Exp $";
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
// !IROUTINE:  ESMC_TimeIntervalSet - initializer to support F90 interface
//
// !INTERFACE:
      int ESMC_TimeInterval::ESMC_TimeIntervalSet(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMF_KIND_I4 *yy,        // in - integer number of interval years
                               //                           (>= 32-bit)
      ESMF_KIND_I8 *yy_i8,     // in - integer number of interval years
                               //                           (large, >= 64-bit)
      ESMF_KIND_I4 *mm,        // in - integer number of interval months
                               //                           (>= 32-bit)
      ESMF_KIND_I8 *mm_i8,     // in - integer number of interval months
                               //                           (large, >= 64-bit)
      ESMF_KIND_I4 *d,         // in - integer number of interval days
                               //                           (>= 32-bit)
      ESMF_KIND_I8 *d_i8,      // in - integer number of interval days
                               //                           (large, >= 64-bit)
      ESMF_KIND_I4 *h,         // in - integer hours
      ESMF_KIND_I4 *m,         // in - integer minutes
      ESMF_KIND_I4 *s,         // in - integer seconds (>= 32-bit)
      ESMF_KIND_I8 *s_i8,      // in - integer seconds (large, >= 64-bit)
      ESMF_KIND_I4 *ms,        // in - integer milliseconds
      ESMF_KIND_I4 *us,        // in - integer microseconds
      ESMF_KIND_I4 *ns,        // in - integer nanoseconds
      ESMF_KIND_R8 *d_r8,      // in - floating point days
      ESMF_KIND_R8 *h_r8,      // in - floating point hours
      ESMF_KIND_R8 *m_r8,      // in - floating point minutes
      ESMF_KIND_R8 *s_r8,      // in - floating point seconds
      ESMF_KIND_R8 *ms_r8,     // in - floating point milliseconds
      ESMF_KIND_R8 *us_r8,     // in - floating point microseconds
      ESMF_KIND_R8 *ns_r8,     // in - floating point nanoseconds
      ESMF_KIND_I4 *sN,        // in - fractional seconds numerator
      ESMF_KIND_I4 *sD) {      // in - fractional seconds denominator
//
// !DESCRIPTION:
//      Initialzes a {\tt ESMC\_TimeInterval} with values given in F90
//      variable arg list.
//
//EOP
// !REQUIREMENTS:  

    // TODO: ensure initialization if called via F90 interface;
    //       cannot call constructor, because destructor is subsequently
    //       called automatically, returning initialized values to garbage.

    // save current value to restore in case of failure
    ESMC_TimeInterval saveTimeInterval = *this;

    this->s  = 0;
    this->sN = 0;
    this->sD = 1;
    this->yy = 0;
    this->mm = 0;
    this->d  = 0;
    
    // TODO: validate inputs (individual and combos), set basetime values
    //       e.g. integer and float specifiers are mutually exclusive

    // calendar interval
    if (yy != ESMC_NULL_POINTER) {
      this->yy = *yy;  // >= 32-bit
    } else if (yy_i8 != ESMC_NULL_POINTER) {
      this->yy = *yy_i8; // >= 64-bit
    }
    if (mm != ESMC_NULL_POINTER) {
      this->mm = *mm;  // >= 32-bit
    } else if (mm_i8 != ESMC_NULL_POINTER) {
      this->mm = *mm_i8; // >= 64-bit
    }
    if (d != ESMC_NULL_POINTER) {
      this->d = *d;  // >= 32-bit
    } else if (d_i8 != ESMC_NULL_POINTER) {
      this->d = *d_i8; // >= 64-bit
    }

    // TODO: use Calendar-defined SecondsPerDay; for now assume 86400
    // get number of seconds in a day
    int secPerDay = SECONDS_PER_DAY;
//    if (Calendar != ESMC_NULL_POINTER) {
//      secPerDay = Calendar->SecondsPerDay;
//    }

    if (d != ESMC_NULL_POINTER) {
      this->s = *d * secPerDay;  // >= 32-bit
    } else if (d_i8 != ESMC_NULL_POINTER) {
      this->s = *d_i8 * secPerDay; // >= 64-bit
    }
    if (d_r8 != ESMC_NULL_POINTER) {
      this->s = (ESMF_KIND_I8) (*d_r8 * secPerDay);
    }

    // use base class set for sub-day values
    ESMC_BaseTimeSet(h, m, s, s_i8, ms, us, ns, h_r8, m_r8, s_r8,
                     ms_r8, us_r8, ns_r8, sN, sD);

    if (ESMC_TimeIntervalValidate() == ESMF_SUCCESS) return(ESMF_SUCCESS);
    else {
      // restore previous value
      *this = saveTimeInterval;
      return(ESMF_FAILURE);
    }

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
      ESMF_KIND_I4 *yy,         // out - integer number of interval years
                                //                           (>= 32-bit)
      ESMF_KIND_I8 *yy_i8,      // out - integer number of interval years
                                //                           (large, >= 64-bit)
      ESMF_KIND_I4 *mm,         // out - integer number of interval months
                                //                           (>= 32-bit)
      ESMF_KIND_I8 *mm_i8,      // out - integer number of interval months
                                //                           (large, >= 64-bit)
      ESMF_KIND_I4 *d,          // out - integer number of interval days
                                //                           (>= 32-bit)
      ESMF_KIND_I8 *d_i8,       // out - integer number of interval days
                                //                           (large, >= 64-bit)
      ESMF_KIND_I4 *h,          // out - integer hours
      ESMF_KIND_I4 *m,          // out - integer minutes
      ESMF_KIND_I4 *s,          // out - integer seconds (>= 32-bit)
      ESMF_KIND_I8 *s_i8,       // out - integer seconds (large, >= 64-bit)
      ESMF_KIND_I4 *ms,         // out - integer milliseconds
      ESMF_KIND_I4 *us,         // out - integer microseconds
      ESMF_KIND_I4 *ns,         // out - integer nanoseconds
      ESMF_KIND_R8 *d_r8,       // out - floating point days
      ESMF_KIND_R8 *h_r8,       // out - floating point hours
      ESMF_KIND_R8 *m_r8,       // out - floating point minutes
      ESMF_KIND_R8 *s_r8,       // out - floating point seconds
      ESMF_KIND_R8 *ms_r8,      // out - floating point milliseconds
      ESMF_KIND_R8 *us_r8,      // out - floating point microseconds
      ESMF_KIND_R8 *ns_r8,      // out - floating point nanoseconds
      ESMF_KIND_I4 *sN,         // out - fractional seconds numerator
      ESMF_KIND_I4 *sD,         // out - fractional seconds denominator
      char *timeString) const { // out - ISO 8601 format PyYmMdDThHmMsS
//
// !DESCRIPTION:
//      Gets a {\tt ESMC\_TimeInterval}'s values in user-specified format.
//      This version supports the F90 interface.
//
//EOP
// !REQUIREMENTS:  

    // TODO: fractional, sub-seconds

    // calendar interval

    if (yy != ESMC_NULL_POINTER) {
      if (this->yy >= INT_MIN && this->yy <= INT_MAX) {
        *yy = (ESMF_KIND_I4) this->yy;  // >= 32-bit
      } else {
        // too large to fit in given int
        return(ESMF_FAILURE);
      }
    }
    if (yy_i8 != ESMC_NULL_POINTER) {
      *yy_i8 = this->yy;  // >= 64-bit
    }

    if (mm != ESMC_NULL_POINTER) {
      if (this->mm >= INT_MIN && this->mm <= INT_MAX) {
        *mm = (ESMF_KIND_I4) this->mm;  // >= 32-bit
      } else {
        // too large to fit in given int
        return(ESMF_FAILURE);
      }
    }
    if (mm_i8 != ESMC_NULL_POINTER) {
      *mm_i8 = this->mm;   // >= 64-bit
    }

      // TODO: use when Calendar Intervals implemented
//    if (d != ESMC_NULL_POINTER) {
//      if (this->d >= INT_MIN && this->d <= INT_MAX) {
//        *d = (ESMF_KIND_I4) this->d;  // >= 32-bit
//      } else {
//        // too large to fit in given int
//        return(ESMF_FAILURE);
//      }
//    }
//    if (d_i8 != ESMC_NULL_POINTER) {
//      *d_i8 = this->d;  // >= 64-bit
//    }

    // TODO: use Calendar-defined SecondsPerDay; for now assume 86400
    // get number of seconds in a day
    int secPerDay = SECONDS_PER_DAY;
//    if (Calendar != ESMC_NULL_POINTER) {
//      secPerDay = Calendar->SecondsPerDay;
//    }

    if (d != ESMC_NULL_POINTER) {
      ESMF_KIND_I8 days = this->s / secPerDay;
      if (days >= INT_MIN && days <= INT_MAX) {
        *d = days;
      } else {
        // to large to fit in given int
        return(ESMF_FAILURE);
      }
    }
    if (d_i8 != ESMC_NULL_POINTER) {
      *d_i8 = this->s / secPerDay;
    }
    if (d_r8 != ESMC_NULL_POINTER) {
      *d_r8 = (ESMF_KIND_R8) this->s / (ESMF_KIND_R8) secPerDay;
    }

    // use base class to get sub-day values
    if (ESMC_BaseTimeGet(secPerDay, h, m, s, s_i8, ms, us, ns,
                           h_r8, m_r8, s_r8, ms_r8, us_r8, ns_r8, sN, sD) ==
        ESMF_FAILURE) return(ESMF_FAILURE);

    if (timeString != ESMC_NULL_POINTER) {
      if (ESMC_TimeIntervalGetString(timeString) == ESMF_FAILURE)
        return(ESMF_FAILURE);
    }

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
      const char *timeList,    // in - time interval value specifier string
      ...) {                   // in - specifier values (variable args)
//
// !DESCRIPTION:
//      Sets a {\tt ESMC\_TimeInterval}'s values in user-specified values.
//      Supports native C++ use.
//
//EOP
// !REQUIREMENTS:  

    // TODO
    return(ESMF_SUCCESS);

 }  // end ESMC_TimeIntervalSet

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
      const char *timeList,    // in  - time interval value specifier string
      ...) const {             // out - specifier values (variable args)
//
// !DESCRIPTION:
//      Gets a {\tt ESMC\_TimeInterval}'s values in user-specified format.
//      Supports native C++ use.
//
//EOP
// !REQUIREMENTS:  

    // TODO
    return(ESMF_SUCCESS);

 }  // end ESMC_TimeIntervalGet

#if 0
//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeIntervalSet - shallow class initializer 1
//
// !INTERFACE:
      int ESMC_TimeInterval::ESMC_TimeIntervalSet(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMF_KIND_I8 s,     // in - integer seconds
      ESMF_KIND_I4 sN,    // in - fractional seconds, numerator
      ESMF_KIND_I4 sD,    // in - fractional seconds, denominator
      ESMF_KIND_I8 yy,    // in - calendar interval number of years
      ESMF_KIND_I8 mm)    // in - calendar interval number of months
      ESMF_KIND_I8 d)  {  // in - calendar interval number of days
//
// !DESCRIPTION:
//      Initialzes a {\tt ESMC\_TimeInterval} with given values
//
//EOP
// !REQUIREMENTS:

    // use base class Set()
    if (ESMC_BaseTime::ESMC_BaseTimeSet(s, sN, sD) == ESMF_SUCCESS)
    {
      this->yy = yy;
      this->mm = mm;
      this->d  = d;

      return(ESMF_SUCCESS);
    }
    else return(ESMF_FAILURE);

}  // end ESMC_TimeIntervalSet
#endif

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
//      Gets a {\tt ESMC\_TimeInterval}'s absolute value
//
//EOP
// !REQUIREMENTS:  TMG 1.5.8

    // initialize result to subject time interval
    ESMC_TimeInterval absValue = *this;

    // check individual components (should be all positive or all negative)
    //   note: can't use abs() or labs() since these will be (long long)
    //         (64-bit) on some platforms
    if (s  < 0) absValue.s  *= -1;
    if (sN < 0) absValue.sN *= -1;
    if (yy < 0) absValue.yy *= -1;
    if (mm < 0) absValue.mm *= -1;
//   TODO: use when Calendar Intervals implemented
//    if (d  < 0) absValue.d  *= -1;

    return(absValue);

 }  // end ESMC_TimeIntervalAbsValue

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeIntervalNegAbsValue - Get a Time Interval's negative absolute value
//
// !INTERFACE:
      ESMC_TimeInterval
                  ESMC_TimeInterval::ESMC_TimeIntervalNegAbsValue(void) const {
//
// !RETURN VALUE:
//    ESMC_TimeInterval result
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      Gets a {\tt ESMC\_TimeInterval}'s negative absolute value
//
//EOP
// !REQUIREMENTS:  TMG 1.5.8

    // initialize result to subject time interval
    ESMC_TimeInterval negAbsValue = *this;

    // check individual components (should be all positive or all negative)
    //   note: can't use abs() or labs() since these will be (long long)
    //         (64-bit) on some platforms
    if (s  > 0) negAbsValue.s  *= -1;
    if (sN > 0) negAbsValue.sN *= -1;
    if (yy > 0) negAbsValue.yy *= -1;
    if (mm > 0) negAbsValue.mm *= -1;
//   TODO: use when Calendar Intervals implemented
//    if (d  > 0) negAbsValue.d  *= -1;

    return(negAbsValue);

 }  // end ESMC_TimeIntervalNegAbsValue

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeInterval(/) - Divide two time intervals, return double precision result
//
// !INTERFACE:
      ESMF_KIND_R8 ESMC_TimeInterval::operator/(
//
// !RETURN VALUE:
//    ESMF_KIND_R8 result
//
// !ARGUMENTS:
      const ESMC_TimeInterval &timeInterval) const {  // in - ESMC_TimeInterval
                                                      //        to divide by
//
// !DESCRIPTION:
//    Returns this time interval divided by given time interval as a double
//    precision quotient.
//
//EOP
// !REQUIREMENTS:  

    ESMF_KIND_R8 quotient;

    // TODO: fractional & calendar interval parts

    if (timeInterval.s != 0) {
      quotient = (ESMF_KIND_R8) this->s / (ESMF_KIND_R8) timeInterval.s;
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
      const ESMF_KIND_I4 &divisor) const {   // in - integer divisor
//
// !DESCRIPTION:
//    Divides a {\tt ESMC\_TimeInterval} by an integer divisor,
//    returns quotient as a {\tt ESMC\_TimeInterval}.
//
//EOP
// !REQUIREMENTS:  

    ESMC_TimeInterval quotient;

    // TODO: fractional & calendar interval parts

    if (divisor != 0) {
      quotient.s = this->s / divisor;
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
      const ESMF_KIND_I4 &divisor) {   // in - integer divisor
//
// !DESCRIPTION:
//    Divides a {\tt ESMC\_TimeInterval} by an integer divisor
//
//EOP
// !REQUIREMENTS:  

    // TODO: fractional & calendar interval parts

    if (divisor != 0) {
      this->s /= divisor;
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
      const ESMF_KIND_R8 &divisor) const {   // in - double precision divisor
//
// !DESCRIPTION:
//    Divides a {\tt ESMC\_TimeInterval} by an double divisor,
//    returns quotient as a {\tt ESMC\_TimeInterval}
//
//EOP
// !REQUIREMENTS:  

    ESMC_TimeInterval quotient;

    // TODO: fractional & calendar interval parts

    if (fabs(divisor) > FLT_EPSILON) {
      quotient.s = (ESMF_KIND_I8) ((ESMF_KIND_R8) this->s / divisor);
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
      const ESMF_KIND_R8 &divisor) {   // in - double precision divisor
//
// !DESCRIPTION:
//    Divides a {\tt ESMC\_TimeInterval} by a double precision divisor
//
//EOP
// !REQUIREMENTS:  

    // TODO: fractional & calendar interval parts

    if (fabs(divisor) > FLT_EPSILON) {
      this->s = (ESMF_KIND_I8) ((ESMF_KIND_R8) this->s / divisor);
    }
    // TODO:  else throw exception ?

    return(*this);

}  // end ESMC_TimeInterval::operator/=

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
//EOP
// !REQUIREMENTS:  

    ESMC_Fraction quotient;

    // TODO:

    return(quotient);

}  // end ESMC_TimeInterval::ESMC_TimeIntervalDiv

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeInterval(\%) - Divide two time intervals, return time interval remainder
//
// !INTERFACE:
      ESMC_TimeInterval ESMC_TimeInterval::operator%(
//
// !RETURN VALUE:
//    ESMC_TimeInterval result
//
// !ARGUMENTS:
      const ESMC_TimeInterval &timeInterval) const {  // in - ESMC_TimeInterval
                                                      //        to modulo by
//
// !DESCRIPTION:
//    Returns this time interval modulo by given time interval as a 
//    {\tt ESMC\_TimeInterval}
//
//EOP
// !REQUIREMENTS:  

    ESMC_TimeInterval remainder;

    // TODO: fractional & calendar interval parts

    if (timeInterval.s != 0) {
      remainder.s = this->s % timeInterval.s;
    }
    // TODO:  else throw exception ?

    return(remainder);

}  // end ESMC_TimeInterval::operator%

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeInterval(\%=) - Takes the modulus of two time intervals
//
// !INTERFACE:
      ESMC_TimeInterval& ESMC_TimeInterval::operator%=(
//
// !RETURN VALUE:
//    ESMC_TimeInterval& result
//
// !ARGUMENTS:
      const ESMC_TimeInterval &timeInterval) {  // in - ESMC_TimeInterval
                                                //        to modulo by
//
// !DESCRIPTION:
//    Returns this time interval modulo by given time interval
//
//EOP
// !REQUIREMENTS:  

    // TODO: fractional & calendar interval parts

    if (timeInterval.s != 0) {
      this->s %= timeInterval.s;
    }
    // TODO:  else throw exception ?

    return(*this);

}  // end ESMC_TimeInterval::operator%=

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
      const ESMF_KIND_I4 &multiplier) const {   // in - integer multiplier
//
// !DESCRIPTION:
//     Multiply a {\tt ESMC\_TimeInterval} by an integer, return product as a
//    {\tt ESMC\_TimeInterval}
//
//EOP
// !REQUIREMENTS:  

    ESMC_TimeInterval product;

    // TODO: fractional & calendar interval parts

    product.s = this->s * multiplier;

    return(product);

}  // end ESMC_TimeInterval::operator*

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeInterval(*) - Multiply a time interval by an integer
//
// !INTERFACE:
      ESMC_TimeInterval operator*(
//
// !RETURN VALUE:
//    ESMC_TimeInterval result
//
// !ARGUMENTS:
      const ESMF_KIND_I4 &multiplier,  // in - integer multiplier
      const ESMC_TimeInterval &ti) {   // in - TimeInterval multiplicand
//
// !DESCRIPTION:
//     Multiply a {\tt ESMC\_TimeInterval} by an integer, return product as a
//    {\tt ESMC\_TimeInterval}
//
//EOP
// !REQUIREMENTS:  

    ESMC_TimeInterval product;

    // TODO: fractional & calendar interval parts

    product.s = multiplier * ti.s;

    return(product);

}  // end operator*

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
      const ESMF_KIND_I4 &multiplier) {   // in - integer multiplier
//
// !DESCRIPTION:
//     Multiply a {\tt ESMC\_TimeInterval} by an integer
//
//EOP
// !REQUIREMENTS:  

    // TODO: fractional & calendar interval parts

    this->s *= multiplier;

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
//     Multiply a {\tt ESMC\_TimeInterval} by an fraction, return product as a
//    {\tt ESMC\_TimeInterval}
//
//EOP
// !REQUIREMENTS:  

    ESMC_TimeInterval product;

    // TODO: whole, fractional & calendar interval parts

    return(product);

}  // end ESMC_TimeInterval::operator*

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeInterval(*) - Multiply a time interval by an fraction
//
// !INTERFACE:
      ESMC_TimeInterval operator*(
//
// !RETURN VALUE:
//    ESMC_TimeInterval result
//
// !ARGUMENTS:
      const ESMC_Fraction &multiplier, // in - fraction multiplier
      const ESMC_TimeInterval &ti) {   // in - TimeInterval multiplicand
//
// !DESCRIPTION:
//     Multiply a {\tt ESMC\_TimeInterval} by an fraction, return product as a
//    {\tt ESMC\_TimeInterval}
//
//EOP
// !REQUIREMENTS:  

    ESMC_TimeInterval product;

    // TODO: whole, fractional & calendar interval parts

    return(product);

}  // end operator*

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
//     Multiply a {\tt ESMC\_TimeInterval} by a fraction
//
//EOP
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
      const ESMF_KIND_R8 &multiplier) const {   // in - double precision
                                                 //   multiplier
//
// !DESCRIPTION:
//     Multiply a {\tt ESMC\_TimeInterval} by an double precision,
//     return product as a {\tt ESMC\_TimeInterval}
//
//EOP
// !REQUIREMENTS:  

    ESMC_TimeInterval product;

    // TODO: fractional & calendar interval parts

    product.s = (ESMF_KIND_I8) ((ESMF_KIND_R8) this->s * multiplier);

    return(product);

}  // end ESMC_TimeInterval::operator*

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeInterval(*) - Multiply a time interval by a double precision
//
// !INTERFACE:
      ESMC_TimeInterval operator*(
//
// !RETURN VALUE:
//    ESMC_TimeInterval result
//
// !ARGUMENTS:
      const ESMF_KIND_R8 &multiplier,  // in - double precision
      const ESMC_TimeInterval &ti) {   // in - TimeInterval multiplicand
                                                 //   multiplier
//
// !DESCRIPTION:
//     Multiply a {\tt ESMC\_TimeInterval} by an double precision,
//     return product as a {\tt ESMC\_TimeInterval}
//
//EOP
// !REQUIREMENTS:  

    ESMC_TimeInterval product;

    // TODO: fractional & calendar interval parts

    product.s = (ESMF_KIND_I8) (multiplier * (ESMF_KIND_R8) ti.s);

    return(product);

}  // end operator*

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
      const ESMF_KIND_R8 &multiplier) {   // in - double precision multiplier
//
// !DESCRIPTION:
//     Multiply a {\tt ESMC\_TimeInterval} by a double precision
//
//EOP
// !REQUIREMENTS:  

    // TODO: fractional & calendar interval parts

    this->s = (ESMF_KIND_I8) ((ESMF_KIND_R8) this->s * multiplier);

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
//    Assign {\tt ESMC\_BaseTime} expression to this time intervals.
//    Supports inherited operators from {\tt ESMC\_BaseTime}
//
//EOP
// !REQUIREMENTS:  

    // invoke base class assignment operator
    // TODO:  should be implicit ?
    ESMC_BaseTime::operator=(baseTime);

    return(*this);

}  // end ESMC_TimeInterval::operator=

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeIntervalReadRestart - restore TimeInterval state
//
// !INTERFACE:
      int ESMC_TimeInterval::ESMC_TimeIntervalReadRestart(
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
//      restore {\tt TimeInterval} state for persistence/checkpointing.
//
//EOP
// !REQUIREMENTS:

    int rc = ESMF_SUCCESS;

    // TODO:  read time interval state from iospec/name, then restore
    //        (share code with ESMC_TimeIntervalSet()).

    // TODO: use base class ReadRestart() first
    // rc = ESMC_BaseTime::ESMC_BaseTimeReadRestart(s, sN, sD);

    return(rc);

 }  // end ESMC_TimeIntervalReadRestart

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeIntervalWriteRestart - return TimeInterval state
//
// !INTERFACE:
      int ESMC_TimeInterval::ESMC_TimeIntervalWriteRestart(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_IOSpec *iospec) const {
//
// !DESCRIPTION:
//      Save {\tt TimeInterval} state for persistence/checkpointing
//
//EOP
// !REQUIREMENTS: 

    int rc = ESMF_SUCCESS;

    // TODO: use base class Write() first
    //  rc = ESMC_BaseTime::ESMC_BaseTimeWriteRestart(s, sN, sD);

    // calendar= this->calendar;  // TODO?: this only saves calendar pointer;
                               //  component must be sure to save corresponding
                               //  calendar.
    return(rc);

 }  // end ESMC_TimeIntervalWriteRestart

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
//      validate {\tt ESMC\_TimeInterval} state for testing/debugging
//
//EOP
// !REQUIREMENTS:  

    return (ESMC_BaseTime::ESMC_BaseTimeValidate());

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
//      print {\tt ESMC\_TimeInterval} state for testing/debugging
//
//EOP
// !REQUIREMENTS:  

    cout << "TimeInterval ---------------------------" << endl;

    // parse options
    if (options != ESMC_NULL_POINTER) {
      if (strncmp(options, "string", 6) == 0) {
        char timeString[ESMF_MAXSTR];
        ESMC_TimeIntervalGetString(timeString);
        cout << timeString << endl;
      }
    } else {
      // default
      ESMC_BaseTime::ESMC_BaseTimePrint(options);
      cout << "yy = " << yy << endl;
      cout << "mm = " << mm << endl;
      cout << "d  = " << d << endl;
    }

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
//      Initializes an {\tt ESMC\_TimeInterval} with defaults
//
//EOP
// !REQUIREMENTS:

//   ESMC_BaseTime(0, 0, 1) { // TODO: F90 issue with base class constructor?
   s  = 0;
   sN = 0;
   sD = 0;
   yy = 0;
   mm = 0;
   d  = 0;

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
      ESMF_KIND_I8 s,     // in - integer seconds
      ESMF_KIND_I4 sN,    // in - fractional seconds, numerator
      ESMF_KIND_I4 sD,    // in - fractional seconds, denominator
      ESMF_KIND_I8 yy,    // in - calendar interval number of years
      ESMF_KIND_I8 mm,    // in - calendar interval number of months
      ESMF_KIND_I8 d) :   // in - calendar interval number of days
//
// !DESCRIPTION:
//      Initializes a {\tt ESMC\_TimeInterval} via {\tt ESMC\_BaseTime}
//      base class
//
//EOP
// !REQUIREMENTS:

    ESMC_BaseTime(s, sN, sD) {  // pass to base class constructor

    this->yy = yy;
    this->mm = mm;
    this->d  = d;

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

//-------------------------------------------------------------------------
//  Private methods to support ESMC_TimeIntervalGet() API
//-------------------------------------------------------------------------

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
//      Gets a {\tt ESMC\_TimeInterval}'s value in character format
//
//EOP
// !REQUIREMENTS:  

    // validate input
    if (timeString == ESMC_NULL_POINTER) return (ESMF_FAILURE);

    ESMF_KIND_I8 d_i8, s_i8;
    ESMF_KIND_I4 h, m;

    // TODO: use native C++ Get, not F90 entry point, when ready
    ESMC_TimeIntervalGet((ESMF_KIND_I4 *)ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                          ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                          ESMC_NULL_POINTER, &d_i8, &h, &m,
                          ESMC_NULL_POINTER, &s_i8);
    //ESMC_TimeIntervalGet(&yy_i8, &MM, &d_i8, &h, &m, &s_i8); // TODO: when
                                                     // calendar intervals
                                                     //  implemented

    // ISO 8601 format PyYmMdDThHmMsS
    sprintf(timeString, "P%lldDT%dH%dM%lldS\0", d_i8, h, m, s_i8);
    //sprintf(timeString, "P%lldY%dM%lldDT%dH%dM%lldS\0", // TODO: when calendar
    //        yy_i8, mm_i8, d_i8, h, m, s_i8);         //  intervals implemented

    return(ESMF_SUCCESS);

 }  // end ESMC_TimeIntervalGetString
