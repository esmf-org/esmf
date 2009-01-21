// $Id: ESMC_Fraction.C,v 1.6.2.2 2009/01/21 21:25:24 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
// ESMC Fraction method code (body) file
//
//-------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ {\tt ESMC\_Fraction} methods
// declared in the companion file {\tt ESMC_Fraction.h}
//
//-------------------------------------------------------------------------
//
 #define ESMC_FILENAME "ESMC_Fraction.C"

 // higher level, 3rd party or system includes
 #include <stdio.h>
 #include <limits.h>

 #include <ESMC_LogErr.h>
 #include <ESMF_LogMacros.inc>

 // associated class definition file
 #include <ESMC_Fraction.h>

//-------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_Fraction.C,v 1.6.2.2 2009/01/21 21:25:24 cdeluca Exp $";
//-------------------------------------------------------------------------

//
//-------------------------------------------------------------------------
//-------------------------------------------------------------------------
//
// This section includes all the ESMC_Fraction routines
//
//
//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_FractionSetw - Set fraction's whole number
//
// !INTERFACE:
      int ESMC_Fraction::ESMC_FractionSetw(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_I8 w) {   // input - the whole number value to set
//
// !DESCRIPTION:
//     Sets the fraction's whole number value.
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_FractionSetw()"

    // Initialize return code; assume file not implemented
    int rc = ESMC_RC_NOT_IMPL;

   this->w = w;

   // ensure simplified form
   rc = ESMC_FractionSimplify();
   return(rc);


 }  // end ESMC_FractionSetw

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_FractionSetn - Set fraction's numerator
//
// !INTERFACE:
      int ESMC_Fraction::ESMC_FractionSetn(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_I4 n) {   // input - the numerator value to set
//
// !DESCRIPTION:
//     Sets the fraction's numerator value.
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_FractionSetn()"

    // Initialize return code; assume file not implemented
    int rc = ESMC_RC_NOT_IMPL;

   this->n = n;

   // ensure simplified form
   rc = ESMC_FractionSimplify();
   return(rc);


 }  // end ESMC_FractionSetn

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_FractionSetd - Set fraction's denominator
//
// !INTERFACE:
      int ESMC_Fraction::ESMC_FractionSetd(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_I4 d) {   // input - the denominator value to set
//
// !DESCRIPTION:
//     Sets the fraction's denominator value.
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_FractionSetd()"

    // Initialize return code; assume file not implemented
    int rc = ESMC_RC_NOT_IMPL;

   this->d = d;

   // ensure simplified form
   rc = ESMC_FractionSimplify();
   return(rc);

 }  // end ESMC_FractionSetd

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_FractionGetw - Get fraction's whole number
//
// !INTERFACE:
      ESMC_I8 ESMC_Fraction::ESMC_FractionGetw(void) const {
//
// !RETURN VALUE:
//    The fraction's whole number value
//
// !ARGUMENTS:
//    none.
//
// !DESCRIPTION:
//     Gets the fraction's whole number value.
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_FractionGetw()"

   return(w);

 }  // end ESMC_FractionGetw

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_FractionGetn - Get fraction's numerator
//
// !INTERFACE:
      ESMC_I4 ESMC_Fraction::ESMC_FractionGetn(void) const {
//
// !RETURN VALUE:
//    The fraction's numerator value.
//
// !ARGUMENTS:
//    none.
//
// !DESCRIPTION:
//     Gets the fraction's numerator value.
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_FractionGetn()"

   return(n);

 }  // end ESMC_FractionGetn

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_FractionGetd - Get fraction's denominator
//
// !INTERFACE:
      ESMC_I4 ESMC_Fraction::ESMC_FractionGetd(void) const {
//
// !RETURN VALUE:
//    The fraction's denominator value.
//
// !ARGUMENTS:
//    none.
//
// !DESCRIPTION:
//     Gets the fraction's denominator value.
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_FractionGetd()"

   return(d);

 }  // end ESMC_FractionGetd

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_FractionSet - Set fraction value
//
// !INTERFACE:
      int ESMC_Fraction::ESMC_FractionSet(
//
// !RETURN VALUE:
//    none.
//
// !ARGUMENTS:
      ESMC_I8 *w,
      ESMC_I4 *n,
      ESMC_I4 *d) {
//
// !DESCRIPTION:
//     Sets the fraction's value.  Supports F90 optional args interface
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_FractionSet(*w,*n,*d)"

   if (w != ESMC_NULL_POINTER) this->w = *w;
   if (n != ESMC_NULL_POINTER) this->n = *n;
   if (d != ESMC_NULL_POINTER) this->d = *d;

   // ensure simplified form
   return(ESMC_FractionSimplify());

 }  // end ESMC_FractionSet

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_FractionSet - Set fraction value
//
// !INTERFACE:
      int ESMC_Fraction::ESMC_FractionSet(
//
// !RETURN VALUE:
//    none.
//
// !ARGUMENTS:
      ESMC_I8 w,
      ESMC_I4 n,
      ESMC_I4 d) {
//
// !DESCRIPTION:
//     Sets the fraction's value.
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_FractionSet(w,n,d)"

   this->w = w;
   this->n = n;
   this->d = d;

   // ensure simplified form
   return(ESMC_FractionSimplify());

 }  // end ESMC_FractionSet

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_FractionGet - Get fraction value
//
// !INTERFACE:
      int ESMC_Fraction::ESMC_FractionGet(
//
// !RETURN VALUE:
//    none.
//
// !ARGUMENTS:
      ESMC_I8 *w,
      ESMC_I4 *n,
      ESMC_I4 *d) const {
//
// !DESCRIPTION:
//     Gets the fraction's value.
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_FractionGet()"

   if (w != ESMC_NULL_POINTER) *w = this->w;
   if (n != ESMC_NULL_POINTER) *n = this->n;
   if (d != ESMC_NULL_POINTER) *d = this->d;

   return(ESMF_SUCCESS);

 }  // end ESMC_FractionGet

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_FractionSimplify - Ensure proper fraction (< 1) and sign; reduce to lowest denominator
//
// !INTERFACE:
      int ESMC_Fraction::ESMC_FractionSimplify(void) {
//
// !RETURN VALUE:
//    none.
//
// !ARGUMENTS:
//    none.
//
// !DESCRIPTION:
//     If fraction >= 1, add to whole part, and adjust fraction to remainder.
//     Then reduce to lowest denominator.
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_FractionSimplify()"

    // Initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;

    // check for divide-by-zero
    if (d == 0) {
      ESMC_LogDefault.ESMC_LogFoundError(ESMC_RC_DIV_ZERO, ESMC_NULL_POINTER);
      return(ESMF_FAILURE);
    }

    // normalize to proper fraction (labs(n/d) < 1)
    ESMC_I4 whole;
    if (labs((whole = n/d)) >= 1) {
      w += whole;
      n %= d;
    }

    // ensure whole and fraction parts are same sign
    
    // if whole is positive and fraction is negative
    if (w > 0 && (n < 0 && d > 0 || d < 0 && n > 0)) {
      w--;     // subtract one from whole number
      n += d;  //   and add it to the fraction part

    // else if whole is negative and fraction is positive
    } else if (w < 0 && (n > 0 && d > 0) || (d < 0 && n < 0)) {
      w++;     // add one to whole number
      n -= d;  //   and subtract it from the fraction part
    }

    // normalize fraction sign 
    if (d < 0) {
      d *= -1; n *= -1;  // change signs
    }

    // reduce to lowest denominator

    ESMC_I4 gcd = ESMC_FractionGCD(n,d);
    // this should never happen since GCD never returns zero!
    if (gcd == 0) {
      ESMC_LogDefault.ESMC_LogFoundError(ESMC_RC_DIV_ZERO, ESMC_NULL_POINTER);
      rc = ESMC_RC_DIV_ZERO;
      return(rc);
    }

    n /= gcd;
    d /= gcd;

    rc = ESMF_SUCCESS;
    return(rc);

 }  // end ESMC_FractionSimplify

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_FractionConvert - Convert to given denominator
//
// !INTERFACE:
      int ESMC_Fraction::ESMC_FractionConvert(
//
// !RETURN VALUE:
//    none.
//
// !ARGUMENTS:
      ESMC_I4 denominator) {  // input
//
// !DESCRIPTION:
//     Convert fraction in terms of given denominator
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_FractionConvert()"

    if (d == 0) {
      ESMC_LogDefault.ESMC_LogFoundError(ESMC_RC_DIV_ZERO, ESMC_NULL_POINTER);
      return(ESMF_FAILURE);
    }

    // TODO:  Consider making n/d of type ESMC_I8.
    // used by ESMC_BaseTimeGet()

    ESMC_I8 w_i8 = w;                      // TODO: ensures 
    ESMC_I8 n_i8 = n;                      //         correct
    ESMC_I8 d_i8 = d;                      //           cast
    ESMC_I8 denominator_i8 = denominator;  //             on Cray X1 !
    ESMC_I8 conversion = w_i8 * denominator_i8 +
                                (n_i8 * denominator_i8) / d_i8;
//    ESMC_I8 conversion = (ESMC_I8) w * denominator +
//          ((ESMC_I8) n * (ESMC_I8) denominator) / (ESMC_I8) d;

    if (conversion < INT_MIN || conversion > INT_MAX) {
        char logMsg[ESMF_MAXSTR];
        sprintf(logMsg, "For conversion=%lld out-of-range with respect to "
                        "machine limits (INT_MIN=%d to INT_MAX=%d).",
                        conversion, INT_MIN, INT_MAX);
        ESMC_LogDefault.ESMC_LogWrite(logMsg, ESMC_LOG_ERROR);
        return (ESMF_FAILURE);
    }

    // ok, set new values
    w = 0;
    n = conversion;
    d = denominator;

    return(ESMF_SUCCESS);

 }  // end ESMC_FractionConvert

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_FractionGCD - determine the Greatest Common Divisor
//
// !INTERFACE:
      ESMC_I4 ESMC_FractionGCD(
//
// !RETURN VALUE:
//    The GCD of a and b.
//
// !ARGUMENTS:
      ESMC_I4 a,    // in - the first number 
      ESMC_I4 b) {  // in - the second number
//
// !DESCRIPTION:
//     Uses Euclid's algorithm to determine the Greatest Common Divisor of 
//     a and b.
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_FractionGCD()"

    ESMC_I4 abs_a = labs(a);
    ESMC_I4 abs_b = labs(b);
    ESMC_I4 large = MAX(abs_a, abs_b);
    ESMC_I4 small = MIN(abs_a, abs_b);
    ESMC_I4 remainder;

    // deal with a zero input
    if      (small == 0 && large != 0) return(large);
    else if (small != 0 && large == 0) return(small);
    else if (small == 0 && large == 0) return(1);

    // initial remainder
    remainder = large % small;

    // divide smaller number by previous remainder until remainder goes to 0
    while(remainder != 0) {
      large = small;
      small = remainder;
      remainder = large % small; 
    }

    // the GCD is the last non-zero remainder or the passed-in smallest number
    return(small);

 }  // end ESMC_FractionGCD

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_FractionLCM - determine the Least Common Multiple
//
// !INTERFACE:
      ESMC_I4 ESMC_FractionLCM(
//
// !RETURN VALUE:
//    the LCM of a and b
//
// !ARGUMENTS:
      ESMC_I4 a,    // in - the first number 
      ESMC_I4 b) {  // in - the second number
//
// !DESCRIPTION:
//      Uses GCD to determine the Least Common Multiple of a and b.
//      LCM = (a * b) / GCD(a,b)
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_FractionLCM()"

    ESMC_I4 gcd = ESMC_FractionGCD(a,b);

    // this should never happen since GCD never returns zero!
    if (gcd == 0) {
      ESMC_LogDefault.ESMC_LogFoundError(ESMC_RC_DIV_ZERO, ESMC_NULL_POINTER);
      return(0);
    }

    return(labs((a/gcd) * b));   // avoid (a * b) directly to prevent
                                 //   overflow when a and b are large;
                                 //   return absolute value

 }  // end ESMC_FractionLCM

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Fraction(==) - Fraction equality comparison
//
// !INTERFACE:
      bool ESMC_Fraction::operator==(
//
// !RETURN VALUE:
//    bool result
//
// !ARGUMENTS:
      const ESMC_Fraction &fraction) const {   // in - ESMC_Fraction to compare
//
// !DESCRIPTION:
//      Compare for equality the current object's (this) {\tt ESMC\_Fraction}
//      with given {\tt ESMC\_Fraction}, return result
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_Fraction::operator==()"

    // make local copies; don't change the originals.
    ESMC_Fraction f1 = *this;
    ESMC_Fraction f2 = fraction;

    // ensure proper fractions
    f1.ESMC_FractionSimplify();
    f2.ESMC_FractionSimplify();

    // put both fractions on the same denominator, then compare
    ESMC_I4 lcm = ESMC_FractionLCM(f1.d, f2.d);
    return(f1.w == f2.w && f1.n*(lcm/f1.d) == f2.n*(lcm/f2.d));

}  // end ESMC_Fraction::operator==

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Fraction(!=) - Fraction inequality comparison
//
// !INTERFACE:
      bool ESMC_Fraction::operator!=(
//
// !RETURN VALUE:
//    bool result
//
// !ARGUMENTS:
      const ESMC_Fraction &fraction) const {   // in - ESMC_Fraction to compare
//
// !DESCRIPTION:
//      Compare for inequality the current object's (this)
//      {\tt ESMC\_Fraction} with given {\tt ESMC\_Fraction}, return result
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_Fraction::operator!=()"

    // make local copies; don't change the originals.
    ESMC_Fraction f1 = *this;
    ESMC_Fraction f2 = fraction;

    // put both fractions on the same denominator, then compare
    f1.ESMC_FractionSimplify();
    f2.ESMC_FractionSimplify();

    // put both fractions on the same denominator, then compare
    ESMC_I4 lcm = ESMC_FractionLCM(f1.d, f2.d);
    return(f1.w != f2.w || f1.n*(lcm/f1.d) != f2.n*(lcm/f2.d));

}  // end ESMC_Fraction::operator!=

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Fraction(<) - Fraction less than comparison
//
// !INTERFACE:
      bool ESMC_Fraction::operator<(
//
// !RETURN VALUE:
//    bool result
//
// !ARGUMENTS:
      const ESMC_Fraction &fraction) const {   // in - ESMC_Fraction to compare
//
// !DESCRIPTION:
//      Compare for less than the current object's (this)
//      {\tt ESMC\_Fraction} with given {\tt ESMC\_Fraction}, return result
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_Fraction::operator<()"

    // make local copies; don't change the originals.
    ESMC_Fraction f1 = *this;
    ESMC_Fraction f2 = fraction;

    // ensure proper fractions
    f1.ESMC_FractionSimplify();
    f2.ESMC_FractionSimplify();

    // ignore fractional part if whole parts are different
    if (f1.w != f2.w) return(f1.w < f2.w);
    else { // must look at fractional part
      // put both fractions on the same denominator, then compare
      ESMC_I4 lcm = ESMC_FractionLCM(f1.d, f2.d);
      return(f1.n*(lcm/f1.d) < f2.n*(lcm/f2.d));
    }

}  // end ESMC_Fraction::operator<

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Fraction(>) - Fraction greater than comparison
//
// !INTERFACE:
      bool ESMC_Fraction::operator>(
//
// !RETURN VALUE:
//    bool result
//
// !ARGUMENTS:
      const ESMC_Fraction &fraction) const {   // in - ESMC_Fraction to compare
//
// !DESCRIPTION:
//      Compare for greater than the current object's (this)
//      {\tt ESMC\_Fraction} with given {\tt ESMC\_Fraction}, return result
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_Fraction::operator>()"

    // make local copies; don't change the originals.
    ESMC_Fraction f1 = *this;
    ESMC_Fraction f2 = fraction;

    // ensure proper fractions
    f1.ESMC_FractionSimplify();
    f2.ESMC_FractionSimplify();

    // ignore fractional part if whole parts are different
    if (f1.w != f2.w) return(f1.w > f2.w);
    else { // must look at fractional part
      // put both fractions on the same denominator, then compare
      ESMC_I4 lcm = ESMC_FractionLCM(f1.d, f2.d);
      return(f1.n*(lcm/f1.d) > f2.n*(lcm/f2.d));
    }

}  // end ESMC_Fraction::operator>

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Fraction(<=) - Fraction less or equal than comparison
//
// !INTERFACE:
      bool ESMC_Fraction::operator<=(
//
// !RETURN VALUE:
//    bool result
//
// !ARGUMENTS:
      const ESMC_Fraction &fraction) const {   // in - ESMC_Fraction to compare
//
// !DESCRIPTION:
//      Compare for less than or equal the current object's (this)
//      {\tt ESMC\_Fraction} with given {\tt ESMC\_Fraction}, return result
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_Fraction::operator<=()"

    // just reuse < and == operators defined above!
    return(*this < fraction || *this == fraction);

}  // end ESMC_Fraction::operator<=

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Fraction(>=) - Fraction greater than or equal comparison
//
// !INTERFACE:
      bool ESMC_Fraction::operator>=(
//
// !RETURN VALUE:
//    bool result
//
// !ARGUMENTS:
      const ESMC_Fraction &fraction) const {   // in - ESMC_Fraction to compare
//
// !DESCRIPTION:
//      Compare for greater than or equal the current object's (this)
//      {\tt ESMC\_Fraction} with given {\tt ESMC\_Fraction}, return result
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_Fraction::operator>=()"

    // just reuse > and == operators defined above!
    return(*this > fraction || *this == fraction);

}  // end ESMC_Fraction::operator>=

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Fraction(+) - increment Fraction
//
// !INTERFACE:
      ESMC_Fraction ESMC_Fraction::operator+(
//
// !RETURN VALUE:
//    ESMC_Fraction result
//
// !ARGUMENTS:
      const ESMC_Fraction &fraction) const {   // in - ESMC_Fraction increment
//
// !DESCRIPTION:
//      Increment current object's (this) {\tt ESMC\_Fraction} with given
//      {\tt ESMC\_Fraction}, return result
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_Fraction::operator+()"

    ESMC_Fraction sum;

    // fractional part addition
    sum.d = ESMC_FractionLCM(d, fraction.d);
    sum.n = n*(sum.d/d) + fraction.n*(sum.d/fraction.d);

    // whole part addition
    sum.w = w + fraction.w;

   // ensure simplified form
    sum.ESMC_FractionSimplify();

    return(sum);

}  // end ESMC_Fraction::operator+

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Fraction(-) - decrement Fraction
//
// !INTERFACE:
      ESMC_Fraction ESMC_Fraction::operator-(
//
// !RETURN VALUE:
//    ESMC_Fraction result
//
// !ARGUMENTS:
      const ESMC_Fraction &fraction) const {   // in - ESMC_Fraction decrement
//
// !DESCRIPTION:
//      Decrement current object's (this) {\tt ESMC\_Fraction} with given
//      {\tt ESMC\_Fraction}, return result
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_Fraction::operator-()"

    ESMC_Fraction diff;

    // fractional part subtraction
    diff.d = ESMC_FractionLCM(d, fraction.d);
    diff.n = n*(diff.d/d) - fraction.n*(diff.d/fraction.d);

    // whole part subtraction 
    diff.w = w - fraction.w;

   // ensure simplified form
    diff.ESMC_FractionSimplify();

    return(diff);

}  // end ESMC_Fraction::operator-

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Fraction(+=) - increment Fraction
//
// !INTERFACE:
      ESMC_Fraction& ESMC_Fraction::operator+=(
//
// !RETURN VALUE:
//    ESMC_Fraction& result
//
// !ARGUMENTS:
      const ESMC_Fraction &fraction) {   // in - ESMC_Fraction increment
//
// !DESCRIPTION:
//      Increment current object's (this) {\tt ESMC\_Fraction} with given
//      {\tt ESMC\_Fraction}
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_Fraction::operator+=()"

    // just reuse (+) operator defined above!
    *this = *this + fraction;

    return(*this);

}  // end ESMC_Fraction::operator+=

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Fraction(-=) - decrement Fraction
//
// !INTERFACE:
      ESMC_Fraction& ESMC_Fraction::operator-=(
//
// !RETURN VALUE:
//    ESMC_Fraction& result
//
// !ARGUMENTS:
      const ESMC_Fraction &fraction) {   // in - ESMC_Fraction decrement
//
// !DESCRIPTION:
//      Decrement current object's (this) {\tt ESMC\_Fraction} with given
//      {\tt ESMC\_Fraction}
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_Fraction::operator-=()"

    // just reuse (-) operator defined above!
    *this = *this - fraction;

    return(*this);

}  // end ESMC_Fraction::operator-=

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Fraction(*) - multiply Fraction by integer
//
// !INTERFACE:
      ESMC_Fraction ESMC_Fraction::operator*(
//
// !RETURN VALUE:
//    ESMC_Fraction result
//
// !ARGUMENTS:
      ESMC_I4 multiplier) const {   // in - integer multiplier
//
// !DESCRIPTION:
//      Multiply current object's (this) {\tt ESMC\_Fraction} with given
//      integer, return result
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_Fraction::operator*(integer)"

    ESMC_Fraction product;

    // fractional part multiplication.
    product.n = n * multiplier;
    product.d = d;

    // whole part multiplication
    product.w = w * multiplier;

   // ensure simplified form
    product.ESMC_FractionSimplify();

    return(product);

}  // end ESMC_Fraction::operator*

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Fraction(*=) - multiply Fraction by integer
//
// !INTERFACE:
      ESMC_Fraction& ESMC_Fraction::operator*=(
//
// !RETURN VALUE:
//    ESMC_Fraction& result
//
// !ARGUMENTS:
      ESMC_I4 multiplier) {   // in - integer multiplier
//
// !DESCRIPTION:
//      Multiply current object's (this) {\tt ESMC\_Fraction} with given
//      integer.
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_Fraction::operator*=(integer)"

    // just reuse (*) operator defined above!
    *this = *this * multiplier;

    return(*this);

}  // end ESMC_Fraction::operator*=

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Fraction(/) - divide Fraction by integer
//
// !INTERFACE:
      ESMC_Fraction ESMC_Fraction::operator/(
//
// !RETURN VALUE:
//    ESMC_Fraction result
//
// !ARGUMENTS:
      ESMC_I4 divisor) const {   // in - integer divisor
//
// !DESCRIPTION:
//      Divide current object's (this) {\tt ESMC\_Fraction} with given
//      integer, return result
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_Fraction::operator/(integer)"

    // check for divide-by-zero
    if (divisor == 0) {
      ESMC_LogDefault.ESMC_LogFoundError(ESMC_RC_DIV_ZERO, ESMC_NULL_POINTER);
      return(ESMF_FAILURE);
    }

    ESMC_Fraction quotient;
    ESMC_I4 remainder;
    ESMC_I8 denominator;

    // fractional part division.  don't just blindly multiply denominator;
    //   avoid overflow, especially with large denominators such as
    //   1,000,000,000 for nanoseconds.  So divide numerator and add back
    //   any remainder.
    quotient.n = n / divisor;
    quotient.d = d;

    // check remainder and add back
    if ((remainder = n % divisor) != 0) {
      // upper bounds check of (d * divisor)
      ESMC_I8 d_i8 = d;              // TODO: ensures correct
      ESMC_I8 divisor_i8 = divisor;  //         cast on
      denominator = d_i8 * divisor_i8;    //           Cray X1 !
      //denominator = (ESMC_I8) d * (ESMC_I8) divisor; // must do it!

      if (denominator < INT_MIN || denominator > INT_MAX) {
        char logMsg[ESMF_MAXSTR];
        sprintf(logMsg, "; denominator value abs(%lld) > %d, won't fit in "
                        "fraction denominator", denominator, INT_MAX);
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_NOT_VALID, logMsg,
                                              ESMC_NULL_POINTER);
        return(ESMC_Fraction(0,0,1));
      }
      // if ok, add back
      quotient += ESMC_Fraction(0, remainder, denominator);
    }

    // whole part division; add back any remainder
    quotient.w = w / divisor;
    if ((remainder = w % divisor) != 0) {
      quotient += ESMC_Fraction(0, remainder, divisor);
    }

   // ensure simplified form
    quotient.ESMC_FractionSimplify();

    return(quotient);

}  // end ESMC_Fraction::operator/

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Fraction(/=) - divide Fraction by integer
//
// !INTERFACE:
      ESMC_Fraction& ESMC_Fraction::operator/=(
//
// !RETURN VALUE:
//    ESMC_Fraction& result
//
// !ARGUMENTS:
      ESMC_I4 divisor) {   // in - integer divisor
//
// !DESCRIPTION:
//      Divide current object's (this) {\tt ESMC\_Fraction} with given
//      integer.
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_Fraction::operator/=(integer)"

    // just reuse (/) operator defined above!
    *this = *this / divisor;

    return(*this);

}  // end ESMC_Fraction::operator/=

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Fraction(/) - Divide two fractions, return double precision result
//
// !INTERFACE:
      ESMC_R8 ESMC_Fraction::operator/(
//
// !RETURN VALUE:
//    ESMC_R8 result
//
// !ARGUMENTS:
      const ESMC_Fraction &fraction) const {  // in - ESMC_Fraction
                                              //        to divide by
//
// !DESCRIPTION:
//    Returns this fraction divided by given fraction as a ESMC_R8
//    precision quotient.
//
//EOP
// !REQUIREMENTS:

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_Fraction::operator/(fraction)"

    // check for divide-by-zero
    if (d == 0 || fraction.d == 0 || 
        fraction.w * fraction.d + fraction.n == 0) {
      ESMC_LogDefault.ESMC_LogFoundError(ESMC_RC_DIV_ZERO, ESMC_NULL_POINTER);
      return(ESMF_FAILURE);
    }

    ESMC_R8 quotient = 
      (w + (ESMC_R8) n / (ESMC_R8) d) /
        (fraction.w + (ESMC_R8) fraction.n / (ESMC_R8) fraction.d);

    return(quotient);

}  // end ESMC_Fraction::operator/

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Fraction(\%) - Computes the modulus of two fractions
//
// !INTERFACE:
      ESMC_Fraction ESMC_Fraction::operator%(
//
// !RETURN VALUE:    ESMC_Fraction result
//
// !ARGUMENTS:
      const ESMC_Fraction &fraction) const {  // in - ESMC_Fraction
                                              //        to modulo by
//// !DESCRIPTION:
//    Returns this fraction modulo by given fraction
//
//EOP
// !REQUIREMENTS:

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_Fraction::operator%(fraction)"

    ESMC_Fraction remainder;

    // if no fractional part, just modulus the whole parts
    if (n == 0 && fraction.n == 0) {
      // check for divide-by-zero
      if (fraction.w != 0) {
        remainder.n = w % fraction.w;
      } else {
        ESMC_LogDefault.ESMC_LogFoundError(ESMC_RC_DIV_ZERO, ESMC_NULL_POINTER);
        return(ESMF_FAILURE);
      }

    // otherwise, perform fraction modulus
    } else {
      // check for divide-by-zero
      if (d == 0 || fraction.d == 0) {
        ESMC_LogDefault.ESMC_LogFoundError(ESMC_RC_DIV_ZERO, ESMC_NULL_POINTER);
        return(ESMF_FAILURE);
      }

      ESMC_I4 lcm = ESMC_FractionLCM(d, fraction.d);

      // convert *this fraction and given fraction into improper form with a 
      // common denominator and then compute remainder
      remainder.d = (fraction.w * fraction.d + fraction.n) * (lcm/fraction.d);
      if (remainder.d != 0) {
        remainder.n = ((w*d + n) * (lcm/d)) % remainder.d;
      } else {
        ESMC_LogDefault.ESMC_LogFoundError(ESMC_RC_DIV_ZERO, ESMC_NULL_POINTER);
        return(ESMF_FAILURE);
      }
    }

   // ensure simplified form
    remainder.ESMC_FractionSimplify();

    return(remainder);

}  // end ESMC_Fraction::operator%

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Fraction(\%=) - Computes the modulus of two fractions
//
// !INTERFACE:
      ESMC_Fraction& ESMC_Fraction::operator%=(
//
// !RETURN VALUE:    ESMC_Fraction& result
//
// !ARGUMENTS:
      const ESMC_Fraction &fraction) {  // in - ESMC_Fraction
                                        //        to modulo by
//// !DESCRIPTION:
//    Returns this fraction modulo by given fraction
//
//EOP
// !REQUIREMENTS:

    // just reuse (%) operator defined above!
    *this = *this % fraction;

    return(*this);

}  // end ESMC_Fraction::operator%=

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Fraction(=) - assignment operator
//
// !INTERFACE:
      ESMC_Fraction& ESMC_Fraction::operator=(
//
// !RETURN VALUE:
//    ESMC_Fraction& result
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
    //   ESMC_BaseTime::operator= and ESMC_TimeInterval::operator=

    if (&fraction != this) {
      this->w = fraction.w;
      this->n = fraction.n;
      this->d = fraction.d;
    }

    return(*this);

}  // end ESMC_Fraction::operator=

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_FractionValidate - validate Fraction state
//
// !INTERFACE:
      int ESMC_Fraction::ESMC_FractionValidate(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {     // in - options
//
// !DESCRIPTION:
//      validate {\tt ESMC\_Fraction} state
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_FractionValidate()"

    // Initialize return code; assume file not implemented
    int rc = ESMC_RC_NOT_IMPL;

    // must have non-zero denominator
    if (d == 0) {
      char logMsg[ESMF_MAXSTR];
      sprintf(logMsg, "must have non-zero denominator."); 
      ESMC_LogDefault.ESMC_LogWrite(logMsg, ESMC_LOG_ERROR);
      return(ESMF_FAILURE);
    }

    rc = ESMF_SUCCESS;
    return(rc);

}  // end ESMC_FractionValidate

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_FractionPrint - print Fraction state
//
// !INTERFACE:
      int ESMC_Fraction::ESMC_FractionPrint(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {    // in - print options
//
// !DESCRIPTION:
//      print {\tt ESMC\_Fraction} state for testing/debugging
//
//EOP
// !REQUIREMENTS:  

    // Initialize return code; assume file not implemented
    int rc = ESMC_RC_NOT_IMPL;

    printf("Fraction -------------------------------\n");
    printf("w = %lld\n", w);
    printf("n = %d\n", n);
    printf("d = %d\n", d);
    printf("end Fraction ---------------------------\n\n");

    rc = ESMF_SUCCESS;
    return(rc);

}  // end ESMC_FractionPrint

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Fraction - native default C++ constructor
//
// !INTERFACE:
      ESMC_Fraction::ESMC_Fraction(void) {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      Initializes a {\tt ESMC\_Fraction} with defaults
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_Fraction::ESMC_Fraction(void) constructor"

   w = 0;
   n = 0;
   d = 1;  // to prevent divide-by-zero

 }  // end ESMC_Fraction

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Fraction - native C++ constructor
//
// !INTERFACE:
      ESMC_Fraction::ESMC_Fraction(
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
      ESMC_I8 w,   // Integer (whole) seconds (signed)
      ESMC_I4 n,   // Integer fraction (exact) n/d; numerator (signed)
      ESMC_I4 d) { // Integer fraction (exact) n/d; denominator

// !DESCRIPTION:
//      Initializes a {\tt ESMC\_Fraction} with given values
//
//EOP
// !REQUIREMENTS:  

 // Initialize return code
 int rc = ESMC_RC_NOT_IMPL;

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_Fraction::ESMC_Fraction(w,n,d) constructor"

   this->w = w;
   this->n = n;
   this->d = d;

   // ensure simplified form
   ESMC_FractionSimplify();
   // TODO:  throw exception if ESMC_FractionSimplify() returns ESMF_FAILURE

 }  // end ESMC_Fraction

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Fraction - native C++ constructor
//
// !INTERFACE:
      ESMC_Fraction::ESMC_Fraction(
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
      int w,   // Integer (whole) seconds (signed)
      int n,   // Integer fraction (exact) n/d; numerator (signed)
      int d) { // Integer fraction (exact) n/d; denominator

// !DESCRIPTION:
//      Initializes a {\tt ESMC\_Fraction} with given values
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_Fraction::ESMC_Fraction(w,n,d) constructor"

   this->w = w;
   this->n = n;
   this->d = d;

   // ensure simplified form
   ESMC_FractionSimplify();
   // TODO:  throw exception if ESMC_FractionSimplify() returns ESMF_FAILURE

 }  // end ESMC_Fraction

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ~ESMC_Fraction - native default C++ destructor
//
// !INTERFACE:
      ESMC_Fraction::~ESMC_Fraction(void) {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      Default {\tt ESMC\_Fraction} destructor
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_Fraction::~ESMC_Fraction(void) destructor"

 }  // end ~ESMC_Fraction
