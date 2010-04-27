// $Id: ESMCI_Fraction.C,v 1.6.2.2 2010/04/27 20:13:41 eschwab Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
// ESMF Fraction C++ method code (body) file
//
//-------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ {\tt Fraction} methods
// declared in the companion file {\tt ESMCI\_Fraction.h}
//
//-------------------------------------------------------------------------
//
 #define ESMC_FILENAME "ESMCI_Fraction.C"

 // higher level, 3rd party or system includes
 #include <stdio.h>
 #include <math.h>
 #include <limits.h>
 #include <float.h>  // DBL_DIG

 #include <ESMCI_LogErr.h>
 #include <ESMF_LogMacros.inc>

 // associated class definition file
 #include <ESMCI_Fraction.h>

 // TODO: resolve ambiguity between C and C++ versions of llabs() function,
 //       or otherwise move macro into ESMCI_Util.h.  Note: llabs() is in C99,
 //       but is not in C++98.
 #define LLABS(a) (((a)<0)?(-1*(a)):(a))

//-------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMCI_Fraction.C,v 1.6.2.2 2010/04/27 20:13:41 eschwab Exp $";
//-------------------------------------------------------------------------

// TODO:  Use logarithms for checking if a multiplication or division is about
//        to overflow ESMC_I8.  Could also use logarithms to perform the
//        multiplication, but double precision will not be enough for all
//        conversions back to ESMC_I8 (15 DP digits versus up to 19 64-bit
//        digits).  For addition, use the difference of an addend and
//        (2^63-1, LONG_LONG_MAX) to compare against the other addend for
//        pending overflow.  Similarly use LONG_LONG_MIN for subtraction.
//        If overflow really ever becomes a problem, consider switching
//        the implementation to an arbitrary precision calculation engine
//        such as is used in the GNU bc calculator (refer to manual page
//        "man bc").

 namespace ESMCI{

//
//-------------------------------------------------------------------------
//-------------------------------------------------------------------------
//
// This section includes all the Fraction routines
//
//
//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  Fraction::setw - Set fraction's whole number
//
// !INTERFACE:
      int Fraction::setw(
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
 #define ESMC_METHOD "ESMCI::Fraction::setw()"

    // Initialize return code; assume file not implemented
    int rc = ESMC_RC_NOT_IMPL;

   this->w = w;

   // ensure simplified form
   rc = simplify();
   return(rc);


 }  // end Fraction::setw

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  Fraction::setn - Set fraction's numerator
//
// !INTERFACE:
      int Fraction::setn(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_I8 n) {   // input - the numerator value to set
//
// !DESCRIPTION:
//     Sets the fraction's numerator value.
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::Fraction::setn()"

    // Initialize return code; assume file not implemented
    int rc = ESMC_RC_NOT_IMPL;

   this->n = n;

   // ensure simplified form
   rc = simplify();
   return(rc);

 }  // end Fraction::setn

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  Fraction::setd - Set fraction's denominator
//
// !INTERFACE:
      int Fraction::setd(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_I8 d) {   // input - the denominator value to set
//
// !DESCRIPTION:
//     Sets the fraction's denominator value.
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::Fraction::setd()"

    // Initialize return code; assume file not implemented
    int rc = ESMC_RC_NOT_IMPL;

   this->d = d;

   // ensure simplified form
   rc = simplify();
   return(rc);

 }  // end Fraction::setd

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  Fraction::setr - Set from given real number
//
// !INTERFACE:
      int Fraction::setr(
//
// !RETURN VALUE:
//    none.
//
// !ARGUMENTS:
      ESMC_R8 rin) {  // input double precision real number
//
// !DESCRIPTION:
//     Convert any real number (within limits) to a rational fraction via
//     the method of continued fractions (CF), using a standard variation of
//     Euclid's GCD algorithm.
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::Fraction::setr()"

    ESMC_R8 rabs, target, r, f, p;
    ESMC_I8 a, nprev, nprevprev, dprev, dprevprev; 
    int sign;
    int rc = ESMF_SUCCESS;

    //   Fractional range,
    //      ideally (0 <= fabs(rin) < 1), is (1e-17 <= fabs(rin) <= 1e18) due to
    //      limitation of ESMC_I8 representation.
    rabs = fabs(rin);
    if ((rabs > 0.0 && rabs < 1e-17) || rabs > 1e18) {
      char logMsg[ESMF_MAXSTR];
      sprintf(logMsg, "; Input fabs(rin) = %g > 0 and < 1e-17 or > 1e18\n.",
              rabs);
      ESMC_LogDefault.MsgFoundError(ESMC_RC_ARG_OUTOFRANGE, logMsg, &rc);
      return(rc);
    }

    // save sign
    sign = (rin < 0) ? -1 : 1;

    // Initialize algorithm

    // Strip off any whole number part (w) first to avoid 64-bit overflow in
    // the algorithm if given a large value.
    w = 0; n = 0; d = 1;
    target = rabs;
    if (target == 0.0) return(ESMF_SUCCESS); // if given rin is 0.0, we're done
    if (target >= 1.0) {
      w = (ESMC_I8) rabs;
      target -= (ESMC_R8)w;
      if (target < 1e-17) { // if given rin is an integer, we're done
        // restore sign
        if (sign == -1) w *= sign;
        return(ESMF_SUCCESS);
      }
    }
//printf("rabs, w, target = %g, %lld, %g\n", rabs, w, target);

    // Target precision is relative to given rin (not target, due to possible
    // loss of significant digits from the subraction of w).  Precision is
    // 1/10 of 15th (DBL_DIG) decimal digit (least significant), which is
    // about half a decimal digit more than in double precison, or within half
    // of DBL_EPSILON (2.2e-16) of rin's magnitude (< 0.5*fabs(2.2e-16 * rin)).
    // Anything more stringent can cause instability in the algorithm such as
    // infinite looping due to 64-bit overflow or computation of unreasonably
    // large n and d values.  Test cases: rin = (8+21/23)-8, rin = 9.1 - 9.
    // TODO:  add extra optional input argument for user to specify number
    // of significant decimal digits contained in rin, if less than the full
    // 15 to 16 digits (due to loss in subraction operations in user's code).

    p = pow(10.0, -(DBL_DIG-(int)log10(rabs)));
//printf("p = %g\n", p);
//fflush(stdout);

    r = target;
    nprevprev = 0; nprev = 1;
    dprevprev = 1; dprev = 0;

    // Iterate until double precision representation to the number of decimal
    // significant digits (at least DBL_DIG) is achieved.
    //   Worst case is 37 iterations for the
    //     golden ratio phi = (1 + sqrt(5))/2; the "most irrational number".
    int i=0; f=0.0;
    do {
      a = (ESMC_I8) r;            // Compute
      n = a * nprev + nprevprev;  //  next
      d = a * dprev + dprevprev;  //   convergent n/d.
//printf("i, a, f, r, n/d = %d, %lld, %g, %g, %lld/%lld\n", ++i, a, f, r, n, d);
//printf("diff with given r = %21.18e\n", fabs((ESMC_R8)n/(ESMC_R8)d - target));
//fflush(stdout);
      if (fabs((ESMC_R8)n/(ESMC_R8)d - target) < p) break; // done when double
                                                           //  precision reached
      f = r - (ESMC_R8) a;
      if (f < 1e-17) break; // Prevent divide-by-zero (exceed do-able precision)
                            // Will this condition ever be met, while primary
                            // target termination condition above is not?
                            // I.e., perhaps this line is not necessary, and
                            // the line above and the line below can be
                            // combined into one, eliminating the variable f.
      r = 1.0/f;                     // Prepare
      nprevprev = nprev; nprev = n;  //  for next
      dprevprev = dprev; dprev = d;  //   iteration.
    } while (true);

    // restore sign
    if (sign == -1) {
      w *= sign;
      n *= sign;
    }

    // ensure proper fraction
    // TODO:  may not be necessary; review theorems which show each computed
    // convergent n/d is in reduced form.
    simplify();
   // TODO:  throw exception if simplify() returns ESMF_FAILURE

    return(ESMF_SUCCESS);

 }  // end Fraction::setr()

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  Fraction::getw - Get fraction's whole number
//
// !INTERFACE:
      ESMC_I8 Fraction::getw(void) const {
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
 #define ESMC_METHOD "ESMCI::Fraction::getw()"

   return(w);

 }  // end Fraction::getw

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  Fraction::getn - Get fraction's numerator
//
// !INTERFACE:
      ESMC_I8 Fraction::getn(void) const {
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
 #define ESMC_METHOD "ESMCI::Fraction::getn()"

   return(n);

 }  // end Fraction::getn

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  Fraction::getd - Get fraction's denominator
//
// !INTERFACE:
      ESMC_I8 Fraction::getd(void) const {
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
 #define ESMC_METHOD "ESMCI::Fraction::getd()"

   return(d);

 }  // end Fraction::getd

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  Fraction::getr - Get fraction's value as a real number
//
// !INTERFACE:
      ESMC_R8 Fraction::getr(void) const {
//
// !RETURN VALUE:
//    The fraction's value as a real number.
//
// !ARGUMENTS:
//    none.
//
// !DESCRIPTION:
//     Gets the fraction's value as a real number.
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::Fraction::getr()"

   // check for divide-by-zero
   if (d == 0) {
     ESMC_LogDefault.FoundError(ESMC_RC_DIV_ZERO, ESMC_CONTEXT,
                                ESMC_NULL_POINTER);
     return(0.0);
   }

   return((ESMC_R8)w + (ESMC_R8)n / (ESMC_R8)d);

 }  // end Fraction::getr()

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  Fraction::set - Set fraction value
//
// !INTERFACE:
      int Fraction::set(
//
// !RETURN VALUE:
//    none.
//
// !ARGUMENTS:
      ESMC_I8 *w,
      ESMC_I8 *n,
      ESMC_I8 *d) {
//
// !DESCRIPTION:
//     Sets the fraction's value.  Supports F90 optional args interface
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::Fraction::set(*w,*n,*d)"

   if (w != ESMC_NULL_POINTER) this->w = *w;
   if (n != ESMC_NULL_POINTER) this->n = *n;
   if (d != ESMC_NULL_POINTER) this->d = *d;

   // ensure simplified form
   return(simplify());

 }  // end Fraction::set

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  Fraction::set - Set fraction value
//
// !INTERFACE:
      int Fraction::set(
//
// !RETURN VALUE:
//    none.
//
// !ARGUMENTS:
      ESMC_I8 w,
      ESMC_I8 n,
      ESMC_I8 d) {
//
// !DESCRIPTION:
//     Sets the fraction's value.
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::Fraction::set(w,n,d)"

   this->w = w;
   this->n = n;
   this->d = d;

   // ensure simplified form
   return(simplify());

 }  // end Fraction::set

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  Fraction::get - Get fraction value
//
// !INTERFACE:
      int Fraction::get(
//
// !RETURN VALUE:
//    none.
//
// !ARGUMENTS:
      ESMC_I8 *w,
      ESMC_I8 *n,
      ESMC_I8 *d) const {
//
// !DESCRIPTION:
//     Gets the fraction's value.
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::Fraction::get(w,n,d)"

   if (w != ESMC_NULL_POINTER) *w = this->w;
   if (n != ESMC_NULL_POINTER) *n = this->n;
   if (d != ESMC_NULL_POINTER) *d = this->d;

   return(ESMF_SUCCESS);

 }  // end Fraction::get

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  Fraction::simplify - Ensure proper fraction (< 1) and sign; reduce to lowest denominator
//
// !INTERFACE:
      int Fraction::simplify(void) {
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
 #define ESMC_METHOD "ESMCI::Fraction::simplify()"

    // Initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;

    // check for divide-by-zero
    if (d == 0) {
      ESMC_LogDefault.FoundError(ESMC_RC_DIV_ZERO, ESMC_CONTEXT,
                                 ESMC_NULL_POINTER);
      return(ESMC_RC_DIV_ZERO);
    }

    // normalize to proper fraction (labs(n/d) < 1)
    ESMC_I8 whole;
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

    ESMC_I8 gcd = ESMCI_FractionGCD(n,d);
    // this should never happen since GCD never returns zero!
    if (gcd == 0) {
      ESMC_LogDefault.FoundError(ESMC_RC_DIV_ZERO, ESMC_CONTEXT,
                                 ESMC_NULL_POINTER);
      rc = ESMC_RC_DIV_ZERO;
      return(rc);
    }

    n /= gcd;
    d /= gcd;

    return(ESMF_SUCCESS);

 }  // end Fraction::simplify

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  Fraction::convert - Convert to given denominator
//
// !INTERFACE:
      int Fraction::convert(
//
// !RETURN VALUE:
//    none.
//
// !ARGUMENTS:
      ESMC_I8 denominator) {  // input
//
// !DESCRIPTION:
//     Convert fraction in terms of given denominator
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::Fraction::convert()"

    if (d == 0) {
      ESMC_LogDefault.FoundError(ESMC_RC_DIV_ZERO, ESMC_CONTEXT,
                                 ESMC_NULL_POINTER);
      return(ESMC_RC_DIV_ZERO);
    }

    ESMC_I8 conversion = w * denominator + (n * denominator) / d;

    // set new values
    w = 0;
    n = conversion;
    d = denominator;

    // don't simplify; leave on specified denominator.

    return(ESMF_SUCCESS);

 }  // end Fraction::convert

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMCI_FractionGCD - determine the Greatest Common Divisor
//
// !INTERFACE:
      ESMC_I8 ESMCI_FractionGCD(
//
// !RETURN VALUE:
//    The GCD of a and b.
//
// !ARGUMENTS:
      ESMC_I8 a,    // in - the first number 
      ESMC_I8 b) {  // in - the second number
//
// !DESCRIPTION:
//     Uses Euclid's algorithm to determine the Greatest Common Divisor of 
//     a and b.
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI_FractionGCD()"

    // deal with a zero input
    if      (a == 0 && b != 0) return(LLABS(b));
    else if (a != 0 && b == 0) return(LLABS(a));
    else if (a == 0 && b == 0) return(1);

    ESMC_I8 abs_a = LLABS(a);
    ESMC_I8 abs_b = LLABS(b);
    ESMC_I8 large = MAX(abs_a, abs_b);
    ESMC_I8 small = MIN(abs_a, abs_b);
    ESMC_I8 remainder;

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

 }  // end ESMCI_FractionGCD

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMCI_FractionLCM - determine the Least Common Multiple
//
// !INTERFACE:
      ESMC_I8 ESMCI_FractionLCM(
//
// !RETURN VALUE:
//    the LCM of a and b
//
// !ARGUMENTS:
      ESMC_I8 a,    // in - the first number 
      ESMC_I8 b) {  // in - the second number
//
// !DESCRIPTION:
//      Uses GCD to determine the Least Common Multiple of a and b.
//      LCM = (a * b) / GCD(a,b)
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI_FractionLCM()"

    ESMC_I8 gcd = ESMCI_FractionGCD(a,b);

    // this should never happen since GCD never returns zero!
    if (gcd == 0) {
      ESMC_LogDefault.FoundError(ESMC_RC_DIV_ZERO, ESMC_CONTEXT,
                                 ESMC_NULL_POINTER);
      return(0);
    }

    return(LLABS((a/gcd) * b));   // avoid (a * b) directly to prevent
                                  //   overflow when a and b are large;
                                  //   return absolute value

 }  // end ESMCI_FractionLCM

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  Fraction(==) - Fraction equality comparison
//
// !INTERFACE:
      bool Fraction::operator==(
//
// !RETURN VALUE:
//    bool result
//
// !ARGUMENTS:
      const Fraction &fraction) const {   // in - Fraction to compare
//
// !DESCRIPTION:
//      Compare for equality the current object's (this) {\tt Fraction}
//      with given {\tt Fraction}, return result
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::Fraction::operator==()"

    // make local copies; don't change the originals.
    Fraction f1 = *this;
    Fraction f2 = fraction;

    // ensure proper fractions
    f1.simplify();
    f2.simplify();

    // put both fractions on the same denominator, then compare
    ESMC_I8 lcm = ESMCI_FractionLCM(f1.d, f2.d);
    return(f1.w == f2.w && f1.n*(lcm/f1.d) == f2.n*(lcm/f2.d));

}  // end Fraction::operator==

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  Fraction(!=) - Fraction inequality comparison
//
// !INTERFACE:
      bool Fraction::operator!=(
//
// !RETURN VALUE:
//    bool result
//
// !ARGUMENTS:
      const Fraction &fraction) const {   // in - Fraction to compare
//
// !DESCRIPTION:
//      Compare for inequality the current object's (this)
//      {\tt Fraction} with given {\tt Fraction}, return result
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::Fraction::operator!=()"

    // make local copies; don't change the originals.
    Fraction f1 = *this;
    Fraction f2 = fraction;

    // put both fractions on the same denominator, then compare
    f1.simplify();
    f2.simplify();

    // put both fractions on the same denominator, then compare
    ESMC_I8 lcm = ESMCI_FractionLCM(f1.d, f2.d);
    return(f1.w != f2.w || f1.n*(lcm/f1.d) != f2.n*(lcm/f2.d));

}  // end Fraction::operator!=

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  Fraction(<) - Fraction less than comparison
//
// !INTERFACE:
      bool Fraction::operator<(
//
// !RETURN VALUE:
//    bool result
//
// !ARGUMENTS:
      const Fraction &fraction) const {   // in - Fraction to compare
//
// !DESCRIPTION:
//      Compare for less than the current object's (this)
//      {\tt Fraction} with given {\tt Fraction}, return result
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::Fraction::operator<()"

    // make local copies; don't change the originals.
    Fraction f1 = *this;
    Fraction f2 = fraction;

    // ensure proper fractions
    f1.simplify();
    f2.simplify();

    // ignore fractional part if whole parts are different
    if (f1.w != f2.w) return(f1.w < f2.w);
    else { // must look at fractional part
      // put both fractions on the same denominator, then compare
      ESMC_I8 lcm = ESMCI_FractionLCM(f1.d, f2.d);
      return(f1.n*(lcm/f1.d) < f2.n*(lcm/f2.d));
    }

}  // end Fraction::operator<

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  Fraction(>) - Fraction greater than comparison
//
// !INTERFACE:
      bool Fraction::operator>(
//
// !RETURN VALUE:
//    bool result
//
// !ARGUMENTS:
      const Fraction &fraction) const {   // in - Fraction to compare
//
// !DESCRIPTION:
//      Compare for greater than the current object's (this)
//      {\tt Fraction} with given {\tt Fraction}, return result
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::Fraction::operator>()"

    // make local copies; don't change the originals.
    Fraction f1 = *this;
    Fraction f2 = fraction;

    // ensure proper fractions
    f1.simplify();
    f2.simplify();

    // ignore fractional part if whole parts are different
    if (f1.w != f2.w) return(f1.w > f2.w);
    else { // must look at fractional part
      // put both fractions on the same denominator, then compare
      ESMC_I8 lcm = ESMCI_FractionLCM(f1.d, f2.d);
      return(f1.n*(lcm/f1.d) > f2.n*(lcm/f2.d));
    }

}  // end Fraction::operator>

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  Fraction(<=) - Fraction less or equal than comparison
//
// !INTERFACE:
      bool Fraction::operator<=(
//
// !RETURN VALUE:
//    bool result
//
// !ARGUMENTS:
      const Fraction &fraction) const {   // in - Fraction to compare
//
// !DESCRIPTION:
//      Compare for less than or equal the current object's (this)
//      {\tt Fraction} with given {\tt Fraction}, return result
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::Fraction::operator<=()"

    // just reuse < and == operators defined above!
    return(*this < fraction || *this == fraction);

}  // end Fraction::operator<=

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  Fraction(>=) - Fraction greater than or equal comparison
//
// !INTERFACE:
      bool Fraction::operator>=(
//
// !RETURN VALUE:
//    bool result
//
// !ARGUMENTS:
      const Fraction &fraction) const {   // in - Fraction to compare
//
// !DESCRIPTION:
//      Compare for greater than or equal the current object's (this)
//      {\tt Fraction} with given {\tt Fraction}, return result
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::Fraction::operator>=()"

    // just reuse > and == operators defined above!
    return(*this > fraction || *this == fraction);

}  // end Fraction::operator>=

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  Fraction(+) - increment Fraction
//
// !INTERFACE:
      Fraction Fraction::operator+(
//
// !RETURN VALUE:
//    Fraction result
//
// !ARGUMENTS:
      const Fraction &fraction) const {   // in - Fraction increment
//
// !DESCRIPTION:
//      Increment current object's (this) {\tt Fraction} with given
//      {\tt Fraction}, return result
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::Fraction::operator+()"

    Fraction sum;

    // fractional part addition
    sum.d = ESMCI_FractionLCM(d, fraction.d);
    sum.n = n*(sum.d/d) + fraction.n*(sum.d/fraction.d);

    // whole part addition
    sum.w = w + fraction.w;

   // ensure simplified form
    sum.simplify();

    return(sum);

}  // end Fraction::operator+

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  Fraction(-) - decrement Fraction
//
// !INTERFACE:
      Fraction Fraction::operator-(
//
// !RETURN VALUE:
//    Fraction result
//
// !ARGUMENTS:
      const Fraction &fraction) const {   // in - Fraction decrement
//
// !DESCRIPTION:
//      Decrement current object's (this) {\tt Fraction} with given
//      {\tt Fraction}, return result
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::Fraction::operator-()"

    Fraction diff;

    // fractional part subtraction
    diff.d = ESMCI_FractionLCM(d, fraction.d);
    diff.n = n*(diff.d/d) - fraction.n*(diff.d/fraction.d);

    // whole part subtraction 
    diff.w = w - fraction.w;

   // ensure simplified form
    diff.simplify();

    return(diff);

}  // end Fraction::operator-

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  Fraction(+=) - increment Fraction
//
// !INTERFACE:
      Fraction& Fraction::operator+=(
//
// !RETURN VALUE:
//    Fraction& result
//
// !ARGUMENTS:
      const Fraction &fraction) {   // in - Fraction increment
//
// !DESCRIPTION:
//      Increment current object's (this) {\tt Fraction} with given
//      {\tt Fraction}
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::Fraction::operator+=()"

    // just reuse (+) operator defined above!
    *this = *this + fraction;

    return(*this);

}  // end Fraction::operator+=

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  Fraction(-=) - decrement Fraction
//
// !INTERFACE:
      Fraction& Fraction::operator-=(
//
// !RETURN VALUE:
//    Fraction& result
//
// !ARGUMENTS:
      const Fraction &fraction) {   // in - Fraction decrement
//
// !DESCRIPTION:
//      Decrement current object's (this) {\tt Fraction} with given
//      {\tt Fraction}
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::Fraction::operator-=()"

    // just reuse (-) operator defined above!
    *this = *this - fraction;

    return(*this);

}  // end Fraction::operator-=

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  Fraction(*) - multiply Fraction by integer
//
// !INTERFACE:
      Fraction Fraction::operator*(
//
// !RETURN VALUE:
//    Fraction result
//
// !ARGUMENTS:
      ESMC_I4 multiplier) const {   // in - integer multiplier
//
// !DESCRIPTION:
//      Multiply current object's (this) {\tt Fraction} with given
//      integer, return result
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::Fraction::operator*(integer)"

    Fraction product;

    // fractional part multiplication.
    product.n = n * multiplier;
    product.d = d;

    // whole part multiplication
    product.w = w * multiplier;

   // ensure simplified form
    product.simplify();

    return(product);

}  // end Fraction::operator*

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  Fraction(*=) - multiply Fraction by integer
//
// !INTERFACE:
      Fraction& Fraction::operator*=(
//
// !RETURN VALUE:
//    Fraction& result
//
// !ARGUMENTS:
      ESMC_I4 multiplier) {   // in - integer multiplier
//
// !DESCRIPTION:
//      Multiply current object's (this) {\tt Fraction} with given
//      integer.
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::Fraction::operator*=(integer)"

    // just reuse (*) operator defined above!
    *this = *this * multiplier;

    return(*this);

}  // end Fraction::operator*=

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  Fraction(/) - divide Fraction by integer
//
// !INTERFACE:
      Fraction Fraction::operator/(
//
// !RETURN VALUE:
//    Fraction result
//
// !ARGUMENTS:
      ESMC_I4 divisor) const {   // in - integer divisor
//
// !DESCRIPTION:
//      Divide current object's (this) {\tt Fraction} with given
//      integer, return result
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::Fraction::operator/(integer)"

    // check for divide-by-zero
    if (divisor == 0) {
      ESMC_LogDefault.FoundError(ESMC_RC_DIV_ZERO, ESMC_CONTEXT,
                                 ESMC_NULL_POINTER);
      return(Fraction(0,0,1));
    }

    Fraction quotient;
    ESMC_I8 remainder;
    ESMC_I8 denominator;

    // fractional part division.  don't just blindly multiply denominator;
    //   avoid overflow, especially with large denominators such as
    //   1,000,000,000 for nanoseconds.  So divide numerator and add back
    //   any remainder.
    quotient.n = n / (ESMC_I8) divisor;
    quotient.d = d;

    // check remainder and add back
    if ((remainder = n % (ESMC_I8) divisor) != 0) {
      // upper bounds check of (d * divisor)
      denominator = d * (ESMC_I8) divisor; // must do it!

      // add back
      quotient += Fraction(0, remainder, denominator);
    }

    // whole part division; add back any remainder
    quotient.w = w / (ESMC_I8) divisor;
    if ((remainder = w % (ESMC_I8) divisor) != 0) {
      quotient += Fraction(0, remainder, (ESMC_I8) divisor);
    }

   // ensure simplified form
    quotient.simplify();

    return(quotient);

}  // end Fraction::operator/

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  Fraction(/=) - divide Fraction by integer
//
// !INTERFACE:
      Fraction& Fraction::operator/=(
//
// !RETURN VALUE:
//    Fraction& result
//
// !ARGUMENTS:
      ESMC_I4 divisor) {   // in - integer divisor
//
// !DESCRIPTION:
//      Divide current object's (this) {\tt Fraction} with given
//      integer.
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::Fraction::operator/=(integer)"

    // just reuse (/) operator defined above!
    *this = *this / divisor;

    return(*this);

}  // end Fraction::operator/=

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  Fraction(/) - Divide two fractions, return double precision result
//
// !INTERFACE:
      ESMC_R8 Fraction::operator/(
//
// !RETURN VALUE:
//    ESMC_R8 result
//
// !ARGUMENTS:
      const Fraction &fraction) const {  // in - Fraction to divide by
//
// !DESCRIPTION:
//    Returns this fraction divided by given fraction as a ESMC_R8
//    precision quotient.
//
//EOP
// !REQUIREMENTS:

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::Fraction::operator/(fraction)"

    // check for divide-by-zero
    if (d == 0 || fraction.d == 0 || 
        fraction.w * fraction.d + fraction.n == 0) {
      ESMC_LogDefault.FoundError(ESMC_RC_DIV_ZERO, ESMC_CONTEXT,
                                 ESMC_NULL_POINTER);
      return(ESMC_RC_DIV_ZERO);
    }

    ESMC_R8 quotient = 
      (w + (ESMC_R8) n / (ESMC_R8) d) /
        (fraction.w + (ESMC_R8) fraction.n / (ESMC_R8) fraction.d);

    return(quotient);

}  // end Fraction::operator/

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  Fraction(\%) - Computes the modulus of two fractions
//
// !INTERFACE:
      Fraction Fraction::operator%(
//
// !RETURN VALUE:    Fraction result
//
// !ARGUMENTS:
      const Fraction &fraction) const {  // in - Fraction to modulo by
//
// !DESCRIPTION:
//    Returns this fraction modulo by given fraction
//
//EOP
// !REQUIREMENTS:

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::Fraction::operator%(fraction)"

    Fraction remainder;

    // if no fractional part, just modulus the whole parts
    if (n == 0 && fraction.n == 0) {
      // check for divide-by-zero
      if (fraction.w != 0) {
        remainder.n = w % fraction.w;
      } else {
        ESMC_LogDefault.FoundError(ESMC_RC_DIV_ZERO, ESMC_CONTEXT,
                                   ESMC_NULL_POINTER);
        return(Fraction(0,0,1));
      }

    // otherwise, perform fraction modulus
    } else {
      // check for divide-by-zero
      if (d == 0 || fraction.d == 0) {
        ESMC_LogDefault.FoundError(ESMC_RC_DIV_ZERO, ESMC_CONTEXT,
                                   ESMC_NULL_POINTER);
        return(Fraction(0,0,1));
      }

      ESMC_I8 lcm = ESMCI_FractionLCM(d, fraction.d);

      // convert *this fraction and given fraction into improper form with a 
      // common denominator and then compute remainder
      remainder.d = (fraction.w * fraction.d + fraction.n) * (lcm/fraction.d);
      if (remainder.d != 0) {
        remainder.n = ((w*d + n) * (lcm/d)) % remainder.d;
      } else {
        ESMC_LogDefault.FoundError(ESMC_RC_DIV_ZERO, ESMC_CONTEXT,
                                   ESMC_NULL_POINTER);
        return(Fraction(0,0,1));
      }
    }

   // ensure simplified form
    remainder.simplify();

    return(remainder);

}  // end Fraction::operator%

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  Fraction(\%=) - Computes the modulus of two fractions
//
// !INTERFACE:
      Fraction& Fraction::operator%=(
//
// !RETURN VALUE:    Fraction& result
//
// !ARGUMENTS:
      const Fraction &fraction) {  // in - Fraction to modulo by
//
// !DESCRIPTION:
//    Returns this fraction modulo by given fraction
//
//EOP
// !REQUIREMENTS:

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::Fraction::operator%=(fraction)"

    // just reuse (%) operator defined above!
    *this = *this % fraction;

    return(*this);

}  // end Fraction::operator%=

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  Fraction(=) - assignment operator
//
// !INTERFACE:
      Fraction& Fraction::operator=(
//
// !RETURN VALUE:
//    Fraction& result
//
// !ARGUMENTS:
      const Fraction &fraction) {   // in - Fraction
//
// !DESCRIPTION:
//      Assign current object's (this) {\tt Fraction} with given
//      {\tt Fraction}.  
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::Fraction::operator=(fraction)"

    // TODO: should be implicit, but then won't support
    //   ESMCI::BaseTime::operator= and ESMCI::TimeInterval::operator=

    if (&fraction != this) {
      this->w = fraction.w;
      this->n = fraction.n;
      this->d = fraction.d;
    }

    return(*this);

}  // end Fraction::operator=

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  Fraction::validate - validate Fraction state
//
// !INTERFACE:
      int Fraction::validate(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {     // in - options
//
// !DESCRIPTION:
//      validate {\tt Fraction} state
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::Fraction::validate()"

    // Initialize return code; assume file not implemented
    int rc = ESMC_RC_NOT_IMPL;

    // must have non-zero denominator
    if (d == 0) {
      char logMsg[ESMF_MAXSTR];
      sprintf(logMsg, "must have non-zero denominator."); 
      ESMC_LogDefault.Write(logMsg, ESMC_LOG_ERROR,ESMC_CONTEXT);
      return(ESMC_RC_DIV_ZERO);
    }

    rc = ESMF_SUCCESS;
    return(rc);

}  // end Fraction::validate

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  Fraction::print - print Fraction state
//
// !INTERFACE:
      int Fraction::print(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {    // in - print options
//
// !DESCRIPTION:
//      print {\tt Fraction} state for testing/debugging
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::Fraction::print()"

    // Initialize return code; assume file not implemented
    int rc = ESMC_RC_NOT_IMPL;

    printf("Fraction -------------------------------\n");
    printf("w = %lld\n", w);
    printf("n = %lld\n", n);
    printf("d = %lld\n", d);
    printf("end Fraction ---------------------------\n\n");

    rc = ESMF_SUCCESS;
    return(rc);

}  // end Fraction::print

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  Fraction - native default C++ constructor
//
// !INTERFACE:
      Fraction::Fraction(void) {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      Initializes a {\tt Fraction} with defaults
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::Fraction::Fraction(void) constructor"

   w = 0;
   n = 0;
   d = 1;  // to prevent divide-by-zero

 }  // end Fraction

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  Fraction - native C++ constructor
//
// !INTERFACE:
      Fraction::Fraction(
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
      ESMC_I8 w,   // Integer (whole) seconds (signed)
      ESMC_I8 n,   // Integer fraction (exact) n/d; numerator (signed)
      ESMC_I8 d) { // Integer fraction (exact) n/d; denominator

// !DESCRIPTION:
//      Initializes a {\tt Fraction} with given values
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::Fraction::Fraction(w,n,d) constructor"

 // Initialize return code
 int rc = ESMC_RC_NOT_IMPL;

   this->w = w;
   this->n = n;
   this->d = d;

   // ensure simplified form
   simplify();
   // TODO:  throw exception if simplify() returns ESMF_FAILURE

 }  // end Fraction

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  Fraction - native C++ constructor
//
// !INTERFACE:
      Fraction::Fraction(
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
      ESMC_R8 r) {  // fraction as a real (signed)

// !DESCRIPTION:
//      Initializes a {\tt Fraction} with given real number value.
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::Fraction::Fraction(r) constructor"

   setr(r);

 }  // end Fraction

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ~Fraction - native default C++ destructor
//
// !INTERFACE:
      Fraction::~Fraction(void) {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      Default {\tt Fraction} destructor
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::Fraction::~Fraction(void) destructor"

 }  // end ~Fraction

 } // namespace ESMCI
