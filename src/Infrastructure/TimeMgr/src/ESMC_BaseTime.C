// $Id: ESMC_BaseTime.C,v 1.9 2003/03/28 00:50:06 eschwab Exp $
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
 static const char *const version = "$Id: ESMC_BaseTime.C,v 1.9 2003/03/28 00:50:06 eschwab Exp $";
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

//
// individual get/set methods which perform signed conversion
//

//
// Integer get/sets
//

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BaseTimeGet_D - get BaseTime converted to days
//
// !INTERFACE:
      int ESMC_BaseTime::ESMC_BaseTimeGet_D(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int *D) const {  // out - BaseTime converted to days
//
// !DESCRIPTION:
//      converts {\tt BaseTime} value into days
//
//EOP
// !REQUIREMENTS:  

    if (D != 0) {
      *D = S / 86400;
      return(ESMF_SUCCESS);
    } else return(ESMF_FAILURE);

}  // end ESMC_BaseTimeGet_D

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BaseTimeSet_D - set BaseTime converted from days
//
// !INTERFACE:
      int ESMC_BaseTime::ESMC_BaseTimeSet_D(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int D) {  // out - BaseTime converted from days
//
// !DESCRIPTION:
//      converts days into {\tt BaseTime} value
//
//EOP
// !REQUIREMENTS:  

    S = D * 86400;

    return(ESMF_SUCCESS);

}  // end ESMC_BaseTimeSet_D

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BaseTimeGet_H - get BaseTime converted to hours
//
// !INTERFACE:
      int ESMC_BaseTime::ESMC_BaseTimeGet_H(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int *H) const {  // out - BaseTime converted to hours
//
// !DESCRIPTION:
//      converts {\tt BaseTime} value into hours
//
//EOP
// !REQUIREMENTS:  

    if (H != 0) {
      *H = S / 3600;
      return(ESMF_SUCCESS);
    } else return(ESMF_FAILURE);

}  // end ESMC_BaseTimeGet_H

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BaseTimeSet_H - set BaseTime converted from hours
//
// !INTERFACE:
      int ESMC_BaseTime::ESMC_BaseTimeSet_H(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int H) {  // in - hours to convert into BaseTime
//
// !DESCRIPTION:
//      converts hours into {\tt BaseTime} value
//
//EOP
// !REQUIREMENTS:  

     S = H * 3600;

     return(ESMF_SUCCESS);

}  // end ESMC_BaseTimeSet_H

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BaseTimeGet_M - get BaseTime converted to minutes
//
// !INTERFACE:
      int ESMC_BaseTime::ESMC_BaseTimeGet_M(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int *M) const {  // out - BaseTime converted to minutes
//
// !DESCRIPTION:
//      converts {\tt BaseTime} value into minutes
//
//EOP
// !REQUIREMENTS:  

     if (M != 0) {
       *M = S / 60;
       return(ESMF_SUCCESS);
     } else return(ESMF_FAILURE);

}  // end ESMC_BaseTimeGet_M

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BaseTimeSet_M - set BaseTime converted from minutes
//
// !INTERFACE:
      int ESMC_BaseTime::ESMC_BaseTimeSet_M(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int M) {  // in - minutes to convert into BaseTime
//
// !DESCRIPTION:
//      converts minutes into {\tt BaseTime} value
//
//EOP
// !REQUIREMENTS:  

     S = M * 60;

     return(ESMF_SUCCESS);

}  // end ESMC_BaseTimeSet_M

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BaseTimeGet_S - get BaseTime converted to seconds
//
// !INTERFACE:
      int ESMC_BaseTime::ESMC_BaseTimeGet_S(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int *S) const {  // out - BaseTime converted to seconds
//
// !DESCRIPTION:
//      converts {\tt BaseTime} value into seconds
//
//EOP
// !REQUIREMENTS:  

    if (S != 0) {
      *S = this->S;
      return(ESMF_SUCCESS);
    }
    else return (ESMF_FAILURE);

}  // end ESMC_BaseTimeGet_S

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BaseTimeSet_S - set BaseTime converted from seconds
//
// !INTERFACE:
      int ESMC_BaseTime::ESMC_BaseTimeSet_S(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int S) {  // in - seconds to convert into BaseTime
//
// !DESCRIPTION:
//      converts seconds into {\tt BaseTime} value
//
//EOP
// !REQUIREMENTS:  

    this->S = S;

    return(ESMF_SUCCESS);

}  // end ESMC_BaseTimeSet_S

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BaseTimeGet_MS - get BaseTime converted to milliseconds
//
// !INTERFACE:
      int ESMC_BaseTime::ESMC_BaseTimeGet_MS(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int *MS) const {  // out - BaseTime converted to milliseconds
//
// !DESCRIPTION:
//      converts {\tt BaseTime} value into milliseconds
//
//EOP
// !REQUIREMENTS:  

    if (MS != 0) {
      *MS = S * 1000 + Sn;  // TODO: assume Sd = 1000
      // TODO: convert Sn/Sd fraction
      return(ESMF_SUCCESS);
    }
    else return (ESMF_FAILURE);

}  // end ESMC_BaseTimeGet_MS

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BaseTimeSet_MS - set BaseTime converted from milliseconds
//
// !INTERFACE:
      int ESMC_BaseTime::ESMC_BaseTimeSet_MS(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int MS) {  // in - milliseconds to convert into BaseTime
//
// !DESCRIPTION:
//      converts milliseconds into {\tt BaseTime} value
//
//EOP
// !REQUIREMENTS:  

    S = MS / 1000;
    Sn = MS % 1000;
    Sd = 1000;

    return(ESMF_SUCCESS);

}  // end ESMC_BaseTimeSet_MS

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BaseTimeGet_US - get BaseTime converted to microseconds
//
// !INTERFACE:
      int ESMC_BaseTime::ESMC_BaseTimeGet_US(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int *US) const {  // out - BaseTime converted to microseconds
//
// !DESCRIPTION:
//      converts {\tt BaseTime} value into microseconds
//
//EOP
// !REQUIREMENTS:  

    if (US != 0) {
      *US = S * 1000000 + Sn;  // TODO: assume Sd = 1000000
      // TODO: convert Sn/Sd fraction
      return(ESMF_SUCCESS);
    }
    else return (ESMF_FAILURE);

}  // end ESMC_BaseTimeGet_US

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BaseTimeSet_US - set BaseTime converted from microseconds
//
// !INTERFACE:
      int ESMC_BaseTime::ESMC_BaseTimeSet_US(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int US) {  // in - microseconds to convert into BaseTime
//
// !DESCRIPTION:
//      converts microseconds into {\tt BaseTime} value
//
//EOP
// !REQUIREMENTS:  

    S = US / 1000000;
    Sn = US % 1000000;
    Sd = 1000000;

    return(ESMF_SUCCESS);

}  // end ESMC_BaseTimeSet_US

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BaseTimeGet_NS - get BaseTime converted to nanoseconds
//
// !INTERFACE:
      int ESMC_BaseTime::ESMC_BaseTimeGet_NS(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int *NS) const {  // out - BaseTime converted to nanoseconds
//
// !DESCRIPTION:
//      converts {\tt BaseTime} value into nanoseconds
//
//EOP
// !REQUIREMENTS:  

    if (NS != 0) {
      *NS = S * 1000000000 + Sn;  // TODO:  assume Sd = 1000000000
      // TODO: convert Sn/Sd fraction
      return(ESMF_SUCCESS);
    }
    else return (ESMF_FAILURE);

}  // end ESMC_BaseTimeGet_NS

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BaseTimeSet_NS - set BaseTime converted from nanoseconds
//
// !INTERFACE:
      int ESMC_BaseTime::ESMC_BaseTimeSet_NS(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int NS) {  // in - nanoseconds to convert into BaseTime
//
// !DESCRIPTION:
//      converts nanoseconds into {\tt BaseTime} value
//
//EOP
// !REQUIREMENTS:  

    S = NS / 1000000000;
    Sn = NS % 1000000000;
    Sd = 1000000000;

    return(ESMF_SUCCESS);

}  // end ESMC_BaseTimeSet_NS

//
// Floating point get/sets
//

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BaseTimeGet_d - get BaseTime converted to floating point days
//
// !INTERFACE:
      int ESMC_BaseTime::ESMC_BaseTimeGet_d(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      double *d) const {  // out - BaseTime converted to floating point days
//
// !DESCRIPTION:
//      converts {\tt BaseTime} value into floating point days
//
//EOP
// !REQUIREMENTS:  

    if (d != 0) {
      *d = (double) S / 86400.0;
      return(ESMF_SUCCESS);
    } else return(ESMF_FAILURE);

}  // end ESMC_BaseTimeGet_d

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BaseTimeSet_d - set BaseTime converted from floating point days
//
// !INTERFACE:
      int ESMC_BaseTime::ESMC_BaseTimeSet_d(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      double d) {  // out - BaseTime converted from floating point days
//
// !DESCRIPTION:
//      converts floating point days into {\tt BaseTime} value
//
//EOP
// !REQUIREMENTS:  

    S = (ESMF_IKIND_I8) (d * 86400);
    // TODO convert remainder to Sn/Sd

    return(ESMF_SUCCESS);

}  // end ESMC_BaseTimeSet_d

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BaseTimeGet_h - get BaseTime converted to floating point hours
//
// !INTERFACE:
      int ESMC_BaseTime::ESMC_BaseTimeGet_h(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      double *h) const {  // out - BaseTime converted to hours
//
// !DESCRIPTION:
//      converts {\tt BaseTime} value into hours
//
//EOP
// !REQUIREMENTS:  

    if (h != 0) {
      *h = (double) S / 3600.0;
      return(ESMF_SUCCESS);
    } else return(ESMF_FAILURE);

}  // end ESMC_BaseTimeGet_h

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BaseTimeSet_h - set BaseTime converted from floating point hours
//
// !INTERFACE:
      int ESMC_BaseTime::ESMC_BaseTimeSet_h(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      double h) {  // in - floating point hours to convert into BaseTime
//
// !DESCRIPTION:
//      converts floating point hours into {\tt BaseTime} value
//
//EOP
// !REQUIREMENTS:  

     S = (ESMF_IKIND_I8) (h * 3600);
    // TODO convert remainder to Sn/Sd

     return(ESMF_SUCCESS);

}  // end ESMC_BaseTimeSet_h

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BaseTimeGet_m - get BaseTime converted to floating point minutes
//
// !INTERFACE:
      int ESMC_BaseTime::ESMC_BaseTimeGet_m(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      double *m) const {  // out - BaseTime converted to floating point minutes
//
// !DESCRIPTION:
//      converts {\tt BaseTime} value into floating point minutes
//
//EOP
// !REQUIREMENTS:  

     if (m != 0) {
       *m = (double) S / 60.0;
       return(ESMF_SUCCESS);
     } else return(ESMF_FAILURE);

}  // end ESMC_BaseTimeGet_m

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BaseTimeSet_m - set BaseTime converted from floating point minutes
//
// !INTERFACE:
      int ESMC_BaseTime::ESMC_BaseTimeSet_m(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      double m) {  // in - floating point minutes to convert into BaseTime
//
// !DESCRIPTION:
//      converts floating point minutes into {\tt BaseTime} value
//
//EOP
// !REQUIREMENTS:  

     S = (ESMF_IKIND_I8) (m * 60);
    // TODO convert remainder to Sn/Sd

     return(ESMF_SUCCESS);

}  // end ESMC_BaseTimeSet_m

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BaseTimeGet_s - get BaseTime converted to floating point seconds
//
// !INTERFACE:
      int ESMC_BaseTime::ESMC_BaseTimeGet_s(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      double *s) const {  // out - BaseTime converted to floating point seconds
//
// !DESCRIPTION:
//      converts {\tt BaseTime} value into floating point seconds
//
//EOP
// !REQUIREMENTS:  

    if (s != 0) {
      *s = (double) S;
      return(ESMF_SUCCESS);
    }
    else return (ESMF_FAILURE);

}  // end ESMC_BaseTimeGet_s

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BaseTimeSet_s - set BaseTime converted from floating point seconds
//
// !INTERFACE:
      int ESMC_BaseTime::ESMC_BaseTimeSet_s(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      double s) {  // in - floating point seconds to convert into BaseTime
//
// !DESCRIPTION:
//      converts floating point seconds into {\tt BaseTime} value
//
//EOP
// !REQUIREMENTS:  

    S = (ESMF_IKIND_I8) s;
    // TODO convert remainder to Sn/Sd

    return(ESMF_SUCCESS);

}  // end ESMC_BaseTimeSet_s

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BaseTimeGet_ms - get BaseTime converted to floating point milliseconds
//
// !INTERFACE:
      int ESMC_BaseTime::ESMC_BaseTimeGet_ms(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      double *ms) const {  // out - BaseTime converted to floating point milliseconds
//
// !DESCRIPTION:
//      converts {\tt BaseTime} value into floating point milliseconds
//
//EOP
// !REQUIREMENTS:  

    if (ms != 0) {
      *ms = (double) S * 1000.0 + (double) Sn;  // TODO: assume Sd = 1000
      // TODO: convert Sn/Sd fraction
      return(ESMF_SUCCESS);
    }
    else return (ESMF_FAILURE);

}  // end ESMC_BaseTimeGet_ms

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BaseTimeSet_ms - set BaseTime converted from floating point milliseconds
//
// !INTERFACE:
      int ESMC_BaseTime::ESMC_BaseTimeSet_ms(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      double ms) {  // in - floating point milliseconds to convert into BaseTime
//
// !DESCRIPTION:
//      converts floating point milliseconds into {\tt BaseTime} value
//
//EOP
// !REQUIREMENTS:  

    S = (ESMF_IKIND_I8) (ms / 1000);
    // TODO convert remainder to Sn/Sd

    return(ESMF_SUCCESS);

}  // end ESMC_BaseTimeSet_ms

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BaseTimeGet_us - get BaseTime converted to floating point microseconds
//
// !INTERFACE:
      int ESMC_BaseTime::ESMC_BaseTimeGet_us(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      double *us) const {  // out - BaseTime converted to floating point microseconds
//
// !DESCRIPTION:
//      converts {\tt BaseTime} value into floating point microseconds
//
//EOP
// !REQUIREMENTS:  

    if (us != 0) {
      *us = (double) S * 1000000.0 + (double) Sn;  // TODO: assume Sd = 1000000
      // TODO: convert Sn/Sd fraction
      return(ESMF_SUCCESS);
    }
    else return (ESMF_FAILURE);

}  // end ESMC_BaseTimeGet_us

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BaseTimeSet_us - set BaseTime converted from floating point microseconds
//
// !INTERFACE:
      int ESMC_BaseTime::ESMC_BaseTimeSet_us(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      double us) {  // in - floating point microseconds to convert into BaseTime
//
// !DESCRIPTION:
//      converts floating point microseconds into {\tt BaseTime} value
//
//EOP
// !REQUIREMENTS:  

    S = (ESMF_IKIND_I8) (us / 1000000);
    // TODO convert remainder to Sn/Sd

    return(ESMF_SUCCESS);

}  // end ESMC_BaseTimeSet_us

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BaseTimeGet_ns - get BaseTime converted to floating point nanoseconds
//
// !INTERFACE:
      int ESMC_BaseTime::ESMC_BaseTimeGet_ns(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      double *ns) const {  // out - BaseTime converted to floating point nanoseconds
//
// !DESCRIPTION:
//      converts {\tt BaseTime} value into floating point nanoseconds
//
//EOP
// !REQUIREMENTS:  

    if (ns != 0) {
      *ns = (double) S * 1000000000.0 + (double) Sn;
                                           // TODO:  assume Sd = 1000000000
      // TODO: convert Sn/Sd fraction
      return(ESMF_SUCCESS);
    }
    else return (ESMF_FAILURE);

}  // end ESMC_BaseTimeGet_ns

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BaseTimeSet_ns - set BaseTime converted from floating point nanoseconds
//
// !INTERFACE:
      int ESMC_BaseTime::ESMC_BaseTimeSet_ns(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      double ns) {  // in - floating point nanoseconds to convert into BaseTime
//
// !DESCRIPTION:
//      converts floating point nanoseconds into {\tt BaseTime} value
//
//EOP
// !REQUIREMENTS:  

    S = (ESMF_IKIND_I8) (ns / 1000000000);
    // TODO convert remainder to Sn/Sd

    return(ESMF_SUCCESS);

}  // end ESMC_BaseTimeSet_ns

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
// !IROUTINE:  ESMC_BaseValidate - validate BaseTime state
//
// !INTERFACE:
      int ESMC_BaseTime::ESMC_BaseValidate(
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

}  // end ESMC_BaseValidate

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BasePrint - print BaseTime state
//
// !INTERFACE:
      int ESMC_BaseTime::ESMC_BasePrint(
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

    cout << "S = "  << S  << endl;
    cout << "Sn = " << Sn << endl;
    cout << "Sd = " << Sd << endl << endl;

    return(ESMF_SUCCESS);

}  // end ESMC_BasePrint

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BasePrint - return BaseTime state
//
// !INTERFACE:
      int ESMC_BaseTime::ESMC_BasePrint(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMF_IKIND_I8 *S,              // out - integer seconds
      int *Sn,             // out - fractional seconds, numerator
      int *Sd) const {     // out - fractional seconds, denominator
//
// !DESCRIPTION:
//      return {\tt BaseTime} state for persistence/checkpointing
//
//EOP
// !REQUIREMENTS:  

// TODO: replace with checkpoint routine

    if (S != NULL && Sn != NULL & Sd != NULL)
    {
        *S = this->S;
        *Sn = this->Sn;
        *Sd = this->Sd;

        return(ESMF_SUCCESS);
    }
    else return(ESMF_FAILURE);

}  // end ESMC_BasePrint

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

    ESMC_BaseTimeInit(0, 0, 1);

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

    ESMC_BaseTimeInit(S, Sn, Sd);

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
