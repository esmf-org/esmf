// $Id: ESMC_BaseTime.C,v 1.2 2003/03/14 05:17:39 eschwab Exp $
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
 #include <stdio.h>
 #include <stdlib.h>
 /*
 #include <iostream>
 //#include <stdlib.h>
 using std::cout;
 using std::endl;
 */

 // associated class definition file
 #include <ESMC_BaseTime.h>

//-------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_BaseTime.C,v 1.2 2003/03/14 05:17:39 eschwab Exp $";
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
      ESMF_IKIND_I8 S,              // in - integer seconds
      int Sn,             // in - fractional seconds, numerator
      int Sd ) {          // in - fractional seconds, denominator
//
// !DESCRIPTION:
//      Initialzes a {\tt BaseTime} with given values
//
//EOP
// !REQUIREMENTS:  

    printf("ESMC_BaseTime::ESMC_BaseTimeInit(): S, Sn, Sd = %lld, %d, %d\n",
           S, Sn, Sd);

    // S, Sn must be either both positive or both negative;
    //    Sd always positive and >= 1
    if ( ((S >= 0 && Sn >= 0) || (S <= 0 && Sn <= 0)) && Sd >= 1 )
    {
        this->S = S;
        this->Sn = Sn;
        this->Sd = Sd;

        printf("ESMC_BaseTime::ESMC_BaseTimeInit(): S, Sn, Sd = %lld, %d, %d\n",
                    this->S, this->Sn, this->Sd);

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
// !IROUTINE:  ESMC_BasePrint - print BaseTime state
//
// !INTERFACE:
      int ESMC_BaseTime::ESMC_BasePrint(void) const {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      print {\tt BaseTime} state for testing/debugging
//
//EOP
// !REQUIREMENTS:  

/*
    cout << "S = "  << S  << endl;
    cout << "Sn = " << Sn << endl;
    cout << "Sd = " << Sd << endl << endl;
*/
    return(ESMF_SUCCESS);

}  // end ESMC_BasePrint

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
      ESMC_BaseTime &Time) {   // in - ESMC_BaseTime increment
//
// !DESCRIPTION:
//      Increment current object's (this) {\tt BaseTime} with given {\tt Time},
//      return result
//
//EOP
// !REQUIREMENTS:  

    ESMC_BaseTime sum = *this;

    // assume positive values for now ??
    // fractional part addition -- LCD (assume same denominator for now) ??
    sum.Sn += Time.Sn;

    // normalize (share logic with ESMC_BaseTimeInit() ?? )
    int w;
    if (labs((w = sum.Sn/sum.Sd)) >= 1)
    {
         sum.S += w;
         sum.Sn = sum.Sn % sum.Sd;
    }

    // whole part addition
    sum.S += Time.S;

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
      ESMC_BaseTime &Time) {   // in - ESMC_BaseTime decrement
//
// !DESCRIPTION:
//      Decrement current object's (this) {\tt BaseTime} with given {\tt Time},
//      return result
//
//EOP
// !REQUIREMENTS:  

    ESMC_BaseTime diff = *this;

    // assume positive values for now ??
    // assume this > Time and both normalized for now ??
    // fractional part subtraction -- LCD (assume same denominator for now) ??

    // fractional part subtraction
    if (diff.Sn < Time.Sn)
    {
        // borrow
        diff.Sn += diff.Sd;
        diff.S--;
    }
    diff.Sn -= Time.Sn;

    // whole part subtraction 
    diff.S -= Time.S;

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
      ESMC_BaseTime &Time) {   // in - ESMC_BaseTime increment
//
// !DESCRIPTION:
//      Increment current object's (this) {\tt BaseTime} with given {\tt Time}
//
//EOP
// !REQUIREMENTS:  

    // assume positive values for now ??
    // fractional part addition -- LCD (assume same denominator for now) ??
    Sn += Time.Sn;

    // normalize (share logic with ESMC_BaseTimeInit() ?? )
    int w;
    if (labs((w = Sn/Sd)) >= 1)
    {
         S += w;
         Sn = Sn % Sd;
    }

    // whole part addition
    S += Time.S;

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
      ESMC_BaseTime &Time) {   // in - ESMC_BaseTime decrement
//
// !DESCRIPTION:
//      Decrement current object's (this) {\tt BaseTime} with given {\tt Time}
//
//EOP
// !REQUIREMENTS:  

    // assume positive values for now ??
    // assume this > Time and both normalized for now ??
    // fractional part subtraction -- LCD (assume same denominator for now) ??

    // fractional part subtraction
    if (Sn < Time.Sn)
    {
        // borrow
        Sn += Sd;
        S--;
    }
    Sn -= Time.Sn;

    // whole part subtraction 
    S -= Time.S;

    return(*this);

}  // end ESMC_BaseTime::operator-=


// individual get/set methods which perform signed conversion

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
      int *H) {  // out - BaseTime converted to hours
//
// !DESCRIPTION:
//      converts {\tt BaseTime} value into hours
//
//EOP
// !REQUIREMENTS:  

    return(ESMF_SUCCESS);

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
      int H) {  // int - hours to convert into BaseTime
//
// !DESCRIPTION:
//      converts hours into {\tt BaseTime} value
//
//EOP
// !REQUIREMENTS:  

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
      int *M) {  // out - BaseTime converted to minutes
//
// !DESCRIPTION:
//      converts {\tt BaseTime} value into minutes
//
//EOP
// !REQUIREMENTS:  

     return(ESMF_SUCCESS);

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
      int M) {  // int - minutes to convert into BaseTime
//
// !DESCRIPTION:
//      converts minutes into {\tt BaseTime} value
//
//EOP
// !REQUIREMENTS:  

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
      int *S) {  // out - BaseTime converted to seconds
//
// !DESCRIPTION:
//      converts {\tt BaseTime} value into seconds
//
//EOP
// !REQUIREMENTS:  

    if (S != NULL)
    {
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
      int S) {  // int - seconds to convert into BaseTime
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
