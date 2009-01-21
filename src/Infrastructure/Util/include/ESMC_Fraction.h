// $Id: ESMC_Fraction.h,v 1.5.2.2 2009/01/21 21:25:24 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
// ESMF Fraction C++ definition include file
//
// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
//
 // these lines prevent this file from being read more than once if it
 // ends up being included multiple times

#ifndef ESMC_FRACTION_H
#define ESMC_FRACTION_H

//-------------------------------------------------------------------------

 // Put any constants or macros which apply to the whole component in this file.
 // Anything public or esmf-wide should be up higher at the top level
 // include files.

//-------------------------------------------------------------------------
//BOP
//
// !CLASS: ESMC_Fraction - represent and manipulate rational fractions
//
// !DESCRIPTION:
//      ESMF C++ {\tt ESMC_Fraction} class.  While this class was developed
//      to support fractional seconds functionality in the ESMF Time Manager,
//      it is time-knowledge independent; it simply performs generic fractional
//      arithmetic, manipulations and comparisons. 
//
//-------------------------------------------------------------------------
//
// !USES:
#include <ESMC_Base.h>  // all classes inherit from the ESMC Base class.

// !PUBLIC TYPES:
 class ESMC_Fraction;

// !PRIVATE TYPES:
 // class configuration type:  not needed for ESMC_Fraction

 // class definition type
class ESMC_Fraction
{
  private:
    ESMC_I8 w;  // Integer (whole) seconds (signed)
    ESMC_I4 n;  // Integer fraction (exact) n/d; numerator (signed)
    ESMC_I4 d;  // Integer fraction (exact) n/d; denominator

// !PUBLIC MEMBER FUNCTIONS:

  public:
    // native C++ style Set/Get
    int ESMC_FractionSet(ESMC_I8 w, ESMC_I4 n, ESMC_I4 d);
    int ESMC_FractionSetw(ESMC_I8 w);
    int ESMC_FractionSetn(ESMC_I4 n);
    int ESMC_FractionSetd(ESMC_I4 d);
    ESMC_I8 ESMC_FractionGetw(void) const;
    ESMC_I4 ESMC_FractionGetn(void) const;
    ESMC_I4 ESMC_FractionGetd(void) const;

    // Set/Get to support F90 optional argument style
    int ESMC_FractionSet(ESMC_I8 *w, ESMC_I4 *n, ESMC_I4 *d);
    int ESMC_FractionGet(ESMC_I8 *w, ESMC_I4 *n,
                         ESMC_I4 *d) const;

    int ESMC_FractionSimplify(void);
    int ESMC_FractionConvert(ESMC_I4 denominator);

    // comparison methods (TMG 1.5.3, 2.4.3, 7.2)
    bool operator==(const ESMC_Fraction &) const;
    bool operator!=(const ESMC_Fraction &) const;
    bool operator< (const ESMC_Fraction &) const;
    bool operator> (const ESMC_Fraction &) const;
    bool operator<=(const ESMC_Fraction &) const;
    bool operator>=(const ESMC_Fraction &) const;

    // increment, decrement methods (TMG 1.5.4, 2.4.4, 2.4.5, 2.4.6, 5.1, 5.2,
    //                                   7.2)
    ESMC_Fraction  operator+ (const ESMC_Fraction &) const;
    ESMC_Fraction  operator- (const ESMC_Fraction &) const;
    ESMC_Fraction& operator+=(const ESMC_Fraction &);
    ESMC_Fraction& operator-=(const ESMC_Fraction &);

    // multiplication methods
    ESMC_Fraction  operator* (ESMC_I4 multiplier) const;
    ESMC_Fraction& operator*=(ESMC_I4 multiplier);

    // division methods
    ESMC_Fraction  operator/ (ESMC_I4 divisor) const;
    ESMC_Fraction& operator/=(ESMC_I4 divisor);
    ESMC_R8   operator/ (const ESMC_Fraction &) const;

    // modulus methods
    ESMC_Fraction  operator% (const ESMC_Fraction &) const;
    ESMC_Fraction& operator%=(const ESMC_Fraction &); 

    // explicit assignment operator to support ESMC_BaseTime::operator=
    // and ESMC_TimeInterval::operator=
    // TODO:  should be implicit ?
    ESMC_Fraction& operator=(const ESMC_Fraction &);

    // internal validation
    int ESMC_FractionValidate(const char *options=0) const;

    // for testing/debugging
    int ESMC_FractionPrint(const char *options=0) const;

    // native C++ constructor/destructors
    ESMC_Fraction(void);
    ESMC_Fraction(ESMC_I8 w, ESMC_I4 n=0, ESMC_I4 d=1);
    ESMC_Fraction(int w, int n=0, int d=1);
    ~ESMC_Fraction(void);

 // < declare the rest of the public interface methods here >

// !PRIVATE MEMBER FUNCTIONS:
//
  private:
//
 // < declare private interface methods here >
//
//EOP
//-------------------------------------------------------------------------

};  // end class ESMC_Fraction

    // related general utility functions which do not operate on fraction
    //   objects directly
    ESMC_I4 ESMC_FractionGCD(ESMC_I4 a, ESMC_I4 b);
    ESMC_I4 ESMC_FractionLCM(ESMC_I4 a, ESMC_I4 b);

#endif // ESMC_FRACTION_H
