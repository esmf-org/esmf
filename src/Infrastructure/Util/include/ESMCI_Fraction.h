// $Id: ESMCI_Fraction.h,v 1.1 2008/10/19 03:41:20 eschwab Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2008, University Corporation for Atmospheric Research,
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
// !CLASS: ESMCI::Fraction - represent and manipulate rational fractions
//
// !DESCRIPTION:
//      ESMF C++ {\tt Fraction} class.  While this class was developed
//      to support fractional seconds functionality in the ESMF Time Manager,
//      it is time-knowledge independent; it simply performs generic fractional
//      arithmetic, manipulations and comparisons. 
//
//-------------------------------------------------------------------------
//
// !USES:
#include <ESMC_Base.h>  // all classes inherit from the ESMC Base class.

namespace ESMCI{

// !PUBLIC TYPES:
 class Fraction;

// !PRIVATE TYPES:
 // class configuration type:  not needed for Fraction

 // class definition type
class Fraction
{
  private:
    ESMC_I8 w;  // Integer (whole) seconds (signed)
    ESMC_I4 n;  // Integer fraction (exact) n/d; numerator (signed)
    ESMC_I4 d;  // Integer fraction (exact) n/d; denominator

// !PUBLIC MEMBER FUNCTIONS:

  public:
    // native C++ style Set/Get
    int set(ESMC_I8 w, ESMC_I4 n, ESMC_I4 d);
    int setw(ESMC_I8 w);
    int setn(ESMC_I4 n);
    int setd(ESMC_I4 d);
    ESMC_I8 getw(void) const;
    ESMC_I4 getn(void) const;
    ESMC_I4 getd(void) const;

    // Set/Get to support F90 optional argument style
    int set(ESMC_I8 *w, ESMC_I4 *n, ESMC_I4 *d);
    int get(ESMC_I8 *w, ESMC_I4 *n, ESMC_I4 *d) const;

    int simplify(void);
    int convert(ESMC_I4 denominator);

    // comparison methods (TMG 1.5.3, 2.4.3, 7.2)
    bool operator==(const Fraction &) const;
    bool operator!=(const Fraction &) const;
    bool operator< (const Fraction &) const;
    bool operator> (const Fraction &) const;
    bool operator<=(const Fraction &) const;
    bool operator>=(const Fraction &) const;

    // increment, decrement methods (TMG 1.5.4, 2.4.4, 2.4.5, 2.4.6, 5.1, 5.2,
    //                                   7.2)
    Fraction  operator+ (const Fraction &) const;
    Fraction  operator- (const Fraction &) const;
    Fraction& operator+=(const Fraction &);
    Fraction& operator-=(const Fraction &);

    // multiplication methods
    Fraction  operator* (ESMC_I4 multiplier) const;
    Fraction& operator*=(ESMC_I4 multiplier);

    // division methods
    Fraction  operator/ (ESMC_I4 divisor) const;
    Fraction& operator/=(ESMC_I4 divisor);
    ESMC_R8   operator/ (const Fraction &) const;

    // modulus methods
    Fraction  operator% (const Fraction &) const;
    Fraction& operator%=(const Fraction &); 

    // explicit assignment operator to support ESMCI::BaseTime::operator=
    // and ESMCI::TimeInterval::operator=
    // TODO:  should be implicit ?
    Fraction& operator=(const Fraction &);

    // internal validation
    int validate(const char *options=0) const;

    // for testing/debugging
    int print(const char *options=0) const;

    // native C++ constructor/destructors
    Fraction(void);
    Fraction(ESMC_I8 w, ESMC_I4 n=0, ESMC_I4 d=1);
    Fraction(int w, int n=0, int d=1);
    ~Fraction(void);

 // < declare the rest of the public interface methods here >

// !PRIVATE MEMBER FUNCTIONS:
//
  private:
//
 // < declare private interface methods here >
//
//EOP
//-------------------------------------------------------------------------

};  // end class Fraction

    // related general utility functions which do not operate on fraction
    //   objects directly
    ESMC_I4 ESMCI_FractionGCD(ESMC_I4 a, ESMC_I4 b);
    ESMC_I4 ESMCI_FractionLCM(ESMC_I4 a, ESMC_I4 b);

} // namespace ESMCI

#endif // ESMC_FRACTION_H
