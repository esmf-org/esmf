// $Id: ESMC_Fraction.h,v 1.3 2003/04/25 09:00:05 eschwab Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the GPL.
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
//      ESMF C++ {\tt ESMC_Fraction} class
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
  protected:

    int n;  // Integer fraction (exact) n/d; numerator
    int d;  // Integer fraction (exact) n/d; denominator

// !PUBLIC MEMBER FUNCTIONS:

  public:

    // native C++ constructor/destructor
    ESMC_Fraction(void);
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

#endif // ESMC_FRACTION_H
