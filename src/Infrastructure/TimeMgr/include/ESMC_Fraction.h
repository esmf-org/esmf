// $Id: ESMC_Fraction.h,v 1.2 2003/02/11 19:03:32 eschwab Exp $
#ifndef ESMC_FRACTION_H
#define ESMC_FRACTION_H

#include <ESMC_Base.h>

class ESMC_Fraction
{
//-------------------------------------------------------------------------
//BOP
//
// !CLASS: ESMC_Fraction
//
// !SUPERCLASSES:
//
// !AGGREGATE CLASSES:
//
// !ASSOCIATE CLASSES:
//
// !FRIEND CLASSES:
//
// !PUBLIC DATA MEMBERS:
//
// !PUBLIC MEMBER FUNCTIONS:
  public:

    ESMC_Fraction(void);
    ~ESMC_Fraction(void);
//
// !DESCRIPTION:
//      ESMF C++ {\tt Fraction} class
//
// !BUGS:
//
// !SEE ALSO:
//
// !REVISION HISTORY:
//
//  10Jun02   Earl Schwab  Initial code.
//
//EOP
//-------------------------------------------------------------------------
  protected:

    int n;  // Integer fraction (exact) n/d; numerator
    int d;  // Integer fraction (exact) n/d; denominator
};

#endif // ESMC_FRACTION_H
