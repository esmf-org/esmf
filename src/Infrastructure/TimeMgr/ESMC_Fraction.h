// $Id: ESMC_Fraction.h,v 1.1 2002/08/18 23:22:49 eschwab Exp $
#ifndef ESMC_FRACTION_H
#define ESMC_FRACTION_H

#include <ESMC_Types.h>

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
//      ESMF C++ Fraction class
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

    int32 n;	// Integer fraction (exact) n/d; numerator
    int32 d;  	// Integer fraction (exact) n/d; denominator
};

#endif // ESMC_FRACTION_H
