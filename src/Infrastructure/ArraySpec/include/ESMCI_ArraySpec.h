// $Id: ESMCI_ArraySpec.h,v 1.1.2.7 2009/01/21 21:25:19 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//-------------------------------------------------------------------------
// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMCI_ArraySpec_H
#define ESMCI_ArraySpec_H

//-----------------------------------------------------------------------------
//BOPI
// !CLASS:  ESMCI::ArraySpec - rank and typekind of an array
//
// !DESCRIPTION:
//
// The code in this file defines the internal C++ {\tt ArraySpec} class.
// The companion file {\tt ESMCI\_ArraySpec.C} contains the full code (bodies)
// for the {\tt ArraySpec} methods.
//
//EOPI
//-----------------------------------------------------------------------------

#include <cstdlib>
#include "ESMC_Util.h"

namespace ESMCI {

// classes and structs
class ArraySpec;

// class definition
class ArraySpec {   // NOT inherited from Base class
  private:
    // Allocate enough memory to store members in the Fortran side.
    // Adjust if members are added, rounding up to multiples of 64)
    char shallowMem[192];

  public:
    int set(int rank, ESMC_TypeKind typekind);
    int getRank(int *rc=NULL);
    ESMC_TypeKind getTypeKind(int *rc=NULL);

};

} // namespace ESMCI

#endif  // ESMCI_ArraySpec_H
