// $Id$
//
// Earth System Modeling Framework
// Copyright (c) 2002-2025, University Corporation for Atmospheric Research, 
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

#ifndef ESMCI_DynamicMask_H
#define ESMCI_DynamicMask_H

//-----------------------------------------------------------------------------
//BOPI
// !CLASS:  ESMCI::DynamicMask - rank and typekind of an array
//
// !DESCRIPTION:
//
// The code in this file defines the internal C++ {\tt DynamicMask} class.
// The companion file {\tt ESMCI\_DynamicMask.C} contains the full code (bodies)
// for the {\tt DynamicMask} methods.
//
//EOPI
//-----------------------------------------------------------------------------

#include <cstdlib>
#include "ESMC_Util.h"

namespace ESMCI {

// classes and structs
class DynamicMask;

// class definition
class DynamicMask {   // NOT inherited from Base class
  private:
    // Allocate enough memory to store members in the Fortran side.
    // Adjust if members are added, rounding up to multiples of 64 byte
    char shallowMem[192];

  public:
    int setR8R8R8(ESMC_PredefinedDynamicMask_Flag maskType, bool *handleAllElements, ESMC_R8 *dynamicSrcMaskValue, ESMC_R8 *dynamicDstMaskValue);

};

} // namespace ESMCI

#endif  // ESMCI_DynamicMask_H
