// $Id: ESMC_Grid.h,v 1.9 2007/03/31 05:51:08 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// ESMC Grid include file for C++

// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMC_Grid_H
#define ESMC_Grid_H

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements constants and macros for the Grid...
//
// 
//
// (all lines below between the !BOP and !EOP markers will be included in 
//  the automated document processing.)
//
//-----------------------------------------------------------------------------
//
#include "ESMC_Base.h"


class ESMC_Grid;

// 
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section contains the code to store a pointer to
//  the fortran derived type for a Grid.
//
//

class ESMC_Grid : public ESMC_Base {
    private:
     void *grid;   // pointer to fortran derived type

    public:
     // Add code here.
      ESMC_Grid();
      ~ESMC_Grid();
};


// 
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
//
//

extern "C" {

}

#endif  // ESMC_Grid_H
