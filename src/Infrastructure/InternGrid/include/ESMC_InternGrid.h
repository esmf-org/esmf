// $Id: ESMC_InternGrid.h,v 1.1 2007/06/22 23:21:36 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// ESMC InternGrid include file for C++

// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMC_InternGrid_H
#define ESMC_InternGrid_H

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements constants and macros for the InternGrid...
//
// 
//
// (all lines below between the !BOP and !EOP markers will be included in 
//  the automated document processing.)
//
//-----------------------------------------------------------------------------
//
#include "ESMC_Base.h"


class ESMC_InternGrid;

// 
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section contains the code to store a pointer to
//  the fortran derived type for a InternGrid.
//
//

class ESMC_InternGrid : public ESMC_Base {
    private:
     void *interngrid;   // pointer to fortran derived type

    public:
     // Add code here.
      ESMC_InternGrid();
      ~ESMC_InternGrid();
};


// 
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
//
//

extern "C" {

}

#endif  // ESMC_InternGrid_H
