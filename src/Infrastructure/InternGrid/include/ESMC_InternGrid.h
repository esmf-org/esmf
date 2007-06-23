// $Id: ESMC_InternGrid.h,v 1.2 2007/06/23 04:37:05 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// ESMC IGrid include file for C++

// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMC_IGrid_H
#define ESMC_IGrid_H

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements constants and macros for the IGrid...
//
// 
//
// (all lines below between the !BOP and !EOP markers will be included in 
//  the automated document processing.)
//
//-----------------------------------------------------------------------------
//
#include "ESMC_Base.h"


class ESMC_IGrid;

// 
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section contains the code to store a pointer to
//  the fortran derived type for a IGrid.
//
//

class ESMC_IGrid : public ESMC_Base {
    private:
     void *igrid;   // pointer to fortran derived type

    public:
     // Add code here.
      ESMC_IGrid();
      ~ESMC_IGrid();
};


// 
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
//
//

extern "C" {

}

#endif  // ESMC_IGrid_H
