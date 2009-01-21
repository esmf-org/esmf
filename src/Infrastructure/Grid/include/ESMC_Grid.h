// $Id: ESMC_Grid.h,v 1.11.2.2 2009/01/21 21:25:21 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// ESMC Grid class public include file
//
// (all lines below between the !BOP and !EOP markers will be included in 
//  the automated document processing.)
//-----------------------------------------------------------------------------
//
 // these lines prevent this file from being read more than once if it
 // ends up being included multiple times

#ifndef ESMC_Grid_H
#define ESMC_Grid_H

//-----------------------------------------------------------------------------
//BOP
// !CLASS:  ESMC_Grid - Public C interface to the Grid object
//
// !DESCRIPTION:
//
// The code in this file defines the C public Grid class and declares method 
// signatures (prototypes).  The companion file ESMC\_Grid.C contains
// the definitions (full code bodies) for the Grid methods.
//
//-----------------------------------------------------------------------------
// 
// !USES:
#include "ESMCI_Grid.h"


extern "C" {

// class declaration type
typedef struct {
      ESMCI::Grid *grid;
//
//EOP
//-----------------------------------------------------------------------------

} ESMC_Grid ; // end class ESMC_Grid


// prototypes for the ESMC_Grid API


}; // end extern "C"


#endif  // ESMC_Grid_H

