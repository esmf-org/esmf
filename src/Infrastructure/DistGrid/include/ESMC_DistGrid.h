// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2020, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// This file is part of the pure C public ESMC API
//-----------------------------------------------------------------------------

//-------------------------------------------------------------------------
// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMC_DistGrid_H
#define ESMC_DistGrid_H

//-----------------------------------------------------------------------------
// ESMC_DistGrid - Public C interface to the ESMF DistGrid class
//
// The code in this file defines the public C DistGrid class and declares method
// signatures (prototypes).  The companion file {\tt ESMC\_DistGrid.C} contains
// the definitions (full code bodies) for the DistGrid methods.
//-----------------------------------------------------------------------------


#include "ESMC_Interface.h"

#ifdef __cplusplus
extern "C" {
#endif

// Class declaration type
typedef struct{
  void *ptr;
}ESMC_DistGrid;

// Class API

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_DistGridCreate - Create a DistGrid
//
// !INTERFACE:
ESMC_DistGrid ESMC_DistGridCreate(
  ESMC_InterArrayInt minIndexInterfaceArg,  // in
  ESMC_InterArrayInt maxIndexInterfaceArg,  // in
  int *rc                                   // out
);
// !RETURN VALUE:
//  Newly created ESMC_DistGrid object.
//
// !DESCRIPTION:
//  Create an {\tt ESMC\_DistGrid} from a single logically rectangular (LR) 
//  tile with default decomposition. The default decomposition is 
//  {\tt deCount}$ \times 1 \times ... \times 1$, where {\tt deCount} is the
//  number of DEs in a default DELayout, equal to {\tt petCount}. This means
//  that the default decomposition will be into as many DEs as there are PETs,
//  with 1 DE per PET.
//
//  The arguments are:
//  \begin{description}
//  \item[minIndex]
//    Global coordinate tuple of the lower corner of the tile.
//  \item[maxIndex]
//    Global coordinate tuple of the upper corner of the tile.
//  \item[{[rc]}]
//    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_DistGridDestroy - Destroy a DistGrid
//
// !INTERFACE:
int ESMC_DistGridDestroy(
  ESMC_DistGrid *distgrid         // inout
);
// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//
//  Destroy an {\tt ESMC\_DistGrid} object.
//
//  The arguments are:
//  \begin{description}
//  \item[distgrid] 
//    {\tt ESMC\_DistGrid} object to be destroyed.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_DistGridPrint - Print a DistGrid
//
// !INTERFACE:
int ESMC_DistGridPrint(
  ESMC_DistGrid distgrid          // in
);
// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//
//  Print internal information of the specified {\tt ESMC\_DistGrid} object.
//
//  The arguments are:
//  \begin{description}
//  \item[distgrid] 
//    {\tt ESMC\_DistGrid} object to be destroyed.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

#ifdef __cplusplus
} // extern "C"
#endif

#endif  // ESMC_DistGrid_H
