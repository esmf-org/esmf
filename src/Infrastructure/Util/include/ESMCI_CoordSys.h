// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2018, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
// ESMF Util C++ declaration include file
//
//-----------------------------------------------------------------------------
//

#ifndef ESMCI_COORDSYS_H
#define ESMCI_COORDSYS_H

// Definition of CoordSys flag and constants
#include "ESMC_Util.h"

// Constants for converting between radians and degrees
// (These need to match the ones in ESMF_UtilTypes.F90) 
extern const double ESMC_CoordSys_Deg2Rad;
extern const double ESMC_CoordSys_Rad2Deg;

// Prototypes of CoordSys methods
int ESMCI_CoordSys_CalcCartDim(
                              ESMC_CoordSys_Flag cs,   // Input coordSys
                              int in_dim,        // Dimension to convert to Cart dimension
                              int *cart_dim        // Corresponding Cart dimension
                              );


template <class TYPE>
int ESMCI_CoordSys_ConvertToCart(
                                ESMC_CoordSys_Flag cs,   // Input coordSys
                                int in_dim,        // Dimension of input coordinates
                                // output dim can be found by ESMC_CoordSys_CalcCartDim 
                                TYPE *in_coord,  // Input coordinates 
                                TYPE *cart_coord  // Output cartesian coordinates 
                                );


#endif  // ESMCI_COORDSYS_H
