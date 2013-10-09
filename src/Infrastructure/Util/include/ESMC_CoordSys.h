// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2013, University Corporation for Atmospheric Research,
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

#ifndef ESMC_COORDSYS_H
#define ESMC_COORDSYS_H


enum ESMC_CoordSys_Flag {ESMC_COORDSYS_INVALID=-2,
                    ESMC_COORDSYS_UNINIT,
                    ESMC_COORDSYS_CART,
                    ESMC_COORDSYS_SPH_DEG,
                    ESMC_COORDSYS_SPH_RAD};

// Constants for converting between radians and degrees
// (These need to match the ones in ESMF_UtilTypes.F90) 

const double ESMC_CoordSys_Deg2Rad=0.01745329251994; 
const double ESMC_CoordSys_Rad2Deg=57.2957795130931;


#endif  // ESMC_COORDSYS_H
