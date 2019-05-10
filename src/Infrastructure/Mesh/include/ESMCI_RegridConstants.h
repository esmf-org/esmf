// $Id$
// Earth System Modeling Framework
// Copyright 2002-2019, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMCI_RegridConstants_h
#define ESMCI_RegridConstants_h

#define ESMC_REGRID_STATUS_DST_MASKED 0
#define ESMC_REGRID_STATUS_SRC_MASKED 1
#define ESMC_REGRID_STATUS_OUTSIDE 2
#define ESMC_REGRID_STATUS_MAPPED 4
#define ESMC_REGRID_STATUS_EXTRAP_MAPPED 8

#define ESMC_EXTRAPMETHOD_NONE 0
#define ESMC_EXTRAPMETHOD_NEAREST_STOD 1
#define ESMC_EXTRAPMETHOD_NEAREST_IDAVG 2
#define ESMC_EXTRAPMETHOD_CREEP 3

#endif