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

#ifndef ESMC_MACROS_H
#define ESMC_MACROS_H

// -------------------------------------------------------------------------
//BOPI
// 
//  !DESCRIPTION:
// 
//  Utility macros and constants usable ESMF-wide from the C++ interface.
// 
//  (all lines below between the !BOP and !EOP markers will be included in
//   the automated document processing.)
// 
//EOPI
// -------------------------------------------------------------------------

// These macros MUST stay in sync with the constants defined in
// src/Infrastructure/Util/src/ESMF_UtilTypes.F90

#define ESMF_SUCCESS  0
#define ESMF_FAILURE -1

#define ESMF_MAXSTR 256
#define ESMF_MAXDIM 7
#define ESMF_MAXIGRIDDIM 3
// TODO:FIELDINTEGRATION: Remove the MAXIGRIDDIM and increase the 
// number of MAXGRIDDIM to 7.
#define ESMF_MAXGRIDDIM 3


#define ESMF_VERSION_MAJOR        8
#define ESMF_VERSION_MINOR        1
#define ESMF_VERSION_REVISION     0
#define ESMF_VERSION_PATCHLEVEL   0
#define ESMF_VERSION_PUBLIC       'F'
#define ESMF_VERSION_BETASNAPSHOT 'T'

#define ESMF_VERSION_STRING "8.1.0 beta snapshot"

#endif   // ESMC_MACROS_H

