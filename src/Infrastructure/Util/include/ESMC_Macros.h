// $Id: ESMC_Macros.h,v 1.21.2.10 2009/01/21 21:25:24 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// Public prototypes and constants for the ESMF C++ interface

#ifndef ESMC_MACROS_H
#define ESMC_MACROS_H

//BOP
// -------------------------------------------------------------------------
// 
//  !DESCRIPTION:
// 
//  Utility macros and constants usable ESMF-wide from the C++ interface.
// 
//  (all lines below between the !BOP and !EOP markers will be included in
//   the automated document processing.)
// 
// -------------------------------------------------------------------------
//EOP

// These MUST stay in sync with the constants defined in ESMF_Base.F90
// in the Infrastructure/Base/interface directory.

#define ESMF_SUCCESS  0
#define ESMF_FAILURE -1

#define ESMF_MAXSTR 128
#define ESMF_MAXDIM 7
#define ESMF_MAXDECOMPDIM 3
#define ESMF_MAXIGRIDDIM 3
// TODO:FIELDINTEGRATION: Remove the MAXIGRIDDIM and increase the 
// number of MAXGRIDDIM to 7.
#define ESMF_MAXGRIDDIM 3

#if 0
// this is now in ESMF_Macros.inc
#define ESMF_SRCLINE __FILE__, __LINE__
#endif


#define ESMF_MAJOR_VERSION 3
#define ESMF_MINOR_VERSION 1
#define ESMF_REVISION      0
#define ESMF_PATCHLEVEL    2

#define ESMF_VERSION_STRING "3.1.0rp2"

#endif   // ESMC_MACROS_H

