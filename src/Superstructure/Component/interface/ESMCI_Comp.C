// $Id: ESMCI_Comp.C,v 1.1 2008/08/25 22:03:56 theurich Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2008, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMCI_Comp.C"
//==============================================================================
//
// ESMC Component method implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the ESMCI::Comp interface to the Component
// class implemented in Fortran.
//
//-----------------------------------------------------------------------------

// include associated header file
#include "ESMCI_Comp.h"

// initilalize global character constants to be used by user code written in C
const char *ESMC_SetInit         = "ESMF_Initialize";
const char *ESMC_SetRun          = "ESMF_Run";
const char *ESMC_SetFinal        = "ESMF_Finalize";
const char *ESMC_SetWriteRestart = "ESMF_WriteRestart";
const char *ESMC_SetReadRestart  = "ESMF_ReadRestart";

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id: ESMCI_Comp.C,v 1.1 2008/08/25 22:03:56 theurich Exp $";
//-----------------------------------------------------------------------------
