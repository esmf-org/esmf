// $Id: ESMC_Init.h,v 1.12 2007/04/12 23:09:57 theurich Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// ESMC Init include file for C
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements constants and macros for the C Init Code.
//
// !USES:
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
// prevent this file from being read more than once
#ifndef ESMC_Init_H
#define ESMC_Init_H


// prototypes for C routines
extern "C" {
  int ESMC_Initialize(void);
  int ESMC_Finalize(void);
};


#endif  // ESMC_Init_H
