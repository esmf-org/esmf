// $Id: ESMC_Test.h,v 1.9.4.1 2010/02/05 20:14:07 svasquez Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// ESMC Test include file for C
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// prevent this file from being read more than once
#ifndef ESMC_Test_H
#define ESMC_Test_H

//-----------------------------------------------------------------------------
//BOPI
// !MODULE:  ESMC_Test - Contains general utilities to support testing
//
// !DESCRIPTION:
//
// The code in this file defines the C++ Test members and declares method
// signatures (prototypes).  The companion file ESMC_Test.C contains
// the definitions (full code bodies) for the Test methods.
//
//EOPI
//-----------------------------------------------------------------------------
//
// !PUBLIC MEMBER FUNCTIONS:

#ifdef __cplusplus
extern "C" {
#endif

// TODO: These need an optional Log argument.
int ESMC_TestStart(char *file, int line, int only);
int ESMC_TestEnd(int result, char *file, int line, int only);

int ESMC_Test(int condition, char *name, char *failMsg, int *result,
  char *file, int line, int only);

#ifdef __cplusplus
}  // extern "C"
#endif

#endif // ESMC_Test_H
