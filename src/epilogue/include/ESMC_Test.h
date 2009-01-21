// $Id: ESMC_Test.h,v 1.3.2.2 2009/01/21 21:25:25 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// ESMF Test C++ declaration include file
//
//-----------------------------------------------------------------------------
//
 // these lines prevent this file from being read more than once if it
 // ends up being included multiple times

 #ifndef ESMC_Test_H
 #define ESMC_Test_H

//-----------------------------------------------------------------------------

 // Put any constants or macros which apply to the whole component in this file.
 // Anything public or esmf-wide should be up higher at the top level
 // include files.

//-----------------------------------------------------------------------------
//BOP
// !MODULE:  ESMC_Test - Contains general utilities to support testing
//
// !DESCRIPTION:
//
// The code in this file defines the C++ Test members and declares method
// signatures (prototypes).  The companion file ESMC_Test.C contains
// the definitions (full code bodies) for the Test methods.
//
// 
//
//-----------------------------------------------------------------------------
//
// !PUBLIC MEMBER FUNCTIONS:

// TODO: These need an optional Log argument.
int ESMC_TestStart(char *file, int line, int only = 1);
int ESMC_TestEnd(int result, char *file, int line, int only = 1);

int ESMC_Test(int condition, char *name, char *failMsg, int *result, 
                                         char *file, int line, int only = 1);

bool ESMC_TestNumPETs(int petCount, char *file, int line, int only = 1);
bool ESMC_TestMinPETs(int petCount, char *file, int line, int only = 1);
bool ESMC_TestMaxPETs(int petCount, char *file, int line, int only = 1);

//EOP

 #endif // ESMC_Test_H
