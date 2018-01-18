// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2018, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// ESMCI Test include file for C++
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// prevent this file from being read more than once
#ifndef ESMCI_Test_H
#define ESMCI_Test_H

//-----------------------------------------------------------------------------
//BOPI
// !MODULE:  ESMCI_Test - Contains general utilities to support testing
//
// !DESCRIPTION:
//
// The code in this file defines the C++ Test members and declares method
// signatures (prototypes).  The companion file ESMCI_Test.C contains
// the definitions (full code bodies) for the Test methods.
//
//EOPI
//-----------------------------------------------------------------------------
//
// !PUBLIC MEMBER FUNCTIONS:

namespace ESMCI {

// TODO: These need an optional Log argument.
int TestStart(const char *file, int line, int only = 1);
int TestEnd(const char *file, int line, int only = 1);

int Test(int condition, const char *name, const char *failMsg, int *result, 
  const char *file, int line, int only = 1);

bool TestNumPETs(int petCount, char *file, int line, int only = 1);
bool TestMinPETs(int petCount, char *file, int line, int only = 1);
bool TestMaxPETs(int petCount, char *file, int line, int only = 1);

} // namespace ESMCI

#endif // ESMCI_Test_H
