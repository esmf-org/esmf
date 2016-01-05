// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2016, University Corporation for Atmospheric Research,    
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// ESMCI Test method implementation (body) file
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//  
// !DESCRIPTION:
//  
// The code in this file implements the C++ Test methods declared
// in the companion file ESMCI_Test.h
//
//-----------------------------------------------------------------------------
//

// associated header file
#include "ESMC_Test.h"

// associated ESMCI class definition file
#include "ESMCI_Test.h"

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------


extern "C" {
  //-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Test - Prints whether a test passed or failed
//
// !INTERFACE:   
int ESMC_Test(
//
// !RETURN VALUE:
//    ESMF_SUCCESS or ESMF_FAILURE
//
// !ARGUMENTS:
  int condition,    // in - the test pass/fail condition
  const char *name, // in - the test name
  const char *failMsg, // in - optional message printed on test failure
  int *result,      // in/out - cumulative failure count
  const char *file, // in - test filename
  int line,         // in - test line number in test filename
  int only) {       // in - if set to 0, print on stderr also
// 
// !DESCRIPTION:
//    Prints PASS/FAIL based on passed-in condition.  If FAIL, prints
//    optional failure message and increments failure result counter.
//    If {\tt only} is zero, also print same message to stderr as well
//    as the normal output on stdout.  The default for {\tt only} is 1.
//
//EOP
//-----------------------------------------------------------------------------
  return ESMCI::Test(condition, name, failMsg, result, file, line, only);
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TestEnd - Print a standard end test message
//
// !INTERFACE:   
int ESMC_TestEnd(
//
// !RETURN VALUE:
//    ESMF_SUCCESS or ESMF_FAILURE
//
// !ARGUMENTS:
  const char *file, // in - test filename
  int line,         // in - test line number in test filename
  int only) {       // in - if set to 0, print on stderr also
// 
// !DESCRIPTION:
//    Prints a standard exit message.
//    If {\tt only} is zero, also print same message to stderr as well
//    as the normal output on stdout.  The default for {\tt only} is 1.
//
//EOP
//-----------------------------------------------------------------------------
  return ESMCI::TestEnd(file, line, only);
}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TestStart - Initialize the framework, print a standard msg
//
// !INTERFACE:   
int ESMC_TestStart(
//
// !RETURN VALUE:
//    ESMF_SUCCESS or ESMF_FAILURE
//
// !ARGUMENTS:
  const char *file, // in - test filename
  int line,         // in - test line number in test filename
  int only) {       // in - if set to 0, print on stderr also
// 
// !DESCRIPTION:
//    Initializes the framework, prints out the standard messages needed
//    by the testing scripts.
//    If {\tt only} is zero, also print same message to stderr as well
//    as the normal output on stdout.  The default for {\tt only} is 1.
//
//EOP
//-----------------------------------------------------------------------------
  return ESMCI::TestStart(file, line, only);
}
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOPI
// !IROUTINE:  ESMC_fabs - absolute value of a float
//
// !INTERFACE:   
float ESMC_fabs(
//
// !RETURN VALUE:
//    absolute value
//
// !ARGUMENTS:
  float val) { 
// 
// !DESCRIPTION:
//    Returns the absolute value of a floating point value
//
//EOPI
//-----------------------------------------------------------------------------

  float negone = -1;

  if (val < 0) return val*negone;
  else return val;
}
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//BOPI
// !IROUTINE:  ESMC_dabs - absolute value of a double
//
// !INTERFACE:   
double ESMC_dabs(
//
// !RETURN VALUE:
//    absolute value
//
// !ARGUMENTS:
  double val) { 
// 
// !DESCRIPTION:
//    Returns the absolute value of a double value
//
//EOPI
//-----------------------------------------------------------------------------

  double negone = -1;

  if (val < 0) return val*negone;
  else return val;
}
//-----------------------------------------------------------------------------

}; // extern "C"
