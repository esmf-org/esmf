// $Id: ESMC_Test.C,v 1.2 2005/01/10 23:57:27 nscollins Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research,    
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMC Test method implementation (body) file

//-----------------------------------------------------------------------------
//  
// !DESCRIPTION:
//  
// The code in this file implements the C++ Test methods declared
// in the companion file ESMC_Test.h
//
//-----------------------------------------------------------------------------
//
 // insert any higher level, 3rd party or system includes here
 #include <ESMC_Start.h>
 #include <stdio.h>

 // associated class definition file
 #include <ESMC_Test.h>

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_Test.C,v 1.2 2005/01/10 23:57:27 nscollins Exp $";
//-----------------------------------------------------------------------------

//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the Test routines
//
//

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
      int condition,  // in - the test pass/fail condition
      char *name,     // in - the test name
      char *failMsg,  // in - optional message printed on test failure
      int *result,    // in/out - cumulative failure count
      char *file,     // in - test filename
      int line,       // in - test line number in test filename
      int only) {     // in - if set to 0, print on stderr also
// 
// !DESCRIPTION:
//    Prints PASS/FAIL based on passed-in condition.  If FAIL, prints
//    optional failure message and increments failure result counter.
//    If {\tt only} is zero, also print same message to stderr as well
//    as the normal output on stdout.  The default for {\tt only} is 1.
//
//EOP
// !REQUIREMENTS:  AAAn.n.n

 if (name == 0 || result == 0 || failMsg == 0 || file == 0) {
   printf("FAIL %s, line %d, null pointer(s) passed in\n", __FILE__, __LINE__);
   return(ESMF_FAILURE);
 }

 if (condition) {
   printf("PASS %s, %s, line %d\n", name, file, line);
   if (!only)
      fprintf(stderr, "PASS %s, %s, line %d\n", name, file, line);
 }
 else {
   printf("FAIL %s, %s, line %d, %s\n", name, file, line, failMsg);
   if (!only)
       fprintf(stderr, "FAIL %s, %s, line %d, %s\n", name, file, line, failMsg);
   (*result)++; // count total failures; 0 = all pass
 }
 return(ESMF_SUCCESS);

} // end ESMC_Test
