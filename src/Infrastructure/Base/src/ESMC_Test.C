// $Id: ESMC_Test.C,v 1.2 2003/02/28 18:00:20 eschwab Exp $
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
 #include <iostream.h>
 #include <ESMC.h>

 // associated class definition file
 #include <ESMC_Test.h>

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_Test.C,v 1.2 2003/02/28 18:00:20 eschwab Exp $";
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
      int condition,
      char *failMsg,
      int *result,
      char *file,
      int line) {
// 
// !DESCRIPTION:
//    Prints PASS/FAIL based on passed-in condition.  If FAIL, prints
//    optional failure message and increments failure result counter
//
//EOP
// !REQUIREMENTS:  AAAn.n.n

 if (result == 0 || failMsg == 0 || file == 0) {
   cout << "FAIL " << __FILE__ << ", line " << __LINE__ <<
           ", null pointer(s) passed in" << endl;
   return(ESMF_FAILURE);
 }

 if (condition) {
   cout << "PASS " << file << ", line " << line << endl;
 }
 else {
   cout << "FAIL " << file << ", line " << line << ", " << failMsg << endl;
   (*result)++; // count total failures; 0 = all pass
 }
 return(ESMF_SUCCESS);

} // end ESMC_Test
