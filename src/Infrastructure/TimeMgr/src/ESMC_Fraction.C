// $Id: ESMC_Fraction.C,v 1.4 2004/05/25 21:11:37 eschwab Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the GPL.
//
// ESMC Fraction method code (body) file
//
//-------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ {\tt ESMC\_Fraction} methods
// declared in the companion file {\tt ESMC_Fraction.h}
//
//-------------------------------------------------------------------------
//
 #define ESMC_FILENAME "ESMC_Fraction.C"

 // higher level, 3rd party or system includes

 // associated class definition file
 #include <ESMC_Fraction.h>

//-------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_Fraction.C,v 1.4 2004/05/25 21:11:37 eschwab Exp $";
//-------------------------------------------------------------------------

//
//-------------------------------------------------------------------------
//-------------------------------------------------------------------------
//
// This section includes all the ESMC_Fraction routines
//
//
//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Fraction - native default C++ constructor
//
// !INTERFACE:
      ESMC_Fraction::ESMC_Fraction(void) {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      Initializes a {\tt ESMC\_Fraction} with defaults
//
//EOP
// !REQUIREMENTS:  

   n = 0;
   d = 1;

 }  // end ESMC_Fraction

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ~ESMC_Fraction - native default C++ destructor
//
// !INTERFACE:
      ESMC_Fraction::~ESMC_Fraction(void) {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      Default {\tt ESMC\_Fraction} destructor
//
//EOP
// !REQUIREMENTS:  

 }  // end ~ESMC_Fraction
