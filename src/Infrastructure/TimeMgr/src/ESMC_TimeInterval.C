// $Id: ESMC_TimeInterval.C,v 1.2 2002/10/15 03:26:28 eschwab Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the GPL.
//
// ESMC TimeInterval method code (body) file
//
//-------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ TimeInterval methods declared
// in the companion file ESMC_TimeInterval.h
//
//-------------------------------------------------------------------------

 // higher level, 3rd party or system includes
#include <ESMC_Types.h>
 #include<ESMC_Util.h>

 // associated class definition file
 #include <ESMC_TimeInterval.h>

//-------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_TimeInterval.C,v 1.2 2002/10/15 03:26:28 eschwab Exp $";
//-------------------------------------------------------------------------

//
//-------------------------------------------------------------------------
//-------------------------------------------------------------------------
//
// This section includes all the ESMC_TimeInterval routines
//
//

//-------------------------------------------------------------------------
// Class ESMC_TimeInterval Methods
//-------------------------------------------------------------------------

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeIntvInit - shallow class initializer 1
//
// !INTERFACE:
      int ESMC_TimeInterval::ESMC_TimeIntvInit(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int64 S,              // in - integer seconds
      int32 Sn,             // in - fractional seconds, numerator
      int32 Sd ) {          // in - fractional seconds, denominator
//
// !DESCRIPTION:
//      Initialzes a TimeInterval with given values
//
//EOP
// !REQUIREMENTS:

	ESMC_Time::ESMC_TimeInit(S, Sn, Sd);

	return(ESMC_SUCCESS);

}  // end ESMC_TimeIntvInit

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeInterval - native default C++ constructor
//
// !INTERFACE:
      ESMC_TimeInterval::ESMC_TimeInterval(void) {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      Initializes a ESMC_TimeInterval with defaults via ESMC_Time base class
//
//EOP
// !REQUIREMENTS:

	// uses default base class constructor

} // end ESMC_TimeInterval

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeInterval - native C++ constructor
//
// !INTERFACE:
     ESMC_TimeInterval::ESMC_TimeInterval(
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
      int64 S,              // in - integer seconds
      int32 Sn,             // in - fractional seconds, numerator
      int32 Sd) :           // in - fractional seconds, denominator
    ESMC_Time(S, Sn, Sd) {  // invoke ESMC_Time base class constructor
//
// !DESCRIPTION:
//      Initializes a ESMC_TimeInterval via ESMC_Time base class
//
//EOP
// !REQUIREMENTS:

} // end ESMC_TimeInterval

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ~ESMC_TimeInterval - native default C++ destructor
//
// !INTERFACE:
      ESMC_TimeInterval::~ESMC_TimeInterval(void) {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      Default ESMC_TimeInterval destructor
//
//EOP
// !REQUIREMENTS:

}  // end ~ESMC_TimeInterval
