// $Id: ESMC_TimeInstant.C,v 1.5 2002/10/15 03:26:28 eschwab Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the GPL.
//
// ESMC TimeInstant method code (body) file
//
//-------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ TimeInstant methods declared
// in the companion file ESMC_TimeInstant.h
//
//-------------------------------------------------------------------------

 // higher level, 3rd party or system includes
 #include<ESMC_Util.h>

 // associated class definition file
 #include <ESMC_TimeInstant.h>

//-------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_TimeInstant.C,v 1.5 2002/10/15 03:26:28 eschwab Exp $";
//-------------------------------------------------------------------------

//
//-------------------------------------------------------------------------
//-------------------------------------------------------------------------
//
// This section includes all the ESMC_TimeInstant routines
//
//

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeInstInit - shallow class initializer 1
//
// !INTERFACE:
      int ESMC_TimeInstant::ESMC_TimeInstInit(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int64 S,              // in - integer seconds
      int32 Sn,             // in - fractional seconds, numerator
      int32 Sd,             // in - fractional seconds, denominator
      ESMC_Calendar *Cal,   // in - associated calendar
      int Tz) {             // in - timezone
//
// !DESCRIPTION:
//      Initialzes a TimeInstant with given values
//
//EOP
// !REQUIREMENTS:  

    // use base class Init()
    if (ESMC_Time::ESMC_TimeInit(S, Sn, Sd) == ESMC_SUCCESS)
    {
        this->Calendar = Cal;
        this->Timezone = Tz;

        return(ESMC_SUCCESS);
    }
    else return(ESMC_FAILURE);

 }  // end ESMC_TimeInstInit

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeInstInit - shallow class initializer 2
//
// !INTERFACE:
      int ESMC_TimeInstant::ESMC_TimeInstInit(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *TimeList,    // in - initializer specifier string
      ...) {                   // in - specifier values (variable args)
//
// !DESCRIPTION:
//      Initialzes a TimeInstant with values given in variable arg list
//
//EOP
// !REQUIREMENTS:  

    return(ESMC_SUCCESS);

 }  // end ESMC_TimeInstInit

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeInstPrint - return TimeInstant state
//
// !INTERFACE:
      int ESMC_TimeInstant::ESMC_TimeInstPrint(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int64 *S,              // out - integer seconds
      int32 *Sn,             // out - fractional seconds, numerator
      int32 *Sd) const {     // out - fractional seconds, denominator
//
// !DESCRIPTION:
//      return TimeInstant state for persistence/checkpointing
//
//EOP
// !REQUIREMENTS:  

    // use base class Print() first
    return(ESMC_Time::ESMC_TimePrint(S, Sn, Sd));

 }  // end ESMC_TimeInstPrint

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeInstPrint - print TimeInstant state
//
// !INTERFACE:
      int ESMC_TimeInstant::ESMC_TimeInstPrint(void) const {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      print TimeInstant state for testing/debugging
//
//EOP
// !REQUIREMENTS:  

    // use base class Print
    ESMC_Time::ESMC_TimePrint();

    return(ESMC_SUCCESS);

 }  // end ESMC_TimeInstPrint

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeInstant - native default C++ constructor
//
// !INTERFACE:
      ESMC_TimeInstant::ESMC_TimeInstant(void) {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      Initializes a ESMC_TimeInstant with defaults via ESMC_TimeInstInit
//
//EOP
// !REQUIREMENTS:  

   ESMC_TimeInstInit(0, 0, 1, NULL, 0);

 }  // end ESMC_TimeInstant

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeInstant - native C++ constructor
//
// !INTERFACE:
      ESMC_TimeInstant::ESMC_TimeInstant(
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
      int64 S,              // in - integer seconds
      int32 Sn,             // in - fractional seconds, numerator
      int32 Sd,             // in - fractional seconds, denominator
      ESMC_Calendar *Cal,   // in - associated calendar
      int Tz) {             // in - timezone
//
// !DESCRIPTION:
//      Initializes a ESMC_TimeInstant via ESMC_TimeInstInit
//
//EOP
// !REQUIREMENTS:  

    ESMC_TimeInstInit(S, Sn, Sd, Cal, Tz);

 }  // end ESMC_TimeInstant

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ~ESMC_TimeInstant - native default C++ destructor
//
// !INTERFACE:
      ESMC_TimeInstant::~ESMC_TimeInstant(void) {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      Default ESMC_TimeInstant destructor
//
//EOP
// !REQUIREMENTS:  

 }  // end ~ESMC_TimeInstant
