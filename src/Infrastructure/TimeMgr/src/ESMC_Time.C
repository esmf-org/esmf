// $Id: ESMC_Time.C,v 1.5 2003/02/11 18:33:57 eschwab Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the GPL.
//
// ESMC Time method code (body) file
//
//-------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ {\tt Time} methods declared
// in the companion file {\tt ESMC_Time.h}
//
//-------------------------------------------------------------------------

 // higher level, 3rd party or system includes

 // associated class definition file
 #include <ESMC_Time.h>

//-------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_Time.C,v 1.5 2003/02/11 18:33:57 eschwab Exp $";
//-------------------------------------------------------------------------

//
//-------------------------------------------------------------------------
//-------------------------------------------------------------------------
//
// This section includes all the ESMC_Time routines
//
//

#if 0
//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeInit - shallow class initializer 1
//
// !INTERFACE:
      int ESMC_Time::ESMC_TimeInit(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMF_IKIND_I8 S,              // in - integer seconds
      int Sn,             // in - fractional seconds, numerator
      int Sd,             // in - fractional seconds, denominator
      ESMC_Calendar *Cal,   // in - associated calendar
      int Tz) {             // in - timezone
//
// !DESCRIPTION:
//      Initialzes a {\tt Time} with given values
//
//EOP
// !REQUIREMENTS:  

    // use base class Init()
    if (ESMC_BaseTime::ESMC_BaseTimeInit(S, Sn, Sd) == ESMF_SUCCESS)
    {
        this->Calendar = Cal;
        this->Timezone = Tz;

        return(ESMF_SUCCESS);
    }
    else return(ESMF_FAILURE);

 }  // end ESMC_TimeInit
#endif

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeInit - shallow class initializer 2
//
// !INTERFACE:
      int ESMC_Time::ESMC_TimeInit(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_Calendar *Cal,      // in - associated calendar
      const char *TimeList,    // in - initializer specifier string
      ...) {                   // in - specifier values (variable args)
//
// !DESCRIPTION:
//      Initialzes a {\tt Time} with values given in variable arg list
//
//EOP
// !REQUIREMENTS:  

    return(ESMF_SUCCESS);

 }  // end ESMC_TimeInit

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_TimeIsSameCal - Compares 2 ESMC_Time's Calendar types
//
// !INTERFACE:
      bool ESMC_Time::ESMC_TimeIsSameCal(
//
// !RETURN VALUE:
//    bool true same calendars, false different calendars
//
// !ARGUMENTS:
      ESMC_Time *Time) { // in - Time to compare Calendar types against
//
// !DESCRIPTION:
//      Compares given {\tt Time}'s {\tt Calendar} type with this {\tt Time}'s
//      {\tt Calendar} type
//
//EOP
// !REQUIREMENTS:  

    if (Calendar != 0)
    {
        return(this->Calendar->Type == Time->Calendar->Type);
    }
    else return(false);

 }  // end ESMC_TimeIsSameCal

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BasePrint - return Time state
//
// !INTERFACE:
      int ESMC_Time::ESMC_BasePrint(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMF_IKIND_I8 *S,              // out - integer seconds
      int *Sn,             // out - fractional seconds, numerator
      int *Sd) const {     // out - fractional seconds, denominator
//
// !DESCRIPTION:
//      return {\tt Time} state for persistence/checkpointing
//
//EOP
// !REQUIREMENTS:  

    // use base class Print() first
    return(ESMC_BaseTime::ESMC_BasePrint(S, Sn, Sd));

 }  // end ESMC_TimePrint

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BasePrint - print Time state
//
// !INTERFACE:
      int ESMC_Time::ESMC_BasePrint(void) const {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      print {\tt Time} state for testing/debugging
//
//EOP
// !REQUIREMENTS:  

    // use base class Print
    ESMC_BaseTime::ESMC_BasePrint();

    return(ESMF_SUCCESS);

 }  // end ESMC_TimePrint

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Time - native default C++ constructor
//
// !INTERFACE:
      ESMC_Time::ESMC_Time(void) {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      Initializes a {\tt ESMC\_Time} with defaults via {\tt ESMC\_TimeInit}
//
//EOP
// !REQUIREMENTS:  

   // ESMC_TimeInit(0, 0, 1, 0, 0); // use variable args Init TODO

 }  // end ESMC_Time

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Time - native C++ constructor
//
// !INTERFACE:
      ESMC_Time::ESMC_Time(
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
      ESMF_IKIND_I8 S,              // in - integer seconds
      int Sn,             // in - fractional seconds, numerator
      int Sd,             // in - fractional seconds, denominator
      ESMC_Calendar *Cal,   // in - associated calendar
      int Tz) {             // in - timezone
//
// !DESCRIPTION:
//      Initializes a {\tt ESMC\_Time} via {\tt ESMC\_TimeInit}
//
//EOP
// !REQUIREMENTS:  

    // ESMC_TimeInit(S, Sn, Sd, Cal, Tz); // use variable args Init TODO

 }  // end ESMC_Time

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ~ESMC_Time - native default C++ destructor
//
// !INTERFACE:
      ESMC_Time::~ESMC_Time(void) {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      Default {\tt ESMC\_Time} destructor
//
//EOP
// !REQUIREMENTS:  

 }  // end ~ESMC_Time
