// $Id: ESMC_TimeInstant.C,v 1.4 2002/10/14 16:19:40 eschwab Exp $
//
// ESMC TimeInstant method code (body) file
//
// < Something here from legal about the status of the code, like:
//  This code developed by NASA/NCAR/ESMC whatever, and is covered by
//  the terms of the GNU public license.  See license file for more details. >
//

//-------------------------------------------------------------------------
//
// !PURPOSE:
//
// The code in this file implements the C++ TimeInstant methods defined
// in the companion file ESMC_TimeInstant.h
//
//-------------------------------------------------------------------------

 // higher level or system includes
 #include<ESMC_Util.h>

 // associated class definition file
 #include <ESMC_TimeInstant.h>

//-------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_TimeInstant.C,v 1.4 2002/10/14 16:19:40 eschwab Exp $";
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
// !IROUTINE:  ESMC_TimeInstantInit - shallow class initializer
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
    if (ESMC_Time::Init(S, Sn, Sd) == ESMC_SUCCESS)
    {
        this->Calendar = Cal;
        this->Timezone = Tz;

        return(ESMC_SUCCESS);
    }
    else return(ESMC_FAILURE);

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
      none
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
