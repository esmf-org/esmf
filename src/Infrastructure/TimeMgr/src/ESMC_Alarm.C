// $Id: ESMC_Alarm.C,v 1.3 2003/02/11 19:03:33 eschwab Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMC Alarm method code (body) file

//-------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ {\tt Alarm} methods declared
// in the companion file {\tt ESMC_Alarm.h)
//
//-------------------------------------------------------------------------
//
 // insert any higher level, 3rd party or system includes here

 // associated class definition file
 #include <ESMC_Alarm.h>

//-------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_Alarm.C,v 1.3 2003/02/11 19:03:33 eschwab Exp $";
//-------------------------------------------------------------------------

//
//-------------------------------------------------------------------------
//-------------------------------------------------------------------------
//
// This section includes all the Alarm routines
//
//

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AlarmInit - initializes a Alarm object
//
// !INTERFACE:
      int ESMC_Alarm::ESMC_AlarmInit(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_TimeInterval *RingInterval,  // in
      ESMC_Time         *RingTime,      // in
      ESMC_Time         *StopTime,      // in
      bool               Enabled) {     // in
//
// !DESCRIPTION:
//      ESMF routine which only initializes {\tt Alarm} values; it does not
//      allocate any resources.  
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//
    return(ESMF_SUCCESS);

 } // end ESMC_AlarmInit

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AlarmCheckRingTime - check if time to ring
//
// !INTERFACE:
      bool ESMC_Alarm::ESMC_AlarmCheckRingTime(
//
// !RETURN VALUE:
//    bool is ringing or not
//
// !ARGUMENTS:
      ESMC_Time *CurrTime,  // in - current time to check
      bool positive,    // in - postive or negative ring time crossing trigger
      int  rc) {        // out - error return code
//
// !DESCRIPTION:
//    checks if its time to ring based on current time crossing the ring
//    time in either the positive or negative direction.
//
//EOP
// !REQUIREMENTS:  TMG4.4, 4.6

//
//  code goes here
//
    return(false);

 } // end ESMC_AlarmCheckRingTime

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BaseValidate - internal consistency check for an Alarm
//
// !INTERFACE:
      int ESMC_Alarm::ESMC_BaseValidate(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {    // in - validate options
//
// !DESCRIPTION:
//      Validates that a {\tt Alarm} is internally consistent.
//      Returns error code if problems are found.  {\tt ESMC\_Base}
//      class method.
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

//
//  code goes here
//
    return(ESMF_SUCCESS);

 } // end ESMC_BaseValidate


//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BasePrint - print contents of an Alarm
//
// !INTERFACE:
      int ESMC_Alarm::ESMC_BasePrint(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_TimeInterval *RingInterval,
      ESMC_Time         *RingTime,
      ESMC_Time         *PrevRingTime,
      ESMC_Time         *StopTime,
      bool              *Ringing,
      bool              *Enabled,
      int               *ID) const {

//
// !DESCRIPTION:
//      Print information about an {\tt Alarm}. For persistence/checkpointing
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//
//  code goes here
//
    return(ESMF_SUCCESS);

 } // end ESMC_BasePrint

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BasePrint - print contents of an Alarm
//
// !INTERFACE:
      int ESMC_Alarm::ESMC_BasePrint(void) const {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      Print information about an {\tt Alarm}.  For testing/debugging
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//
//  code goes here
//
    return(ESMF_SUCCESS);

 } // end ESMC_BasePrint

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Alarm - native C++ constructor
//
// !INTERFACE:
      ESMC_Alarm::ESMC_Alarm(void) {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      Calls standard ESMF deep or shallow methods for initialization
//      with default or passed-in values
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//
//  code goes here
//

 } // end ESMC_Alarm

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ~ESMC_Alarm - native C++ destructor
//
// !INTERFACE:
      ESMC_Alarm::~ESMC_Alarm(void) {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      Calls standard ESMF deep or shallow methods for destruction
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//
//  code goes here
//

 } // end ~ESMC_Alarm
