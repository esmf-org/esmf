// $Id: ESMC_Clock.C,v 1.3 2003/02/11 19:03:34 eschwab Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMC Clock method code (body) file

//-------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ {\tt Clock} methods declared
// in the companion file {\tt ESMC_Clock.h}
//
//-------------------------------------------------------------------------
//
 // higher level, 3rd party or system includes here

 // associated class definition file
 #include <ESMC_Clock.h>

//-------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_Clock.C,v 1.3 2003/02/11 19:03:34 eschwab Exp $";
//-------------------------------------------------------------------------

//
//-------------------------------------------------------------------------
//-------------------------------------------------------------------------
//
// This section includes all the Clock routines
//
//

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ClockInit - initializes a Clock object
//
// !INTERFACE:
      int ESMC_Clock::ESMC_ClockInit(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_TimeInterval *TimeStep,    // in
      ESMC_Time         *StartTime,   // in
      ESMC_Time         *StopTime,    // in
      ESMC_Time         *RefTime) {   // in

// !DESCRIPTION:
//      Initializes a {\tt Clock} with given values
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//
	return(ESMF_SUCCESS);

 } // end ESMC_ClockInit

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ClockAdvance - increment a clock's time
//
// !INTERFACE:
      int ESMC_Clock::ESMC_ClockAdvance(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_Alarm *RingingList,            // out - list of ringing alarms
      int        *NumRingingAlarms) {     // out - number of ringing alarms
//
// !DESCRIPTION:
//     Advances a clock's current time by timestep, then checks
//     each of its alarms to see if its time to ring
//
//EOP
// !REQUIREMENTS:  TMG 3.4.1

	PrevTime = CurrTime;   // save current time, then
    CurrTime += TimeStep;  // advance it!

	// call each alarm's CheckRingTime method; compile and return a list of 
    // ringing alarms

	return(ESMF_SUCCESS);

 } // end ESMC_ClockAdvance

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_BaseValidate - internal consistency check for a Clock
//
// !INTERFACE:
      int ESMC_Clock::ESMC_BaseValidate(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {    // in - validate options
//
// !DESCRIPTION:
//      Validates that a {\tt Clock} is internally consistent.
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
// !IROUTINE:  ESMC_BasePrint - print contents of a Clock
//
// !INTERFACE:
      int ESMC_Clock::ESMC_BasePrint(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_TimeInterval *TimeStep,             // out
      ESMC_Time         *StartTime,            // out
      ESMC_Time         *StopTime,             // out
      ESMC_Time         *RefTime,              // out
      ESMC_Time         *CurrTime,             // out
      ESMC_Time         *PrevTime,             // out
      ESMF_IKIND_I8            *AdvanceCount,         // out
      ESMC_Alarm        *AlarmList[],          // out
      int               *NumAlarms ) const {   // out 
//
// !DESCRIPTION:
//      Print information about a {\tt Clock}.  The options control the
//      type of information and level of detail.  {\tt ESMC\_Base}
//      class method.
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
// !IROUTINE:  ESMC_Clock - native C++ constructor
//
// !INTERFACE:
      ESMC_Clock::ESMC_Clock(void) {
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

 } // end ESMC_Clock

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ~ESMC_Clock - native C++ destructor
//
// !INTERFACE:
      ESMC_Clock::~ESMC_Clock(void) {
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

 } // end ~ESMC_Clock
