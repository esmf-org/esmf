// $Id: ESMC_Clock.C,v 1.2 2002/10/15 23:29:54 eschwab Exp $
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
// The code in this file implements the C++ Clock methods declared
// in the companion file ESMC_Clock.h
//
//-------------------------------------------------------------------------
//
 // higher level, 3rd party or system includes here
 #include <ESMC_Util.h>

 // associated class definition file
 #include <ESMC_Clock.h>

//-------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_Clock.C,v 1.2 2002/10/15 23:29:54 eschwab Exp $";
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
      ESMC_TimeInstant  *StartTime,   // in
      ESMC_TimeInstant  *StopTime,    // in
      ESMC_TimeInstant  *RefTime) {   // in

// !DESCRIPTION:
//      Initializes a Clock with given values
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//
	return(ESMC_SUCCESS);

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
      ESMC_Alarm *RingingList) {  // out - list of ringing alarms
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

	return(ESMC_SUCCESS);

 } // end ESMC_ClockGet<Value>

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ClockValidate - internal consistency check for a Clock
//
// !INTERFACE:
      int ESMC_Clock::ESMC_ClockValidate(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {    // in - validate options
//
// !DESCRIPTION:
//      Validates that a Clock is internally consistent.
//      Returns error code if problems are found.  ESMC_Base class method.
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

//
//  code goes here
//
	return(ESMC_SUCCESS);

 } // end ESMC_ClockValidate


//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ClockPrint - print contents of a Clock
//
// !INTERFACE:
      int ESMC_Clock::ESMC_ClockPrint(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_TimeInterval *TimeStep,             // out
      ESMC_TimeInstant  *StartTime,            // out
      ESMC_TimeInstant  *StopTime,             // out
      ESMC_TimeInstant  *RefTime,              // out
      ESMC_TimeInstant  *CurrTime,             // out
      ESMC_TimeInstant  *PrevTime,             // out
      uint32            *AdvanceCount,         // out
      ESMC_Alarm        *AlarmList[] ) const { // out 
//
// !DESCRIPTION:
//      Print information about a Clock.  The options control the
//      type of information and level of detail.  ESMC_Base class method.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//
//  code goes here
//
	return(ESMC_SUCCESS);

 } // end ESMC_ClockPrint

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
