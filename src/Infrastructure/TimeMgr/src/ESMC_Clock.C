// $Id: ESMC_Clock.C,v 1.6 2003/03/26 01:09:38 eschwab Exp $
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
 static const char *const version = "$Id: ESMC_Clock.C,v 1.6 2003/03/26 01:09:38 eschwab Exp $";
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
      ESMC_TimeInterval *timeStep,    // in
      ESMC_Time         *startTime,   // in
      ESMC_Time         *stopTime,    // in
      ESMC_Time         *refTime) {   // in

// !DESCRIPTION:
//      Initializes a {\tt Clock} with given values
//
//EOP
// !REQUIREMENTS:  

    TimeStep  = *timeStep;
    StartTime = *startTime;
    StopTime  = *stopTime;
    RefTime   = *refTime;

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
      ESMC_Alarm *ringingList,            // out - list of ringing alarms
      int        *numRingingAlarms) {     // out - number of ringing alarms
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
// !IROUTINE:  ESMC_ClockIsStopTime - check if Clock's stop time has been reached
//
// !INTERFACE:
      bool ESMC_Clock::ESMC_ClockIsStopTime(
//
// !RETURN VALUE:
//    bool is stop time or not
//
// !ARGUMENTS:
      int  *rc) const {        // out - error return code
//
// !DESCRIPTION:
//    checks if {\tt Clock}'s stop time has been reached.
//
//EOP
// !REQUIREMENTS:

    *rc = ESMF_SUCCESS;

    return(CurrTime >= StopTime);

 } // end ESMC_ClockIsStopTime

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
      ESMC_TimeInterval *timeStep,             // out
      ESMC_Time         *startTime,            // out
      ESMC_Time         *stopTime,             // out
      ESMC_Time         *refTime,              // out
      ESMC_Time         *currTime,             // out
      ESMC_Time         *prevTime,             // out
      ESMF_IKIND_I8     *advanceCount,         // out
      ESMC_Alarm        *alarmList[],          // out
      int               *numAlarms ) const {   // out 
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
