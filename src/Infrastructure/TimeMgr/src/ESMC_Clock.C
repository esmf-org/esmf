// $Id: ESMC_Clock.C,v 1.9 2003/03/29 01:41:21 eschwab Exp $
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
 #include <iostream.h>

 // associated class definition file
 #include <ESMC_Clock.h>

//-------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_Clock.C,v 1.9 2003/03/29 01:41:21 eschwab Exp $";
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

    if (timeStep  != 0) TimeStep  = *timeStep;
    if (startTime != 0) StartTime = *startTime;
    if (stopTime  != 0) StopTime  = *stopTime;
    if (refTime   != 0) RefTime   = *refTime;
    else RefTime = StartTime;

    CurrTime = StartTime;
    PrevTime = CurrTime;
    AdvanceCount = 0;

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
    AdvanceCount++;

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
// !IROUTINE:  ESMC_ClockGetAdvanceCount - get clock's advance count
//
// !INTERFACE:
      int ESMC_Clock::ESMC_ClockGetAdvanceCount(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMF_IKIND_I8 *advanceCount) {      // out - advance count
//
// !DESCRIPTION:
//     Get the number of times a clock has been advanced (time stepped)
//
//EOP
// !REQUIREMENTS: TMG 3.5.1

    if (advanceCount != 0) *advanceCount = AdvanceCount;

    return(ESMF_SUCCESS);

 } // end ESMC_ClockGetAdvanceCount

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Read - restore contents of a Clock
//
// !INTERFACE:
      int ESMC_Clock::ESMC_Read(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_TimeInterval *timeStep,             // in
      ESMC_Time         *startTime,            // in
      ESMC_Time         *stopTime,             // in
      ESMC_Time         *refTime,              // in
      ESMC_Time         *currTime,             // in
      ESMC_Time         *prevTime,             // in
      ESMF_IKIND_I8      advanceCount,         // in
      ESMC_Alarm        *alarmList[],          // in
      int                numAlarms ) {         // in 
//
// !DESCRIPTION:
//      Restore information about a {\tt Clock}.  The options control the
//      type of information and level of detail.  {\tt ESMC\_Base}
//      class method.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

    if (timeStep == 0 || startTime == 0 || stopTime == 0 || refTime == 0 ||
        currTime == 0 || prevTime == 0 || alarmList == 0) {
      cout << "ESMC_Clock::ESMC_Read(): null pointer(s) passed in" << endl;
      return(ESMF_FAILURE);
    }
    
    TimeStep     = *timeStep;
    StartTime    = *startTime;
    StopTime     = *stopTime;
    RefTime      = *refTime;
    CurrTime     = *currTime;
    PrevTime     = *prevTime;
    AdvanceCount = advanceCount;
    NumAlarms    = numAlarms;
    for (int i=0; i<NumAlarms; i++) AlarmList[i] = alarmList[i];
                              // TODO: component must be sure Alarms are
                              // restored first
    
    return(ESMF_SUCCESS);

 } // end ESMC_Read

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Write - save contents of a Clock
//
// !INTERFACE:
      int ESMC_Clock::ESMC_Write(
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
//      Save information about a {\tt Clock}.  The options control the
//      type of information and level of detail.  {\tt ESMC\_Base}
//      class method.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

    if (timeStep == 0 || startTime == 0 || stopTime == 0 || refTime == 0 ||
        currTime == 0 || prevTime == 0 || advanceCount == 0 || alarmList == 0 ||
        numAlarms == 0) {
      cout << "ESMC_Clock::ESMC_Write(): null pointer(s) passed in" << endl;
      return(ESMF_FAILURE);
    }
    
    *timeStep     = TimeStep;
    *startTime    = StartTime;
    *stopTime     = StopTime;
    *refTime      = RefTime;
    *currTime     = CurrTime;
    *prevTime     = PrevTime;
    *advanceCount = AdvanceCount;
    *numAlarms    = NumAlarms;
    for (int i=0; i<NumAlarms; i++) alarmList[i] = AlarmList[i];
                               // TODO: only saves pointers; component must be
                               // sure Alarms are saved afterward
    return(ESMF_SUCCESS);

 } // end ESMC_Write

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Validate - internal consistency check for a Clock
//
// !INTERFACE:
      int ESMC_Clock::ESMC_Validate(
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

 } // end ESMC_Validate

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Print - print contents of a Clock
//
// !INTERFACE:
      int ESMC_Clock::ESMC_Print(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {    // in - print options
//
// !DESCRIPTION:
//      Prints a {\tt Clock}'s contents for testing/debugging
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

    cout << "Clock:" << endl;
    cout << "TimeStep = "     << TimeStep.ESMC_Print(options)  << endl;
    cout << "StartTime = "    << StartTime.ESMC_Print(options) << endl;
    cout << "StopTime = "     << StopTime.ESMC_Print(options) << endl;
    cout << "RefTime = "      << RefTime.ESMC_Print(options) << endl;
    cout << "CurrTime = "     << CurrTime.ESMC_Print(options) << endl;
    cout << "PrevTime = "     << PrevTime.ESMC_Print(options) << endl;
    cout << "AdvanceCount = " << AdvanceCount << endl;
    cout << "NumAlarms = "    << NumAlarms << endl;
    cout << "AlarmList = " << endl;
    for (int i=0; i<NumAlarms; i++) cout << AlarmList[i]->ESMC_Print(options);

    // TODO print ClockMutex ?

    return(ESMF_SUCCESS);

 } // end ESMC_Print

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

    AdvanceCount = 0;
    NumAlarms = 0;

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
