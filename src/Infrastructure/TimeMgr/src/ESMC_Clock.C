// $Id: ESMC_Clock.C,v 1.24 2003/06/07 00:42:00 eschwab Exp $
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
 #include <string.h>

 // associated class definition file
 #include <ESMC_Clock.h>

//-------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_Clock.C,v 1.24 2003/06/07 00:42:00 eschwab Exp $";
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
// !IROUTINE:  ESMC_ClockSet - initializes a Clock object
//
// !INTERFACE:
      int ESMC_Clock::ESMC_ClockSet(
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

    // TODO: ensure initialization if called via F90 interface;
    //       cannot call constructor, because destructor is subsequently
    //       called automatically, returning initialized values to garbage.
    NumAlarms = 0;

    if (timeStep  != ESMC_NULL_POINTER) TimeStep  = *timeStep;
    if (startTime != ESMC_NULL_POINTER) StartTime = *startTime;
    if (stopTime  != ESMC_NULL_POINTER) StopTime  = *stopTime;
    if (refTime   != ESMC_NULL_POINTER) RefTime   = *refTime;
    else RefTime = StartTime;

    CurrTime = StartTime;
    PrevTime = CurrTime;
    AdvanceCount = 0;

    return(ESMC_ClockValidate());

 } // end ESMC_ClockSet

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
      ESMF_IKIND_I8 *advanceCount) const {      // out - advance count
//
// !DESCRIPTION:
//     Get the number of times a clock has been advanced (time stepped)
//
//EOP
// !REQUIREMENTS: TMG 3.5.1

    if (advanceCount != ESMC_NULL_POINTER) {
      *advanceCount = AdvanceCount;
      return(ESMF_SUCCESS);
    }
    else {
      cout << "ESMC_Clock::ESMC_ClockGetAdvanceCount(): null pointer passed in"
           << endl;
      return(ESMF_FAILURE);
    }

 } // end ESMC_ClockGetAdvanceCount

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ClockGetTimeStep - get clock's time step interval
//
// !INTERFACE:
      int ESMC_Clock::ESMC_ClockGetTimeStep(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_TimeInterval *timeStep) const {      // out - time step
//
// !DESCRIPTION:
//     Get the clock's time step
//
//EOP
// !REQUIREMENTS: TMG 3.5.2

    if (timeStep != ESMC_NULL_POINTER) {
      *timeStep = TimeStep;
      return(ESMF_SUCCESS);
    }
    else {
      cout << "ESMC_Clock::ESMC_ClockGetTimeStep(): null pointer passed in"
           << endl;
      return(ESMF_FAILURE);
    }

 } // end ESMC_ClockGetTimeStep

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ClockSetTimeStep - set clock's time step interval
//
// !INTERFACE:
      int ESMC_Clock::ESMC_ClockSetTimeStep(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_TimeInterval *timeStep) {      // in - time step
//
// !DESCRIPTION:
//     Set the clock's time step
//
//EOP
// !REQUIREMENTS: TMG 3.4.2

    if (timeStep != ESMC_NULL_POINTER) {
      TimeStep = *timeStep;
      return(ESMF_SUCCESS);
    }
    else {
      cout << "ESMC_Clock::ESMC_ClockSetTimeStep(): null pointer passed in"
           << endl;
      return(ESMF_FAILURE);
    }

 } // end ESMC_ClockSetTimeStep

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ClockGetCurrTime - get clock's current time
//
// !INTERFACE:
      int ESMC_Clock::ESMC_ClockGetCurrTime(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_Time *currTime) const {      // out - current time
//
// !DESCRIPTION:
//     Get the clock's current time
//
//EOP
// !REQUIREMENTS: TMG 3.5.4

    if (currTime != ESMC_NULL_POINTER) {
      *currTime = CurrTime;
      return(ESMF_SUCCESS);
    }
    else {
      cout << "ESMC_Clock::ESMC_ClockGetCurrTime(): null pointer passed in"
           << endl;
      return(ESMF_FAILURE);
    }

 } // end ESMC_ClockGetCurrTime

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ClockSetCurrTime - set clock's current time
//
// !INTERFACE:
      int ESMC_Clock::ESMC_ClockSetCurrTime(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_Time *currTime) {      // in - current time
//
// !DESCRIPTION:
//     Set the clock's current time
//
//EOP
// !REQUIREMENTS: TMG 3.4.3

    if (currTime != ESMC_NULL_POINTER) {
      PrevTime = CurrTime;   // save current time, then
      CurrTime = *currTime;  // reset it
      return(ESMF_SUCCESS);
    }
    else {
      cout << "ESMC_Clock::ESMC_ClockSetCurrTime(): null pointer passed in"
           << endl;
      return(ESMF_FAILURE);
    }

 } // end ESMC_ClockSetCurrTime

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ClockGetStartTime - get clock's starting time
//
// !INTERFACE:
      int ESMC_Clock::ESMC_ClockGetStartTime(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_Time *startTime) const {      // out - start time
//
// !DESCRIPTION:
//     Get the clock's start time
//
//EOP
// !REQUIREMENTS: TMG 3.5.3

    if (startTime != ESMC_NULL_POINTER) {
      *startTime = StartTime;
      return(ESMF_SUCCESS);
    }
    else {
      cout << "ESMC_Clock::ESMC_ClockGetStartTime(): null pointer passed in"
           << endl;
      return(ESMF_FAILURE);
    }

 } // end ESMC_ClockGetStartTime

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ClockGetStopTime - get clock's stopping time
//
// !INTERFACE:
      int ESMC_Clock::ESMC_ClockGetStopTime(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_Time *stopTime) const {      // out - stop time
//
// !DESCRIPTION:
//     Get the clock's stop time
//
//EOP
// !REQUIREMENTS: TMG 3.5.3

    if (stopTime != ESMC_NULL_POINTER) {
      *stopTime = StopTime;
      return(ESMF_SUCCESS);
    }
    else {
      cout << "ESMC_Clock::ESMC_ClockGetStopTime(): null pointer passed in"
           << endl;
      return(ESMF_FAILURE);
    }

 } // end ESMC_ClockGetStopTime

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ClockGetRefTime - get clock's reference (base) time
//
// !INTERFACE:
      int ESMC_Clock::ESMC_ClockGetRefTime(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_Time *refTime) const {      // out - reference time
//
// !DESCRIPTION:
//     Get the clock's reference (base) time
//
//EOP
// !REQUIREMENTS: TMG 3.5.3

    if (refTime != ESMC_NULL_POINTER) {
      *refTime = RefTime;
      return(ESMF_SUCCESS);
    }
    else {
      cout << "ESMC_Clock::ESMC_ClockGetRefTime(): null pointer passed in"
           << endl;
      return(ESMF_FAILURE);
    }

 } // end ESMC_ClockGetRefTime

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ClockGetPrevTime - get clock's previous time
//
// !INTERFACE:
      int ESMC_Clock::ESMC_ClockGetPrevTime(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_Time *prevTime) const {      // out - previous time
//
// !DESCRIPTION:
//     Get the clock's previous time
//
//EOP
// !REQUIREMENTS: TMG 3.5.4

    if (prevTime != ESMC_NULL_POINTER) {
      *prevTime = PrevTime;
      return(ESMF_SUCCESS);
    }
    else {
      cout << "ESMC_Clock::ESMC_ClockGetPrevTime(): null pointer passed in"
           << endl;
      return(ESMF_FAILURE);
    }

 } // end ESMC_ClockGetPrevTime

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ClockGetCurrSimTime - get clock's current simulation time
//
// !INTERFACE:
      int ESMC_Clock::ESMC_ClockGetCurrSimTime(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_TimeInterval *currSimTime) const {  // out - current simulation time
//
// !DESCRIPTION:
//     Get the clock's current simulation time
//
//EOP
// !REQUIREMENTS: TMG 3.5.5

    if (currSimTime != ESMC_NULL_POINTER) {
      *currSimTime = (CurrTime - RefTime);
      return(ESMF_SUCCESS);
    }
    else {
      cout << "ESMC_Clock::ESMC_ClockGetCurrSimTime(): null pointer passed in"
           << endl;
      return(ESMF_FAILURE);
    }

 } // end ESMC_ClockGetCurrSimTime

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ClockGetPrevSimTime - get clock's previous simulation time
//
// !INTERFACE:
      int ESMC_Clock::ESMC_ClockGetPrevSimTime(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_TimeInterval *prevSimTime) const { // out - previous simulation time
//
// !DESCRIPTION:
//     Get the clock's previous simulation time
//
//EOP
// !REQUIREMENTS: TMG 3.5.5

    if (prevSimTime != ESMC_NULL_POINTER) {
      *prevSimTime = (PrevTime - RefTime);
      return(ESMF_SUCCESS);
    }
    else {
      cout << "ESMC_Clock::ESMC_ClockGetPrevSimTime(): null pointer passed in"
           << endl;
      return(ESMF_FAILURE);
    }

 } // end ESMC_ClockGetPrevSimTime

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ClockAddAlarm - add alarm to clock's alarm list
//
// !INTERFACE:
      int ESMC_Clock::ESMC_ClockAddAlarm(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_Alarm *alarm) {   // in - alarm to add
//
// !DESCRIPTION:
//     Adds given alarm to a clock's alarm list
//
//EOP
// !REQUIREMENTS:  TMG 4.1, 4.2

    // validate inputs
    if (NumAlarms == MAX_ALARMS || alarm == ESMC_NULL_POINTER) {
      return(ESMF_FAILURE);
    }

    AlarmList[NumAlarms++] = alarm;

    return(ESMF_SUCCESS);

 } // end ESMC_ClockAddAlarm

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ClockGetAlarmList - get a clock's alarm list
//
// !INTERFACE:
      int ESMC_Clock::ESMC_ClockGetAlarmList(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_Alarm ***alarmList,            // out - alarm list
      int          *numAlarms) const {    // out - number of alarms in list
//
// !DESCRIPTION:
//     Get a clock's alarm list and number of alarms
//
//EOP
// !REQUIREMENTS:  TMG 4.3

    // validate inputs
    if (alarmList == ESMC_NULL_POINTER || numAlarms == ESMC_NULL_POINTER) {
      return(ESMF_FAILURE);
    }

    // copy the list of alarm pointers
    for(int i=0; i<NumAlarms; i++) {
      (*alarmList)[i] = AlarmList[i];  
    }

    // return number of alarms
    *numAlarms = NumAlarms;

    return(ESMF_SUCCESS);

 } // end ESMC_ClockAddAlarm

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ClockGetAlarmList - get a clock's alarm list
//
// !INTERFACE:
      int ESMC_Clock::ESMC_ClockGetAlarmList(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_Alarm **alarmList,            // out - alarm list
      int         *numAlarms) const {    // out - number of alarms in list
//
// !DESCRIPTION:
//     Get a clock's alarm list and number of alarms
//
//EOP
// !REQUIREMENTS:  TMG 4.3

    // validate inputs
    if (alarmList == ESMC_NULL_POINTER || numAlarms == ESMC_NULL_POINTER) {
      return(ESMF_FAILURE);
    }

    // copy the list of alarms
    for(int i=0; i<NumAlarms; i++) {
      (*alarmList)[i] = *(AlarmList[i]);  
    }

    // return number of alarms
    *numAlarms = NumAlarms;

    return(ESMF_SUCCESS);

 } // end ESMC_ClockAddAlarm

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ClockGetNumAlarms - get the number of alarms in a clock's list
//
// !INTERFACE:
      int ESMC_Clock::ESMC_ClockGetNumAlarms(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int *numAlarms) const {    // out - number of alarms in list
//
// !DESCRIPTION:
//     Get the number of alarms defined in a clock's list
//
//EOP
// !REQUIREMENTS:  TMG 4.3

    // validate inputs
    if (numAlarms == ESMC_NULL_POINTER) {
      return(ESMF_FAILURE);
    }

    // return number of alarms
    *numAlarms = NumAlarms;

    return(ESMF_SUCCESS);

 } // end ESMC_ClockNumAlarms

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ClockSyncToWallClock - synchronize a clock to the wall clock time
//
// !INTERFACE:
      int ESMC_Clock::ESMC_ClockSyncToWallClock(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//    Synchronize a clock to the wall clock time
//
//EOP
// !REQUIREMENTS:  TMG 3.4.5

    // set current time to wall clock time
    return(CurrTime.ESMC_TimeGetRealTime());

 } // end ESMC_ClockSyncToWallClock

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

    // TODO: call each alarm's CheckRingTime method;
    //   compile and return a list of ringing alarms

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

    // positive time step ?
    if (StopTime > StartTime) {
      return(CurrTime >= StopTime);

    // or negative time step ?
    } else if (StopTime < StartTime) {
      return(CurrTime <= StopTime);

    // or no time step? (StopTime == StartTime)
    } else return(CurrTime == StopTime);

 } // end ESMC_ClockIsStopTime

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ClockRead - restore contents of a Clock
//
// !INTERFACE:
      int ESMC_Clock::ESMC_ClockRead(
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

    if (timeStep  == ESMC_NULL_POINTER || startTime == ESMC_NULL_POINTER ||
        stopTime  == ESMC_NULL_POINTER || refTime   == ESMC_NULL_POINTER ||
        currTime  == ESMC_NULL_POINTER || prevTime  == ESMC_NULL_POINTER ||
        alarmList == ESMC_NULL_POINTER) {
      cout << "ESMC_Clock::ESMC_ClockRead(): null pointer(s) passed in" << endl;
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

 } // end ESMC_ClockRead

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ClockWrite - save contents of a Clock
//
// !INTERFACE:
      int ESMC_Clock::ESMC_ClockWrite(
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

    if (timeStep     == ESMC_NULL_POINTER || startTime == ESMC_NULL_POINTER ||
        stopTime     == ESMC_NULL_POINTER || refTime   == ESMC_NULL_POINTER ||
        currTime     == ESMC_NULL_POINTER || prevTime  == ESMC_NULL_POINTER ||
        advanceCount == ESMC_NULL_POINTER || alarmList == ESMC_NULL_POINTER ||
        numAlarms    == ESMC_NULL_POINTER) {
      cout << "ESMC_Clock::ESMC_ClockWrite(): null pointer(s) passed in"
           << endl;
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

 } // end ESMC_ClockWrite

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
//      Validates that a {\tt Clock} is internally consistent.
//      Returns error code if problems are found.  {\tt ESMC\_Base}
//      class method.
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

    if(TimeStep.ESMC_TimeIntervalValidate() != ESMF_SUCCESS ||
       StartTime.ESMC_TimeValidate() != ESMF_SUCCESS ||
       StopTime.ESMC_TimeValidate() != ESMF_SUCCESS ||
       RefTime.ESMC_TimeValidate() != ESMF_SUCCESS ||
       CurrTime.ESMC_TimeValidate() != ESMF_SUCCESS ||
       PrevTime.ESMC_TimeValidate()) return(ESMF_FAILURE);
    
    return(ESMF_SUCCESS);

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
      const char *options) const {    // in - print options
//
// !DESCRIPTION:
//      Prints a {\tt Clock}'s contents for testing/debugging
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

    cout << "Clock ----------------------------------" << endl;

    // print out individually selected components
    // TODO: enable multiple simultaneous options (token parsing)
    //       (currently mutually exclusive)
    if (options != ESMC_NULL_POINTER) {

      if (strncmp(options, "timestep", 8) == 0) {
        cout << "TimeStep = " << endl;
        if (strstr(options, "string") != ESMC_NULL_POINTER) {
          TimeStep.ESMC_TimeIntervalPrint("string");
        } else {
          TimeStep.ESMC_TimeIntervalPrint();
        }
      }
      else if (strncmp(options, "starttime", 9) == 0) {
        cout << "StartTime = " << endl;
        if (strstr(options, "string") != ESMC_NULL_POINTER) {
          StartTime.ESMC_TimePrint("string");
        } else {
          StartTime.ESMC_TimePrint();
        }
      }
      else if (strncmp(options, "stoptime", 8) == 0) {
        cout << "StopTime = " << endl;
        if (strstr(options, "string") != ESMC_NULL_POINTER) {
          StopTime.ESMC_TimePrint("string");
        } else {
          StopTime.ESMC_TimePrint();
        }
      }
      else if (strncmp(options, "reftime", 7) == 0) {
        cout << "RefTime = " << endl;
        if (strstr(options, "string") != ESMC_NULL_POINTER) {
          RefTime.ESMC_TimePrint("string");
        } else {
          RefTime.ESMC_TimePrint();
        }
      }
      else if (strncmp(options, "currtime", 8) == 0) {
        cout << "CurrTime = " << endl;
        if (strstr(options, "string") != ESMC_NULL_POINTER) {
          CurrTime.ESMC_TimePrint("string");
        } else {
          CurrTime.ESMC_TimePrint();
        }
      }
      else if (strncmp(options, "prevtime", 8) == 0) {
        cout << "PrevTime = " << endl;
        if (strstr(options, "string") != ESMC_NULL_POINTER) {
          PrevTime.ESMC_TimePrint("string");
        } else {
          PrevTime.ESMC_TimePrint();
        }
      }
      else if (strncmp(options, "advancecount", 12) == 0) {
        cout << "AdvanceCount = " << AdvanceCount << endl;
      }
      else if (strncmp(options, "numalarms", 9) == 0) {
        cout << "NumAlarms = " << NumAlarms << endl;
      }
      else if (strncmp(options, "alarmlist", 9) == 0) {
        cout << "AlarmList = " << endl;
        for (int i=0; i<NumAlarms; i++) {
          cout << AlarmList[i]->ESMC_AlarmPrint();
        }
      }

    } else {
      // default:  print out all components

      cout << "TimeStep = "  << endl; TimeStep.ESMC_TimeIntervalPrint(options);
      cout << "StartTime = " << endl; StartTime.ESMC_TimePrint(options);
      cout << "StopTime = "  << endl; StopTime.ESMC_TimePrint(options);
      cout << "RefTime = "   << endl; RefTime.ESMC_TimePrint(options);
      cout << "CurrTime = "  << endl; CurrTime.ESMC_TimePrint(options);
      cout << "PrevTime = "  << endl; PrevTime.ESMC_TimePrint(options);
      cout << "AdvanceCount = " << AdvanceCount << endl;
      cout << "NumAlarms = "    << NumAlarms << endl;
      cout << "AlarmList = " << endl;
      for (int i=0; i<NumAlarms; i++) {
        cout << AlarmList[i]->ESMC_AlarmPrint(options);
      }
    }

    cout << "end Clock ------------------------------" << endl << endl;

    // TODO print ClockMutex ?

    return(ESMF_SUCCESS);

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
