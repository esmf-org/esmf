// $Id: ESMC_Clock.C,v 1.33 2003/09/11 00:05:05 eschwab Exp $
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
// The code in this file implements the C++ {\tt ESMC\_Clock} methods declared
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
 static const char *const version = "$Id: ESMC_Clock.C,v 1.33 2003/09/11 00:05:05 eschwab Exp $";
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
// !IROUTINE:  ESMC_ClockSetup - Initializes a Clock
//
// !INTERFACE:
      int ESMC_Clock::ESMC_ClockSetup(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_TimeInterval *timeStep,   // in
      ESMC_Time         *startTime,  // in
      ESMC_Time         *stopTime,   // in
      ESMC_Time         *refTime) {  // in

// !DESCRIPTION:
//      Initializes a {\tt ESMC\_Clock} with given values
//
//EOP
// !REQUIREMENTS:  

    // TODO: ensure initialization if called via F90 interface;
    //       cannot call constructor, because destructor is subsequently
    //       called automatically, returning initialized values to garbage.
    this->numAlarms    = 0;
    this->advanceCount = 0;

    if (timeStep  != ESMC_NULL_POINTER) this->timeStep  = *timeStep;
    if (startTime != ESMC_NULL_POINTER) this->startTime = *startTime;
    if (stopTime  != ESMC_NULL_POINTER) this->stopTime  = *stopTime;

    if (refTime   != ESMC_NULL_POINTER) this->refTime   = *refTime;
    else this->refTime = this->startTime;

    this->currTime = this->startTime;
    this->prevTime = this->currTime;

    return(ESMC_ClockValidate());

 } // end ESMC_ClockSetup

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ClockSet - Sets a Clock object's properties
//
// !INTERFACE:
      int ESMC_Clock::ESMC_ClockSet(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_TimeInterval *timeStep,       // in
      ESMC_Time         *startTime,      // in
      ESMC_Time         *stopTime,       // in
      ESMC_Time         *refTime,        // in
      ESMC_Time         *currTime,       // in
      ESMF_KIND_I8      *advanceCount) { // in

// !DESCRIPTION:
//      Sets a {\tt ESMC\_Clock}'s properties
//
//EOP
// !REQUIREMENTS:  

    // save current values to restore in case of failure;
    ESMC_Clock saveClock = *this;

    if (timeStep  != ESMC_NULL_POINTER) this->timeStep  = *timeStep;
    if (startTime != ESMC_NULL_POINTER) this->startTime = *startTime;
    if (stopTime  != ESMC_NULL_POINTER) this->stopTime  = *stopTime;
    if (refTime   != ESMC_NULL_POINTER) this->refTime   = *refTime;

    if (currTime  != ESMC_NULL_POINTER) {
      this->prevTime = this->currTime;
      this->currTime = *currTime;
    }

    if (advanceCount != ESMC_NULL_POINTER) this->advanceCount = *advanceCount;

    if (ESMC_ClockValidate() == ESMF_SUCCESS) return(ESMF_SUCCESS);
    else {
      // restore original clock values
      *this = saveClock;
      return(ESMF_FAILURE);
    }

 } // end ESMC_ClockSet

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ClockGet - gets a Clock object's properties
//
// !INTERFACE:
      int ESMC_Clock::ESMC_ClockGet(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_TimeInterval *timeStep,       // out
      ESMC_Time         *startTime,      // out
      ESMC_Time         *stopTime,       // out
      ESMC_Time         *refTime,        // out
      ESMC_Time         *currTime,       // out
      ESMC_Time         *prevTime,       // out
      ESMC_Time         *currSimTime,    // out
      ESMC_Time         *prevSimTime,    // out
      ESMF_KIND_I8      *advanceCount,   // out
      int               *numAlarms) {    // out

// !DESCRIPTION:
//      Gets a {\tt ESMC\_Clock}'s property values
//
//EOP
// !REQUIREMENTS:  

    if (timeStep  != ESMC_NULL_POINTER) *timeStep  = this->timeStep;
    if (startTime != ESMC_NULL_POINTER) *startTime = this->startTime;
    if (stopTime  != ESMC_NULL_POINTER) *stopTime  = this->stopTime;
    if (refTime   != ESMC_NULL_POINTER) *refTime   = this->refTime;
    if (currTime  != ESMC_NULL_POINTER) *currTime  = this->currTime;
    if (prevTime  != ESMC_NULL_POINTER) *prevTime  = this->prevTime;

    // Get the clock's current simulation time
    if (currSimTime != ESMC_NULL_POINTER) {
      *currSimTime = (this->currTime - this->refTime);
    }
    // Get the clock's previous simulation time
    if (prevSimTime != ESMC_NULL_POINTER) {
      *prevSimTime = (this->prevTime - this->refTime);
    }

    if (advanceCount != ESMC_NULL_POINTER) *advanceCount = this->advanceCount;
    if (numAlarms    != ESMC_NULL_POINTER) *numAlarms    = this->numAlarms;

    return(ESMF_SUCCESS);

 } // end ESMC_ClockGet

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
    if (numAlarms == MAX_ALARMS || alarm == ESMC_NULL_POINTER) {
      return(ESMF_FAILURE);
    }

    alarmList[numAlarms++] = alarm;

    return(ESMF_SUCCESS);

 } // end ESMC_ClockAddAlarm

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ClockGetAlarm - get alarm from clock's alarm list
//
// !INTERFACE:
      int ESMC_Clock::ESMC_ClockGetAlarm(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int          i,         // in  - the i'th alarm to get
      ESMC_Alarm **alarm) {   // out - the i'th alarm
//
// !DESCRIPTION:
//     Retrieve's the clock's i'th alarm from the alarm list
//
//EOP
// !REQUIREMENTS:  TMG 4.1, 4.2

    // validate inputs
    if (i < 1 || i > numAlarms || alarm == ESMC_NULL_POINTER) {
      return(ESMF_FAILURE);
    }

    *alarm = alarmList[i-1];  

    return(ESMF_SUCCESS);

 } // end ESMC_ClockGetAlarm

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ClockGetRingingAlarm - get ringing alarm from clock's alarm list
//
// !INTERFACE:
      int ESMC_Clock::ESMC_ClockGetRingingAlarm(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int          i,         // in  - the i'th alarm to get
      ESMC_Alarm **alarm) {   // out - the i'th alarm
//
// !DESCRIPTION:
//     Retrieve's the clock's i'th ringing alarm from the alarm list
//
//EOP
// !REQUIREMENTS:  TMG 4.1, 4.2

    // validate inputs
    if (i < 1 || i > numAlarms || alarm == ESMC_NULL_POINTER) {
      return(ESMF_FAILURE);
    }

    int rc;
    for (int j=0, r=0; j < numAlarms; j++) {
      if (alarmList[j]->ESMC_AlarmIsRinging(&rc)) {
        if (++r == i) *alarm = alarmList[j];
        return(ESMF_SUCCESS);
      }
    }

    return(ESMF_FAILURE);

 } // end ESMC_ClockGetRingingAlarm

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
    for(int i=0; i<this->numAlarms; i++) {
      (*alarmList)[i] = this->alarmList[i];  
    }

    // return number of alarms
    *numAlarms = this->numAlarms;

    return(ESMF_SUCCESS);

 } // end ESMC_ClockGetAlarmList

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
    for(int i=0; i<this->numAlarms; i++) {
      (*alarmList)[i] = *(this->alarmList[i]);  
    }

    // return number of alarms
    *numAlarms = this->numAlarms;

    return(ESMF_SUCCESS);

 } // end ESMC_ClockAddAlarm

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
      ESMC_TimeInterval *timeStep,            // in  - optional new timestep
      int               *numRingingAlarms) {  // out - number of ringing alarms
//
// !DESCRIPTION:
//     Advances a clock's current time by timestep, then checks
//     each of its alarms to see if its time to ring
//
//EOP
// !REQUIREMENTS:  TMG 3.4.1

    // save new time step if specified
    if (timeStep != ESMC_NULL_POINTER) this->timeStep = *timeStep;

    prevTime = currTime;         // save current time, then
    currTime += this->timeStep;  // advance it!
    advanceCount++;

    // TODO: validate (range check) new time against its calendar ?

    // TODO: call each alarm's CheckRingTime method;
    //   compile and return a list of ringing alarms
    //   ESMC_AlarmCheckRingTime(currTime, ...)

    return(ESMF_SUCCESS);

 } // end ESMC_ClockAdvance

#if 0
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

    prevTime = currTime;   // save current time, then
    currTime += timeStep;  // advance it!
    advanceCount++;

    // TODO: call each alarm's CheckRingTime method;
    //   compile and return a list of ringing alarms
    //   ESMC_AlarmCheckRingTime(currTime, ...)

    return(ESMF_SUCCESS);

 } // end ESMC_ClockAdvance
#endif

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
//    checks if {\tt ESMC\_Clock}'s stop time has been reached.
//
//EOP
// !REQUIREMENTS:

    *rc = ESMF_SUCCESS;

    // positive time step ?
    if (stopTime > startTime) {
      return(currTime >= stopTime);

    // or negative time step ?
    } else if (stopTime < startTime) {
      return(currTime <= stopTime);

    // or no time step? (stopTime == startTime)
    } else return(currTime == stopTime);

 } // end ESMC_ClockIsStopTime

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ClockSyncToRealTime - synchronize a clock to the wall clock time
//
// !INTERFACE:
      int ESMC_Clock::ESMC_ClockSyncToRealTime(void) {
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
    return(currTime.ESMC_TimeSyncToRealTime());

 } // end ESMC_ClockSyncToRealTime

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ClockReadRestart - restore contents of a Clock
//
// !INTERFACE:
      int ESMC_Clock::ESMC_ClockReadRestart(
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
      ESMF_KIND_I8       advanceCount,         // in
      int                numAlarms,            // in 
      ESMC_Alarm        *alarmList[]) {        // in
//
// !DESCRIPTION:
//      Restore information about a {\tt ESMC\_Clock}.
//      {\tt ESMC\_Base} class method.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

    if (timeStep  == ESMC_NULL_POINTER || startTime == ESMC_NULL_POINTER ||
        stopTime  == ESMC_NULL_POINTER || refTime   == ESMC_NULL_POINTER ||
        currTime  == ESMC_NULL_POINTER || prevTime  == ESMC_NULL_POINTER ||
        alarmList == ESMC_NULL_POINTER) {
      cout << "ESMC_Clock::ESMC_ClockReadRestart(): null pointer(s) passed in" << endl;
      return(ESMF_FAILURE);
    }
    
    this->timeStep     = *timeStep;
    this->startTime    = *startTime;
    this->stopTime     = *stopTime;
    this->refTime      = *refTime;
    this->currTime     = *currTime;
    this->prevTime     = *prevTime;
    this->advanceCount = advanceCount;
    this->numAlarms    = numAlarms;
    for (int i=0; i<this->numAlarms; i++) this->alarmList[i] = alarmList[i];
                              // TODO: component must be sure Alarms are
                              // restored first
    
    return(ESMF_SUCCESS);

 } // end ESMC_ClockReadRestart

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ClockWriteRestart - save contents of a Clock
//
// !INTERFACE:
      int ESMC_Clock::ESMC_ClockWriteRestart(
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
      ESMF_KIND_I8      *advanceCount,         // out
      int               *numAlarms,            // out 
      ESMC_Alarm        *alarmList[]) const {  // out
//
// !DESCRIPTION:
//      Save information about a {\tt ESMC\_Clock}.
//      {\tt ESMC\_Base} class method.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

    if (timeStep     == ESMC_NULL_POINTER || startTime == ESMC_NULL_POINTER ||
        stopTime     == ESMC_NULL_POINTER || refTime   == ESMC_NULL_POINTER ||
        currTime     == ESMC_NULL_POINTER || prevTime  == ESMC_NULL_POINTER ||
        advanceCount == ESMC_NULL_POINTER || alarmList == ESMC_NULL_POINTER ||
        numAlarms    == ESMC_NULL_POINTER) {
      cout << "ESMC_Clock::ESMC_ClockWriteRestart(): null pointer(s) passed in"
           << endl;
      return(ESMF_FAILURE);
    }
    
    *timeStep     = this->timeStep;
    *startTime    = this->startTime;
    *stopTime     = this->stopTime;
    *refTime      = this->refTime;
    *currTime     = this->currTime;
    *prevTime     = this->prevTime;
    *advanceCount = this->advanceCount;
    *numAlarms    = this->numAlarms;
    for (int i=0; i<this->numAlarms; i++) alarmList[i] = this->alarmList[i];
                               // TODO: only saves pointers; component must be
                               // sure Alarms are saved afterward
    return(ESMF_SUCCESS);

 } // end ESMC_ClockWriteRestart

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
//      Validates that a {\tt ESMC\_Clock} is internally consistent.
//      Returns error code if problems are found.  {\tt ESMC\_Base}
//      class method.
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

    if(timeStep.ESMC_TimeIntervalValidate() != ESMF_SUCCESS ||
       startTime.ESMC_TimeValidate() != ESMF_SUCCESS ||
       stopTime.ESMC_TimeValidate() != ESMF_SUCCESS ||
       refTime.ESMC_TimeValidate() != ESMF_SUCCESS ||
       currTime.ESMC_TimeValidate() != ESMF_SUCCESS ||
       prevTime.ESMC_TimeValidate()) return(ESMF_FAILURE);

    // TODO:  if stoptime > startTime, then timeStep should be
    //        positive and vice versa
    // TODO:  if timeStep > 0, then currTime >= prevTime and vice versa
    
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
//      Prints a {\tt ESMC\_Clock}'s contents for testing/debugging
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

    cout << "Clock ----------------------------------" << endl;

    // print out individually selected components
    // TODO: enable multiple simultaneous options (token parsing)
    //       (currently mutually exclusive)
    if (options != ESMC_NULL_POINTER) {

      if (strncmp(options, "timestep", 8) == 0) {
        cout << "timeStep = " << endl;
        if (strstr(options, "string") != ESMC_NULL_POINTER) {
          timeStep.ESMC_TimeIntervalPrint("string");
        } else {
          timeStep.ESMC_TimeIntervalPrint();
        }
      }
      else if (strncmp(options, "starttime", 9) == 0) {
        cout << "startTime = " << endl;
        if (strstr(options, "string") != ESMC_NULL_POINTER) {
          startTime.ESMC_TimePrint("string");
        } else {
          startTime.ESMC_TimePrint();
        }
      }
      else if (strncmp(options, "stoptime", 8) == 0) {
        cout << "stopTime = " << endl;
        if (strstr(options, "string") != ESMC_NULL_POINTER) {
          stopTime.ESMC_TimePrint("string");
        } else {
          stopTime.ESMC_TimePrint();
        }
      }
      else if (strncmp(options, "reftime", 7) == 0) {
        cout << "refTime = " << endl;
        if (strstr(options, "string") != ESMC_NULL_POINTER) {
          refTime.ESMC_TimePrint("string");
        } else {
          refTime.ESMC_TimePrint();
        }
      }
      else if (strncmp(options, "currtime", 8) == 0) {
        cout << "currTime = " << endl;
        if (strstr(options, "string") != ESMC_NULL_POINTER) {
          currTime.ESMC_TimePrint("string");
        } else {
          currTime.ESMC_TimePrint();
        }
      }
      else if (strncmp(options, "prevtime", 8) == 0) {
        cout << "prevTime = " << endl;
        if (strstr(options, "string") != ESMC_NULL_POINTER) {
          prevTime.ESMC_TimePrint("string");
        } else {
          prevTime.ESMC_TimePrint();
        }
      }
      else if (strncmp(options, "advancecount", 12) == 0) {
        cout << "advanceCount = " << advanceCount << endl;
      }
      else if (strncmp(options, "numalarms", 9) == 0) {
        cout << "numAlarms = " << numAlarms << endl;
      }
      else if (strncmp(options, "alarmlist", 9) == 0) {
        cout << "alarmList = " << endl;
        for (int i=0; i<numAlarms; i++) {
          cout << alarmList[i]->ESMC_AlarmPrint();
        }
      }

    } else {
      // default:  print out all components

      cout << "timeStep = "  << endl; timeStep.ESMC_TimeIntervalPrint(options);
      cout << "startTime = " << endl; startTime.ESMC_TimePrint(options);
      cout << "stopTime = "  << endl; stopTime.ESMC_TimePrint(options);
      cout << "refTime = "   << endl; refTime.ESMC_TimePrint(options);
      cout << "currTime = "  << endl; currTime.ESMC_TimePrint(options);
      cout << "prevTime = "  << endl; prevTime.ESMC_TimePrint(options);
      cout << "advanceCount = " << advanceCount << endl;
      cout << "numAlarms = "    << numAlarms << endl;
      cout << "alarmList = " << endl;
      for (int i=0; i<numAlarms; i++) {
        cout << alarmList[i]->ESMC_AlarmPrint(options);
      }
    }

    cout << "end Clock ------------------------------" << endl << endl;

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

    advanceCount = 0;
    numAlarms = 0;

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
