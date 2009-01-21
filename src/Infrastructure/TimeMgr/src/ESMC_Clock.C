// $Id: ESMC_Clock.C,v 1.82.2.4 2009/01/21 21:25:23 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
// ESMC Clock method code (body) file
//
//-------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ {\tt ESMC\_Clock} methods declared
// in the companion file {\tt ESMC\_Clock.h}
//
//-------------------------------------------------------------------------
//
 #define ESMC_FILENAME "ESMC_Clock.C"

 // higher level, 3rd party or system includes here
 #include <stdio.h>
 #include <string.h>
 #include <ctype.h>
 #include <ESMC_LogErr.h>
 #include <ESMF_LogMacros.inc>
 #include <ESMC_Alarm.h>

 // associated class definition file
 #include <ESMC_Clock.h>

//-------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_Clock.C,v 1.82.2.4 2009/01/21 21:25:23 cdeluca Exp $";
//-------------------------------------------------------------------------

// initialize static clock instance counter
// TODO: inherit from ESMC_Base class
int ESMC_Clock::count=0;

//
//-------------------------------------------------------------------------
//-------------------------------------------------------------------------
//
// This section includes all the Clock routines
//
//

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ClockCreate - Allocates and Initializes a Clock object
//
// !INTERFACE:
      ESMC_Clock *ESMC_ClockCreate(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMC_Clock
//
// !ARGUMENTS:
      int                nameLen,          // in
      const char        *name,             // in
      ESMC_TimeInterval *timeStep,         // in
      ESMC_Time         *startTime,        // in
      ESMC_Time         *stopTime,         // in
      ESMC_TimeInterval *runDuration,      // in
      int               *runTimeStepCount, // in
      ESMC_Time         *refTime,          // in
      int               *rc) {             // out - return code

// !DESCRIPTION:
//      Allocates and Initializes a {\tt ESMC\_Clock} with given values
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_ClockCreate(new)"

    int returnCode;
    ESMC_Clock *clock;

    // default return code
    if (rc != ESMC_NULL_POINTER) *rc = ESMC_RC_NOT_IMPL;

    // allocate a clock object & set defaults via constructor
    try {
      clock = new ESMC_Clock;
    }
    catch (...) {
      ESMC_LogDefault.ESMC_LogAllocError(rc);
      return(ESMC_NULL_POINTER);
    }

    // TODO: use inherited methods from ESMC_Base
    if (name != ESMC_NULL_POINTER) {
      if (nameLen < ESMF_MAXSTR) {
        strncpy(clock->name, name, nameLen);
        clock->name[nameLen] = '\0';  // null terminate
      } else {
        // truncate
        strncpy(clock->name, name, ESMF_MAXSTR-1);
        clock->name[ESMF_MAXSTR-1] = '\0';  // null terminate

        char logMsg[ESMF_MAXSTR];
        sprintf(logMsg, "clock name %s, length >= ESMF_MAXSTR; truncated.",
                name);
        ESMC_LogDefault.ESMC_LogWrite(logMsg, ESMC_LOG_WARN);
        // TODO: return ESMF_WARNING when defined
        // if (rc != ESMC_NULL_POINTER) *rc = ESMF_WARNING;
      }
    } else {
      // create default name "ClockNNN"
      sprintf(clock->name, "Clock%3.3d\0", clock->id);
    }

    if (timeStep  != ESMC_NULL_POINTER) {
      clock->timeStep  = *timeStep;
      clock->currAdvanceTimeStep = clock->prevAdvanceTimeStep = clock->timeStep;
    }
    if (startTime != ESMC_NULL_POINTER) clock->startTime = *startTime;
    if (stopTime  != ESMC_NULL_POINTER) {
      clock->stopTime  = *stopTime;
      clock->stopTimeEnabled = true;
    }

    if (runDuration != ESMC_NULL_POINTER) {
      clock->stopTime = clock->startTime + *runDuration;
      clock->stopTimeEnabled = true;
    }
    if (runTimeStepCount != ESMC_NULL_POINTER) {
      // use passed-in timestep if specified, otherwise use the clock's
      ESMC_TimeInterval duration;
      if (timeStep != ESMC_NULL_POINTER) {
        duration = *runTimeStepCount * *timeStep;
      } else {
        duration = *runTimeStepCount * clock->timeStep;
      }
      clock->stopTime = clock->startTime + duration;
#if 0
// TODO: this code breaks Apple's g++ 3.3 compiler!
      clock->stopTime = clock->startTime + (*runTimeStepCount *
        (timeStep != ESMC_NULL_POINTER) ? *timeStep : clock->timeStep);
#endif
      clock->stopTimeEnabled = true;
    }

    if (refTime != ESMC_NULL_POINTER) clock->refTime = *refTime;
    else clock->refTime = clock->startTime;

    clock->prevTime = clock->currTime = clock->startTime;

    returnCode = clock->ESMC_ClockValidate();
    ESMC_LogDefault.ESMC_LogMsgFoundError(returnCode, ESMF_ERR_PASSTHRU, rc);
    return(clock);

 } // end ESMC_ClockCreate (new)

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ClockCreate - Creates a copy of a clock
//
// !INTERFACE:
      ESMC_Clock *ESMC_ClockCreate(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMC_Clock
//
// !ARGUMENTS:
      ESMC_Clock *clock,  // in  - clock to copy
      int        *rc) {   // out - return code

// !DESCRIPTION:
//      Creates a new copy of the given clock.
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_ClockCreate(copy)"

    int returnCode;
    ESMC_Clock *clockCopy;

    // default return code
    if (rc != ESMC_NULL_POINTER) *rc = ESMC_RC_NOT_IMPL;

    // can't copy a non-existent object
    if (clock == ESMC_NULL_POINTER) {
      ESMC_LogDefault.ESMC_LogWrite("Can't copy a non-existent clock",
                                    ESMC_LOG_ERROR);
      return(ESMC_NULL_POINTER);
    }

    try {
      // allocate new clock and pass given clock to copy constructor.
      clockCopy = new ESMC_Clock(*clock);
    }
    catch (...) {
      ESMC_LogDefault.ESMC_LogAllocError(rc);
      return(ESMC_NULL_POINTER);
    }

    returnCode = clockCopy->ESMC_ClockValidate();
    ESMC_LogDefault.ESMC_LogMsgFoundError(returnCode, ESMF_ERR_PASSTHRU, rc);

    return(clockCopy);

 } // end ESMC_ClockCreate (copy)

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ClockDestroy - free a Clock created with Create
//
// !INTERFACE:
      int ESMC_ClockDestroy(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_Clock **clock) {  // in - ESMC_Clock to destroy
//
// !DESCRIPTION:
//      ESMF routine which destroys a Clock object previously allocated
//      via an {\tt ESMC\_ClockCreate} routine.  Define for deep classes only.
//
//EOP

   // TODO: clock->ESMC_ClockDestruct(); constructor calls it!
   delete *clock; // ok to delete null pointer

   *clock = ESMC_NULL_POINTER;
   return(ESMF_SUCCESS);

 } // end ESMC_ClockDestroy

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
      int                nameLen,          // in
      const char        *name,             // in
      ESMC_TimeInterval *timeStep,         // in
      ESMC_Time         *startTime,        // in
      ESMC_Time         *stopTime,         // in
      ESMC_TimeInterval *runDuration,      // in
      int               *runTimeStepCount, // in
      ESMC_Time         *refTime,          // in
      ESMC_Time         *currTime,         // in
      ESMC_I8      *advanceCount,     // in
      ESMC_Direction    *direction) {      // in

// !DESCRIPTION:
//      Sets a {\tt ESMC\_Clock}'s properties
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_ClockSet()"

    int rc = ESMF_SUCCESS;

    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", &rc);
      return(rc);
    }

    // save current values to restore in case of failure;
    ESMC_Clock saveClock = *this;

    // TODO: use inherited methods from ESMC_Base
    if (name != ESMC_NULL_POINTER) {
      if (nameLen < ESMF_MAXSTR) {
        strncpy(this->name, name, nameLen); 
        this->name[nameLen] = '\0';  // null terminate
      } else {
        // truncate
        strncpy(this->name, name, ESMF_MAXSTR-1);
        this->name[ESMF_MAXSTR-1] = '\0';  // null terminate

        char logMsg[ESMF_MAXSTR];
        sprintf(logMsg, "clock name %s, length >= ESMF_MAXSTR; truncated.",
                name);
        ESMC_LogDefault.ESMC_LogWrite(logMsg, ESMC_LOG_WARN);
        // TODO: return ESMF_WARNING when defined
        // rc = ESMF_WARNING;
      }
    }

    if (timeStep  != ESMC_NULL_POINTER) this->timeStep  = *timeStep;
    if (startTime != ESMC_NULL_POINTER) this->startTime = *startTime;
    if (stopTime  != ESMC_NULL_POINTER) {
      this->stopTime  = *stopTime;
      this->stopTimeEnabled = true;
    }

    if (runDuration != ESMC_NULL_POINTER) {
      this->stopTime = this->startTime + *runDuration;
      this->stopTimeEnabled = true;
    }
    if (runTimeStepCount != ESMC_NULL_POINTER) {
      // use passed-in timestep if specified, otherwise use the clock's
      ESMC_TimeInterval duration;
      if (timeStep != ESMC_NULL_POINTER) {
        duration = *runTimeStepCount * *timeStep;
      } else {
        duration = *runTimeStepCount * this->timeStep;
      }
      this->stopTime = this->startTime + duration;
#if 0
// TODO: this code breaks Apple's g++ 3.3 compiler!
      this->stopTime = this->startTime + (*runTimeStepCount *
        (timeStep != ESMC_NULL_POINTER) ? *timeStep : this->timeStep);
#endif
      this->stopTimeEnabled = true;
    }

    if (refTime   != ESMC_NULL_POINTER) this->refTime   = *refTime;

    if (currTime  != ESMC_NULL_POINTER) {
      this->prevTime = this->currTime;
      this->currTime = *currTime;
    }

    if (advanceCount != ESMC_NULL_POINTER) this->advanceCount = *advanceCount;

    if (direction != ESMC_NULL_POINTER) this->direction = *direction;

    rc = ESMC_ClockValidate();
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &rc)) {
      // restore original clock values
      *this = saveClock;
    }

    return(rc);

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
      int                nameLen,          // in
      int               *tempNameLen,      // out
      char              *tempName,         // out
      ESMC_TimeInterval *timeStep,         // out
      ESMC_Time         *startTime,        // out
      ESMC_Time         *stopTime,         // out
      ESMC_TimeInterval *runDuration,      // out
      ESMC_R8      *runTimeStepCount, // out
      ESMC_Time         *refTime,          // out
      ESMC_Time         *currTime,         // out
      ESMC_Time         *prevTime,         // out
      ESMC_TimeInterval *currSimTime,      // out
      ESMC_TimeInterval *prevSimTime,      // out
      ESMC_Calendar    **calendar,         // out
      ESMC_CalendarType *calendarType,     // out
      int               *timeZone,         // out
      ESMC_I8      *advanceCount,     // out
      int               *alarmCount,       // out
      ESMC_Direction    *direction) {      // out

// !DESCRIPTION:
//      Gets a {\tt ESMC\_Clock}'s property values
//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_ClockGet()"

    int rc = ESMF_SUCCESS;

    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", &rc);
      return(rc);
    }

    // TODO: use inherited methods from ESMC_Base
    if (nameLen > 0) {
      if (strlen(this->name) < nameLen) {
        // copy all of it
        strcpy(tempName, this->name);
      } else {
        // TODO: copy what will fit and return ESMF_FAILURE ?
        strncpy(tempName, this->name, nameLen-1);
        tempName[nameLen] = '\0';  // null terminate

        char logMsg[ESMF_MAXSTR];
        sprintf(logMsg, "clock name %s, "
                "length >= given character array; truncated.", this->name);
        ESMC_LogDefault.ESMC_LogWrite(logMsg, ESMC_LOG_WARN);
        // TODO: return ESMF_WARNING when defined
        // rc = ESMF_WARNING;
      }
      // report how many characters were copied
      *tempNameLen = strlen(tempName);
    }

    if (timeStep  != ESMC_NULL_POINTER) *timeStep  = this->timeStep;
    if (startTime != ESMC_NULL_POINTER) *startTime = this->startTime;
    if (stopTime  != ESMC_NULL_POINTER) *stopTime  = this->stopTime;

    if (runDuration != ESMC_NULL_POINTER) {
      *runDuration = this->stopTime - this->startTime;
    }
    if (runTimeStepCount != ESMC_NULL_POINTER) {
      *runTimeStepCount = (this->stopTime - this->startTime) / this->timeStep;
    }

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

    if (calendar != ESMC_NULL_POINTER) {
      // get calendar from currTime, but could get from any other clock Time,
      //   since they all use the same calendar
      // TODO: use native C++ Get, not F90 entry point, when ready
      rc = this->currTime.ESMC_TimeGet((ESMC_I4 *)ESMC_NULL_POINTER,
                      ESMC_NULL_POINTER, ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                      ESMC_NULL_POINTER, ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                      ESMC_NULL_POINTER, ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                      ESMC_NULL_POINTER, ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                      ESMC_NULL_POINTER, ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                      ESMC_NULL_POINTER, ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                      ESMC_NULL_POINTER, ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                      calendar);
      ESMC_LogDefault.ESMC_LogMsgFoundError(rc,
                                         "ESMC_TimeGet(...calendar) failed.",
                                         &rc);
    }
    if (calendarType != ESMC_NULL_POINTER) {
      // get calendar type from currTime, but could get from any other clock
      // Time, since they all use the same calendar
      // TODO: use native C++ Get, not F90 entry point, when ready
      rc = this->currTime.ESMC_TimeGet((ESMC_I4 *)ESMC_NULL_POINTER,
                      ESMC_NULL_POINTER, ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                      ESMC_NULL_POINTER, ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                      ESMC_NULL_POINTER, ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                      ESMC_NULL_POINTER, ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                      ESMC_NULL_POINTER, ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                      ESMC_NULL_POINTER, ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                      ESMC_NULL_POINTER, ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                      ESMC_NULL_POINTER, calendarType);
      ESMC_LogDefault.ESMC_LogMsgFoundError(rc,
                                       "ESMC_TimeGet(...calendarType) failed.",
                                       &rc);
    }
    if (timeZone != ESMC_NULL_POINTER) {
      // get timeZone from currTime, but could get from any other clock Time,
      //   since they all are in the same timezone
      // TODO: use native C++ Get, not F90 entry point, when ready
      rc = this->currTime.ESMC_TimeGet((ESMC_I4 *)ESMC_NULL_POINTER,
                      ESMC_NULL_POINTER, ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                      ESMC_NULL_POINTER, ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                      ESMC_NULL_POINTER, ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                      ESMC_NULL_POINTER, ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                      ESMC_NULL_POINTER, ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                      ESMC_NULL_POINTER, ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                      ESMC_NULL_POINTER, ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                      ESMC_NULL_POINTER, ESMC_NULL_POINTER, timeZone);
      ESMC_LogDefault.ESMC_LogMsgFoundError(rc,
                                         "ESMC_TimeGet(...timeZone) failed.",
                                         &rc);
    }

    if (advanceCount != ESMC_NULL_POINTER) *advanceCount = this->advanceCount;
    if (alarmCount   != ESMC_NULL_POINTER) *alarmCount   = this->alarmCount;
    if (direction    != ESMC_NULL_POINTER) *direction    = this->direction;

    return(rc);

 } // end ESMC_ClockGet

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
      ESMC_TimeInterval *timeStep,               // in  - optional new timestep
      char              *ringingAlarmList1stElementPtr, // out - optional array
                                                 //        of ringing alarms
      char              *ringingAlarmList2ndElementPtr, // in - address of 2nd
                                                 //        element to calculate
                                                 //        F90 array element
                                                 //        size
      int                sizeofRingingAlarmList, // in  - size of given array
                                                 //       of ringing alarms
      int               *ringingAlarmCount) {    // out - number of ringing
                                                 //       alarms
//
// !DESCRIPTION:
//     Advances a clock's current time by timestep, then checks
//     each of its alarms to see if its time to ring.
//
//EOP
// !REQUIREMENTS:  TMG 3.4.1
 
 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_ClockAdvance()"

    int rc = ESMF_SUCCESS;

    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", &rc);
      return(rc);
    }

    if (direction == ESMF_MODE_FORWARD) {

      // save current time, then advance it
      prevTime = currTime;

      // use passed-in timestep if specified, otherwise use the clock's;
      // keep track of timeStep previously used in advance, to detect
      // direction (sign) change -- used for alarm consistency
      prevAdvanceTimeStep = currAdvanceTimeStep;
      currAdvanceTimeStep = (timeStep != ESMC_NULL_POINTER) ?
                            *timeStep : this->timeStep;
      currTime += currAdvanceTimeStep;

      // count number of timesteps
      advanceCount++;

    } else { // ESMF_MODE_REVERSE

      // TODO: make more robust by removing simplifying assumptions:
      //       1) timeSteps are constant throughout clock run.

      // Note:  Clocks can be stepped in reverse without first having been
      //        stepped forward.  This implies that the logic cannot use
      //        the prevTime state variable in order to step back; the
      //        currTime state variable must be reconstructed from timeStep.
      //        advanceCount represents the number of actual calls to
      //        ClockAdvance(), so if a clock is jumped forward via a
      //        ClockSet() and then reversed, the advanceCount does not
      //        account for the "missing" timeSteps.

      // step backwards; use passed-in timestep if specified, otherwise
      //   use the clock's prevTime
      currTime -= (timeStep != ESMC_NULL_POINTER) ? *timeStep : this->timeStep;

      // restore previous "previous" time
      // TODO:  remove assumption of constant timeStep; need to allow for
      //        variable timeSteps
      prevTime -= this->timeStep;

      // decrement timeStep count
      advanceCount--;

    }

    // TODO: validate (range check) new time against its calendar ?

    if (ringingAlarmCount != ESMC_NULL_POINTER) *ringingAlarmCount = 0;

    // Calculate element size of F90 array of ESMC_Alarm pointers since we
    // cannont depend on C++ element size to be the same as F90's across all
    // platforms.  It is assumed that all F90 platforms allocate arrays
    // contiguously and uniformly in memory, in either ascending or descending
    // address order.
    // TODO:  calculate once during ESMF_FrameworkInitialize() (runtime),
    //        since this is a platform constant. compile-time constant ?
    //        put into src/prologue directory.
    // see also ESMC_ClockGetAlarmList().

    int f90ArrayElementSize = 0; 
    if (ringingAlarmList1stElementPtr != ESMC_NULL_POINTER &&
        ringingAlarmList2ndElementPtr != ESMC_NULL_POINTER) {
        f90ArrayElementSize = (int)(ringingAlarmList2ndElementPtr -
                                    ringingAlarmList1stElementPtr);
    }

    // traverse alarm list (i) for ringing alarms (j)
    for(int i=0, j=0; i<alarmCount; i++) {
      int rc;
      bool ringing;

      // check each alarm to see if it's time to ring
      ringing = alarmList[i]->ESMC_AlarmCheckRingTime(&rc);

      // report ringing alarms if requested
      if (ringing) {
        // report number of ringing alarms
        if (ringingAlarmCount != ESMC_NULL_POINTER) (*ringingAlarmCount)++;

        // report ringing alarm list
        if (ringingAlarmList1stElementPtr != ESMC_NULL_POINTER) {
          if (j < sizeofRingingAlarmList) {
            // F90 equivalent: ringingAlarmList(j) = alarmList(i)
            //                 j = j + 1
            // calculate F90 array address for the j'th element ...
            char *f90ArrayElementJ;
            f90ArrayElementJ = ringingAlarmList1stElementPtr +
                               (j++ * f90ArrayElementSize);
            // ... then copy it in!
            *((ESMC_Alarm**)f90ArrayElementJ) = alarmList[i];
          } else {
            // list overflow!
            char logMsg[ESMF_MAXSTR];
            sprintf(logMsg, "For clock %s, "
                    "trying to report %dth ringing alarm, but given "
                    "ringingAlarmList array can only hold %d.",
                    this->name, j+1, sizeofRingingAlarmList);
            ESMC_LogDefault.ESMC_LogWrite(logMsg, ESMC_LOG_ERROR);
            rc = ESMF_FAILURE;
          }
        }
      }
    }

    return(rc);

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
//    checks if {\tt ESMC\_Clock}'s stop time has been reached.
//
//EOP
// !REQUIREMENTS:

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_ClockIsStopTime()"

    if (rc != ESMC_NULL_POINTER) *rc = ESMF_SUCCESS;

    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", rc);
      return(false);
    }

    if (!stopTimeEnabled) return(false);

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
// !IROUTINE:  ESMC_ClockStopTimeEnable - enables a Clock's stopTime to function
//
// !INTERFACE:
      int ESMC_Clock::ESMC_ClockStopTimeEnable(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_Time *stopTime) {              // in  - optional new stop time
//
// !DESCRIPTION:
//      ESMF routine which enables a {\tt ESMC\_Clock}'s stopTime to function.
//
//EOP
// !REQUIREMENTS:  WRF

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_ClockStopTimeEnable()"

    int rc = ESMF_SUCCESS;

    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", &rc);
      return(rc);
    }

    if (stopTime != ESMC_NULL_POINTER) {
      this->stopTime = *stopTime;
    }

    stopTimeEnabled = true;

    return(ESMF_SUCCESS);

 } // end ESMC_ClockStopTimeEnable

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ClockStopTimeDisable - disables a Clock's stopTime from functioning
//
// !INTERFACE:
      int ESMC_Clock::ESMC_ClockStopTimeDisable(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      ESMF routine which disables a {\tt ESMC\_Clock}'s stopTime from
//      functioning.
//
//EOP
// !REQUIREMENTS:  WRF

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_ClockStopTimeDisable()"

    int rc = ESMF_SUCCESS;

    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", &rc);
      return(rc);
    }

    stopTimeEnabled = false;

    return(ESMF_SUCCESS);

 } // end ESMC_ClockStopTimeDisable

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ClockIsStopTimeEnabled - check if Clock's stop time is enabled
//
// !INTERFACE:
      bool ESMC_Clock::ESMC_ClockIsStopTimeEnabled(
//
// !RETURN VALUE:
//    bool is stop time enabled or not
//
// !ARGUMENTS:
      int  *rc) const {        // out - error return code
//
// !DESCRIPTION:
//    checks if {\tt ESMC\_Clock}'s stop time is enabled.
//
//EOP
// !REQUIREMENTS:

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_ClockIsStopTimeEnabled()"

    if (rc != ESMC_NULL_POINTER) *rc = ESMF_SUCCESS;

    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", rc);
      return(false);
    }

    return(stopTimeEnabled);

 } // end ESMC_ClockIsStopTimeEnabled

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ClockIsDone - check if Clock's stop time or start time has been reached, depending on direction
//
// !INTERFACE:
      bool ESMC_Clock::ESMC_ClockIsDone(
//
// !RETURN VALUE:
//    bool is done or not
//
// !ARGUMENTS:
      int  *rc) const {        // out - error return code
//
// !DESCRIPTION:
//    Checks if {\tt ESMC\_Clock}'s stop time has been reached if in
//    {\tt ESMF\_MODE\_FORWARD} or if it has reached start time if in
//    {\tt ESMF\_MODE\_REVERSE}.
//
//EOP
// !REQUIREMENTS:

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_ClockIsDone()"

    if (rc != ESMC_NULL_POINTER) *rc = ESMF_SUCCESS;

    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", rc);
      return(false);
    }

    if (direction == ESMF_MODE_FORWARD) {

      if (!stopTimeEnabled) return(false);

      // check if stopTime has been reached or crossed

      // positive stopTime direction ?
      if (stopTime > startTime) {
        return(currTime >= stopTime);
  
      // or negative stopTime direction ?
      } else if (stopTime < startTime) {
        return(currTime <= stopTime);

      // or no stopTime direction ? (stopTime == startTime)
      } else return(currTime == stopTime);

    } else { // ESMF_MODE_REVERSE

      // check if startTime has been reached or crossed

      // positive stopTime direction ?
      if (stopTime > startTime) {
        return(currTime <= startTime);
  
      // or negative stopTime direction ?
      } else if (stopTime < startTime) {
        return(currTime >= startTime);

      // or no stopTime direction ? (stopTime == startTime)
      } else return(currTime == startTime);

    }

 } // end ESMC_ClockIsDone

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ClockIsReverse - test if clock is in reverse mode
//
// !INTERFACE:
      bool ESMC_Clock::ESMC_ClockIsReverse(
//
// !RETURN VALUE:
//    bool is reverse mode or not
//
// !ARGUMENTS:
      int  *rc) const {        // out - error return code
//
// !DESCRIPTION:
//    Checks if {\tt ESMC\_Clock}'s direction is {\tt ESMF\_MODE\_REVERSE}.
//
//EOP
// !REQUIREMENTS:

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_ClockIsReverse()"

    if (rc != ESMC_NULL_POINTER) *rc = ESMF_SUCCESS;

    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", rc);
      return(false);
    }

    return(direction == ESMF_MODE_REVERSE);

 } // end ESMC_ClockIsReverse

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ClockGetNextTime - calculates a Clock's next time 
//
// !INTERFACE:
      int ESMC_Clock::ESMC_ClockGetNextTime(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_Time         *nextTime,       // out
      ESMC_TimeInterval *timeStep) {     // in

// !DESCRIPTION:
//      Calculates what the next time of the {\tt ESMF\_Clock} will be, based
//      on the clock's current timestep or an optionally passed-in timestep.

//
//EOP
// !REQUIREMENTS:  

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_ClockGetNextTime()"

    int rc = ESMF_SUCCESS;

    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", &rc);
      return(rc);
    }

    if (timeStep != ESMC_NULL_POINTER) {
      // use passed-in timeStep if specified
      *nextTime = currTime + *timeStep;
    } else {
      // otherwise use clock's own timestep
      *nextTime = currTime + this->timeStep;
    }

    return(ESMF_SUCCESS);

 } // end ESMC_ClockGetNextTime

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
      int          nameLen,   // in  - the length of the alarm name
      char        *name,      // in  - the alarm "name" to get
      ESMC_Alarm **alarm) {   // out - the alarm named "name"
//
// !DESCRIPTION:
//     Retrieve's the clock's alarm named "name" from the alarm list
//
//EOP
// !REQUIREMENTS:  TMG x.x

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_ClockGetAlarm()"

    int rc = ESMF_SUCCESS;

    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", &rc);
      return(rc);
    }

    if (nameLen >= ESMF_MAXSTR) {
      char logMsg[ESMF_MAXSTR];
      sprintf(logMsg, "For alarm name %s, length >= ESMF_MAXSTR, "
                      "truncated.", name);
      ESMC_LogDefault.ESMC_LogWrite(logMsg, ESMC_LOG_WARN);
      // TODO: return ESMF_WARNING when defined
    }

    // TODO: use inherited methods from ESMC_Base
    char alarmName[ESMF_MAXSTR];
    strncpy(alarmName, name, nameLen);
    alarmName[nameLen] = '\0';  // null terminate

    // linear search for alarm name
    for(int i=0; i<alarmCount; i++) {
      if (strcmp(alarmName, alarmList[i]->name) == 0) {
        // found, return alarm
        *alarm = alarmList[i];
        return(ESMF_SUCCESS);
      }
    }

    // not found, return null ...
    *alarm = ESMC_NULL_POINTER;
    return(ESMF_FAILURE);

 } // end ESMC_ClockGetAlarm

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ClockGetAlarmList - Get a list of alarms from a clock
//
// !INTERFACE:
      int ESMC_Clock::ESMC_ClockGetAlarmList(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_AlarmListType type,                   // in - list type to get
      char              *alarmList1stElementPtr, // out - array of alarms
      char              *alarmList2ndElementPtr, // in  - address of 2nd
                                                 //        element to calculate
                                                 //        F90 array element
                                                 //        size
      int                sizeofAlarmList,        // in  - size of given array
                                                 //       of alarms
      int               *alarmCount,             // out - number of alarms
      ESMC_TimeInterval *timeStep) {             // in  - optional time step to
                                                 //         use instead of the
                                                 //         clock's (only use
                                                 //         with alarm list type
                                                 // ESMF_ALARMLIST_NEXTRINGING)
//
// !DESCRIPTION:
//    Gets an {\tt ESMF\_Clock}'s list of alarms.
//
//EOP
// !REQUIREMENTS:  TMG 4.3, 4.8

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_ClockGetAlarmList()"

    int rc = ESMF_SUCCESS;

    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", &rc);
      return(rc);
    }

    *alarmCount = 0;

    // Calculate element size of F90 array of ESMC_Alarm pointers since we
    // cannont depend on C++ element size to be the same as F90's across all
    // platforms.  It is assumed that all F90 platforms allocate arrays
    // contiguously and uniformly in memory, in either ascending or descending
    // address order.
    // TODO:  calculate once during ESMF_FrameworkInitialize() (runtime),
    //        since this is a platform constant. compile-time constant ?
    // see also ESMC_ClockAdvance().

    int f90ArrayElementSize = 0; 
    if (alarmList2ndElementPtr != ESMC_NULL_POINTER) {
        f90ArrayElementSize = (int)(alarmList2ndElementPtr -
                                    alarmList1stElementPtr);
    }

    // traverse clock's alarm list (i) for alarms to return in
    //   requested list (j)
    for(int i=0, j=0; i < this->alarmCount; i++) {
      int rc;
      bool returnAlarm;

      // based on requested list type, check if this (i'th) alarm is
      //   to be returned
      switch (type)
      {
        case ESMF_ALARMLIST_ALL:
          // return all alarms!
          returnAlarm = true;
          break;

        case ESMF_ALARMLIST_RINGING:
          // return alarm if it's ringing
          returnAlarm = alarmList[i]->ESMC_AlarmIsRinging(&rc);
          break;

        case ESMF_ALARMLIST_NEXTRINGING:
          // return alarm if it will ring upon the next clock time step
          returnAlarm = alarmList[i]->ESMC_AlarmWillRingNext(timeStep, &rc);
          break;

        case ESMF_ALARMLIST_PREVRINGING:
          // return alarm if it was ringing on the previous clock time step
          returnAlarm = alarmList[i]->ESMC_AlarmWasPrevRinging(&rc);
          break;

        default :
          // unknown alarm list type; return empty list
          char logMsg[ESMF_MAXSTR];
          sprintf(logMsg, "For clock %s, unknown alarm list type %d.",
                  this->name, type);
          ESMC_LogDefault.ESMC_LogWrite(logMsg, ESMC_LOG_ERROR);
          return(ESMF_FAILURE);
      }

      // copy alarm pointers to be returned into given F90 array
      if (returnAlarm) {
        // count and report number of returned alarms
        (*alarmCount)++;

        // copy if there's space in the given F90 array
        if (j < sizeofAlarmList) {
          // F90/C++ equivalent: AlarmList(j) = this->alarmList[i]
          //                 j = j + 1
          // calculate F90 array address for the j'th element ...
          char *f90ArrayElementJ;
          f90ArrayElementJ = alarmList1stElementPtr +
                                                (j++ * f90ArrayElementSize);
          // ... then copy it in!
          *((ESMC_Alarm**)f90ArrayElementJ) = alarmList[i];
        } else {
          // list overflow!
          char logMsg[ESMF_MAXSTR];
          sprintf(logMsg, "For clock %s, "
                  "trying to return %dth requested alarm, but given "
                  "alarmList array can only hold %d.",
                  this->name, j+1, sizeofAlarmList);
          ESMC_LogDefault.ESMC_LogWrite(logMsg, ESMC_LOG_ERROR);
          rc = ESMF_FAILURE;
        }
      }
    }
    return(rc);

 } // end ESMC_ClockGetAlarmList

#if 0
//
// possible native C++ versions
//
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
      int          *alarmCount) const {   // out - number of alarms in list
//
// !DESCRIPTION:
//     Get a clock's alarm list and number of alarms
//
//EOP
// !REQUIREMENTS:  TMG 4.3

    // validate inputs
    if (alarmList == ESMC_NULL_POINTER || alarmCount == ESMC_NULL_POINTER) {
      return(ESMF_FAILURE);
    }

    // copy the list of alarm pointers
    for(int i=0; i<this->alarmCount; i++) {
      (*alarmList)[i] = this->alarmList[i];  
    }

    // return number of alarms
    *alarmCount = this->alarmCount;

    return(ESMF_SUCCESS);

 } // end ESMC_ClockGetAlarmList

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
    if (i < 1 || i > alarmCount || alarm == ESMC_NULL_POINTER) {
      return(ESMF_FAILURE);
    }

    int rc;
    for (int j=0, r=0; j < alarmCount; j++) {
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
      ESMC_Alarm **alarmList,            // out - alarm list
      int         *alarmCount) const {   // out - number of alarms in list
//
// !DESCRIPTION:
//     Get a clock's alarm list and number of alarms
//
//EOP
// !REQUIREMENTS:  TMG 4.3

    // validate inputs
    if (alarmList == ESMC_NULL_POINTER || alarmCount == ESMC_NULL_POINTER) {
      return(ESMF_FAILURE);
    }

    // copy the list of alarms
    for(int i=0; i<this->alarmCount; i++) {
      (*alarmList)[i] = *(this->alarmList[i]);  
    }

    // return number of alarms
    *alarmCount = this->alarmCount;

    return(ESMF_SUCCESS);

 } // end ESMC_ClockGetAlarmList
#endif

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

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_ClockSyncToRealTime()"

    int rc = ESMF_SUCCESS;

    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", &rc);
      return(rc);
    }

    // set current time to wall clock time
    // TODO:  ensure current time is within startTime and stopTime
    rc = currTime.ESMC_TimeSyncToRealTime();
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &rc))
      return(rc);
    return(ESMC_ClockValidate());

 } // end ESMC_ClockSyncToRealTime

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Clock(=) - assignment operator
//
// !INTERFACE:
      ESMC_Clock& ESMC_Clock::operator=(
//
// !RETURN VALUE:
//    ESMC_Clock& result
//
// !ARGUMENTS:
      const ESMC_Clock &clock) {   // in - ESMC_Clock to copy
//
// !DESCRIPTION:
//      Assign current object's (this) {\tt ESMC\_Clock} with given
//      {\tt ESMC\_Clock}.  
//EOP
// !REQUIREMENTS:  

    // check for self-assignment
    if (&clock != this) {

      // reallocate alarmList if not same size
      if (alarmListCapacity != clock.alarmListCapacity) {
        delete [] alarmList;
        try {
          alarmList = new ESMC_AlarmPtr[clock.alarmListCapacity];
        }
        catch (...) {
          ESMC_LogDefault.ESMC_LogAllocError(ESMC_NULL_POINTER);
          return(*this);  // TODO:  throw exception
        }
        alarmListCapacity = clock.alarmListCapacity;
      }
  
      // copy alarm list (array of pointers)
      for(int i=0; i<clock.alarmCount; i++) {
        alarmList[i] = clock.alarmList[i];
      }
      alarmCount = clock.alarmCount;

      // copy all other members
      strcpy(name,   clock.name);
      timeStep     = clock.timeStep;
      startTime    = clock.startTime;
      stopTime     = clock.stopTime;
      refTime      = clock.refTime;
      currTime     = clock.currTime;
      prevTime     = clock.prevTime;
      advanceCount = clock.advanceCount;
      direction    = clock.direction;
      stopTimeEnabled = clock.stopTimeEnabled;
      id           = clock.id;

      // copy = true;   // TODO: Unique copy ? (id = ++count) (review operator==
                        //       and operator!=)  Must do same in assignment
                        //       overloaded method and interface from F90.
                        //       Also, inherit from ESMC_Base class.
                        //       See also copy constructor
    }

    return(*this);

}  // end ESMC_Clock::operator=

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Clock(==) - Clock equality comparison
// 
// !INTERFACE:
      bool ESMC_Clock::operator==(
//   
// !RETURN VALUE:
//    bool result
//
// !ARGUMENTS:
      const ESMC_Clock &clock) const {   // in - ESMC_Clock to compare
//
// !DESCRIPTION:
//      Compare for equality the current object's (this) {\tt ESMC\_Clock} with
//      given {\tt ESMC\_Clock}, return result.  Comparison is based on IDs.
//   
//EOP
// !REQUIREMENTS:

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_Clock::operator==()"

    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", ESMC_NULL_POINTER);
      return(false);
    }

    return(id == clock.id);

}  // end ESMC_Clock::operator==

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Clock(!=) - Clock inequality comparison
// 
// !INTERFACE:
      bool ESMC_Clock::operator!=(
//   
// !RETURN VALUE:
//    bool result
//
// !ARGUMENTS:
      const ESMC_Clock &clock) const {   // in - ESMC_Clock to compare
//
// !DESCRIPTION:
//      Compare for inequality the current object's (this)
//      {\tt ESMC\_Clock} with given {\tt ESMC\_Clock}, return result.
//      Comparison is based on IDs.
//   
//EOP
// !REQUIREMENTS:

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_Clock::operator!=()"

    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", ESMC_NULL_POINTER);
      return(false);
    }

    return(id != clock.id);

}  // end ESMC_Clock::operator!=

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ClockReadRestart - restore contents of a Clock
//
// !INTERFACE:
      ESMC_Clock *ESMC_ClockReadRestart(
//
// !RETURN VALUE:
//    pointer to newly allocated and restored ESMC_Clock
//
// !ARGUMENTS:
      int          nameLen,  // in
      const char  *name,     // in
      ESMC_IOSpec *iospec,   // in
      int         *rc ) {    // out - return code

//
// !DESCRIPTION:
//      Restore information about an {\tt ESMC\_Clock}.
//      For persistence/checkpointing.
// 
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_ClockReadRestart()"

    // TODO:  read clock state from iospec/name, then allocate/restore
    //        (share code with ESMC_ClockCreate()).

    return(ESMC_NULL_POINTER);

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
      ESMC_IOSpec *iospec) const {
//
// !DESCRIPTION:
//      Save information about an {\tt ESMC\_Clock}.
//      For persistence/checkpointing
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_ClockWriteRestart()"

    int rc = ESMF_SUCCESS;

    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", &rc);
      return(rc);
    }

    // TODO:  save clock state using iospec/name.  Default to disk file.

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

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_ClockValidate()"

    int rc = ESMF_SUCCESS;

    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", &rc);
      return(rc);
    }

    // validate required individual properties
    if(ESMC_LogDefault.ESMC_LogMsgFoundError(timeStep.ESMC_TimeIntervalValidate(),
                          "timeStep.ESMC_TimeIntervalValidate() failed", &rc) ||
       ESMC_LogDefault.ESMC_LogMsgFoundError(startTime.ESMC_TimeValidate(),
                                 "startTime.ESMC_TimeValidate() failed", &rc) ||
       ESMC_LogDefault.ESMC_LogMsgFoundError(refTime.ESMC_TimeValidate(),
                                 "refTime.ESMC_TimeValidate() failed", &rc)   ||
       ESMC_LogDefault.ESMC_LogMsgFoundError(currTime.ESMC_TimeValidate(),
                                 "currTime.ESMC_TimeValidate() failed", &rc)  ||
       ESMC_LogDefault.ESMC_LogMsgFoundError(prevTime.ESMC_TimeValidate(),
                                 "prevTime.ESMC_TimeValidate() failed", &rc)) {
       return(rc);
    }

    if (direction != ESMF_MODE_FORWARD && direction != ESMF_MODE_REVERSE) {
      char logMsg[ESMF_MAXSTR];
      sprintf(logMsg, "direction property %d is not ESMF_MODE_FORWARD or "
              "ESMF_MODE_REVERSE", direction);
      ESMC_LogDefault.ESMC_LogWrite(logMsg, ESMC_LOG_ERROR);
      return(ESMF_FAILURE);
    }

    // validate optional stopTime property if set
    if (stopTimeEnabled) {
      if(ESMC_LogDefault.ESMC_LogMsgFoundError(stopTime.ESMC_TimeValidate(),
                                     "stopTime.ESMC_TimeValidate() failed",
                                     &rc)) return(rc);

//  The following fixes bugs 801366, 801409, & 806784

      // startTime and stopTime calendars should generally be the same.
      //   (Conceptually could be different, if based on same zero-point).
      //   So only produce ESMC_LOG_WARN if different.
      //   (TODO: check only if stopTime set)
      // TODO: use native C++ Get, not F90 entry point, when ready
      ESMC_Calendar *startCal, *stopCal;
      int rc = startTime.ESMC_TimeGet((ESMC_I4 *)ESMC_NULL_POINTER, 
                          ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                          ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                          ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                          ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                          ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                          ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                          ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                          ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                          ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                          ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                          ESMC_NULL_POINTER, &startCal);

      if(ESMC_LogDefault.ESMC_LogMsgFoundError(rc,
         "startTime.ESMC_TimeGet(...startCal) failed.", &rc)) {
        return(rc);   
      }

      rc = stopTime.ESMC_TimeGet((ESMC_I4 *)ESMC_NULL_POINTER, 
                          ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                          ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                          ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                          ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                          ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                          ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                          ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                          ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                          ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                          ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                          ESMC_NULL_POINTER, &stopCal);

      if(ESMC_LogDefault.ESMC_LogMsgFoundError(rc,
         "stopTime.ESMC_TimeGet(...stopCal) failed.", &rc)) {
        return(rc);   
      }

      if (startCal == ESMC_NULL_POINTER || stopCal == ESMC_NULL_POINTER) {
        ESMC_LogDefault.ESMC_LogWrite("startCal or stopCal is NULL.",
                                      ESMC_LOG_ERROR);
        return(ESMF_FAILURE);
      }

      if (*startCal != *stopCal) {
        ESMC_LogDefault.ESMC_LogWrite("startCal not equal to stopCal.",
                                      ESMC_LOG_WARN);
        return(ESMC_RC_OBJ_BAD); 
      }

      ESMC_TimeInterval zeroTimeStep(0,0,1,0,0,0);

      // The following checks only produce ESMC_LOG_WARN because
      // the user may want or need to do these things.

      // check if current time is out-of-range
      if (stopTime > startTime) {
        if (currTime < startTime || currTime > stopTime) {
          ESMC_LogDefault.ESMC_LogWrite("currTime out-of-range (startTime to "
                                        "stopTime).", ESMC_LOG_WARN);
          return(ESMC_RC_VAL_OUTOFRANGE);
        }
        if (currTime == startTime && timeStep < zeroTimeStep) {
          ESMC_LogDefault.ESMC_LogWrite("timeStep negative for positive "
                                        "startTime to stopTime range).",
                                        ESMC_LOG_WARN);
          return(ESMC_RC_VAL_OUTOFRANGE);
        }
      } else if (stopTime < startTime) {
        if (currTime > startTime || currTime < stopTime) {
          ESMC_LogDefault.ESMC_LogWrite("currTime out-of-range (startTime to "
                                        "stopTime).", ESMC_LOG_WARN);
          return(ESMC_RC_VAL_OUTOFRANGE);
        }
        if (currTime == startTime && timeStep > zeroTimeStep) {
          ESMC_LogDefault.ESMC_LogWrite("timeStep positive for negative "
                                        "startTime to stopTime range).",
                                        ESMC_LOG_WARN);
          return(ESMC_RC_VAL_OUTOFRANGE);
        }
      } else { // stopTime == startTime
        ESMC_LogDefault.ESMC_LogWrite("stopTime equals startTime.",
                                      ESMC_LOG_WARN);
        return(ESMC_RC_VAL_WRONG);
      }

      // check for zero time step
      if(timeStep == zeroTimeStep) {
        ESMC_LogDefault.ESMC_LogWrite("timeStep equals zero.", ESMC_LOG_WARN);
        return(ESMC_RC_VAL_WRONG);
      }

      // note:  don't check prevTime relative to currTime, as user could
      //        change direction with a variable time step.  Also, prevTime
      //        is exclusively maintained internally.

    } // endif stopTimeEnabled


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

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_ClockPrint()"

    int rc = ESMF_SUCCESS;

    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", &rc);
      return(rc);
    }

    printf("Clock ----------------------------------\n");

    // print out individually selected components
    // TODO: enable multiple simultaneous options (token parsing)
    //       (currently mutually exclusive)
    if (options != ESMC_NULL_POINTER) {

      // make options case insensitive
      // TODO: put this into function to share
      char opts[ESMF_MAXSTR];
      int i;
      for(i=0; i<strlen(options) && i<ESMF_MAXSTR-1; i++) {
        opts[i] = tolower(options[i]);
      }
      opts[i] = '\0';

      if (strncmp(opts, "name", 4) == 0) {
        printf("name = %s\n", name);
      }
      else if (strncmp(opts, "timestep", 8) == 0) {
        printf("timeStep = \n");
        // TODO:  timeStep.ESMC_TimeIntervalPrint(&opts(8)); ?
        if (strstr(opts, "string") != ESMC_NULL_POINTER) {
          timeStep.ESMC_TimeIntervalPrint("string");
        } else {
          timeStep.ESMC_TimeIntervalPrint();
        }
      }
      else if (strncmp(opts, "starttime", 9) == 0) {
        printf("startTime = \n");
        if (strstr(opts, "string") != ESMC_NULL_POINTER) {
          startTime.ESMC_TimePrint("string");
        } else {
          startTime.ESMC_TimePrint();
        }
      }
      else if (strncmp(opts, "stoptime", 8) == 0) {
        printf("stopTime = \n");
        if (strstr(opts, "string") != ESMC_NULL_POINTER) {
          stopTime.ESMC_TimePrint("string");
        } else {
          stopTime.ESMC_TimePrint();
        }
      }
      else if (strncmp(opts, "stoptimeenabled", 15) == 0) {
        printf("stopTimeEnabled = %s\n", stopTimeEnabled ? "true" : "false");
      }
      else if (strncmp(opts, "reftime", 7) == 0) {
        printf("refTime = \n");
        if (strstr(opts, "string") != ESMC_NULL_POINTER) {
          refTime.ESMC_TimePrint("string");
        } else {
          refTime.ESMC_TimePrint();
        }
      }
      else if (strncmp(opts, "currtime", 8) == 0) {
        printf("currTime = \n");
        if (strstr(opts, "string") != ESMC_NULL_POINTER) {
          currTime.ESMC_TimePrint("string");
        } else {
          currTime.ESMC_TimePrint();
        }
      }
      else if (strncmp(opts, "prevtime", 8) == 0) {
        printf("prevTime = \n");
        if (strstr(opts, "string") != ESMC_NULL_POINTER) {
          prevTime.ESMC_TimePrint("string");
        } else {
          prevTime.ESMC_TimePrint();
        }
      }
      else if (strncmp(opts, "advancecount", 12) == 0) {
        printf("advanceCount = %lld\n", advanceCount);
      }
      else if (strncmp(opts, "direction", 9) == 0) {
        printf("direction = %d\n", direction);
      }
      else if (strncmp(opts, "alarmcount", 10) == 0) {
        printf("alarmCount = %d\n", alarmCount);
      }
      else if (strncmp(opts, "alarmlist", 9) == 0) {
        printf("alarmList = \n");
        for (int i=0; i<alarmCount; i++) {
          alarmList[i]->ESMC_AlarmPrint(&opts[9]);
        }
      }
    } 

    if (options == ESMC_NULL_POINTER || strncmp(options, "string", 6) == 0) {

      // default:  print out all properties

      printf("name = %s\n", name);
      printf("timeStep = \n");  timeStep.ESMC_TimeIntervalPrint(options);
      printf("startTime = \n"); startTime.ESMC_TimePrint(options);
      printf("stopTime = \n");  stopTime.ESMC_TimePrint(options);
      printf("stopTimeEnabled = %s\n", stopTimeEnabled ? "true" : "false");
      printf("refTime = \n");   refTime.ESMC_TimePrint(options);
      printf("currTime = \n");  currTime.ESMC_TimePrint(options);
      printf("prevTime = \n");  prevTime.ESMC_TimePrint(options);
      printf("advanceCount = %lld\n", advanceCount);
      printf("direction = %d\n", direction);
      printf("alarmCount = %d\n", alarmCount);
      printf("alarmList = \n");
      for (int i=0; i<alarmCount; i++) {
        alarmList[i]->ESMC_AlarmPrint(options);
      }
    }

    printf("end Clock ------------------------------\n\n");

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
//      Initializes for either C++ or F90, since {\tt ESMC\_Clock} is a deep,
//      dynamically allocated class.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_Clock::ESMC_Clock() native constructor"

    // allocate the clock's alarm list (array of pointers)
    try {
      alarmList = new ESMC_AlarmPtr[ESMF_ALARM_BLOCK_SIZE];
    }
    catch (...) {
      ESMC_LogDefault.ESMC_LogAllocError(ESMC_NULL_POINTER);
      return;
    }
    alarmCount = 0;
    alarmListCapacity = ESMF_ALARM_BLOCK_SIZE;

    name[0] = '\0';
    advanceCount = 0;
    direction = ESMF_MODE_FORWARD;
    stopTimeEnabled = false;
    id = ++count;  // TODO: inherit from ESMC_Base class
    // copy = false;  // TODO: see notes in constructors and destructor below

 } // end ESMC_Clock

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Clock - native C++ copy constructor
//
// !INTERFACE:
      ESMC_Clock::ESMC_Clock(
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
      const ESMC_Clock &clock) {  // in - clock to copy
//
// !DESCRIPTION:
//      Copies members of given clock.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_Clock::ESMC_Clock() copy constructor"

    // allocate the new clock's own alarm list (array of pointers)
    try {
      alarmList = new ESMC_AlarmPtr[clock.alarmListCapacity];
    }
    catch (...) {
      ESMC_LogDefault.ESMC_LogAllocError(ESMC_NULL_POINTER);
      return;
    }
    alarmListCapacity = clock.alarmListCapacity;

    // memberwise copy (invokes overloaded assignment operator=)
    *this = clock;

    // copy = true;   // TODO: Unique copy ? (id = ++count) (review operator==
                      //       and operator!=)  Must do same in assignment
                      //       overloaded method and interface from F90.
                      //       Also, inherit from ESMC_Base class.
                      //       See also overloaded assignment operator=

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

  delete [] alarmList;

  // TODO: Decrement static count for one less object; but don't decrement   //       for copies.  Must create and set a copy flag property to detect.
  //       Also must set copy flag in copy constructor and overloaded
  //       assignment method, and provide interface from F90.
  // if (!copy) count--;

 } // end ~ESMC_Clock

//-------------------------------------------------------------------------
// Private methods
//-------------------------------------------------------------------------

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
//     Adds given alarm to a clock's alarm list. 
//     Used by {\tt ESMC\_AlarmCreate().}
//
//EOP
// !REQUIREMENTS:  TMG 4.1, 4.2

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_ClockAddAlarm()"

    int rc = ESMF_SUCCESS;

    // validate inputs

    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", &rc);
      return(rc);
    }

    if (alarm == ESMC_NULL_POINTER) {
      char logMsg[ESMF_MAXSTR];
      sprintf(logMsg, "For clock %s, given alarm is NULL.", this->name);
      ESMC_LogDefault.ESMC_LogWrite(logMsg, ESMC_LOG_ERROR);
      return(ESMF_FAILURE);
    }

    // if alarm list full, re-allocate it
    if (alarmCount == alarmListCapacity) {
      char logMsg[ESMF_MAXSTR];
      sprintf(logMsg, "For clock %s, alarm list is full (%d alarms), "
              "re-allocating to hold %d alarms.",
                this->name, alarmListCapacity, 
                alarmListCapacity+ESMF_ALARM_BLOCK_SIZE);
      ESMC_LogDefault.ESMC_LogWrite(logMsg, ESMC_LOG_INFO);

      // re-allocate clock's alarm list to next block size
      ESMC_Alarm **tempList;
      try {
        tempList = new ESMC_AlarmPtr[alarmListCapacity + ESMF_ALARM_BLOCK_SIZE];
      }
      catch (...) {
        ESMC_LogDefault.ESMC_LogAllocError(&rc);
        return(rc);
      }

      // copy alarm list to re-allocated array
      for(int i=0; i<alarmCount; i++) {
        tempList[i] = alarmList[i];
      }

      // deallocate the old alarmList and reset to the new one
      delete [] alarmList;
      alarmList = tempList;
      alarmListCapacity += ESMF_ALARM_BLOCK_SIZE;
    }

    // append given alarm to list and count it
    alarmList[alarmCount++] = alarm;

    // check new alarm to see if it's time to ring
    alarm->ESMC_AlarmCheckRingTime(&rc);

    return(rc);

 } // end ESMC_ClockAddAlarm
