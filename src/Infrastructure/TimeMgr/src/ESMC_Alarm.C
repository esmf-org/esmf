// $Id: ESMC_Alarm.C,v 1.61.2.6 2009/01/21 21:25:23 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
// ESMC Alarm method code (body) file
//
//-------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ {\tt ESMC\_Alarm} methods declared
// in the companion file {\tt ESMC\_Alarm.h)
//
//-------------------------------------------------------------------------
//
 #define ESMC_FILENAME "ESMC_Alarm.C"

 // insert any higher level, 3rd party or system includes here
 #include <stdio.h>
 #include <string.h>
 #include <ctype.h>

 #include <ESMC_LogErr.h>
 #include <ESMF_LogMacros.inc>
 #include <ESMC_Clock.h>

 // associated class definition file
 #include <ESMC_Alarm.h>

//-------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_Alarm.C,v 1.61.2.6 2009/01/21 21:25:23 cdeluca Exp $";
//-------------------------------------------------------------------------

// initialize static alarm instance counter
// TODO: inherit from ESMC_Base class
int ESMC_Alarm::count=0;

//
//-------------------------------------------------------------------------
//-------------------------------------------------------------------------
//
// This section includes all the Alarm routines
//
//

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AlarmCreate - allocates and initializes an Alarm object
//
// !INTERFACE:
      ESMC_Alarm *ESMC_AlarmCreate(
//
// !RETURN VALUE:
//    pointer to newly allocated ESMC_Alarm
//
// !ARGUMENTS:
      int                nameLen,           // in
      const char        *name,              // in
      ESMC_Clock        *clock,             // in
      ESMC_Time         *ringTime,          // in
      ESMC_TimeInterval *ringInterval,      // in
      ESMC_Time         *stopTime,          // in
      ESMC_TimeInterval *ringDuration,      // in
      int               *ringTimeStepCount, // in
      ESMC_Time         *refTime,           // in
      bool              *enabled,           // in
      bool              *sticky,            // in
      int               *rc ) {             // out - return code
//
// !DESCRIPTION:
//      ESMF routine which allocates and initializes {\tt ESMC\_Alarm} values.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_AlarmCreate(new)"

    ESMC_Alarm *alarm;
    int returnCode;

    // default return code
    if (rc != ESMC_NULL_POINTER) *rc = ESMC_RC_NOT_IMPL;

    if (ringTime == ESMC_NULL_POINTER && ringInterval == ESMC_NULL_POINTER) {
      ESMC_LogDefault.ESMC_LogWrite("Must specify at least one of ringTime or "
                                    "ringInterval.", ESMC_LOG_ERROR);
      return(ESMC_NULL_POINTER);
    }
 
    try {
      alarm = new ESMC_Alarm;
    }
    catch (...) {
      ESMC_LogDefault.ESMC_LogAllocError(rc);
      return(ESMC_NULL_POINTER);
    }
    
    // associate this alarm with given clock
    alarm->clock = clock;

    // TODO: use inherited methods from ESMC_Base
    if (name != ESMC_NULL_POINTER) {
      if (nameLen < ESMF_MAXSTR) {
        strncpy(alarm->name, name, nameLen);
        alarm->name[nameLen] = '\0';  // null terminate
      } else {
        // truncate
        strncpy(alarm->name, name, ESMF_MAXSTR-1);
        alarm->name[ESMF_MAXSTR-1] = '\0';  // null terminate

        char logMsg[ESMF_MAXSTR];
        sprintf(logMsg, "alarm name %s, length >= ESMF_MAXSTR; truncated.", 
                name);
        ESMC_LogDefault.ESMC_LogWrite(logMsg, ESMC_LOG_WARN);
        // TODO: return ESMF_WARNING when defined
        // if (rc != ESMC_NULL_POINTER) *rc = ESMF_WARNING;
      }
    } else {
      // create default name "AlarmNNN"
      sprintf(alarm->name, "Alarm%3.3d\0", alarm->id);
    }

    if (ringTime != ESMC_NULL_POINTER) {
      alarm->ringTime = alarm->prevRingTime = alarm->firstRingTime = *ringTime;
    }
    if (ringInterval != ESMC_NULL_POINTER) {
      alarm->ringInterval = *ringInterval;

      // if ringTime not specified, calculate
      //   ringTime from the current clock time

      // TODO: handle case where *ringTime < clock->currTime;
      //       same or similar to refTime

      if (ringTime == ESMC_NULL_POINTER) {
        // works for positive or negative ringInterval
        alarm->ringTime = clock->currTime + alarm->ringInterval;
        alarm->prevRingTime = alarm->firstRingTime = alarm->ringTime;
      }
    }
    if (stopTime != ESMC_NULL_POINTER) {
      alarm->stopTime = *stopTime;
    }
    if (ringDuration != ESMC_NULL_POINTER) {
      alarm->ringDuration = *ringDuration;
    }
    if (ringTimeStepCount != ESMC_NULL_POINTER) {
      alarm->ringTimeStepCount = *ringTimeStepCount;
    }
    if (refTime != ESMC_NULL_POINTER) {
      alarm->refTime = *refTime;
      // TODO:  for ringInterval, calculate 1st ringTime > clock->currTime,
      //        (&& > ringTime, if specified), using refTime as the base.
    } else {
      // TODO:  default to clock's current time (or ringTime, if specified?)
    }
    if (enabled != ESMC_NULL_POINTER) {
      alarm->enabled = *enabled;
    }
    if (sticky != ESMC_NULL_POINTER) {
      alarm->sticky = *sticky;
    }

    // TODO:  invoke private method, shared with ESMC_AlarmSet(), to calculate
    //        first ringTime for interval alarms, given ringInterval and none,
    //        one or both of refTime and ringTime.  Will replace logic in
    //        corresponding above sections.
    //        this->ringTime > clock->currTime &&
    //        this->ringTime > (passed in) ringTime

    returnCode = alarm->ESMC_AlarmValidate();
    ESMC_LogDefault.ESMC_LogMsgFoundError(returnCode, ESMF_ERR_PASSTHRU, rc);

    // add this new valid alarm to the given clock
    if (returnCode == ESMF_SUCCESS) {
      returnCode = clock->ESMC_ClockAddAlarm(alarm);
      ESMC_LogDefault.ESMC_LogMsgFoundError(returnCode, ESMF_ERR_PASSTHRU, rc);
    }

    return(alarm);

 } // end ESMC_AlarmCreate (new)

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AlarmCreate- Creates a copy of an alarm
//
// !INTERFACE:
      ESMC_Alarm *ESMC_AlarmCreate(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMC_Alarm
//
// !ARGUMENTS:
      ESMC_Alarm *alarm,  // in  - alarm to copy
      int        *rc) {   // out - return code 

// !DESCRIPTION:
//      Creates a new copy of the given alarm.
//
//EOP
// !REQUIREMENTS:

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_AlarmCreate(copy)"

    ESMC_Alarm *alarmCopy;
    int returnCode;

    // default return code
    if (rc != ESMC_NULL_POINTER) *rc = ESMC_RC_NOT_IMPL;

    // can't copy a non-existent object
    if (alarm == ESMC_NULL_POINTER) {
      ESMC_LogDefault.ESMC_LogWrite("Can't copy a non-existent alarm",
                                    ESMC_LOG_ERROR);
      return(ESMC_NULL_POINTER);
    }

    try {
      // allocate new alarm and pass given alarm to copy constructor.
      alarmCopy = new ESMC_Alarm(*alarm);
    }
    catch (...) {
      ESMC_LogDefault.ESMC_LogAllocError(rc);
      return(ESMC_NULL_POINTER);
    }

    returnCode = alarmCopy->ESMC_AlarmValidate();
    ESMC_LogDefault.ESMC_LogMsgFoundError(returnCode,
                                          ESMF_ERR_PASSTHRU, rc);
    return(alarmCopy);     

 } // end ESMC_AlarmCreate (copy)

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AlarmDestroy - free a Alarm created with Create
//
// !INTERFACE:
      int ESMC_AlarmDestroy(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_Alarm **alarm) {  // in - ESMC_Alarm to destroy
//
// !DESCRIPTION:
//      ESMF routine which destroys a Alarm object previously allocated
//      via an {\tt ESMC\_AlarmCreate} routine.  Define for deep classes only.
//
//EOP

  // Initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;

  // TODO: alarm->ESMC_AlarmDestruct(); constructor calls it!
  delete *alarm;   // ok to delete null pointer
  *alarm = ESMC_NULL_POINTER;
  rc = ESMF_SUCCESS;
  return(rc);

 } // end ESMC_AlarmDestroy

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AlarmSet - Sets an Alarm's properties
//
// !INTERFACE:
      int ESMC_Alarm::ESMC_AlarmSet(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int                nameLen,           // in
      const char        *name,              // in
      ESMC_Clock       **clock,             // in
      ESMC_Time         *ringTime,          // in
      ESMC_TimeInterval *ringInterval,      // in
      ESMC_Time         *stopTime,          // in
      ESMC_TimeInterval *ringDuration,      // in
      int               *ringTimeStepCount, // in
      ESMC_Time         *refTime,           // in
      bool              *ringing,           // in
      bool              *enabled,           // in
      bool              *sticky) {          // in
//
// !DESCRIPTION:
//      ESMF routine which only initializes {\tt ESMC\_Alarm} values;
//      it does not allocate any resources.  
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_AlarmSet()"

  // Initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;
 
    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", &rc);
      return(rc);
    }

    // save current values to restore in case of failure
    ESMC_Alarm saveAlarm = *this;

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
        sprintf(logMsg, "alarm name %s, length >= ESMF_MAXSTR; truncated.", 
                name);
        ESMC_LogDefault.ESMC_LogWrite(logMsg, ESMC_LOG_WARN);
        // TODO: return ESMF_WARNING when defined
        // rc = ESMF_WARNING;
      }
    }

    if (clock != ESMC_NULL_POINTER) {
      this->clock = *clock;
    }
    if (ringTime != ESMC_NULL_POINTER) {
      if (this->ringTime != *ringTime) {
        this->ringTime = *ringTime;
        this->prevRingTime = this->ringTime;
        this->userChangedRingTime = true;
      }
    }
    if (ringInterval != ESMC_NULL_POINTER) {
      if (this->ringInterval != *ringInterval) {
        this->ringInterval = *ringInterval;
        this->userChangedRingInterval = true;
      }
    }
    if (stopTime != ESMC_NULL_POINTER) {
      this->stopTime = *stopTime;
    }
    if (ringDuration != ESMC_NULL_POINTER) {
      this->ringDuration = *ringDuration;
    }
    if (ringTimeStepCount != ESMC_NULL_POINTER) {
      this->ringTimeStepCount = *ringTimeStepCount;
    }
    if (refTime != ESMC_NULL_POINTER) {
      this->refTime = *refTime;
    }
    if (ringing != ESMC_NULL_POINTER) {
      this->ringing = *ringing;
    }
    if (enabled != ESMC_NULL_POINTER) {
      this->enabled = *enabled;
    }
    if (sticky != ESMC_NULL_POINTER) {
      this->sticky = *sticky;
    }
     
    // TODO:  invoke private method, shared with ESMC_AlarmCreate(), to
    //        calculate next ringTime for interval alarms, given ringInterval
    //        and none, one or both of refTime and ringTime.  Will replace
    //        logic in corresponding above sections.
    //        this->ringTime > clock->currTime &&
    //        this->ringTime > (passed in) ringTime

    rc = ESMC_AlarmValidate();
    if (ESMC_LogDefault.ESMC_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &rc)) {
      // restore original alarm values
      *this = saveAlarm;
    }

    rc = ESMF_SUCCESS;
    return(rc);

 } // end ESMC_AlarmSet

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AlarmGet - Gets an alarm's properties
//
// !INTERFACE:
      int ESMC_Alarm::ESMC_AlarmGet(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int                nameLen,                // in
      int               *tempNameLen,            // out
      char              *tempName,               // out
      ESMC_Clock       **clock,                  // out
      ESMC_Time         *ringTime,               // out
      ESMC_Time         *prevRingTime,           // out
      ESMC_TimeInterval *ringInterval,           // out
      ESMC_Time         *stopTime,               // out
      ESMC_TimeInterval *ringDuration,           // out
      int               *ringTimeStepCount,      // out
      int               *timeStepRingingCount,   // out
      ESMC_Time         *ringBegin,              // out
      ESMC_Time         *ringEnd,                // out
      ESMC_Time         *refTime,                // out
      bool              *ringing,                // out
      bool              *ringingOnPrevTimeStep,  // out
      bool              *enabled,                // out
      bool              *sticky) {               // out
//
// !DESCRIPTION:
//      Gets {\tt ESMC\_Alarm} property values;
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_AlarmGet()"

  // Initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;

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
        sprintf(logMsg, "For alarm name %s, "
                "length >= given character array; truncated.", this->name);
        ESMC_LogDefault.ESMC_LogWrite(logMsg, ESMC_LOG_WARN);
        // TODO: return ESMF_WARNING when defined
        // rc = ESMF_WARNING;
      }
      // report how many characters were copied
      *tempNameLen = strlen(tempName);
    }

    if (clock != ESMC_NULL_POINTER) {
      *clock = this->clock;
    }
    if (ringTime != ESMC_NULL_POINTER) {
      *ringTime = this->ringTime;
    }
    if (prevRingTime != ESMC_NULL_POINTER) {
      *prevRingTime = this->prevRingTime;
    }
    if (ringInterval != ESMC_NULL_POINTER) {
      *ringInterval = this->ringInterval;
    }
    if (stopTime != ESMC_NULL_POINTER) {
      *stopTime = this->stopTime;
    }
    if (ringDuration != ESMC_NULL_POINTER) {
      *ringDuration = this->ringDuration;
    }
    if (ringTimeStepCount != ESMC_NULL_POINTER) {
      *ringTimeStepCount = this->ringTimeStepCount;
    }
    if (timeStepRingingCount != ESMC_NULL_POINTER) {
      *timeStepRingingCount = this->timeStepRingingCount;
    }
    if (ringBegin != ESMC_NULL_POINTER) {
      *ringBegin = this->ringBegin;
    }
    if (ringEnd != ESMC_NULL_POINTER) {
      *ringEnd = this->ringEnd;
    }
    if (refTime != ESMC_NULL_POINTER) {
      *refTime = this->refTime;
    }
    if (ringing != ESMC_NULL_POINTER) {
      *ringing = this->ringing;
    }
    if (ringingOnPrevTimeStep != ESMC_NULL_POINTER) {
      *ringingOnPrevTimeStep = this->ringingOnPrevTimeStep;
    }
    if (enabled != ESMC_NULL_POINTER) {
      *enabled = this->enabled;
    }
    if (sticky != ESMC_NULL_POINTER) {
      *sticky = this->sticky;
    }

    rc = ESMF_SUCCESS;
    return(rc);
     
 } // end ESMC_AlarmGet

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AlarmEnable - enables an Alarm object to function
//
// !INTERFACE:
      int ESMC_Alarm::ESMC_AlarmEnable(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      ESMF routine which enables an {\tt ESMC\_Alarm} object to function
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_AlarmEnable()"

  // Initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;
 
    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", &rc);
      return(rc);
    }

    enabled = true;

    rc = ESMF_SUCCESS;
    return(rc);           

 } // end ESMC_AlarmEnable

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AlarmDisable - disables an Alarm object from functioning
//
// !INTERFACE:
      int ESMC_Alarm::ESMC_AlarmDisable(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      ESMF routine which disables an {\tt ESMC\_Alarm} object from functioning
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_AlarmDisable()"

  // Initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;
 
    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", &rc);
      return(rc);
    }

    ringing = false;
    enabled = false;

    rc = ESMF_SUCCESS;
    return(rc);           

 } // end ESMC_AlarmDisable

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AlarmIsEnabled - check if Alarm is enabled
//
// !INTERFACE:
      bool ESMC_Alarm::ESMC_AlarmIsEnabled(
//
// !RETURN VALUE:
//    bool is enabled or not
//
// !ARGUMENTS:
      int  *rc) const {        // out - error return code
//
// !DESCRIPTION:
//    checks if {\tt ESMC\_Alarm}'s enabled state is set.
//
//EOP
// !REQUIREMENTS:

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_AlarmIsEnabled()"

    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", rc);
      return(false);
    }

    // Initialize return code; assume routine not implemented
    if (rc != ESMC_NULL_POINTER) *rc = ESMF_SUCCESS;

    return(enabled);

 } // end ESMC_AlarmIsEnabled

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AlarmRingerOn - sets an Alarm to the ringing state
//
// !INTERFACE:
      int ESMC_Alarm::ESMC_AlarmRingerOn(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      ESMF routine which sets an {\tt ESMC\_Alarm} object to the
//      ringing state.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_AlarmRingerOn()"

  // Initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;
 
    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", &rc);
      return(rc);
    }

    if(!enabled) {
      char logMsg[ESMF_MAXSTR];
      sprintf(logMsg, "Attempted to turn on ringer of disabled alarm %s.",
              this->name);
      ESMC_LogDefault.ESMC_LogWrite(logMsg, ESMC_LOG_WARN);
      // TODO: return ESMF_WARNING when defined
      // return(ESMF_WARNING);
      return(ESMF_FAILURE);
    }

    ringing = true;

    rc = ESMF_SUCCESS;
    return(rc);      

 } // end ESMC_AlarmRingerOn

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AlarmRingerOff - turns off an Alarm's ringing state
//
// !INTERFACE:
      int ESMC_Alarm::ESMC_AlarmRingerOff(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      ESMF routine which turns off an {\tt ESMC\_Alarm}'s ringing state.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_AlarmRingerOff()"

  // Initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;
 
    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", &rc);
      return(rc);
    }

    // turn alarm off
    ringing = false;
    timeStepRingingCount = 0;

    if (clock->direction == ESMF_MODE_FORWARD) {
      // remember this time, so if we go in reverse, we will know when
      //   to turn the alarm back on.
      //   TODO:  Assumes constant ringInterval between successive ringEnds;
      //          saves only last ringEnd.  Make ringEnd an array to save all
      //          end times, which may vary (e.g. due to variable timeSteps).
      ringEnd = clock->currTime;

    } else {      // ESMF_MODE_REVERSE
      // for sticky alarms, step back ring times
      if (sticky && ringTime != firstRingTime) {
        ringTime     -= ringInterval;
        prevRingTime -= ringInterval; 

        // get clock's timestep direction: positive or negative
        bool positive =
               (clock->currAdvanceTimeStep.ESMC_TimeIntervalAbsValue() ==
                clock->currAdvanceTimeStep) ? true : false;

        // step back ringEnd only if it was advanced past ringTime
        //  before the clock loop ended (covers case where last ringTime
        //  equals the clock->stopTime and alarm is processed before
        //  the clockAdvance()).
        if (positive  && ringEnd > ringTime ||
            !positive && ringEnd < ringTime) {
          ringEnd -= ringInterval;
        }
      }
    }

    rc = ESMF_SUCCESS;
    return(rc);    

 } // end ESMC_AlarmRingerOff

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AlarmIsRinging - check if Alarm is ringing
//
// !INTERFACE:
      bool ESMC_Alarm::ESMC_AlarmIsRinging(
//
// !RETURN VALUE:
//    bool is ringing or not
//
// !ARGUMENTS:
      int  *rc) const {        // out - error return code
//
// !DESCRIPTION:
//    Checks if {\tt ESMC\_Alarm}'s ringing state is set.
//
//    See also method ESMC\_ClockGetAlarmList(areRinging, ...) to get
//    a list of all ringing alarms belonging to a {\tt ESMC\_Clock}.
//
//EOP
// !REQUIREMENTS:

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_AlarmIsRinging()"

    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", rc);
      return(false);
    }

    // Initialize return code; assume routine not implemented
    if (rc != ESMC_NULL_POINTER) *rc = ESMF_SUCCESS;

    return(enabled && ringing);

 } // end ESMC_AlarmIsRinging

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AlarmWillRingNext - check if Alarm will ring upon the next clock timestep
//
// !INTERFACE:
      bool ESMC_Alarm::ESMC_AlarmWillRingNext(
//
// !RETURN VALUE:
//    bool will ring or not
//
// !ARGUMENTS:
      ESMC_TimeInterval *timeStep,   // in - optional timestep to use instead
                                     //      of the clock's
      int  *rc) const {              // out - error return code
//
// !DESCRIPTION:
//    Checks if {\tt ESMC\_Alarm}'s ringing state will be set on the next
//    clock timestep, using either the clock's current timestep,
//    or a passed-in one.
//
//    See also method ESMC\_ClockGetAlarmList(willRingNext, ...) to get
//    a list of all alarms belonging to a {\tt ESMC\_Clock} that will ring on
//    the next time step.

//
//EOP
// !REQUIREMENTS:

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_AlarmWillRingNext()"

    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", rc);
      return(false);
    }

    // default return code
    if (rc != ESMC_NULL_POINTER) *rc = ESMC_RC_NOT_IMPL;

    // must be associated with a clock
    if(clock == ESMC_NULL_POINTER) {
      char logMsg[ESMF_MAXSTR];
      sprintf(logMsg, "alarm %s is not associated with any clock.", name);
      ESMC_LogDefault.ESMC_LogWrite(logMsg, ESMC_LOG_ERROR);
      return(false);
    }

    // get clock's next time
    ESMC_Time clockNextTime;
    clock->ESMC_ClockGetNextTime(&clockNextTime, timeStep);

    // if specified, use passed-in timestep, otherwise use clock's
    ESMC_TimeInterval tStep = (timeStep != ESMC_NULL_POINTER) ?
                               *timeStep : clock->timeStep;

    // get timestep direction: positive or negative
    bool positive = tStep.ESMC_TimeIntervalAbsValue() == tStep ? true : false;

    // check if alarm will turn on
    bool willRing = false;
    if (enabled) {
      willRing = (positive) ?
                  clockNextTime >= ringTime && clock->currTime < ringTime :
                  clockNextTime <= ringTime && clock->currTime > ringTime;
    }

    if (rc != ESMC_NULL_POINTER) *rc = ESMF_SUCCESS;
    return(willRing);

 } // end ESMC_AlarmWillRingNext

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AlarmWasPrevRinging - check if Alarm was ringing on the previous clock timestep
//
// !INTERFACE:
      bool ESMC_Alarm::ESMC_AlarmWasPrevRinging(
//
// !RETURN VALUE:
//    bool was previously ringing or not
//
// !ARGUMENTS:
      int  *rc) const {        // out - error return code
//
// !DESCRIPTION:
//    Checks if {\tt ESMC\_Alarm}'s ringing state was set on the previous
//    clock timestep.
//
//    See also method ESMC\_ClockGetAlarmList(werePrevRinging, ...) to
//    get a list of all alarms belonging to a {\tt ESMC\_Clock} that were
//    ringing on the previous time step.

//
//EOP
// !REQUIREMENTS:

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_AlarmWasPrevRinging()"

    // Initialize return code; assume routine not implemented
    if (rc != ESMC_NULL_POINTER) *rc = ESMC_RC_NOT_IMPL;

    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", rc);
      return(false);
    }

    if (rc != ESMC_NULL_POINTER) *rc = ESMF_SUCCESS;

    return(ringingOnPrevTimeStep);

 } // end ESMC_AlarmWasPrevRinging

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AlarmSticky - sets an Alarm's sticky state 
//
// !INTERFACE:
      int ESMC_Alarm::ESMC_AlarmSticky(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      ESMF routine which sets an {\tt ESMC\_Alarm}'s sticky flag.
//
//EOP
// !REQUIREMENTS:

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_AlarmSticky()"

  // Initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;
 
    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", &rc);
      return(rc);
    }

    sticky = true;

    rc = ESMF_SUCCESS;
    return(rc);          

 } // end ESMC_AlarmSticky

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AlarmNotSticky - unsets an Alarm's sticky state 
//
// !INTERFACE:
      int ESMC_Alarm::ESMC_AlarmNotSticky(ESMC_TimeInterval *ringDuration,
                                          int *ringTimeStepCount) {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      ESMF routine which unsets an {\tt ESMC\_Alarm}'s sticky flag,
//      and optionally sets a ring duration either in terms of an
//      {\tt ESMC\_TimeInterval} or a integer number of clock timesteps.
//
//EOP
// !REQUIREMENTS:

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_AlarmNotSticky()"

  // Initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;
 
    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", &rc);
      return(rc);
    }

    sticky = false;

    // mutually exclusive: can only specify one ring duration type
    if (ringDuration != ESMC_NULL_POINTER &&
        ringTimeStepCount != ESMC_NULL_POINTER) {
      char logMsg[ESMF_MAXSTR];
      sprintf(logMsg, 
              "Alarm %s: can only specify one type of ring duration, not both.",
              name);
      ESMC_LogDefault.ESMC_LogWrite(logMsg, ESMC_LOG_ERROR);
      return(ESMF_FAILURE);
    }

    if (ringDuration != ESMC_NULL_POINTER) {
      this->ringDuration = *ringDuration;
    }
    if (ringTimeStepCount != ESMC_NULL_POINTER) {
      this->ringTimeStepCount = *ringTimeStepCount;
    }

    rc = ESMF_SUCCESS;
    return(rc);          

 } // end ESMC_AlarmNotSticky

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AlarmIsSticky - check if Alarm is sticky
//
// !INTERFACE:
      bool ESMC_Alarm::ESMC_AlarmIsSticky(
//
// !RETURN VALUE:
//    bool is sticky or not
//
// !ARGUMENTS:
      int  *rc) const {        // out - error return code
//
// !DESCRIPTION:
//    checks if {\tt ESMC\_Alarm}'s sticky state is set.
//
//EOP
// !REQUIREMENTS:

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_AlarmIsSticky()"

    // Initialize return code; assume routine not implemented
    if (rc != ESMC_NULL_POINTER) *rc = ESMC_RC_NOT_IMPL;

    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", rc);
      return(false);
    }

    if (rc != ESMC_NULL_POINTER) *rc = ESMF_SUCCESS;

    return(sticky);

 } // end ESMC_AlarmIsSticky

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
      int *rc) {         // out - error return code

// !DESCRIPTION:
//    Checks if its time to ring based on current clock time crossing the ring
//    time in either the positive or negative direction. If already ringing,
//    checks if its time to turn off.
//
//EOP
// !REQUIREMENTS:  TMG4.4, 4.6

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_AlarmCheckRingTime()"

    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", rc);
      return(false);
    }

    // default return code
    if (rc != ESMC_NULL_POINTER) *rc = ESMC_RC_NOT_IMPL;

    // must be associated with a clock
    if(clock == ESMC_NULL_POINTER) {
      char logMsg[ESMF_MAXSTR];
      sprintf(logMsg, "alarm %s is not associated with any clock.", name);
      ESMC_LogDefault.ESMC_LogWrite(logMsg, ESMC_LOG_ERROR);
      return(false);
    }

    // get clock's timestep direction: positive or negative
    bool positive = (clock->currAdvanceTimeStep.ESMC_TimeIntervalAbsValue() ==
                     clock->currAdvanceTimeStep) ? true : false;

    if (clock->direction == ESMF_MODE_FORWARD) {

      // carry previous flag forward
      ringingOnPrevTimeStep = ringingOnCurrTimeStep;

      if (enabled) {
        // if clock timeStep direction (sign) changed, adjust alarm
        // accordingly before checking if time to ring
        ESMC_TimeInterval zeroTimeStep;
        if ( (clock->currAdvanceTimeStep < zeroTimeStep &&
              clock->prevAdvanceTimeStep > zeroTimeStep) ||
             (clock->currAdvanceTimeStep > zeroTimeStep &&
              clock->prevAdvanceTimeStep < zeroTimeStep) ) {
          if (!userChangedRingInterval) {
            // change sign to match clock timeStep
            ringInterval *= -1;
          } else {
            // check that user's new ringInterval is same sign as timeStep
            if ((ringInterval > zeroTimeStep &&
                 clock->currAdvanceTimeStep < zeroTimeStep) ||
                (ringInterval < zeroTimeStep &&
                 clock->currAdvanceTimeStep > zeroTimeStep) ) {
              ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_VAL_WRONG,
                 "; alarm ringInterval not same sign as clock timeStep.", rc);
              return(false);
            }
          }
          if (!userChangedRingTime) {
            // pull back ringTime into ringable range
            while (positive ? clock->prevTime >= ringTime :
                              clock->prevTime <= ringTime) {
              ringTime += ringInterval;
            } 
          } else {
            // check that user's new ringTime is within ringable range
            if (positive ? clock->currTime > ringTime :
                           clock->currTime < ringTime) {
              ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_VAL_OUTOFRANGE,
                 "; alarm ringTime not within clock ringable range", rc);
              return(false);
            }
          }
        }  
      }

      // done processing changed flags, reset if necessary
      if (userChangedRingTime)     userChangedRingTime     = false;
      if (userChangedRingInterval) userChangedRingInterval = false;

      // check if time to turn on alarm
      if (!ringing && enabled) 
         ESMC_AlarmCheckTurnOn(positive);

      // else if not sticky, check if time to turn off alarm
      //   (user is responsible for turning off sticky alarms via RingerOff())
      // TODO:  maybe should not be else clause, just an "if" on its own, since
      // ringTimeStepCount=1 would imply turning off in the same timeStep? But
      // would need to move timeStepRingingCount++ up.
      else if (!sticky && ringing && enabled) {

        // first check if next alarm time has been reached,
        // then check if time to turn off alarm.
        if (!ESMC_AlarmCheckTurnOn(positive)) {
          if (ringTimeStepCount > 0) { // use ringTimeStepCount ...
            if (timeStepRingingCount >= ringTimeStepCount) {
              ringingOnCurrTimeStep = ringing = false;
              timeStepRingingCount = 0;
            }
          } else {  // ... otherwise use ringDuration
            ESMC_TimeInterval cumulativeRinging;
            cumulativeRinging = clock->currTime - ringBegin;
            if (cumulativeRinging.ESMC_TimeIntervalAbsValue() >= ringDuration) {
              ringingOnCurrTimeStep = ringing = false;
              timeStepRingingCount = 0;
            }
          }
        }
      }

      // count for how many clock time steps the alarm is ringing
      if (ringing) timeStepRingingCount++;

    } else { // ESMF_MODE_REVERSE

      // TODO: Make more robust by removing the following simplifying
      //       assumptions:
      //
      //       1) timeSteps are constant throughout clock run.
      //       2) ringInterval, ringDuration are constant throughout clock run.
      //       3) sticky alarms must have traversed through at least one alarm
      //          (to save the ringEnd time) in order to reverse.  For
      //          repeating sticky alarms, previous ringEnds are assumed to be
      //          equally spaced by a constant ringInterval.
      //
      //       The solution will involve saving clock and alarm state at every
      //       timeStep, which means dynamically allocated arrays (probably
      //       arrays of clock and alarm objects).  These arrays need to be
      //       initially sized at Create() time, then reallocated as necessary
      //       (eg. upon those Set() calls which would require more space).
      //       Will need flag upon Create() for user to hint at need for
      //       this extra overhead for reversible clocks and alarms.

      // Note:  Sticky alarms need to have been traversed forward in order
      //          to be reversed (to save ringEnd upon user RingerOff() event).
      //          In contrast, non-sticky alarms can be reversed without first
      //          having been traversed forward.  This implies that the logic
      //          cannot use prev* state variables in order to step back; all
      //          state variables must be reconstructed from timeStep,
      //          ringInterval, and ringDuration.

      // if sticky alarm, must have traversed forward far enough to have
      //   called RingerOff(), causing the ringEnd time to be saved.
      if(sticky && ringEnd.ESMC_TimeValidate("initialized") != ESMF_SUCCESS) {
        char logMsg[ESMF_MAXSTR];
        sprintf(logMsg, "Sticky alarm %s cannot be reversed since it has "
                        "not been traversed forward and turned off via "
                        "a user call to ESMF_AlarmRingerOff(), thereby "
                        "enabling Time Manager to know the time to turn it "
                        "back on in reverse.", name);
        ESMC_LogDefault.ESMC_LogWrite(logMsg, ESMC_LOG_ERROR);
        return(false);
      }

      // check if ringEnd is past clock currTime (e.g. 1st step in reverse)
      if (sticky && ringTime != firstRingTime) {
        if (positive  && ringEnd > clock->currTime ||
            !positive && ringEnd < clock->currTime) {
          ringEnd      -= ringInterval;
          ringTime     -= ringInterval;
          prevRingTime -= ringInterval;
        }
      }

      // determine when alarm ends ringing in forward mode
      ESMC_Time ringTimeEnd;
      if (sticky) {
        ringTimeEnd = ringEnd;
      } else { // non-sticky
        if (ringTimeStepCount > 0) {  // use ringTimeStepCount ...
          // TODO:  base on ringBegin rather than ringTime, to be consistent
          //  with checkTurnOn() logic?
          ringTimeEnd = ringTime + ringTimeStepCount * clock->timeStep;
        } else { // ... otherwise use ringDuration
          ringTimeEnd = ringTime + ringDuration; 
        }
      }

      // check if time to turn alarm back on in reverse mode
      if (!ringing && enabled) {
        if (sticky) {
          ringingOnCurrTimeStep = ringing = (clock->currTime == ringTimeEnd);
        } else {
          ringingOnCurrTimeStep = ringing = (positive) ?
                    // TODO: <= && > (for positive), >= && < (for negative) ?
                    (clock->currTime < ringTimeEnd) &&
                      (clock->currTime + clock->timeStep) >= ringTimeEnd :
                                       // (negative)
                    (clock->currTime > ringTimeEnd) &&
                      (clock->currTime + clock->timeStep) <= ringTimeEnd;
        }

        if (ringing) {
          // determine what ringBegin was for this alarm event
          ESMC_AlarmResetRingBegin(positive);

          // determine what the ending timeStepRingingCount was
          //   for this alarm event
          timeStepRingingCount =
                    (int) ((clock->currTime - ringBegin) / clock->timeStep) + 1;
        }

      // else check if time to turn non-sticky alarm back off in reverse mode
      //   (user is responsible for turning off sticky alarms via RingerOff())
      } else if (!sticky && ringing && enabled) {

        bool turnAlarmOff = false;

        if (ringTimeStepCount > 0) {  // use ringTimeStepCount ...
          if (timeStepRingingCount <= 0) {  // if count down to zero
              turnAlarmOff = true;
          }
        } else { // ... otherwise use ringDuration
          if ((positive && clock->currTime < ringTime &&
            (clock->currTime + clock->timeStep) >= ringTime) ||
            (!positive && clock->currTime > ringTime &&
            (clock->currTime + clock->timeStep) <= ringTime)) {
              turnAlarmOff = true;
          }
        }

        if (turnAlarmOff) {

          // turn alarm off
          ringingOnCurrTimeStep = ringing = false;
          timeStepRingingCount = 0;

          // look back and reset ringing times for previous alarm event,
          //   if not past firstRingTime
          // TODO: remove assumption of constant ringInterval; allow for
          //       variable ringIntervals
          if (ringTime != firstRingTime) { 
            ringTime -= ringInterval;
            prevRingTime -= ringInterval; 
            if (ringTimeStepCount > 0) {  // use ringTimeStepCount ...
              ringTimeEnd = ringTime + ringTimeStepCount * clock->timeStep;
            } else { // ... otherwise use ringDuration
              ringTimeEnd = ringTime + ringDuration; 
            }
          } else { // reset to initial condition
            prevRingTime = ringTime;
          }

          // determine what ringBegin was for *previous* alarm event
          ESMC_AlarmResetRingBegin(positive);

        } else {  // keep alarm ringing
          // reverse count for how many clock time steps the alarm was ringing
          timeStepRingingCount--;
        }

      } // if (!sticky)

      // determine if alarm was ringing on previous timeStep
      if (enabled) {
        ringingOnPrevTimeStep = (positive) ?
                  clock->prevTime >= ringTime && clock->prevTime < ringTimeEnd :
                  clock->prevTime <= ringTime && clock->prevTime > ringTimeEnd;
      }

    }  // end if ESMF_MODE_REVERSE

    if (rc != ESMC_NULL_POINTER) *rc = ESMF_SUCCESS;

    return(ringing && enabled);

 } // end ESMC_AlarmCheckRingTime

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Alarm(==) - Alarm equality comparison    
//
// !INTERFACE:
      bool ESMC_Alarm::operator==(
//
// !RETURN VALUE:
//    bool result
//
// !ARGUMENTS:
      const ESMC_Alarm &alarm) const {   // in - ESMC_Alarm to compare
//
// !DESCRIPTION:
//      Compare for equality the current object's (this) {\tt ESMC\_Alarm} with
//      given {\tt ESMC\_Alarm}, return result.  Comparison is based on IDs.
//
//EOP
// !REQUIREMENTS:

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_Alarm::operator==()"

    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", ESMC_NULL_POINTER);
      return(false);
    }

    return(id == alarm.id);

}  // end ESMC_Alarm::operator==

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Alarm(!=) - Alarm inequality comparison    
//
// !INTERFACE:
      bool ESMC_Alarm::operator!=(
//
// !RETURN VALUE:
//    bool result
//
// !ARGUMENTS:
      const ESMC_Alarm &alarm) const {   // in - ESMC_Alarm to compare
//
// !DESCRIPTION:
//      Compare for inequality the current object's (this)
//      {\tt ESMC\_Alarm} with given {\tt ESMC\_Alarm}, return result.
//      Comparison is based on IDs.
//
//EOP
// !REQUIREMENTS:

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_Alarm::operator!=()"

    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", ESMC_NULL_POINTER);
      return(false);
    }

    return(id != alarm.id);

}  // end ESMC_Alarm::operator!=

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AlarmReadRestart - restore contents of an Alarm
//
// !INTERFACE:
      ESMC_Alarm *ESMC_AlarmReadRestart(
//
// !RETURN VALUE:
//    pointer to newly allocated and restored ESMC_Alarm
//
// !ARGUMENTS:
      int          nameLen,  // in
      const char  *name,     // in
      ESMC_IOSpec *iospec,   // in
      int         *rc ) {    // out - return code

//
// !DESCRIPTION:
//      Restore information about an {\tt ESMC\_Alarm}.
//      For persistence/checkpointing.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_AlarmReadRestart()"

    // TODO:  read alarm state from iospec/name, then allocate/restore
    //        (share code with ESMC_AlarmCreate()).

    // Initialize return code; assume routine not implemented
    if (rc != ESMC_NULL_POINTER) *rc = ESMC_RC_NOT_IMPL;

    return(ESMC_NULL_POINTER);

 } // end ESMC_AlarmReadRestart

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AlarmWriteRestart - save contents of an Alarm
//
// !INTERFACE:
      int ESMC_Alarm::ESMC_AlarmWriteRestart(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_IOSpec *iospec) const {
//
// !DESCRIPTION:
//      Save information about an {\tt ESMC\_Alarm}.
//      For persistence/checkpointing
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_AlarmWriteRestart()"

  // Initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;
 
    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", &rc);
      return(rc);
    }

    // TODO:  save alarm state using iospec/name.  Default to disk file.

    rc = ESMF_SUCCESS;
    return(rc);

 } // end ESMC_AlarmWriteRestart

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AlarmValidate - internal consistency check for an Alarm
//
// !INTERFACE:
      int ESMC_Alarm::ESMC_AlarmValidate(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {    // in - validate options
//
// !DESCRIPTION:
//      Validates that a {\tt ESMC\_Alarm} is internally consistent.
//      Returns error code if problems are found.  {\tt ESMC\_Base}
//      class method.
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_AlarmValidate()"

  // Initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;
 
    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", &rc);
      return(rc);
    }

    // must have a ring time; ringDuration, stopTime, prevRingTime optional
    if (ringTime.ESMC_TimeValidate() != ESMF_SUCCESS) {
      char logMsg[ESMF_MAXSTR];
      sprintf(logMsg, "Alarm %s: invalid ringTime.", name);
      ESMC_LogDefault.ESMC_LogWrite(logMsg, ESMC_LOG_ERROR);
      return(ESMF_FAILURE);
    }

    // invalid state
    if (!enabled && ringing) {
      char logMsg[ESMF_MAXSTR];
      sprintf(logMsg, "Alarm %s: invalid state: disabled and ringing.", name);
      ESMC_LogDefault.ESMC_LogWrite(logMsg, ESMC_LOG_ERROR);
      return(ESMF_FAILURE);
    }

    // TODO: validate id ?

    rc = ESMF_SUCCESS;
    return(rc);

 } // end ESMC_AlarmValidate

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AlarmPrint - print contents of an Alarm
//
// !INTERFACE:
      int ESMC_Alarm::ESMC_AlarmPrint(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {    // in - print options
//
// !DESCRIPTION:
//      Print information about an {\tt ESMC\_Alarm}.  For testing/debugging.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_AlarmPrint()"

  // Initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;
 
    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", &rc);
      return(rc);
    }

    printf("Alarm ----------------------------------\n");

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
      else if (strncmp(opts, "clock", 5) == 0) {
        printf("clock = \n");
        if (strstr(opts, "name") != ESMC_NULL_POINTER) {
          clock->ESMC_ClockPrint("name");
        } else {
          clock->ESMC_ClockPrint();
        }
      }
      else if (strncmp(opts, "ringinterval", 12) == 0) {
        printf("ringInterval = \n");
        if (strstr(opts, "string") != ESMC_NULL_POINTER) {
          ringInterval.ESMC_TimeIntervalPrint("string");
        } else {
          ringInterval.ESMC_TimeIntervalPrint();
        }
      }
      else if (strncmp(opts, "ringduration", 12) == 0) {
        printf("ringDuration = \n");
        if (strstr(opts, "string") != ESMC_NULL_POINTER) {
          ringDuration.ESMC_TimeIntervalPrint("string");
        } else {
          ringDuration.ESMC_TimeIntervalPrint();
        }
      }
      else if (strncmp(opts, "ringtime", 8) == 0) {
        printf("ringTime = \n");
        if (strstr(opts, "string") != ESMC_NULL_POINTER) {
          ringTime.ESMC_TimePrint("string");
        } else {
          ringTime.ESMC_TimePrint();
        }
      }
      else if (strncmp(opts, "firstringtime", 13) == 0) {
        printf("firstRingTime = \n");
        if (strstr(opts, "string") != ESMC_NULL_POINTER) {
          firstRingTime.ESMC_TimePrint("string");
        } else {
          firstRingTime.ESMC_TimePrint();
        }
      }
      else if (strncmp(opts, "prevringtime", 12) == 0) {
        printf("prevRingTime = \n");
        if (strstr(opts, "string") != ESMC_NULL_POINTER) {
          prevRingTime.ESMC_TimePrint("string");
        } else {
          prevRingTime.ESMC_TimePrint();
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
      else if (strncmp(opts, "ringbegin", 9) == 0) {
        printf("ringBegin = \n");
        if (strstr(opts, "string") != ESMC_NULL_POINTER) {
          ringBegin.ESMC_TimePrint("string");
        } else {
          ringBegin.ESMC_TimePrint();
        }
      }
      else if (strncmp(opts, "ringend", 7) == 0) {
        printf("ringEnd = \n");
        if (strstr(opts, "string") != ESMC_NULL_POINTER) {
          ringEnd.ESMC_TimePrint("string");
        } else {
          ringEnd.ESMC_TimePrint();
        }
      }
      else if (strncmp(opts, "reftime", 7) == 0) {
        printf("refTime = \n");
        if (strstr(opts, "string") != ESMC_NULL_POINTER) {
          refTime.ESMC_TimePrint("string");
        } else {
          refTime.ESMC_TimePrint();
        }
      }
      else if (strncmp(opts, "ringtimestepcount", 17) == 0) {
        printf("ringTimeStepCount = %d\n", ringTimeStepCount);
      }
      else if (strncmp(opts, "timestepringingcount", 20) == 0) {
        printf("timeStepRingingCount = %d\n", timeStepRingingCount);
      }
      else if (strncmp(opts, "ringing", 7) == 0) {
        printf("ringing = %s\n", ringing ? "true" : "false");
      }
      else if (strncmp(opts, "ringingonprevtimestep", 21) == 0) {
        printf("ringingOnPrevTimeStep = %s\n",
                ringingOnPrevTimeStep ? "true" : "false");
      }
      else if (strncmp(opts, "enabled", 7) == 0) {
        printf("enabled = %s\n", enabled ? "true" : "false");
      }
      else if (strncmp(opts, "sticky", 6) == 0) {
        printf("sticky = %s\n", sticky ? "true" : "false");
      }

    } 

    if (options == ESMC_NULL_POINTER || strncmp(options, "string", 6) == 0) {
      // default:  print out all properties

      printf("name = %s\n", name);
      printf("ringInterval = \n"); ringInterval.ESMC_TimeIntervalPrint(options);
      printf("ringDuration = \n"); ringDuration.ESMC_TimeIntervalPrint(options);
      printf("ringTime = \n");      ringTime.ESMC_TimePrint(options);
      printf("firstRingTime = \n"); firstRingTime.ESMC_TimePrint(options);
      printf("prevRingTime = \n");  prevRingTime.ESMC_TimePrint(options);
      printf("stopTime = \n");      stopTime.ESMC_TimePrint(options);
      printf("ringBegin = \n");     ringBegin.ESMC_TimePrint(options);
      printf("ringEnd = \n");       ringEnd.ESMC_TimePrint(options);
      printf("refTime = \n");       refTime.ESMC_TimePrint(options);
      printf("ringTimeStepCount = %d\n",    ringTimeStepCount);
      printf("timeStepRingingCount = %d\n", timeStepRingingCount);
      printf("ringing = %s\n", ringing ? "true" : "false");
      printf("ringingOnPrevTimeStep = %s\n",
              ringingOnPrevTimeStep ?    "true" : "false");
      printf("enabled = %s\n", enabled ? "true" : "false");
      printf("sticky = %s\n",  sticky ?  "true" : "false");
    }

    printf("end Alarm ------------------------------\n\n");

    rc = ESMF_SUCCESS;
    return(rc);

 } // end ESMC_AlarmPrint

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
//      Initializes a {\tt ESMC\_Alarm} with defaults for either
//      C++ or F90, since {\tt ESMC\_Alarm} is a deep, dynamically
//      allocated class.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_Alarm(void) constructor"

    // Initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;

    name[0] = '\0';
    clock = ESMC_NULL_POINTER;
    ringTimeStepCount = 0;
    timeStepRingingCount = 0;
    ringing = ringingOnCurrTimeStep = ringingOnPrevTimeStep = false;
    userChangedRingTime = false;
    userChangedRingInterval = false;
    enabled = true;
    sticky  = true;
    id = ++count;  // TODO: inherit from ESMC_Base class
    // copy = false;  // TODO: see notes in constructors and destructor below

    // initialize ring interval to zero
    ESMC_I4 s = 0;
    // TODO: use native C++ method when ready
    rc = ringInterval.ESMC_TimeIntervalSet(ESMC_NULL_POINTER,
                                      ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                      ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                      ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                      ESMC_NULL_POINTER, &s);

    ESMC_LogDefault.ESMC_LogMsgFoundError(rc, ESMF_ERR_PASSTHRU, &rc);

 } // end ESMC_Alarm

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Alarm - native C++ copy constructor
//
// !INTERFACE:
      ESMC_Alarm::ESMC_Alarm(
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
      const ESMC_Alarm &alarm) {  // in - alarm to copy
//
// !DESCRIPTION:
//      Copies members of given alarm.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

    *this = alarm;
    // copy = true;   // TODO: Unique copy ? (id = ++count) (review operator==
                      //       and operator!=)  Must do same in assignment
                      //       overloaded method and interface from F90.
                      //       Also, inherit from ESMC_Base class.

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
//      Calls standard ESMF deep or shallow methods for destruction.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

  // TODO: Decrement static count for one less object; but don't decrement   //       for copies.  Must create and set a copy flag property to detect.
  //       Also must set copy flag in copy constructor and overloaded 
  //       assignment method, and provide interface from F90. 
  // if (!copy) count--;

 } // end ~ESMC_Alarm

//-------------------------------------------------------------------------
//  Private methods 
//-------------------------------------------------------------------------

//-------------------------------------------------------------------------
//BOPI
// !IROUTINE:  ESMC_AlarmCheckTurnOn - check if time to turn on alarm
//
// !INTERFACE:
      bool ESMC_Alarm::ESMC_AlarmCheckTurnOn(
//
// !RETURN VALUE:
//    bool whether to turn on alarm
//
// !ARGUMENTS:
      bool timeStepPositive) {  // in - sign of clock's timeStep,
//                              //        true: positive, false: negative
//
// !DESCRIPTION:
//    Checks whether alarm should be ringing
//
//EOPI
// !REQUIREMENTS:

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_AlarmCheckTurnOn()"

    // The original comment few lines below indicates that the ringing state
    // would be turned off elsewhere. However, it is initialized (turn off) 
    // here for the sake of X1 compiler.  That may be taken off later.
    ringingOnCurrTimeStep = false;

    bool checkRinging;

    if (clock->advanceCount != 0) { // clock has been advanced; use prevTime
      checkRinging = (timeStepPositive) ?
             clock->currTime >= ringTime && clock->prevTime < ringTime :
             clock->currTime <= ringTime && clock->prevTime > ringTime;
    } else {  // clock in initial state; don't use prevTime since
              //   it equals currTime
      checkRinging = (timeStepPositive) ?
             clock->currTime >= ringTime :
             clock->currTime <= ringTime ;
    }

    if (checkRinging) {
      // if so, refresh ringing state; if not, leave previous state alone:
      //   turn off determined elsewhere.
      ringingOnCurrTimeStep = ringing = true;

      // note time,
      ringBegin = clock->currTime;

      // and update next ringing time
      bool updateNextRingingTime = true;
      if (stopTime.ESMC_TimeValidate("initialized") == ESMF_SUCCESS) {
        updateNextRingingTime = (timeStepPositive) ?
                               clock->currTime < (stopTime - ringInterval):
                               clock->currTime > (stopTime - ringInterval);
      }
      if (updateNextRingingTime) {
        prevRingTime = ringTime;
        ringTime += ringInterval;
      }
    }

    return(checkRinging);

} // end ESMC_AlarmCheckTurnOn

//-------------------------------------------------------------------------
//BOPI
// !IROUTINE:  ESMC_AlarmResetRingBegin - reset ringBegin during ESMF_MODE_REVERSE
//
// !INTERFACE:
      int ESMC_Alarm::ESMC_AlarmResetRingBegin(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      bool timeStepPositive) {  // in - sign of clock's timeStep,
//                              //        true: positive, false: negative
//
// !DESCRIPTION:
//      Reconstructs ringBegin for an alarm event during
//      {\tt ESMF\_MODE\_REVERSE}
//
//EOPI
// !REQUIREMENTS:

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMC_AlarmResetRingBegin()"

    int rc = ESMC_RC_NOT_IMPL;

    // determine ringBegin for previous alarm event, aligned to the clock
    //  startTime
    // TODO:  assumes constant timeStep; allow for variable timeSteps
    ESMC_TimeInterval zeroTimeInterval(0,0,1,0,0,0);
    ESMC_TimeInterval remainder =
                               (ringTime - clock->startTime) % clock->timeStep;
    if (remainder == zeroTimeInterval) {
      ringBegin = ringTime;  // ringBegin coincident with ringTime
    } else { // ringBegin is first timeStep beyond ringTime
      if (!timeStepPositive) remainder = -remainder;
      ringBegin = (ringTime - remainder) + clock->timeStep;
    }

    rc = ESMF_SUCCESS;
    return(rc);

} // end ESMC_AlarmResetRingBegin
