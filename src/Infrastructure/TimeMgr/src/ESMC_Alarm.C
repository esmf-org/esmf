// $Id: ESMC_Alarm.C,v 1.45 2004/05/25 21:11:35 eschwab Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.
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
 #include <iostream.h>
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
 static const char *const version = "$Id: ESMC_Alarm.C,v 1.45 2004/05/25 21:11:35 eschwab Exp $";
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
    if (rc != ESMC_NULL_POINTER) *rc = ESMF_FAILURE;

// ============================================================================
#if 1
    ESMC_LogDefault.ESMC_LogWrite("Test message number 1", ESMC_LOG_INFO);
    ESMC_LogDefault.ESMC_LogWrite("Test message number 2", ESMC_LOG_WARN);
    char logMsg[ESMF_MAXSTR];
    sprintf(logMsg, "alarm name %s, Test message number 3", name);
    ESMC_LogDefault.ESMC_LogWrite(logMsg, ESMC_LOG_ERROR);
    ESMC_LogDefault.ESMC_LogFoundError(ESMF_FAILURE, rc);
    ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_MEM, ESMF_ERR_PASSTHRU, rc);
#endif
// ============================================================================

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
      alarm->ringTime = *ringTime;
      alarm->prevRingTime = alarm->ringTime;
    }
    if (ringInterval != ESMC_NULL_POINTER) {
      alarm->ringInterval = *ringInterval;

      // if ringTime not specified, or ringTime == clockCurrTime, calculate
      //   ringTime from the current clock time

      bool ringTimeIsCurrTime;
      if (ringTime != ESMC_NULL_POINTER) {
         ringTimeIsCurrTime = (*ringTime == clock->currTime);
         // TODO: handle case where *ringTime < clock->currTime;
         //       same or similar to refTime
      }
      
      if (ringTime == ESMC_NULL_POINTER || ringTimeIsCurrTime) {
        // works for positive or negative ringInterval
        alarm->ringTime = clock->currTime + alarm->ringInterval;
        alarm->prevRingTime = alarm->ringTime;
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
      clock->ESMC_ClockAddAlarm(alarm);
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
 #define ESMC_METHOD "ESMC_ClockCreate(copy)"

    ESMC_Alarm *alarmCopy;
    int returnCode;

    // default return code
    if (rc != ESMC_NULL_POINTER) *rc = ESMF_FAILURE;

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

  // TODO: alarm->ESMC_AlarmDestruct(); constructor calls it!
  delete *alarm;   // ok to delete null pointer
  *alarm = ESMC_NULL_POINTER;
  return(ESMF_SUCCESS);

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

    int rc = ESMF_SUCCESS;
 
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
      this->ringTime = *ringTime;
      this->prevRingTime = this->ringTime;
    }
    if (ringInterval != ESMC_NULL_POINTER) {
      this->ringInterval = *ringInterval;
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

    int rc = ESMF_SUCCESS;

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

    enabled = true;

    return(ESMF_SUCCESS);

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

    ringing = false;
    enabled = false;

    return(ESMF_SUCCESS);

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

    return(ESMF_SUCCESS);

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

    ringing = false;

    return(ESMF_SUCCESS);

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

    // default return code
    if (rc != ESMC_NULL_POINTER) *rc = ESMF_FAILURE;

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

    sticky = true;

    return(ESMF_SUCCESS);

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

    return(ESMF_SUCCESS);

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

    // default return code
    if (rc != ESMC_NULL_POINTER) *rc = ESMF_FAILURE;

    // must be associated with a clock
    if(clock == ESMC_NULL_POINTER) {
      char logMsg[ESMF_MAXSTR];
      sprintf(logMsg, "alarm %s is not associated with any clock.", name);
      ESMC_LogDefault.ESMC_LogWrite(logMsg, ESMC_LOG_ERROR);
      return(false);
    }

    // carry previous flag forward
    ringingOnPrevTimeStep = ringingOnCurrTimeStep;
    
    // get clock's timestep direction: positive or negative
    bool positive = (clock->timeStep.ESMC_TimeIntervalAbsValue() ==
                     clock->timeStep) ? true : false;

    // check if time to turn on alarm
    if (!ringing && enabled) {
      ringingOnCurrTimeStep = ringing = (positive) ?
                  clock->currTime >= ringTime && clock->prevTime < ringTime :
                  clock->currTime <= ringTime && clock->prevTime > ringTime;
      if (ringing) {
        // update next ringing time if ringInterval nonzero
        prevRingTime = ringTime;
        ringTime += ringInterval;
      }
    }
    // else check if time to turn off alarm
    else if (!sticky && enabled) {
      ESMC_TimeInterval cumulativeRinging;
      cumulativeRinging = clock->currTime - ringBegin;
      if (cumulativeRinging.ESMC_TimeIntervalAbsValue() >= ringDuration) {
        ringingOnCurrTimeStep = ringing = false;
      }
    }

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

    // TODO:  read alarm state from iospec/name, then allocate/restore
    //        (share code with ESMC_AlarmCreate()).

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

    // TODO:  save alarm state using iospec/name.  Default to disk file.

    return(ESMF_SUCCESS);

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

    return(ESMF_SUCCESS);

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

    cout << "Alarm ----------------------------------" << endl;

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
        cout << "name = " << name << endl;
      }
      else if (strncmp(opts, "clock", 5) == 0) {
        cout << "clock = " << endl;
        if (strstr(opts, "name") != ESMC_NULL_POINTER) {
          clock->ESMC_ClockPrint("name");
        } else {
          clock->ESMC_ClockPrint();
        }
      }
      else if (strncmp(opts, "ringinterval", 12) == 0) {
        cout << "ringInterval = " << endl;
        if (strstr(opts, "string") != ESMC_NULL_POINTER) {
          ringInterval.ESMC_TimeIntervalPrint("string");
        } else {
          ringInterval.ESMC_TimeIntervalPrint();
        }
      }
      else if (strncmp(opts, "ringduration", 12) == 0) {
        cout << "ringDuration = " << endl;
        if (strstr(opts, "string") != ESMC_NULL_POINTER) {
          ringDuration.ESMC_TimeIntervalPrint("string");
        } else {
          ringDuration.ESMC_TimeIntervalPrint();
        }
      }
      else if (strncmp(opts, "ringtime", 8) == 0) {
        cout << "ringTime = " << endl;
        if (strstr(opts, "string") != ESMC_NULL_POINTER) {
          ringTime.ESMC_TimePrint("string");
        } else {
          ringTime.ESMC_TimePrint();
        }
      }
      else if (strncmp(opts, "prevringtime", 12) == 0) {
        cout << "prevRingTime = " << endl;
        if (strstr(opts, "string") != ESMC_NULL_POINTER) {
          prevRingTime.ESMC_TimePrint("string");
        } else {
          prevRingTime.ESMC_TimePrint();
        }
      }
      else if (strncmp(opts, "stoptime", 8) == 0) {
        cout << "stopTime = " << endl;
        if (strstr(opts, "string") != ESMC_NULL_POINTER) {
          stopTime.ESMC_TimePrint("string");
        } else {
          stopTime.ESMC_TimePrint();
        }
      }
      else if (strncmp(opts, "ringbegin", 9) == 0) {
        cout << "ringBegin = " << endl;
        if (strstr(opts, "string") != ESMC_NULL_POINTER) {
          ringBegin.ESMC_TimePrint("string");
        } else {
          ringBegin.ESMC_TimePrint();
        }
      }
      else if (strncmp(opts, "reftime", 7) == 0) {
        cout << "refTime = " << endl;
        if (strstr(opts, "string") != ESMC_NULL_POINTER) {
          refTime.ESMC_TimePrint("string");
        } else {
          refTime.ESMC_TimePrint();
        }
      }
      else if (strncmp(opts, "ringtimestepcount", 17) == 0) {
        cout << "ringTimeStepCount = " << ringTimeStepCount << endl;
      }
      else if (strncmp(opts, "timestepringingcount", 20) == 0) {
        cout << "timeStepRingingCount = " << timeStepRingingCount << endl;
      }
      else if (strncmp(opts, "ringing", 7) == 0) {
        cout << "ringing = " << ringing << endl;
      }
      else if (strncmp(opts, "ringingonprevtimestep", 21) == 0) {
        cout << "ringingOnPrevTimeStep = " << ringingOnPrevTimeStep << endl;
      }
      else if (strncmp(opts, "enabled", 7) == 0) {
        cout << "enabled = " << enabled << endl;
      }
      else if (strncmp(opts, "sticky", 6) == 0) {
        cout << "sticky = " << sticky << endl;
      }

    } else {
      // default:  print out all properties

      cout << "name = "         << name << endl;
      cout << "ringInterval = " << endl;
                                   ringInterval.ESMC_TimeIntervalPrint(options);
      cout << "ringDuration = " << endl;
                                   ringDuration.ESMC_TimeIntervalPrint(options);
      cout << "ringTime = "     << endl; ringTime.ESMC_TimePrint(options);
      cout << "prevRingTime = " << endl; prevRingTime.ESMC_TimePrint(options);
      cout << "stopTime = "     << endl; stopTime.ESMC_TimePrint(options);
      cout << "ringBegin = "    << endl; ringBegin.ESMC_TimePrint(options);
      cout << "refTime = "      << endl; refTime.ESMC_TimePrint(options);
      cout << "ringTimeStepCount = "    << ringTimeStepCount << endl;
      cout << "timeStepRingingCount = " << timeStepRingingCount << endl;
      cout << "ringing = "      << ringing << endl;
      cout << "ringingOnPrevTimeStep = "  << ringingOnPrevTimeStep << endl;
      cout << "enabled = "      << enabled << endl;
      cout << "sticky = "       << sticky << endl;
    }

    cout << "end Alarm ------------------------------" << endl << endl;

    return(ESMF_SUCCESS);

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

    name[0] = '\0';
    ringTimeStepCount = 0;
    timeStepRingingCount = 0;
    ringing = ringingOnCurrTimeStep = ringingOnPrevTimeStep = false;
    enabled = true;
    sticky  = true;
    id = ++count;  // TODO: inherit from ESMC_Base class
    // copy = false;  // TODO: see notes in constructors and destructor below

    // initialize ring interval to zero
    ESMF_KIND_I4 s = 0;
    // TODO: use native C++ method when ready
    int rc = ringInterval.ESMC_TimeIntervalSet(ESMC_NULL_POINTER,
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
