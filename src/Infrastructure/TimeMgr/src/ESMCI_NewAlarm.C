// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2021, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
// ESMC NewAlarm method code (body) file
//
//-------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ {\tt ESMC\_NewAlarm} methods declared
// in the companion file {\tt ESMC\_NewAlarm.h)
//
//-------------------------------------------------------------------------
#define ESMC_FILENAME "ESMCI_NewAlarm.C"

// associated class definition file
#include "ESMCI_NewAlarm.h"

// insert any higher level, 3rd party or system includes here
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <iostream>
#include <limits.h>

#include "ESMCI_LogErr.h"
#include "ESMCI_Clock.h"

//-------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-------------------------------------------------------------------------

namespace ESMCI{

// initialize static newalarm instance counter
// TODO: inherit from ESMC_Base class
int NewAlarm::count=0;

//
//-------------------------------------------------------------------------
//-------------------------------------------------------------------------
//
// This section includes all the NewAlarm routines
//
//

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMCI_newalarmCreate - allocates and initializes an NewAlarm object
//
// !INTERFACE:
      NewAlarm *ESMCI_newalarmCreate(
//
// !RETURN VALUE:
//    pointer to newly allocated NewAlarm
//
// !ARGUMENTS:
      int                nameLen,           // in
      const char        *name,              // in
      Clock        *clock,             // in
      Time         *ringTime,          // in
      TimeInterval *ringInterval,      // in
      Time         *stopTime,          // in
      TimeInterval *ringDuration,      // in
      int               *ringTimeStepCount, // in
      Time         *refTime,           // in
      bool              *enabled,           // in
      bool              *sticky,            // in
      int               *rc ) {             // out - return code
//
// !DESCRIPTION:
//      ESMF routine which allocates and initializes {\tt ESMC\_NewAlarm} values.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI_newalarmCreate(new)"

    NewAlarm *newalarm;
    int returnCode;

    // default return code
    if (rc != ESMC_NULL_POINTER) *rc = ESMC_RC_NOT_IMPL;

    if (ringTime == ESMC_NULL_POINTER && ringInterval == ESMC_NULL_POINTER) {
      ESMC_LogDefault.Write("Must specify at least one of ringTime or "
                            "ringInterval.", ESMC_LOGMSG_WARN,ESMC_CONTEXT);
      return(ESMC_NULL_POINTER);
    }
 
    try {
      newalarm = new NewAlarm;
    }
    catch (...) {
      ESMC_LogDefault.AllocError(ESMC_CONTEXT,rc);
      return(ESMC_NULL_POINTER);
    }
    
    // associate this newalarm with given clock
    newalarm->clock = clock;

    // TODO: use inherited methods from ESMC_Base
    if (name != ESMC_NULL_POINTER) {
      if (nameLen < ESMF_MAXSTR) {
        strncpy(newalarm->name, name, nameLen);
        newalarm->name[nameLen] = '\0';  // null terminate
      } else {
        // truncate
        strncpy(newalarm->name, name, ESMF_MAXSTR-1);
        newalarm->name[ESMF_MAXSTR-1] = '\0';  // null terminate

        char logMsg[ESMF_MAXSTR];
        sprintf(logMsg, "newalarm name %s, length >= ESMF_MAXSTR; truncated.", 
                name);
        ESMC_LogDefault.Write(logMsg, ESMC_LOGMSG_WARN,ESMC_CONTEXT);
        // TODO: return ESMF_WARNING when defined
        // if (rc != ESMC_NULL_POINTER) *rc = ESMF_WARNING;
      }
    } else {
      // create default name "NewAlarmNNN"
      sprintf(newalarm->name, "NewAlarm%3.3d", newalarm->id);
    }

    if (ringTime != ESMC_NULL_POINTER) {
      newalarm->ringTime = newalarm->prevRingTime = newalarm->firstRingTime = *ringTime;
    }
    if (ringInterval != ESMC_NULL_POINTER) {
      newalarm->ringInterval = *ringInterval;

      // if ringTime not specified, calculate
      //   ringTime from the current clock time

      // TODO: handle case where *ringTime < clock->currTime;
      //       same or similar to refTime
      
      if (ringTime == ESMC_NULL_POINTER) {
        // works for positive or negative ringInterval
        newalarm->ringTime = clock->currTime + newalarm->ringInterval;
        newalarm->prevRingTime = newalarm->firstRingTime = newalarm->ringTime;
      }
    }
    if (stopTime != ESMC_NULL_POINTER) {
      newalarm->stopTime = *stopTime;
    }
    if (ringDuration != ESMC_NULL_POINTER) {
      newalarm->ringDuration = *ringDuration;
    }
    if (ringTimeStepCount != ESMC_NULL_POINTER) {
      newalarm->ringTimeStepCount = *ringTimeStepCount;
    }
    if (refTime != ESMC_NULL_POINTER) {
      newalarm->refTime = *refTime;
      // TODO:  for ringInterval, calculate 1st ringTime > clock->currTime,
      //        (&& > ringTime, if specified), using refTime as the base.
    } else {
      // TODO:  default to clock's current time (or ringTime, if specified?)
    }
    if (enabled != ESMC_NULL_POINTER) {
      newalarm->enabled = *enabled;
    }
    if (sticky != ESMC_NULL_POINTER) {
      newalarm->sticky = *sticky;
    }

    // TODO:  invoke private method, shared with NewAlarm::set(), to calculate
    //        first ringTime for interval newalarms, given ringInterval and none,
    //        one or both of refTime and ringTime.  Will replace logic in
    //        corresponding above sections.
    //        this->ringTime > clock->currTime &&
    //        this->ringTime > (passed in) ringTime

    returnCode = newalarm->NewAlarm::validate();
    if (ESMC_LogDefault.MsgFoundError(returnCode, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, rc)) {
      // TODO: distinguish non-fatal rc's (warnings, info) at this level (C++),
      //   and at the F90 level, so isInit flag can be set to usable value.
      delete newalarm;
      return(ESMC_NULL_POINTER);
    } else {
      // add this new valid newalarm to the given clock
      if (rc != ESMC_NULL_POINTER) *rc = ESMF_SUCCESS;
      returnCode = clock->Clock::addNewAlarm(newalarm);
      ESMC_LogDefault.MsgFoundError(returnCode, ESMCI_ERR_PASSTHRU,
        ESMC_CONTEXT, rc);
    }
 
    return(newalarm);

 } // end ESMCI_newalarmCreate (new)

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMCI_newalarmCreate- Creates a copy of an newalarm
//
// !INTERFACE:
      NewAlarm *ESMCI_newalarmCreate(
//
// !RETURN VALUE:
//     pointer to newly allocated NewAlarm
//
// !ARGUMENTS:
      NewAlarm *newalarm,  // in  - newalarm to copy
      int        *rc) {   // out - return code 

// !DESCRIPTION:
//      Creates a new copy of the given newalarm.
//
//EOP
// !REQUIREMENTS:

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI_newalarmCreate(copy)"

    NewAlarm *newalarmCopy;
    int returnCode;

    // default return code
    if (rc != ESMC_NULL_POINTER) *rc = ESMC_RC_NOT_IMPL;

    // can't copy a non-existent object
    if (newalarm == ESMC_NULL_POINTER) {
      ESMC_LogDefault.Write("Can't copy a non-existent newalarm",
                                    ESMC_LOGMSG_WARN,ESMC_CONTEXT);
      return(ESMC_NULL_POINTER);
    }

    try {
      // allocate new newalarm and pass given newalarm to copy constructor.
      newalarmCopy = new NewAlarm(*newalarm);
    }
    catch (...) {
      ESMC_LogDefault.AllocError(ESMC_CONTEXT,rc);
      return(ESMC_NULL_POINTER);
    }

    returnCode = newalarmCopy->NewAlarm::validate();
    if (ESMC_LogDefault.MsgFoundError(returnCode, ESMCI_ERR_PASSTHRU,
      ESMC_CONTEXT, rc)) {
      // TODO: distinguish non-fatal rc's (warnings, info) at this level (C++),
      //   and at the F90 level, so isInit flag can be set to usable value.
      delete newalarmCopy;
      return(ESMC_NULL_POINTER);
    } else {
      // add this new valid newalarm copy to the same clock as the original newalarm
      if (rc != ESMC_NULL_POINTER) *rc = ESMF_SUCCESS;
      if (newalarmCopy->clock != ESMC_NULL_POINTER) {
        returnCode = (newalarmCopy->clock)->Clock::addNewAlarm(newalarmCopy);
        ESMC_LogDefault.MsgFoundError(returnCode, ESMCI_ERR_PASSTHRU,
          ESMC_CONTEXT, rc);
      }
    }

    if (rc != ESMC_NULL_POINTER) *rc = ESMF_SUCCESS;
    return(newalarmCopy);     

 } // end ESMCI_newalarmCreate (copy)

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMCI_newalarmDestroy - free an NewAlarm created with Create
//
// !INTERFACE:
      int ESMCI_newalarmDestroy(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      NewAlarm **newalarm) {  // in - NewAlarm to destroy
//
// !DESCRIPTION:
//      ESMF routine which destroys a NewAlarm object previously allocated
//      via an {\tt ESMCI_newalarmCreate} routine.  Define for deep classes only.
//
//EOP

  // Initialize return code; assume routine not implemented
  int rc = ESMC_RC_NOT_IMPL;

  // can't work with a non-existent object
  if (newalarm == ESMC_NULL_POINTER) {
    ESMC_LogDefault.Write("newalarm pointer NULL", ESMC_LOGMSG_WARN, ESMC_CONTEXT);
    return(ESMF_FAILURE);
  }

  // TODO: newalarm->NewAlarm::destruct(); constructor calls it!

  // remove newalarm from associated clock's newalarmList
  if ((*newalarm)->clock != ESMC_NULL_POINTER) {
    rc = ((*newalarm)->clock)->Clock::removeNewAlarm(*newalarm);
    ESMC_LogDefault.MsgFoundError(rc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc);
  }
  delete *newalarm;   // ok to delete null pointer
  *newalarm = ESMC_NULL_POINTER;
  return(rc);

 } // end ESMCI_newalarmDestroy

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  NewAlarm::set - Sets an NewAlarm's properties
//
// !INTERFACE:
      int NewAlarm::set(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int                nameLen,           // in
      const char        *name,              // in
      Clock       **clock,             // in
      Time         *ringTime,          // in
      TimeInterval *ringInterval,      // in
      Time         *stopTime,          // in
      TimeInterval *ringDuration,      // in
      int               *ringTimeStepCount, // in
      Time         *refTime,           // in
      bool              *ringing,           // in
      bool              *enabled,           // in
      bool              *sticky) {          // in
//
// !DESCRIPTION:
//      ESMF routine which only initializes {\tt ESMC\_NewAlarm} values;
//      it does not allocate any resources.  
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::NewAlarm::set()"

  // Initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;
 
    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", ESMC_CONTEXT, &rc);
      return(rc);
    }

    // save current values to restore in case of failure
    NewAlarm saveNewAlarm = *this;

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
        sprintf(logMsg, "newalarm name %s, length >= ESMF_MAXSTR; truncated.", 
                name);
        ESMC_LogDefault.Write(logMsg, ESMC_LOGMSG_WARN,ESMC_CONTEXT);
        // TODO: return ESMF_WARNING when defined
        // rc = ESMF_WARNING;
      }
    }

    if (clock != ESMC_NULL_POINTER) {
      // remove this newalarm from associated clock's newalarmList
      if (this->clock != ESMC_NULL_POINTER) {
        rc = (this->clock)->Clock::removeNewAlarm(this);
        ESMC_LogDefault.MsgFoundError(rc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
          &rc);
      }

      // and add it to the given clock's newalarmList
      rc = (*clock)->Clock::addNewAlarm(this);
      ESMC_LogDefault.MsgFoundError(rc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc);

      // this newalarm is now associated with the given clock
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
     
    // TODO:  invoke private method, shared with ESMCI_newalarmCreate(), to
    //        calculate next ringTime for interval newalarms, given ringInterval
    //        and none, one or both of refTime and ringTime.  Will replace
    //        logic in corresponding above sections.
    //        this->ringTime > clock->currTime &&
    //        this->ringTime > (passed in) ringTime

    rc = NewAlarm::validate();
    if (ESMC_LogDefault.MsgFoundError(rc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT,
      &rc)) {
      // restore original newalarm values
      *this = saveNewAlarm;
    }

    rc = ESMF_SUCCESS;
    return(rc);

 } // end NewAlarm::set

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  NewAlarm::get - Gets an newalarm's properties
//
// !INTERFACE:
      int NewAlarm::get(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int                nameLen,                // in
      int               *tempNameLen,            // out
      char              *tempName,               // out
      Clock       **clock,                  // out
      Time         *ringTime,               // out
      Time         *prevRingTime,           // out
      TimeInterval *ringInterval,           // out
      Time         *stopTime,               // out
      TimeInterval *ringDuration,           // out
      int               *ringTimeStepCount,      // out
      int               *timeStepRingingCount,   // out
      Time         *ringBegin,              // out
      Time         *ringEnd,                // out
      Time         *refTime,                // out
      bool              *ringing,                // out
      bool              *ringingOnPrevTimeStep,  // out
      bool              *enabled,                // out
      bool              *sticky) {               // out
//
// !DESCRIPTION:
//      Gets {\tt ESMC\_NewAlarm} property values;
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::NewAlarm::get()"

  // Initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;

    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", ESMC_CONTEXT, &rc);
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
        sprintf(logMsg, "For newalarm name %s, "
                "length >= given character array; truncated.", this->name);
        ESMC_LogDefault.Write(logMsg, ESMC_LOGMSG_WARN,ESMC_CONTEXT);
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
     
 } // end NewAlarm::get

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  NewAlarm::enable - enables an NewAlarm object to function
//
// !INTERFACE:
      int NewAlarm::enable(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      ESMF routine which enables an {\tt ESMC\_NewAlarm} object to function
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::NewAlarm::enable()"

  // Initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;
 
    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", ESMC_CONTEXT, &rc);
      return(rc);
    }

    enabled = true;

    rc = ESMF_SUCCESS;
    return(rc);           

 } // end NewAlarm::enable

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  NewAlarm::disable - disables an NewAlarm object from functioning
//
// !INTERFACE:
      int NewAlarm::disable(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      ESMF routine which disables an {\tt ESMC\_NewAlarm} object from functioning
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::NewAlarm::disable()"

  // Initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;
 
    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", ESMC_CONTEXT, &rc);
      return(rc);
    }

    ringing = false;
    enabled = false;

    rc = ESMF_SUCCESS;
    return(rc);           

 } // end NewAlarm::disable

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  NewAlarm::isEnabled - check if NewAlarm is enabled
//
// !INTERFACE:
      bool NewAlarm::isEnabled(
//
// !RETURN VALUE:
//    bool is enabled or not
//
// !ARGUMENTS:
      int  *rc) const {        // out - error return code
//
// !DESCRIPTION:
//    checks if {\tt ESMC\_NewAlarm}'s enabled state is set.
//
//EOP
// !REQUIREMENTS:

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::NewAlarm::isEnabled()"

    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", ESMC_CONTEXT, rc);
      return(false);
    }

    // Initialize return code; assume routine not implemented
    if (rc != ESMC_NULL_POINTER) *rc = ESMF_SUCCESS;

    return(enabled);

 } // end NewAlarm::isEnabled

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  NewAlarm::ringerOn - sets an NewAlarm to the ringing state
//
// !INTERFACE:
      int NewAlarm::ringerOn(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      ESMF routine which sets an {\tt ESMC\_NewAlarm} object to the
//      ringing state.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::NewAlarm::ringerOn()"

  // Initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;
 
    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", ESMC_CONTEXT, &rc);
      return(rc);
    }

    if(!enabled) {
      char logMsg[ESMF_MAXSTR];
      sprintf(logMsg, "Attempted to turn on ringer of disabled newalarm %s.",
              this->name);
      ESMC_LogDefault.Write(logMsg, ESMC_LOGMSG_WARN,ESMC_CONTEXT);
      // TODO: return ESMF_WARNING when defined
      // return(ESMF_WARNING);
      return(ESMF_FAILURE);
    }

    ringing = true;

    rc = ESMF_SUCCESS;
    return(rc);      

 } // end NewAlarm::ringerOn

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  NewAlarm::ringerOff - turns off an NewAlarm's ringing state
//
// !INTERFACE:
      int NewAlarm::ringerOff(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      ESMF routine which turns off an {\tt ESMC\_NewAlarm}'s ringing state.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::NewAlarm::ringerOff()"

  // Initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;
 
    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", ESMC_CONTEXT, &rc);
      return(rc);
    }

    // turn newalarm off
    ringing = false;
    timeStepRingingCount = 0;

    if (clock->direction == ESMF_DIRECTION_FORWARD) {
      // remember this time, so if we go in reverse, we will know when
      //   to turn the newalarm back on.
      //   TODO:  Assumes constant ringInterval between successive ringEnds;
      //          saves only last ringEnd.  Make ringEnd an array to save all
      //          end times, which may vary (e.g. due to variable timeSteps).
      ringEnd = clock->currTime;

    } else {      // ESMF_DIRECTION_REVERSE
      // for sticky newalarms, step back ring times
      if (sticky && ringTime != firstRingTime) {
        ringTime     -= ringInterval;
        prevRingTime -= ringInterval; 

        // get clock's timestep direction: positive or negative
        bool positive =
               (clock->currAdvanceTimeStep.absValue() ==
                clock->currAdvanceTimeStep) ? true : false;

        // step back ringEnd only if it was advanced past ringTime
        //  before the clock loop ended (covers case where last ringTime
        //  equals the clock->stopTime and newalarm is processed before
        //  the clockAdvance()).
        if ((positive  && ringEnd > ringTime) ||
            (!positive && ringEnd < ringTime)) {
          ringEnd -= ringInterval;
        }
      }
    }

    rc = ESMF_SUCCESS;
    return(rc);    

 } // end NewAlarm::ringerOff

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  NewAlarm::isRinging - check if NewAlarm is ringing
//
// !INTERFACE:
      bool NewAlarm::isRinging(
//
// !RETURN VALUE:
//    bool is ringing or not
//
// !ARGUMENTS:
      int  *rc) const {        // out - error return code
//
// !DESCRIPTION:
//    Checks if {\tt ESMC\_NewAlarm}'s ringing state is set.
//
//    See also method ESMC\_ClockGetNewAlarmList(areRinging, ...) to get
//    a list of all ringing newalarms belonging to a {\tt ESMC\_Clock}.
//
//EOP
// !REQUIREMENTS:

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::NewAlarm::isRinging()"

    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", ESMC_CONTEXT, rc);
      return(false);
    }

    // Initialize return code; assume routine not implemented
    if (rc != ESMC_NULL_POINTER) *rc = ESMF_SUCCESS;

    return(enabled && ringing);

 } // end NewAlarm::isRinging

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  NewAlarm::willRingNext - check if NewAlarm will ring upon the next clock timestep
//
// !INTERFACE:
      bool NewAlarm::willRingNext(
//
// !RETURN VALUE:
//    bool will ring or not
//
// !ARGUMENTS:
      TimeInterval *timeStep,   // in - optional timestep to use instead
                                     //      of the clock's
      int  *rc) const {              // out - error return code
//
// !DESCRIPTION:
//    Checks if {\tt ESMC\_NewAlarm}'s ringing state will be set on the next
//    clock timestep, using either the clock's current timestep,
//    or a passed-in one.
//
//    See also method ESMC\_ClockGetNewAlarmList(willRingNext, ...) to get
//    a list of all newalarms belonging to a {\tt ESMC\_Clock} that will ring on
//    the next time step.

//
//EOP
// !REQUIREMENTS:

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::NewAlarm::willRingNext()"

    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", ESMC_CONTEXT, rc);
      return(false);
    }

    // default return code
    if (rc != ESMC_NULL_POINTER) *rc = ESMC_RC_NOT_IMPL;

    // must be associated with a clock
    if(clock == ESMC_NULL_POINTER) {
      char logMsg[ESMF_MAXSTR];
      sprintf(logMsg, "newalarm %s is not associated with any clock.", name);
      ESMC_LogDefault.Write(logMsg, ESMC_LOGMSG_WARN,ESMC_CONTEXT);
      if (rc != ESMC_NULL_POINTER) *rc = ESMC_RC_PTR_NULL;
      return(false);
    }

    // get clock's next time
    Time clockNextTime;
    clock->Clock::getNextTime(&clockNextTime, timeStep);

    // if specified, use passed-in timestep, otherwise use clock's
    TimeInterval tStep = (timeStep != ESMC_NULL_POINTER) ?
                               *timeStep : clock->timeStep;

    // get timestep direction: positive or negative
    bool positive = tStep.TimeInterval::absValue() == tStep ? true : false;

    // check if newalarm will turn on
    bool willRing = false;
    if (enabled) {
      willRing = (positive) ?
                  clockNextTime >= ringTime && clock->currTime < ringTime :
                  clockNextTime <= ringTime && clock->currTime > ringTime;
    }

    if (rc != ESMC_NULL_POINTER) *rc = ESMF_SUCCESS;
    return(willRing);

 } // end NewAlarm::willRingNext

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  NewAlarm::wasPrevRinging - check if NewAlarm was ringing on the previous clock timestep
//
// !INTERFACE:
      bool NewAlarm::wasPrevRinging(
//
// !RETURN VALUE:
//    bool was previously ringing or not
//
// !ARGUMENTS:
      int  *rc) const {        // out - error return code
//
// !DESCRIPTION:
//    Checks if {\tt ESMC\_NewAlarm}'s ringing state was set on the previous
//    clock timestep.
//
//    See also method ESMC\_ClockGetNewAlarmList(werePrevRinging, ...) to
//    get a list of all newalarms belonging to a {\tt ESMC\_Clock} that were
//    ringing on the previous time step.

//
//EOP
// !REQUIREMENTS:

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::NewAlarm::wasPrevRinging()"

    // Initialize return code; assume routine not implemented
    if (rc != ESMC_NULL_POINTER) *rc = ESMC_RC_NOT_IMPL;

    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", ESMC_CONTEXT, rc);
      return(false);
    }

    if (rc != ESMC_NULL_POINTER) *rc = ESMF_SUCCESS;

    return(ringingOnPrevTimeStep);

 } // end NewAlarm::wasPrevRinging

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  NewAlarm::setToSticky - sets an NewAlarm's sticky state 
//
// !INTERFACE:
      int NewAlarm::setToSticky(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      ESMF routine which sets an {\tt ESMC\_NewAlarm}'s sticky flag.
//
//EOP
// !REQUIREMENTS:

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::NewAlarm::setToSticky()"

  // Initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;
 
    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", ESMC_CONTEXT, &rc);
      return(rc);
    }

    sticky = true;

    rc = ESMF_SUCCESS;
    return(rc);          

 } // end NewAlarm::setToSticky

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  NewAlarm::notSticky - unsets an NewAlarm's sticky state 
//
// !INTERFACE:
      int NewAlarm::notSticky(TimeInterval *ringDuration,
                                          int *ringTimeStepCount) {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      ESMF routine which unsets an {\tt ESMC\_NewAlarm}'s sticky flag,
//      and optionally sets a ring duration either in terms of an
//      {\tt ESMC\_TimeInterval} or a integer number of clock timesteps.
//
//EOP
// !REQUIREMENTS:

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::NewAlarm::notSticky()"

  // Initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;
 
    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", ESMC_CONTEXT, &rc);
      return(rc);
    }

    sticky = false;

    // mutually exclusive: can only specify one ring duration type
    if (ringDuration != ESMC_NULL_POINTER &&
        ringTimeStepCount != ESMC_NULL_POINTER) {
      char logMsg[ESMF_MAXSTR];
      sprintf(logMsg, 
              "NewAlarm %s: can only specify one type of ring duration, not both.",
              name);
      ESMC_LogDefault.Write(logMsg, ESMC_LOGMSG_WARN,ESMC_CONTEXT);
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

 } // end NewAlarm::notSticky

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  NewAlarm::isSticky - check if NewAlarm is sticky
//
// !INTERFACE:
      bool NewAlarm::isSticky(
//
// !RETURN VALUE:
//    bool is sticky or not
//
// !ARGUMENTS:
      int  *rc) const {        // out - error return code
//
// !DESCRIPTION:
//    checks if {\tt ESMC\_NewAlarm}'s sticky state is set.
//
//EOP
// !REQUIREMENTS:

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::NewAlarm::isSticky()"

    // Initialize return code; assume routine not implemented
    if (rc != ESMC_NULL_POINTER) *rc = ESMC_RC_NOT_IMPL;

    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", ESMC_CONTEXT, rc);
      return(false);
    }

    if (rc != ESMC_NULL_POINTER) *rc = ESMF_SUCCESS;

    return(sticky);

 } // end NewAlarm::isSticky

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  NewAlarm::checkRingTime - check if time to ring
//
// !INTERFACE:
      bool NewAlarm::checkRingTime(
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
 #define ESMC_METHOD "ESMCI::NewAlarm::checkRingTime()"

    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", ESMC_CONTEXT, rc);
      return(false);
    }

    // default return code
    if (rc != ESMC_NULL_POINTER) *rc = ESMC_RC_NOT_IMPL;

    // must be associated with a clock
    if(clock == ESMC_NULL_POINTER) {
      char logMsg[ESMF_MAXSTR];
      sprintf(logMsg, "newalarm %s is not associated with any clock.", name);
      ESMC_LogDefault.Write(logMsg, ESMC_LOGMSG_WARN,ESMC_CONTEXT);
      return(false);
    }

    // get clock's timestep direction: positive or negative
    bool positive = (clock->currAdvanceTimeStep.absValue() ==
                     clock->currAdvanceTimeStep) ? true : false;

    if (clock->direction == ESMF_DIRECTION_FORWARD) {

      // carry previous flag forward
      ringingOnPrevTimeStep = ringingOnCurrTimeStep;
    
      // perform pre-checks first ...

      if (enabled) {
        if (userChangedRingInterval) {
          // check that user's new ringInterval is same sign as timeStep
          TimeInterval zeroTimeStep;
          if ((ringInterval > zeroTimeStep &&
               clock->currAdvanceTimeStep < zeroTimeStep) ||
              (ringInterval < zeroTimeStep &&
               clock->currAdvanceTimeStep > zeroTimeStep) ) {
            ESMC_LogDefault.MsgFoundError(ESMC_RC_VAL_WRONG,
              "; user changed newalarm ringInterval, "
              "which is not same sign as clock timeStep.", ESMC_CONTEXT, rc);
            return(false);
          }
        }
        if (userChangedRingTime) {
          // check that user's new ringTime is within ringable range
          if (positive ? clock->currTime > ringTime :
                         clock->currTime < ringTime) {
            ESMC_LogDefault.MsgFoundError(ESMC_RC_VAL_OUTOFRANGE,
              "; user changed newalarm ringTime, "
              "which is not within clock ringable range", ESMC_CONTEXT, rc);
            return(false);
          }
        }
        // if clock timeStep sign changed, adjust ringInterval accordingly 
        TimeInterval zeroTimeStep;
        bool userChangedTimeStepSign = 
           ( (clock->currAdvanceTimeStep < zeroTimeStep &&
              clock->prevAdvanceTimeStep > zeroTimeStep) ||
             (clock->currAdvanceTimeStep > zeroTimeStep &&
              clock->prevAdvanceTimeStep < zeroTimeStep) );
        if (userChangedTimeStepSign) { 
          if (!userChangedRingInterval) {
            // change sign to match clock timeStep
            ringInterval *= -1;
            ringDuration *= -1;
          }
        }
        // if either clock timeStep sign changed or clock direction mode
        //  changed, pull back ringTime into ringable range
        TimeInterval zeroTimeInterval1(0,0,1,0,0,0);
        if ( (userChangedTimeStepSign || clock->userChangedDirection) && (ringInterval != zeroTimeInterval1)) {
          if (!userChangedRingTime) {
            // std::cout << "stopTime: " << (stopTime.Time::validate("initialized")) << (stopTime.Time::validate("initialized") == ESMF_SUCCESS) << std::endl;
            bool stopTimeEnabled = 
                        stopTime.Time::validate("initialized") == ESMF_SUCCESS;
            while (positive ? clock->prevTime >= ringTime :
                              clock->prevTime <= ringTime) {
              // check if ringing stopTime limit reached,  TODO: test
              if (stopTimeEnabled) { 
                if (positive ? ringTime >= (stopTime - ringInterval) :
                               ringTime <= (stopTime - ringInterval) ) break;
              }
              // otherwise increment it
              //std::cout << "ringInterval: " << std::endl; 
              //ringInterval.print();
              //std::cout << "ringTime before: " << std::endl; 
              //ringTime.print();

              prevRingTime = ringTime;
              ringTime += ringInterval;

              //std::cout << "ringTime after: " << std::endl; 
              //ringTime.print();
              //std::cout << "clock->prevTime: " << std::endl; 
              //clock->prevTime.print();
            }
          }
        }
        // done processing changed flags, reset if necessary
        if (userChangedRingTime)     userChangedRingTime     = false;
        if (userChangedRingInterval) userChangedRingInterval = false;
        //if (clock->userChangedDirection) clock->userChangedDirection = false;
      }

      // ... then check if time to turn on newalarm
      if (!ringing && enabled) 
         NewAlarm::checkTurnOn(positive);

      // else if not sticky, check if time to turn off newalarm
      //   (user is responsible for turning off sticky newalarms via RingerOff())
      // TODO:  maybe should not be else clause, just an "if" on its own, since
      // ringTimeStepCount=0 would imply turning off in the same timeStep? But
      // would need to move timeStepRingingCount++ up.
      else if (!sticky && ringing && enabled) {

        // first check if next newalarm time has been reached,
        // then check if time to turn off newalarm.
        if (!NewAlarm::checkTurnOn(positive)) {
          TimeInterval zeroTimeInterval(0,0,1,0,0,0);
          if (ringTimeStepCount == 1 && 
              ringDuration != zeroTimeInterval) { // use ringDuration ...
            TimeInterval cumulativeRinging;
            cumulativeRinging = clock->currTime - ringBegin;
            if (cumulativeRinging.TimeInterval::absValue() >=
                ringDuration.TimeInterval::absValue()) {
              ringingOnCurrTimeStep = ringing = false;
              timeStepRingingCount = 0;
            }
          // ... otherwise use ringTimeStepCount
          } else if (ringTimeStepCount >= 1) {
            if (timeStepRingingCount >= ringTimeStepCount) {
              ringingOnCurrTimeStep = ringing = false;
              timeStepRingingCount = 0;
            }
          } // TODO: else error, ringTimeStepCount <= 0 (ringing counter is
            // always positive) Validate() ?
        }
      }

      // count for how many clock time steps the newalarm is ringing
      if (ringing) timeStepRingingCount++;

      // ensure a sticky repeatable newalarm's ringTime remains in ringable range,
      // in case it is not turned off for a while, or if
      // clock->timeStep >= ringInterval
      TimeInterval zeroTimeInterval(0,0,1,0,0,0);
      if (sticky && ringInterval != zeroTimeInterval &&
                    clock->advanceCount != 0) {
        //printf("ringTime before:\n");
        //print("ringTime string");
        bool stopTimeEnabled = 
                        stopTime.Time::validate("initialized") == ESMF_SUCCESS;
        // works for positive and negative ringIntervals TODO: test negative
        while (positive ? clock->currTime >= ringTime :
                          clock->currTime <= ringTime) {
          // check if ringing stopTime limit reached,  TODO: test
          if (stopTimeEnabled) { 
            if (positive ? ringTime >= (stopTime - ringInterval) :
                           ringTime <= (stopTime - ringInterval) ) break;
          }
          // otherwise increment it
          prevRingTime = ringTime;
          ringTime += ringInterval;
          // TODO:  if in practice, users use a timeStep which is much, much
          //        greater than ringInterval, then a single-step calculated
          //        approach to updating the ringTime, rather than a loop
          //        approach, may be more efficient.
        }
        //printf("ringTime after:\n");
        //print("ringTime string");
      }

    } else { // ESMF_DIRECTION_REVERSE

      // TODO: Make more robust by removing the following simplifying
      //       assumptions:
      //
      //       1) timeSteps are constant throughout clock run (including sign).
      //       2) ringInterval, ringDuration are constant throughout clock run.
      //       3) sticky newalarms must have traversed through at least one newalarm
      //          (to save the ringEnd time) in order to reverse.  For
      //          repeating sticky newalarms, previous ringEnds are assumed to be
      //          equally spaced by a constant ringInterval.
      //
      //       The solution will involve saving clock and newalarm state at every
      //       timeStep, which means dynamically allocated stacks (stacks of
      //       clock and newalarm objects).  These stacks can be
      //       initially sized at Create() time, then reallocated as necessary
      //       (upon those advance() calls which would require more space).
      //       Will need flag upon Create() for user to hint at need for
      //       this extra overhead for reversible clocks and newalarms.

      // Note:  Sticky newalarms need to have been traversed forward in order
      //          to be reversed (to save ringEnd upon user RingerOff() event).
      //          In contrast, non-sticky newalarms can be reversed without first
      //          having been traversed forward.  This implies that the logic
      //          cannot use prev* state variables in order to step back; all
      //          state variables must be reconstructed from timeStep,
      //          ringInterval, and ringDuration.  Hence the use of ringTimeEnd
      //          below.

      // if sticky newalarm, must have traversed forward far enough to have
      //   called RingerOff(), causing the ringEnd time to be saved.
      if(sticky && ringEnd.Time::validate("initialized") != ESMF_SUCCESS) {
        char logMsg[ESMF_MAXSTR];
        sprintf(logMsg, "Sticky newalarm %s cannot be reversed since it has "
                        "not been traversed forward and turned off via "
                        "a user call to ESMF_NewAlarmRingerOff(), thereby "
                        "enabling Time Manager to know the time to turn it "
                        "back on in reverse.", name);
        ESMC_LogDefault.Write(logMsg, ESMC_LOGMSG_WARN,ESMC_CONTEXT);
        return(false);
      }

      // adjust ring state variables if needed
      //   (pull back ringTime, etc. into ringable range if necessary)

      // ... adjust if sticky newalarm ...
      if (sticky) {
        while ( ((positive && ringEnd > clock->currTime) ||
                (!positive && ringEnd < clock->currTime)) &&
                 (ringTime != firstRingTime)) {
          ringEnd      -= ringInterval;
          ringTime      = prevRingTime;
          prevRingTime -= ringInterval;
        }
      }
      // ... or non-sticky newalarm, if user just changed clock direction to
      //   REVERSE ...
      if (clock->userChangedDirection) {
        //clock->userChangedDirection = false; // reset changed flag
        if (!sticky) {
          if (((positive && ringTime > (clock->currTime + clock->timeStep)) ||
              (!positive && ringTime < (clock->currTime + clock->timeStep))) &&
                   (ringTime != firstRingTime)) {
            ringTime      = prevRingTime;
            prevRingTime -= ringInterval;
            NewAlarm::resetRingBegin(positive);
          }
        }
      }

      // ... then determine when newalarm ended ringing in forward mode ...
      Time ringTimeEnd;
      if (enabled) {
        if (sticky) {
          ringTimeEnd = ringEnd;
        } else { // non-sticky
          TimeInterval zeroTimeInterval(0,0,1,0,0,0);
          if (ringTimeStepCount == 1 && 
              ringDuration != zeroTimeInterval) { // use ringDuration ...
            ringTimeEnd = ringBegin + ringDuration; 
          // ... otherwise use ringTimeStepCount
          } else if (ringTimeStepCount >= 1) {
            // If ringBegin hasn't been initialized, set ringBegin to clock's startTime
            if(ringBegin.getCalendar() == NULL){
              ringBegin = clock->startTime;
            }
            ringTimeEnd = ringBegin + ringTimeStepCount * clock->timeStep;
          } // TODO: else error, ringTimeStepCount <= 0 (ringing counter is
            // always positive) Validate() ?
        }
      }

      // ... and use it to check if time to turn newalarm back *on* in reverse mode
      if (!ringing && enabled) {
        if (sticky) {
          ringingOnCurrTimeStep = ringing = (clock->currTime == ringTimeEnd);
        } else {
          ringingOnCurrTimeStep = ringing = (positive) ?
                    (clock->currTime < ringTimeEnd) &&
                      (clock->currTime + clock->timeStep) >= ringTimeEnd :
                                       // (negative)
                    (clock->currTime > ringTimeEnd) &&
                      (clock->currTime + clock->timeStep) <= ringTimeEnd;
        }

        // if just turned on, reconstruct the rest of the state of this
        //   newalarm event
        if (ringing) {
          // determine what ringBegin was for this newalarm event TODO:sticky only?
          //NewAlarm::resetRingBegin(positive);

          // determine what the ending timeStepRingingCount was
          //   for this newalarm event
          timeStepRingingCount =
                    (int) ((clock->currTime - ringBegin) / clock->timeStep) + 1;
        }

      // otherwise check if time to turn *non-sticky* newalarm back *off* in
      // reverse mode (user is responsible for turning off *sticky* newalarms via
      // RingerOff())
      } else if (!sticky && ringing && enabled) {
        if (timeStepRingingCount <= 1) {  // if count down to last one

          // turn newalarm off
          ringingOnCurrTimeStep = ringing = false;
          timeStepRingingCount = 0;

          // look back and reset ringing times for previous newalarm event,
          //   if not past firstRingTime
          // TODO: remove assumption of constant ringInterval; allow for
          //       variable ringIntervals
          if (ringTime != firstRingTime) { 
            ringTime      = prevRingTime;
            prevRingTime -= ringInterval; 
            TimeInterval zeroTimeInterval(0,0,1,0,0,0);
            if (ringTimeStepCount == 1 && 
                ringDuration != zeroTimeInterval) { // use ringDuration ...
              ringTimeEnd = ringTime + ringDuration; 
            // ... otherwise use ringTimeStepCount
            } else if (ringTimeStepCount >= 1) {
              ringTimeEnd = ringTime + ringTimeStepCount * clock->timeStep;
            } // TODO: else error, ringTimeStepCount <= 0 (ringing counter is
              // always positive) Validate() ?
          } else { // reset to initial condition
            prevRingTime = ringTime;
          }

          // determine what ringBegin was for *previous* newalarm event
          NewAlarm::resetRingBegin(positive);

        } else {  // keep newalarm ringing
          // reverse count for how many clock time steps the newalarm was ringing
          timeStepRingingCount--;
        }

      } // if (!sticky)

      // reconstruct whether newalarm was ringing on previous timeStep
      if (enabled) {
        ringingOnPrevTimeStep = (positive) ?
                  clock->prevTime >= ringTime && clock->prevTime < ringTimeEnd :
                  clock->prevTime <= ringTime && clock->prevTime > ringTimeEnd;
      }

    }  // end if ESMF_DIRECTION_REVERSE

    if (rc != ESMC_NULL_POINTER) *rc = ESMF_SUCCESS;

    return(ringing && enabled);

 } // end NewAlarm::checkRingTime

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  NewAlarm(==) - NewAlarm equality comparison    
//
// !INTERFACE:
      bool NewAlarm::operator==(
//
// !RETURN VALUE:
//    bool result
//
// !ARGUMENTS:
      const NewAlarm &newalarm) const {   // in - NewAlarm to compare
//
// !DESCRIPTION:
//      Compare for equality the current object's (this) {\tt ESMC\_NewAlarm} with
//      given {\tt ESMC\_NewAlarm}, return result.  Comparison is based on IDs.
//
//EOP
// !REQUIREMENTS:

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::NewAlarm::operator==()"

    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", ESMC_CONTEXT, ESMC_NULL_POINTER);
      return(false);
    }

    return(id == newalarm.id);

}  // end NewAlarm::operator==

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  NewAlarm(!=) - NewAlarm inequality comparison    
//
// !INTERFACE:
      bool NewAlarm::operator!=(
//
// !RETURN VALUE:
//    bool result
//
// !ARGUMENTS:
      const NewAlarm &newalarm) const {   // in - NewAlarm to compare
//
// !DESCRIPTION:
//      Compare for inequality the current object's (this)
//      {\tt ESMC\_NewAlarm} with given {\tt ESMC\_NewAlarm}, return result.
//      Comparison is based on IDs.
//
//EOP
// !REQUIREMENTS:

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::NewAlarm::operator!=()"

    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", ESMC_CONTEXT, ESMC_NULL_POINTER);
      return(false);
    }

    return(id != newalarm.id);

}  // end NewAlarm::operator!=

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMCI_newalarmReadRestart - restore contents of an NewAlarm
//
// !INTERFACE:
      NewAlarm *ESMCI_newalarmReadRestart(
//
// !RETURN VALUE:
//    pointer to newly allocated and restored NewAlarm
//
// !ARGUMENTS:
      int          nameLen,  // in
      const char  *name,     // in
      int         *rc ) {    // out - return code

//
// !DESCRIPTION:
//      Restore information about an {\tt ESMC\_NewAlarm}.
//      For persistence/checkpointing.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI_newalarmReadRestart()"

    // TODO:  read newalarm state from name, then allocate/restore
    //        (share code with ESMCI_newalarmCreate()).

    // Initialize return code; assume routine not implemented
    if (rc != ESMC_NULL_POINTER) *rc = ESMC_RC_NOT_IMPL;

    return(ESMC_NULL_POINTER);

 } // end ESMCI_newalarmReadRestart

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  NewAlarm::writeRestart - save contents of an NewAlarm
//
// !INTERFACE:
      int NewAlarm::writeRestart(void) const {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      Save information about an {\tt ESMC\_NewAlarm}.
//      For persistence/checkpointing
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::NewAlarm::writeRestart()"

  // Initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;
 
    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", ESMC_CONTEXT, &rc);
      return(rc);
    }

    // TODO:  save newalarm state using name.  Default to disk file.

    rc = ESMF_SUCCESS;
    return(rc);

 } // end NewAlarm::writeRestart

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  NewAlarm::validate - internal consistency check for an NewAlarm
//
// !INTERFACE:
      int NewAlarm::validate(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {    // in - validate options
//
// !DESCRIPTION:
//      Validates that a {\tt ESMC\_NewAlarm} is internally consistent.
//      Returns error code if problems are found.  {\tt ESMC\_Base}
//      class method.
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::NewAlarm::validate()"

  // Initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;
 
    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", ESMC_CONTEXT, &rc);
      return(rc);
    }

    // must have a ring time; ringDuration, stopTime, prevRingTime optional
    if (ringTime.Time::validate() != ESMF_SUCCESS) {
      char logMsg[ESMF_MAXSTR];
      sprintf(logMsg, "NewAlarm %s: invalid ringTime.", name);
      ESMC_LogDefault.Write(logMsg, ESMC_LOGMSG_WARN,ESMC_CONTEXT);
      return(ESMF_FAILURE);
    }

    // invalid state
    if (!enabled && ringing) {
      char logMsg[ESMF_MAXSTR];
      sprintf(logMsg, "NewAlarm %s: invalid state: disabled and ringing.", name);
      ESMC_LogDefault.Write(logMsg, ESMC_LOGMSG_WARN,ESMC_CONTEXT);
      return(ESMF_FAILURE);
    }

    // TODO: validate id ?

    rc = ESMF_SUCCESS;
    return(rc);

 } // end NewAlarm::validate

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  NewAlarm::print - print contents of an NewAlarm
//
// !INTERFACE:
      int NewAlarm::print(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const char *options) const {    // in - print options
//
// !DESCRIPTION:
//      Print information about an {\tt ESMC\_NewAlarm}.  For testing/debugging.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::NewAlarm::print()"

  // Initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;
 
    if (this == ESMC_NULL_POINTER) {
      ESMC_LogDefault.MsgFoundError(ESMC_RC_PTR_NULL,
         "; 'this' pointer is NULL.", ESMC_CONTEXT, &rc);
      return(rc);
    }

    printf("NewAlarm ----------------------------------\n");

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
          clock->Clock::print("name");
        } else {
          clock->Clock::print();
        }
      }
      else if (strncmp(opts, "ringinterval", 12) == 0) {
        printf("ringInterval = \n");
        if (strstr(opts, "string") != ESMC_NULL_POINTER) {
          ringInterval.TimeInterval::print("string");
        } else {
          ringInterval.TimeInterval::print();
        }
      }
      else if (strncmp(opts, "ringduration", 12) == 0) {
        printf("ringDuration = \n");
        if (strstr(opts, "string") != ESMC_NULL_POINTER) {
          ringDuration.TimeInterval::print("string");
        } else {
          ringDuration.TimeInterval::print();
        }
      }
      else if (strncmp(opts, "ringtimestepcount", 17) == 0) {
        printf("ringTimeStepCount = %d\n", ringTimeStepCount);
      }
      else if (strncmp(opts, "ringtime", 8) == 0) {
        printf("ringTime = \n");
        if (strstr(opts, "string") != ESMC_NULL_POINTER) {
          ringTime.Time::print("string");
        } else {
          ringTime.Time::print();
        }
      }
      else if (strncmp(opts, "firstringtime", 13) == 0) {
        printf("firstRingTime = \n");
        if (strstr(opts, "string") != ESMC_NULL_POINTER) {
          firstRingTime.Time::print("string");
        } else {
          firstRingTime.Time::print();
        }
      }
      else if (strncmp(opts, "prevringtime", 12) == 0) {
        printf("prevRingTime = \n");
        if (strstr(opts, "string") != ESMC_NULL_POINTER) {
          prevRingTime.Time::print("string");
        } else {
          prevRingTime.Time::print();
        }
      }
      else if (strncmp(opts, "stoptime", 8) == 0) {
        printf("stopTime = \n");
        if (strstr(opts, "string") != ESMC_NULL_POINTER) {
          stopTime.Time::print("string");
        } else {
          stopTime.Time::print();
        }
      }
      else if (strncmp(opts, "ringbegin", 9) == 0) {
        printf("ringBegin = \n");
        if (strstr(opts, "string") != ESMC_NULL_POINTER) {
          ringBegin.Time::print("string");
        } else {
          ringBegin.Time::print();
        }
      }
      else if (strncmp(opts, "ringend", 7) == 0) {
        printf("ringEnd = \n");
        if (strstr(opts, "string") != ESMC_NULL_POINTER) {
          ringEnd.Time::print("string");
        } else {
          ringEnd.Time::print();
        }
      }
      else if (strncmp(opts, "reftime", 7) == 0) {
        printf("refTime = \n");
        if (strstr(opts, "string") != ESMC_NULL_POINTER) {
          refTime.Time::print("string");
        } else {
          refTime.Time::print();
        }
      }
      else if (strncmp(opts, "timestepringingcount", 20) == 0) {
        printf("timeStepRingingCount = %d\n", timeStepRingingCount);
      }
      else if (strncmp(opts, "ringingonprevtimestep", 21) == 0) {
        printf("ringingOnPrevTimeStep = %s\n",
                ringingOnPrevTimeStep ? "true" : "false");
      }
      else if (strncmp(opts, "ringing", 7) == 0) {
        printf("ringing = %s\n", ringing ? "true" : "false");
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
      printf("ringInterval = \n"); ringInterval.TimeInterval::print(options);
      printf("ringDuration = \n"); ringDuration.TimeInterval::print(options);
      printf("ringTime = \n");      ringTime.Time::print(options);
      printf("firstRingTime = \n"); firstRingTime.Time::print(options);
      printf("prevRingTime = \n");  prevRingTime.Time::print(options);
      printf("stopTime = \n");      stopTime.Time::print(options);
      printf("ringBegin = \n");     ringBegin.Time::print(options);
      printf("ringEnd = \n");       ringEnd.Time::print(options);
      printf("refTime = \n");       refTime.Time::print(options);
      printf("ringTimeStepCount = %d\n",    ringTimeStepCount);
      printf("timeStepRingingCount = %d\n", timeStepRingingCount);
      printf("ringing = %s\n", ringing ? "true" : "false");
      printf("ringingOnPrevTimeStep = %s\n",
              ringingOnPrevTimeStep ?    "true" : "false");
      printf("enabled = %s\n", enabled ? "true" : "false");
      printf("sticky = %s\n",  sticky ?  "true" : "false");
    }

    printf("end NewAlarm ------------------------------\n\n");

    rc = ESMF_SUCCESS;
    return(rc);

 } // end NewAlarm::print

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  NewAlarm - native C++ constructor
//
// !INTERFACE:
      NewAlarm::NewAlarm(void) {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      Initializes a {\tt ESMC\_NewAlarm} with defaults for either
//      C++ or F90, since {\tt ESMC\_NewAlarm} is a deep, dynamically
//      allocated class.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::NewAlarm(void) constructor"

    // Initialize return code; assume routine not implemented
    int rc = ESMC_RC_NOT_IMPL;

    name[0] = '\0';
    clock = ESMC_NULL_POINTER;
    ringTimeStepCount = 1;
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
    rc = ringInterval.TimeInterval::set(ESMC_NULL_POINTER,
                                      ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                      ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                      ESMC_NULL_POINTER, ESMC_NULL_POINTER,
                                      ESMC_NULL_POINTER, &s);

    ESMC_LogDefault.MsgFoundError(rc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc);

 } // end NewAlarm

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  NewAlarm - native C++ copy constructor
//
// !INTERFACE:
      NewAlarm::NewAlarm(
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
      const NewAlarm &newalarm) {  // in - newalarm to copy
//
// !DESCRIPTION:
//      Copies members of given newalarm.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

    *this = newalarm;
    // copy = true;   // TODO: Unique copy ? (id = ++count) (review operator==
                      //       and operator!=)  Must do same in assignment
                      //       overloaded method and interface from F90.
                      //       Also, inherit from ESMC_Base class.

 } // end NewAlarm

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ~NewAlarm - native C++ destructor
//
// !INTERFACE:
      NewAlarm::~NewAlarm(void) {
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

 } // end ~NewAlarm

//-------------------------------------------------------------------------
//  Private methods 
//-------------------------------------------------------------------------

//-------------------------------------------------------------------------
//BOPI
// !IROUTINE:  NewAlarm::checkTurnOn - check if time to turn on newalarm
//
// !INTERFACE:
      bool NewAlarm::checkTurnOn(
//
// !RETURN VALUE:
//    bool whether to turn on newalarm
//
// !ARGUMENTS:
      bool timeStepPositive) {  // in - sign of clock's timeStep,
//                              //        true: positive, false: negative
//
// !DESCRIPTION:
//    Checks whether newalarm should be ringing
//
//EOPI
// !REQUIREMENTS:

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::NewAlarm::checkTurnOn()"

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
      if (stopTime.Time::validate("initialized") == ESMF_SUCCESS) {
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

} // end NewAlarm::checkTurnOn

//-------------------------------------------------------------------------
//BOPI
// !IROUTINE:  NewAlarm::resetRingBegin - reset ringBegin during ESMF_DIRECTION_REVERSE
//
// !INTERFACE:
      int NewAlarm::resetRingBegin(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      bool timeStepPositive) {  // in - sign of clock's timeStep,
//                              //        true: positive, false: negative
//
// !DESCRIPTION:
//      Reconstructs ringBegin for an newalarm event during
//      {\tt ESMF\_DIRECTION\_REVERSE}
//
//EOPI
// !REQUIREMENTS:

 #undef  ESMC_METHOD
 #define ESMC_METHOD "ESMCI::NewAlarm::resetRingBegin()"

    int rc = ESMC_RC_NOT_IMPL;

    // determine ringBegin for previous newalarm event, aligned to the clock
    //  startTime
    // TODO:  assumes constant timeStep; allow for variable timeSteps
    TimeInterval zeroTimeInterval(0,0,1,0,0,0);
    TimeInterval remainder = (ringTime - clock->startTime) % clock->timeStep;
    if (remainder == zeroTimeInterval) {
      ringBegin = ringTime;  // ringBegin coincident with ringTime
    } else { // ringBegin is first timeStep beyond ringTime
      if (!timeStepPositive) remainder = -remainder;
      ringBegin = (ringTime - remainder) + clock->timeStep;
    }

    rc = ESMF_SUCCESS;
    return(rc);

} // end NewAlarm::resetRingBegin

} // namespace ESMCI
