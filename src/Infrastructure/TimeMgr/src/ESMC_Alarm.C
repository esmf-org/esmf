// $Id: ESMC_Alarm.C,v 1.17 2003/08/29 05:31:58 eschwab Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMC Alarm method code (body) file

//-------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ {\tt ESMC\_Alarm} methods declared
// in the companion file {\tt ESMC\_Alarm.h)
//
//-------------------------------------------------------------------------
//
 // insert any higher level, 3rd party or system includes here
 #include <iostream.h>

 // associated class definition file
 #include <ESMC_Alarm.h>

//-------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_Alarm.C,v 1.17 2003/08/29 05:31:58 eschwab Exp $";
//-------------------------------------------------------------------------

//
//-------------------------------------------------------------------------
//-------------------------------------------------------------------------
//
// This section includes all the Alarm routines
//
//

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AlarmSetup - initializes a Alarm object
//
// !INTERFACE:
      int ESMC_Alarm::ESMC_AlarmSetup(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_Time         *ringTime,      // in
      ESMC_TimeInterval *ringInterval,  // in
      ESMC_Time         *stopTime,      // in
      ESMC_TimeInterval *ringDuration,  // in
      int               *nRingDurationTimeSteps, // in
      ESMC_Time         *refTime,       // in
      int               *id,            // in
      bool              *enabled,       // in
      bool              *sticky) {      // in
//
// !DESCRIPTION:
//      ESMF routine which only initializes {\tt ESMC\_Alarm} values;
//      it does not allocate any resources.  
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

    // TODO: ensure initialization if called via F90 interface;
    //       cannot call constructor, because destructor is subsequently
    //       called automatically, returning initialized values to garbage.
    this->ringing = false;
    this->enabled = true;
    this->sticky  = true;
    
    if (ringTime != ESMC_NULL_POINTER) {
      this->ringTime = *ringTime;
    }
    if (ringInterval != ESMC_NULL_POINTER) {
      this->ringInterval = *ringInterval;
      // if ringTime not specified, calculate it from the current time
      if (ringTime == ESMC_NULL_POINTER) {
        ESMC_Time currTime;
        currTime.ESMC_TimeSyncToRealTime();
        this->ringTime = currTime + this->ringInterval;
      }
    }
    if (stopTime != ESMC_NULL_POINTER) {
      this->stopTime = *stopTime;
    }
    if (ringDuration != ESMC_NULL_POINTER) {
      this->ringDuration = *ringDuration;
    }
    if (nRingDurationTimeSteps != ESMC_NULL_POINTER) {
      this->nRingDurationTimeSteps = *nRingDurationTimeSteps;
    }
    if (refTime != ESMC_NULL_POINTER) {
      this->refTime = *refTime;
    }
    if (id != ESMC_NULL_POINTER) {
      this->id = *id;
    }
    if (enabled != ESMC_NULL_POINTER) {
      this->enabled = *enabled;
    }
    if (sticky != ESMC_NULL_POINTER) {
      this->sticky = *sticky;
    }
     
    return(ESMC_AlarmValidate());

 } // end ESMC_AlarmSetup

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
      ESMC_Time         *ringTime,      // in
      ESMC_TimeInterval *ringInterval,  // in
      ESMC_Time         *stopTime,      // in
      ESMC_TimeInterval *ringDuration,  // in
      int               *nRingDurationTimeSteps, // in
      ESMC_Time         *refTime,       // in
      int               *id,            // in
      bool              *ringing,       // in
      bool              *enabled,       // in
      bool              *sticky) {      // in
//
// !DESCRIPTION:
//      ESMF routine which only initializes {\tt ESMC\_Alarm} values;
//      it does not allocate any resources.  
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

    if (ringTime != ESMC_NULL_POINTER) {
      this->ringTime = *ringTime;
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
    if (nRingDurationTimeSteps != ESMC_NULL_POINTER) {
      this->nRingDurationTimeSteps = *nRingDurationTimeSteps;
    }
    if (refTime != ESMC_NULL_POINTER) {
      this->refTime = *refTime;
    }
    if (id != ESMC_NULL_POINTER) {
      this->id = *id;
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
     
    return(ESMC_AlarmValidate());

 } // end ESMC_AlarmSet

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AlarmGet - initializes a Alarm object
//
// !INTERFACE:
      int ESMC_Alarm::ESMC_AlarmGet(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_Time         *ringTime,          // in
      ESMC_Time         *prevRingTime,      // in
      ESMC_TimeInterval *ringInterval,      // in
      ESMC_Time         *stopTime,          // in
      ESMC_TimeInterval *ringDuration,      // in
      int               *nRingDurationTimeSteps, // in
      int               *nTimeStepsRinging, // in
      ESMC_Time         *ringBegin,         // in
      ESMC_Time         *refTime,           // in
      int               *id,                // in
      bool              *ringing,           // in
      bool              *enabled,           // in
      bool              *sticky) {          // in
//
// !DESCRIPTION:
//      Gets {\tt ESMC\_Alarm} property values;
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

    if (ringTime != ESMC_NULL_POINTER) {
      *ringTime = this->ringTime;
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
    if (nRingDurationTimeSteps != ESMC_NULL_POINTER) {
      *nRingDurationTimeSteps = this->nRingDurationTimeSteps;
    }
    if (refTime != ESMC_NULL_POINTER) {
      *refTime = this->refTime;
    }
    if (id != ESMC_NULL_POINTER) {
      *id = this->id;
    }
    if (ringing != ESMC_NULL_POINTER) {
      *ringing = this->ringing;
    }
    if (enabled != ESMC_NULL_POINTER) {
      *enabled = this->enabled;
    }
    if (sticky != ESMC_NULL_POINTER) {
      *sticky = this->sticky;
    }
     
    return(ESMC_AlarmValidate());

 } // end ESMC_AlarmSet

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

    *rc = ESMF_SUCCESS;

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

    if(!enabled) return(ESMF_FAILURE);

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
//    checks if {\tt ESMC\_Alarm}'s ringing state is set.
//
//EOP
// !REQUIREMENTS:

    *rc = ESMF_SUCCESS;

    return(enabled && ringing);

 } // end ESMC_AlarmIsRinging

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
                                          int *nRingDurationTimeSteps) {
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

    sticky = false;

    // mutually exclusive: can only specify one ring duration type
    if (ringDuration == ESMC_NULL_POINTER &&
        nRingDurationTimeSteps == ESMC_NULL_POINTER) {
      cout << "ESMC_Alarm::ESMC_AlarmNotSticky(): can only specify " <<
              "one type of ring duration, not both." << endl;
      return(ESMF_FAILURE);
    }

    if (ringDuration != ESMC_NULL_POINTER) {
      this->ringDuration = *ringDuration;
    }
    if (nRingDurationTimeSteps != ESMC_NULL_POINTER) {
      this->nRingDurationTimeSteps = *nRingDurationTimeSteps;
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

    *rc = ESMF_SUCCESS;

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
      ESMC_Time *ClockCurrTime,  // in - current time to check
      bool positive,      // in - postive or negative ring time crossing trigger
      int  *rc) {         // out - error return code

// !DESCRIPTION:
//    Checks if its time to ring based on current time crossing the ring
//    time in either the positive or negative direction. If already ringing,
//    checks if its time to turn off.
//
//EOP
// !REQUIREMENTS:  TMG4.4, 4.6

    *rc = ESMF_SUCCESS;

    // check if time to turn on alarm
    if (!ringing && enabled) {
      if (positive) {
        ringing = *ClockCurrTime >= ringTime;
      } else {
        ringing = *ClockCurrTime <= ringTime;
      }
    // else check if time to turn off alarm
    } else if (!sticky && enabled) {
      ESMC_TimeInterval cumulativeRinging;
      cumulativeRinging = *ClockCurrTime - ringBegin;
      if (cumulativeRinging.ESMC_TimeIntervalAbsValue() >=
            ringDuration)
      {
        ringing = false;
      }
    }

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
//      given {\tt ESMC\_Alarm}, return result
//
//EOP
// !REQUIREMENTS:

    return(id == alarm.id);

}  // end ESMC_Alarm::operator==

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AlarmReadRestart - restore contents of an Alarm
//
// !INTERFACE:
      int ESMC_Alarm::ESMC_AlarmReadRestart(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_TimeInterval *ringInterval,
      ESMC_TimeInterval *ringDuration,
      ESMC_Time         *ringTime,
      ESMC_Time         *prevRingTime,
      ESMC_Time         *stopTime,
      ESMC_Time         *ringBegin,
      ESMC_Time         *refTime,
      int               nRingDurationTimeSteps,
      int               nTimeStepsRinging,
      int               id,
      bool              ringing,
      bool              enabled,
      bool              sticky) {

//
// !DESCRIPTION:
//      Restore information about an {\tt ESMC\_Alarm}.
//      For persistence/checkpointing
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

    if (ringInterval == ESMC_NULL_POINTER || ringDuration == ESMC_NULL_POINTER
        || ringTime == ESMC_NULL_POINTER  || prevRingTime == ESMC_NULL_POINTER
        || stopTime == ESMC_NULL_POINTER  || ringBegin == ESMC_NULL_POINTER
        || refTime  == ESMC_NULL_POINTER) {
      // TODO: log error
      cout << "ESMC_Alarm::ESMC_AlarmReadRestart(): null pointer(s) passed in"
           << endl;
      return(ESMF_FAILURE);
    }

    this->ringInterval = *ringInterval;
    this->ringDuration = *ringDuration;
    this->ringTime     = *ringTime;
    this->prevRingTime = *prevRingTime;
    this->stopTime     = *stopTime;
    this->ringBegin    = *ringBegin;
    this->refTime      = *refTime;
    this->nRingDurationTimeSteps = nRingDurationTimeSteps;
    this->nTimeStepsRinging      = nTimeStepsRinging;
    this->id           = id;
    this->ringing      = ringing;
    this->enabled      = enabled;
    this->sticky       = sticky;

    return(ESMF_SUCCESS);

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
      ESMC_TimeInterval *ringInterval,
      ESMC_TimeInterval *ringDuration,
      ESMC_Time         *ringTime,
      ESMC_Time         *prevRingTime,
      ESMC_Time         *stopTime,
      ESMC_Time         *ringBegin,
      ESMC_Time         *refTime,
      int               *nRingDurationTimeSteps,
      int               *nTimeStepsRinging,
      int               *id,
      bool              *ringing,
      bool              *enabled,
      bool              *sticky) const {

//
// !DESCRIPTION:
//      Save information about an {\tt ESMC\_Alarm}.
//      For persistence/checkpointing
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

    if (ringInterval == ESMC_NULL_POINTER || ringDuration == ESMC_NULL_POINTER
        || ringTime == ESMC_NULL_POINTER  || prevRingTime == ESMC_NULL_POINTER
        || stopTime == ESMC_NULL_POINTER  || ringBegin == ESMC_NULL_POINTER
        || refTime  == ESMC_NULL_POINTER
        || nRingDurationTimeSteps == ESMC_NULL_POINTER
        || nTimeStepsRinging == ESMC_NULL_POINTER
        || id == ESMC_NULL_POINTER || ringing == ESMC_NULL_POINTER
        || enabled == ESMC_NULL_POINTER || sticky == ESMC_NULL_POINTER) {
      // TODO: log error
      cout << "ESMC_Alarm::ESMC_AlarmWriteRestart(): null pointer(s) passed in"
           << endl;
      return(ESMF_FAILURE);
    }

    *ringInterval = this->ringInterval;
    *ringDuration = this->ringDuration;
    *ringTime     = this->ringTime;
    *prevRingTime = this->prevRingTime;
    *stopTime     = this->stopTime;
    *ringBegin    = this->ringBegin;
    *refTime      = this->refTime;
    *nRingDurationTimeSteps = this->nRingDurationTimeSteps;
    *nTimeStepsRinging      = this->nTimeStepsRinging;
    *id           = this->id;
    *ringing      = this->ringing;
    *enabled      = this->enabled;
    *sticky       = this->sticky;

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

    // must have a ring time; ringDuration, stopTime, prevRingTime optional
    if (ringTime.ESMC_TimeValidate() != ESMF_SUCCESS) return(ESMF_FAILURE);

    // invalid state
    if (!enabled && ringing) return(ESMF_FAILURE);

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
    cout << "ringInterval = " << endl;
                                 ringInterval.ESMC_TimeIntervalPrint(options);
    cout << "ringDuration = " << endl;
                                 ringDuration.ESMC_TimeIntervalPrint(options);
    cout << "ringTime = "     << endl; ringTime.ESMC_TimePrint(options);
    cout << "prevRingTime = " << endl; prevRingTime.ESMC_TimePrint(options);
    cout << "stopTime = "     << endl; stopTime.ESMC_TimePrint(options);
    cout << "ringBegin = "    << endl; ringBegin.ESMC_TimePrint(options);
    cout << "refTime = "      << endl; refTime.ESMC_TimePrint(options);
    cout << "nRingDurationTimeSteps = " << nRingDurationTimeSteps << endl;
    cout << "nTimeStepsRinging = " << nTimeStepsRinging << endl;
    cout << "id = "           << id << endl;
    cout << "ringing = "      << ringing << endl;
    cout << "enabled = "      << enabled << endl;
    cout << "sticky = "       << sticky << endl;
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
//      Calls standard ESMF deep or shallow methods for initialization
//      with default or passed-in values.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

   ringing = false;
   enabled = true;
   sticky  = true;

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

//
//  code goes here TODO
//

 } // end ~ESMC_Alarm
