// $Id: ESMC_Alarm.C,v 1.16 2003/08/18 16:08:02 eschwab Exp $
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
 static const char *const version = "$Id: ESMC_Alarm.C,v 1.16 2003/08/18 16:08:02 eschwab Exp $";
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
// !IROUTINE:  ESMC_AlarmSet - initializes a Alarm object
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
      bool               enabled) {     // in
//
// !DESCRIPTION:
//      ESMF routine which only initializes {\tt ESMC\_Alarm} values; it does not
//      allocate any resources.  
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

    // TODO: ensure initialization if called via F90 interface;
    //       cannot call constructor, because destructor is subsequently
    //       called automatically, returning initialized values to garbage.
    Ringing = false;
    Enabled = true;
    
    if (ringTime     != ESMC_NULL_POINTER) RingTime     = *ringTime;
    if (ringInterval != ESMC_NULL_POINTER) RingInterval = *ringInterval;
    if (stopTime     != ESMC_NULL_POINTER) StopTime     = *stopTime;
    Enabled = enabled;

    return(ESMC_AlarmValidate());

 } // end ESMC_AlarmSet

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AlarmGetRingInterval - gets an Alarm's ring interval
//
// !INTERFACE:
      int ESMC_Alarm::ESMC_AlarmGetRingInterval(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_TimeInterval *ringInterval) const {    // out - ring interval
//
// !DESCRIPTION:
//      Gets an {\tt ESMC\_Alarm}'s ring interval
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

    if (ringInterval == ESMC_NULL_POINTER) return(ESMF_FAILURE);

    *ringInterval = RingInterval;

    return(ESMF_SUCCESS);

 } // end ESMC_AlarmGetRingInterval

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AlarmSetRingInterval - sets an Alarm's ring interval
//
// !INTERFACE:
      int ESMC_Alarm::ESMC_AlarmSetRingInterval(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_TimeInterval *ringInterval) {    // in - ring interval
//
// !DESCRIPTION:
//      Sets an {\tt ESMC\_Alarm}'s ring interval
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

    if (ringInterval == ESMC_NULL_POINTER) return(ESMF_FAILURE);

    RingInterval = *ringInterval;

    return(ESMF_SUCCESS);

 } // end ESMC_AlarmSetRingInterval

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AlarmGetRingDuration - gets an Alarm's ring duration
//
// !INTERFACE:
      int ESMC_Alarm::ESMC_AlarmGetRingDuration(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_TimeInterval *ringduration) const {    // out - ring duration
//
// !DESCRIPTION:
//      Gets an {\tt ESMC\_Alarm}'s ring duration
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

    if (ringduration == ESMC_NULL_POINTER) return(ESMF_FAILURE);

    *ringduration = ringDuration;

    return(ESMF_SUCCESS);

 } // end ESMC_AlarmGetRingDuration

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AlarmSetRingDuration - sets an Alarm's ring duration
//
// !INTERFACE:
      int ESMC_Alarm::ESMC_AlarmSetRingDuration(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_TimeInterval *ringduration,  // in - ring duration
      int numTimeSteps,                 // in - number of timesteps
      ESMC_TimeInterval *timestep) {    // in - timestep
//
// !DESCRIPTION:
//      Sets an {\tt ESMC\_Alarm}'s ring duration
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

    // must have either a ringDuration or a timeStep
    if (ringduration == ESMC_NULL_POINTER &&
        timestep == ESMC_NULL_POINTER) return(ESMF_FAILURE);

    // if ringDuration is present, use it; otherwise use numTimeSteps * timeStep
    ringDuration = (ringduration != ESMC_NULL_POINTER) ?
                    *ringduration : ((*timestep) * numTimeSteps);

    return(ESMF_SUCCESS);

 } // end ESMC_AlarmSetRingDuration

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AlarmGetRingTime - gets an Alarm's ring time
//
// !INTERFACE:
      int ESMC_Alarm::ESMC_AlarmGetRingTime(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_Time *ringTime) const {    // out - ring time
//
// !DESCRIPTION:
//      Gets an {\tt ESMC\_Alarm}'s ring time
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

    if (ringTime == ESMC_NULL_POINTER) return(ESMF_FAILURE);

    *ringTime = RingTime;

    return(ESMF_SUCCESS);

 } // end ESMC_AlarmGetRingTime

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AlarmSetRingTime - sets an Alarm's ring time
//
// !INTERFACE:
      int ESMC_Alarm::ESMC_AlarmSetRingTime(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_Time *ringTime) {    // in - ring time
//
// !DESCRIPTION:
//      Sets an {\tt ESMC\_Alarm}'s ring time
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

    if (ringTime == ESMC_NULL_POINTER) return(ESMF_FAILURE);

    RingTime = *ringTime;

    return(ESMF_SUCCESS);

 } // end ESMC_AlarmSetRingTime

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AlarmGetPrevRingTime - gets an Alarm's previous ring time
//
// !INTERFACE:
      int ESMC_Alarm::ESMC_AlarmGetPrevRingTime(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_Time *prevRingTime) const {    // out - previous ring time
//
// !DESCRIPTION:
//      Gets an {\tt ESMC\_Alarm}'s previous ring time
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

    if (prevRingTime == ESMC_NULL_POINTER) return(ESMF_FAILURE);

    *prevRingTime = PrevRingTime;

    return(ESMF_SUCCESS);

 } // end ESMC_AlarmGetPrevRingTime

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AlarmSetPrevRingTime - sets an Alarm's previous ring time
//
// !INTERFACE:
      int ESMC_Alarm::ESMC_AlarmSetPrevRingTime(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_Time *prevRingTime) {    // in - previous ring time
//
// !DESCRIPTION:
//      Sets an {\tt ESMC\_Alarm}'s previous ring time
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

    if (prevRingTime == ESMC_NULL_POINTER) return(ESMF_FAILURE);

    PrevRingTime = *prevRingTime;

    return(ESMF_SUCCESS);

 } // end ESMC_AlarmSetPrevRingTime

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AlarmGetStopTime - gets an Alarm's stop time
//
// !INTERFACE:
      int ESMC_Alarm::ESMC_AlarmGetStopTime(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_Time *stopTime) const {    // out - stop time
//
// !DESCRIPTION:
//      Gets an {\tt ESMC\_Alarm}'s stop time
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

    if (stopTime == ESMC_NULL_POINTER) return(ESMF_FAILURE);

    *stopTime = StopTime;

    return(ESMF_SUCCESS);

 } // end ESMC_AlarmGetStopTime

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AlarmSetStopTime - sets an Alarm's stop time
//
// !INTERFACE:
      int ESMC_Alarm::ESMC_AlarmSetStopTime(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_Time *stopTime) {    // in - stop time
//
// !DESCRIPTION:
//      Sets an {\tt ESMC\_Alarm}'s stop time
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

    if (stopTime == ESMC_NULL_POINTER) return(ESMF_FAILURE);

    StopTime = *stopTime; 

    return(ESMF_SUCCESS);

 } // end ESMC_AlarmSetStopTime

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AlarmGetRefTime - gets an Alarm's reference time
//
// !INTERFACE:
      int ESMC_Alarm::ESMC_AlarmGetRefTime(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_Time *reftime) const {    // out - reference time
//
// !DESCRIPTION:
//      Gets an {\tt ESMC\_Alarm}'s reference time
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

    if (reftime == ESMC_NULL_POINTER) return(ESMF_FAILURE);

    *reftime = refTime;

    return(ESMF_SUCCESS);

 } // end ESMC_AlarmGetRefTime

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AlarmSetRefTime - sets an Alarm's reference time
//
// !INTERFACE:
      int ESMC_Alarm::ESMC_AlarmSetRefTime(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_Time *reftime) {    // in - reference time
//
// !DESCRIPTION:
//      Sets an {\tt ESMC\_Alarm}'s reference time
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

    if (reftime == ESMC_NULL_POINTER) return(ESMF_FAILURE);

    refTime = *reftime;

    return(ESMF_SUCCESS);

 } // end ESMC_AlarmSetRefTime

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

    Enabled = true;

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

    Ringing = false;
    Enabled = false;

    return(ESMF_SUCCESS);

 } // end ESMC_AlarmDisable

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AlarmTurnOn - sets an Alarm to the ringing state
//
// !INTERFACE:
      int ESMC_Alarm::ESMC_AlarmTurnOn(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      ESMF routine which sets an {\tt ESMC\_Alarm} object to the ringing state
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

    if(!Enabled) return(ESMF_FAILURE);

    Ringing = true;

    return(ESMF_SUCCESS);

 } // end ESMC_AlarmTurnOn

//-------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_AlarmTurnOff - turns off an Alarm's ringing state
//
// !INTERFACE:
      int ESMC_Alarm::ESMC_AlarmTurnOff(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      ESMF routine which turns off an {\tt ESMC\_Alarm}'s ringing state
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

    Ringing = false;

    return(ESMF_SUCCESS);

 } // end ESMC_AlarmTurnOn

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
      int ESMC_Alarm::ESMC_AlarmNotSticky(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      ESMF routine which unsets an {\tt ESMC\_Alarm}'s sticky flag.
//
//EOP
// !REQUIREMENTS:

    sticky = false;

    return(ESMF_SUCCESS);

 } // end ESMC_AlarmNotSticky

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

    return(Enabled && Ringing);

 } // end ESMC_AlarmIsRinging

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
    if (!Ringing && Enabled) {
      if (positive) {
        Ringing = *ClockCurrTime >= RingTime;
      } else {
        Ringing = *ClockCurrTime <= RingTime;
      }
    // else check if time to turn off alarm
    } else if (!sticky && Enabled) {
      ESMC_TimeInterval cumulativeRinging;
      cumulativeRinging = *ClockCurrTime - ringBegin;
      if (cumulativeRinging.ESMC_TimeIntervalAbsValue() >=
            ringDuration)
      {
        Ringing = false;
      }
    }

    return(Ringing && Enabled);

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

    return(ID == alarm.ID);

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
      ESMC_TimeInterval *ringduration,
      ESMC_Time         *ringTime,
      ESMC_Time         *prevRingTime,
      ESMC_Time         *stopTime,
      ESMC_Time         *ringbegin,
      ESMC_Time         *reftime,
      bool              ringing,
      bool              enabled,
      bool              Sticky,
      int               id) {

//
// !DESCRIPTION:
//      Restore information about an {\tt ESMC\_Alarm}. For persistence/checkpointing
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

    if (ringInterval == ESMC_NULL_POINTER || ringduration == ESMC_NULL_POINTER
        || ringTime == ESMC_NULL_POINTER  || prevRingTime == ESMC_NULL_POINTER
        || stopTime == ESMC_NULL_POINTER  || ringbegin == ESMC_NULL_POINTER
        || reftime  == ESMC_NULL_POINTER) {
      // TODO: log error
      cout << "ESMC_Alarm::ESMC_AlarmReadRestart(): null pointer(s) passed in"
           << endl;
      return(ESMF_FAILURE);
    }

    RingInterval = *ringInterval;
    ringDuration = *ringduration;
    RingTime     = *ringTime;
    PrevRingTime = *prevRingTime;
    StopTime     = *stopTime;
    ringBegin    = *ringbegin;
    refTime      = *reftime;
    Ringing      = ringing;
    Enabled      = enabled;
    sticky       = Sticky;
    ID           = id;

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
      ESMC_TimeInterval *ringduration,
      ESMC_Time         *ringTime,
      ESMC_Time         *prevRingTime,
      ESMC_Time         *stopTime,
      ESMC_Time         *ringbegin,
      ESMC_Time         *reftime,
      bool              *ringing,
      bool              *enabled,
      bool              *Sticky,
      int               *id) const {

//
// !DESCRIPTION:
//      Save information about an {\tt ESMC\_Alarm}. For persistence/checkpointing
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

    if (ringInterval == ESMC_NULL_POINTER || ringduration == ESMC_NULL_POINTER
        || ringTime == ESMC_NULL_POINTER  || prevRingTime == ESMC_NULL_POINTER
        || stopTime == ESMC_NULL_POINTER  || ringbegin == ESMC_NULL_POINTER
        || reftime  == ESMC_NULL_POINTER  || ringing == ESMC_NULL_POINTER
        || enabled == ESMC_NULL_POINTER || Sticky == ESMC_NULL_POINTER
        || id == ESMC_NULL_POINTER) {
      // TODO: log error
      cout << "ESMC_Alarm::ESMC_AlarmWriteRestart(): null pointer(s) passed in"
           << endl;
      return(ESMF_FAILURE);
    }

    *ringInterval = RingInterval;
    *ringduration = ringDuration;
    *ringTime     = RingTime;
    *prevRingTime = PrevRingTime;
    *stopTime     = StopTime;
    *ringbegin    = ringBegin;
    *reftime      = refTime;
    *ringing      = Ringing;
    *enabled      = Enabled;
    *Sticky       = sticky;
    *id           = ID;

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

    // must have a ring time; RingDuration, StopTime, PrevRingTime optional
    if (RingTime.ESMC_TimeValidate() != ESMF_SUCCESS) return(ESMF_FAILURE);

    // invalid state
    if (!Enabled && Ringing) return(ESMF_FAILURE);

    // TODO: validate ID ?

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
//      Print information about an {\tt ESMC\_Alarm}.  For testing/debugging
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

    cout << "Alarm ----------------------------------" << endl;
    cout << "RingDuration = " << endl;
                                 RingInterval.ESMC_TimeIntervalPrint(options);
    cout << "RingTime = "     << endl; RingTime.ESMC_TimePrint(options);
    cout << "PrevRingTime = " << endl; PrevRingTime.ESMC_TimePrint(options);
    cout << "StopTime = "     << endl; StopTime.ESMC_TimePrint(options);
    cout << "Ringing = "      << Ringing << endl;
    cout << "Enabled = "      << Enabled << endl;
    cout << "ID = "           << ID << endl;
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
//      with default or passed-in values
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

   Ringing = false;
   Enabled = true;
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
//      Calls standard ESMF deep or shallow methods for destruction
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//
//  code goes here TODO
//

 } // end ~ESMC_Alarm
