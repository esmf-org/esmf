// $Id: ESMC_Alarm.h,v 1.6 2003/03/28 01:29:47 eschwab Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the GPL.
//
// ESMF TimeInstant C++ definition include file
//
// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
//
 // these lines prevent this file from being read more than once if it
 // ends up being included multiple times

#ifndef ESMC_ALARM_H
#define ESMC_ALARM_H

//-------------------------------------------------------------------------

 // put any constants or macros which apply to the whole component in this file.
 // anything public or esmf-wide should be up higher at the top level
 // include files.
 #include <ESMF_TimeMgr.inc>
 #include <pthread.h>

//-------------------------------------------------------------------------
//BOP
// 
// !CLASS: ESMC_Alarm - maintains ringing times and ringing state
// 
// !DESCRIPTION:
//
// The code in this file defines the C++ {\tt Alarm} members and method
// signatures (prototypes).  The companion file {\tt ESMC\_Alarm.C} contains
// the full code (bodies) for the {\tt Alarm} methods.
//
// The {\tt Alarm} class encapsulates the required alarm behavior, triggering
// its ringing state on either a one-shot or repeating interval basis.
//
// The {\tt Alarm} class contains {\tt Time} instants and a {\tt TimeInterval}
// to perform one-shot and interval alarming.  A single {\tt TimeInterval}
// holds the alarm interval if used.  A {\tt Time} instant is defined for the
// ring time, used for either the one-shot alarm time or for the next interval
// alarm time.  A {\tt Time} instant is also defined for the previous ring
// time to keep track of alarm intervals.  A {\tt Time} instant for stop time
// defines when alarm intervals end.  If a one-shot alarm is defined, only
// the ring time attribute is used, the others are not.  To keep track of
// alarm state, two logical attributes are defined, one for ringing on/off,
// and the other for alarm enabled/disabled.  An alarm is enabled by default;
// if disabled by the user, it does not function at all.
//
// The primary method is to check whether it is time to set the ringer, which
// is called by the associated clock after performing a time step.  The clock
// will pass a parameter telling the alarm check method whether the ringer is
// to be set upon crossing the ring time in the positive or negative direction.
// This is to handle both positive and negative clock timesteps.  After the
// ringer is set for interval alarms, the check method will recalculate the
// next ring time.  This can be in the positve or negative direction, again
// depending on the parameter passed in by the clock.
//
// Other methods are defined for getting the ringing state, turning the
// ringer on/off, enabling/disabling the alarm, and getting/setting the
// time attributes defined above.
//
// Notes:
//    TMG 4.1, 4.2:  Multiple alarms may be instantiated and associated
//                   with a clock via clock methods
//
//-------------------------------------------------------------------------
//
// !USES:
 #include <ESMC_Base.h>
 #include <ESMC_TimeInterval.h>
 #include <ESMC_Time.h>

// !PUBLIC TYPES:
 class ESMC_Alarm;

// !PRIVATE TYPES:
 // class configuration type:  not needed for Alarm

 // class definition type
class ESMC_Alarm {

  private:   // corresponds to F90 module 'type ESMF_Alarm' members
    ESMC_TimeInterval RingInterval; // (TMG 4.5.2)
    ESMC_Time         RingTime;     // (TMG 4.5.1)
    ESMC_Time         PrevRingTime;
    ESMC_Time         StopTime;

    bool              Ringing;    // (TMG 4.4)
    bool              Enabled;    // able to ring (TMG 4.5.3)

    int               ID;         // used to distinguish among
                                  //   multiple clock alarms

//    pthread_mutex_t   AlarmMutex; // (TMG 7.5)

// !PUBLIC MEMBER FUNCTIONS:

  public:

    // Alarm is a shallow class, so only Init methods are needed
    // (TMG 4.1, 4.7)
    int ESMC_AlarmInit(ESMC_TimeInterval *ringInterval,
                       ESMC_Time         *ringTime,
                       ESMC_Time         *stopTime,
                       bool enabled);

    // Alarm doesn't need configuration, hence GetConfig/SetConfig
    // methods are not required

    int ESMC_AlarmEnable(void);    // TMG4.5.3
    int ESMC_AlarmDisable(void);

    int ESMC_AlarmTurnOn(void);    // TMG4.6: manually turn on/off
    int ESMC_AlarmTurnOff(void);

    bool ESMC_AlarmIsRinging(int *rc) const;
                                         // TMG 4.4: synchronous query for apps
    bool ESMC_AlarmCheckRingTime(ESMC_Time *clockCurrTime, bool positive,
                                 int *rc) const;
                         // associated clock should invoke after advance:
                         // TMG4.4, 4.6
                         // Check for crossing RingTime in either positive or
                         //   negative direction
                         // Can be basis for asynchronous alarm reporting

    bool operator==(const ESMC_Alarm &) const; 

    // accessor methods

    int ESMC_AlarmGetRingInterval(ESMC_TimeInterval *ringInterval) const;
                                                           // TMG4.7
    int ESMC_AlarmSetRingInterval(ESMC_TimeInterval *ringInterval);
                                                           // TMG4.5.2, 4.7

    int ESMC_AlarmGetRingTime(ESMC_Time *ringTime) const;
                                                           // TMG4.7, 4.8
    int ESMC_AlarmSetRingTime(ESMC_Time *ringTime); 
                                                           // TMG4.5.1, 4.7, 4.8

    int ESMC_AlarmGetPrevRingTime(ESMC_Time *prevRingTime) const;
                                                           // TMG 4.7, 4.8
    int ESMC_AlarmSetPrevRingTime(ESMC_Time *prevRingTime);
                                                           // TMG 4.7, 4.8

    int ESMC_AlarmGetStopTime(ESMC_Time *stopTime) const ; // TMG 4.5.2, 4.7
    int ESMC_AlarmSetStopTime(ESMC_Time *stopTime); // TMG 4.5.2, 4.7


    // required methods inherited and overridden from the ESMC_Base class

    // internal validation
    int ESMC_BaseValidate(const char *options) const;

    // for testing/debugging
    int ESMC_BasePrint(const char *options) const;

    // for persistence/checkpointing
    int ESMC_BasePrint(ESMC_TimeInterval *ringInterval,
                       ESMC_Time         *ringTime,
                       ESMC_Time         *prevRingTime,
                       ESMC_Time         *stopTime,
                       bool              *ringing,
                       bool              *enabled,
                       int               *id) const;

    // native C++ constructors/destructors
    ESMC_Alarm(void);
    ~ESMC_Alarm(void);

 // < declare the rest of the public interface methods here >

// !PRIVATE MEMBER FUNCTIONS:
//
  private:
//
 // < declare private interface methods here >

//
//EOP
//-------------------------------------------------------------------------

};  // end class ESMC_Alarm

#endif // ESMC_ALARM_H
