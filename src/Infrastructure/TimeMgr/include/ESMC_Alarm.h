// $Id: ESMC_Alarm.h,v 1.35.2.5 2009/01/21 21:25:23 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
// ESMF Alarm C++ definition include file
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
#include "ESMC_Start.h"
#include "ESMF_TimeMgr.inc"

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
#include "ESMC_Base.h"
#include "ESMC_IOSpec.h"    // IOSpec class for ReadRestart()/WriteRestart()
#include "ESMC_TimeInterval.h"
#include "ESMC_Time.h"
 class ESMC_Clock;

 // alarm list types to query from clock
 enum ESMC_AlarmListType {ESMF_ALARMLIST_ALL = 1,
                          ESMF_ALARMLIST_RINGING,   
                          ESMF_ALARMLIST_NEXTRINGING,
                          ESMF_ALARMLIST_PREVRINGING};

// !PUBLIC TYPES:
 class ESMC_Alarm;
 typedef ESMC_Alarm* ESMC_AlarmPtr;

// !PRIVATE TYPES:
 // class configuration type:  not needed for Alarm

 // class definition type
class ESMC_Alarm {
//class ESMC_Alarm : public ESMC_Base { // TODO: inherit from ESMC_Base class
                                        // when fully aligned with F90 equiv

  private:   // corresponds to F90 module 'type ESMF_Alarm' members
    char              name[ESMF_MAXSTR];  // name of alarm
                                          // TODO: inherit from ESMC_Base class
    ESMC_Clock       *clock;        // associated clock
    ESMC_TimeInterval ringInterval; // (TMG 4.5.2) for periodic alarming
    ESMC_TimeInterval ringDuration; // how long alarm stays on
    ESMC_Time         ringTime;     // (TMG 4.5.1) next time to ring
    ESMC_Time         firstRingTime;    // the first ring time
                                        //   (save for reverse mode)
    ESMC_Time         prevRingTime; // previous alarm time 
    ESMC_Time         stopTime;     // when alarm intervals end.
    ESMC_Time         ringBegin;    // note time when alarm turns on.
    ESMC_Time         ringEnd;      // save time when alarm is turned off via
                                    //   ESMC_RingerOff().  For reverse mode.
                                    //   TODO: make array for variable
                                    //   turn off durations.
    ESMC_Time         refTime;      // reference time.
    int               ringTimeStepCount;      // how long alarm rings;
                                              //  mutually exclusive with
                                              //  ringDuration
    int               timeStepRingingCount;   // how long alarm has been
                                              //   ringing in terms of a 
                                              //   number of time steps.

    bool              ringing;    // (TMG 4.4) currently ringing
    bool              ringingOnCurrTimeStep; // was ringing immedidately after
                                             // current clock timestep.
                                             // (could have been turned off
                                             //  later due to RingerOff or
                                             //  Disable commands or
                                             //  non-sticky alarm expiration).
    bool              ringingOnPrevTimeStep; // was ringing immediately after
                                             // previous clock timestep.
    bool              userChangedRingTime;       // true if changed via Set(),
    bool              userChangedRingInterval;   // used to determine whether
                                                 // to adjust alarm on timeStep
                                                 // direction (sign) change

    bool              enabled;    // able to ring (TMG 4.5.3)
    bool              sticky;     // must be turned off via
                                  //   ESMC_AlarmRingerOff(),
                                  //  otherwise will turn self off after
                                  //  ringDuration or ringTimeStepCount.
    int               id;         // unique identifier. used for equality
                                  //    checks and to generate unique default
                                  //    names.
                                  //    TODO: inherit from ESMC_Base class
    static int        count;      // number of alarms created. Thread-safe
                                  //   because int is atomic.
                                  //    TODO: inherit from ESMC_Base class

//    bool              pad1;       //  TODO:  align on byte boundary

//    pthread_mutex_t   alarmMutex; // TODO: (TMG 7.5)

// !PUBLIC MEMBER FUNCTIONS:

  public:

    // Alarm doesn't need configuration, hence GetConfig/SetConfig
    // methods are not required

    // accessor methods

    int ESMC_AlarmSet(int                nameLen,
                      const char        *name=0,
                      ESMC_Clock       **clock=0,
                      ESMC_Time         *ringTime=0,
                      ESMC_TimeInterval *ringInterval=0,
                      ESMC_Time         *stopTime=0,
                      ESMC_TimeInterval *ringDuration=0,
                      int               *ringTimeStepCount=0,
                      ESMC_Time         *refTime=0,
                      bool              *ringing=0,
                      bool              *enabled=0,  // (TMG 4.1, 4.7)
                      bool              *sticky=0);

    int ESMC_AlarmGet(int                nameLen,
                      int               *tempNameLen,
                      char              *tempName=0,
                      ESMC_Clock       **clock=0,
                      ESMC_Time         *ringTime=0,
                      ESMC_Time         *prevRingTime=0,
                      ESMC_TimeInterval *ringInterval=0,
                      ESMC_Time         *stopTime=0,
                      ESMC_TimeInterval *ringDuration=0,
                      int               *ringTimeStepCount=0,
                      int               *timeStepRingingCount=0,
                      ESMC_Time         *ringBegin=0,
                      ESMC_Time         *ringEnd=0,
                      ESMC_Time         *refTime=0,
                      bool              *ringing=0,
                      bool              *ringingOnPrevTimeStep=0,
                      bool              *enabled=0,  // (TMG 4.1, 4.7)
                      bool              *sticky=0);

    int  ESMC_AlarmEnable(void);    // TMG4.5.3
    int  ESMC_AlarmDisable(void);
    bool ESMC_AlarmIsEnabled(int *rc=0) const;

    int  ESMC_AlarmRingerOn(void);    // TMG4.6: manually turn on/off
    int  ESMC_AlarmRingerOff(void);
    bool ESMC_AlarmIsRinging(int *rc=0) const;
                                         // TMG 4.4: synchronous query for apps
    bool ESMC_AlarmWillRingNext(ESMC_TimeInterval *timeStep, int *rc=0) const;
    bool ESMC_AlarmWasPrevRinging(int *rc=0) const;

    int  ESMC_AlarmSticky(void);
    int  ESMC_AlarmNotSticky(ESMC_TimeInterval *ringDuration=0,
                             int *ringTimeStepCount=0);
    bool ESMC_AlarmIsSticky(int *rc=0) const;

    bool ESMC_AlarmCheckRingTime(int *rc=0);
                         // associated clock should invoke after advance:
                         // TMG4.4, 4.6
                         // Check for crossing ringTime in either positive or
                         //   negative direction
                         // Can be basis for asynchronous alarm reporting

    bool operator==(const ESMC_Alarm &) const; 
    bool operator!=(const ESMC_Alarm &) const; 

    // required methods inherited and overridden from the ESMC_Base class

    // for persistence/checkpointing

    // friend to restore state
    friend ESMC_Alarm *ESMC_AlarmReadRestart(int, const char*,
                                             ESMC_IOSpec*, int*);
    // save state
    int ESMC_AlarmWriteRestart(ESMC_IOSpec *iospec=0) const;

    // internal validation
    int ESMC_AlarmValidate(const char *options=0) const;

    // for testing/debugging
    int ESMC_AlarmPrint(const char *options=0) const;

    // native C++ constructors/destructors
    ESMC_Alarm(void);
    ESMC_Alarm(const ESMC_Alarm &alarm);
    ~ESMC_Alarm(void);

 // < declare the rest of the public interface methods here >

    // friend to allocate and initialize alarm from heap
    friend ESMC_Alarm *ESMC_AlarmCreate(int, const char*, ESMC_Clock*, 
                                 ESMC_Time*, ESMC_TimeInterval*, ESMC_Time*, 
                                 ESMC_TimeInterval*, int*, ESMC_Time*, bool*,
                                 bool*, int*);

    // friend function to copy an alarm
    friend ESMC_Alarm *ESMC_AlarmCreate(ESMC_Alarm*, int*);

    // friend to de-allocate alarm
    friend int ESMC_AlarmDestroy(ESMC_Alarm **);


// !PRIVATE MEMBER FUNCTIONS:
//
  private:
//
 // < declare private interface methods here >

    // check if time to turn on alarm
    bool ESMC_AlarmCheckTurnOn(bool timeStepPositive);

    // reconstruct ringBegin during ESMF_MODE_REVERSE
    int ESMC_AlarmResetRingBegin(bool timeStepPositive);

    // friend class alarm
    friend class ESMC_Clock;

//
//EOP
//-------------------------------------------------------------------------

};  // end class ESMC_Alarm

    // Note: though seemingly redundant with the friend declarations within
    // the class definition above, the following declarations are necessary
    // to appease some compilers (most notably IBM), as well as ANSI C++. 
    // These also establish defaults to match F90 optional args.

    ESMC_Alarm *ESMC_AlarmCreate(int nameLen,
                                 const char*        name=0,
                                 ESMC_Clock*        clock=0, 
                                 ESMC_Time*         ringTime=0,
                                 ESMC_TimeInterval* ringInterval=0,
                                 ESMC_Time*         stopTime=0, 
                                 ESMC_TimeInterval* ringDuration=0,
                                 int*               ringTimeStepCount=0,
                                 ESMC_Time*         refTime=0,
                                 bool*              enabled=0,
                                 bool*              sticky=0,
                                 int*               rc=0);

    // friend function to copy a alarm
    ESMC_Alarm *ESMC_AlarmCreate(ESMC_Alarm *alarm, int *rc=0);

    // friend to de-allocate alarm
    int ESMC_AlarmDestroy(ESMC_Alarm **alarm);

    // friend to restore state
    ESMC_Alarm *ESMC_AlarmReadRestart(int nameLen,
                                      const char*  name=0,
                                      ESMC_IOSpec* iospec=0,
                                      int*         rc=0);

#endif // ESMC_ALARM_H
