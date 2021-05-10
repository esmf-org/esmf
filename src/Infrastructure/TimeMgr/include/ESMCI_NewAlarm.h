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
// ESMF NewAlarm C++ definition include file
//
// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
//
 // these lines prevent this file from being read more than once if it
 // ends up being included multiple times

#ifndef ESMC_NEWALARM_H
#define ESMC_NEWALARM_H

//-------------------------------------------------------------------------

 // put any constants or macros which apply to the whole component in this file.
 // anything public or esmf-wide should be up higher at the top level
 // include files.
#include "ESMCI_Util.h"
#include "ESMCI_Macros.h"
#include "ESMF_TimeMgr.inc"

//-------------------------------------------------------------------------
//BOP
// 
// !CLASS: ESMCI::NewAlarm - maintains ringing times and ringing state
// 
// !DESCRIPTION:
//
// The code in this file defines the C++ {\tt NewAlarm} members and method
// signatures (prototypes).  The companion file {\tt ESMC\_NewAlarm.C} contains
// the full code (bodies) for the {\tt NewAlarm} methods.
//
// The {\tt NewAlarm} class encapsulates the required newalarm behavior, triggering
// its ringing state on either a one-shot or repeating interval basis.
//
// The {\tt NewAlarm} class contains {\tt Time} instants and a {\tt TimeInterval}
// to perform one-shot and interval newalarming.  A single {\tt TimeInterval}
// holds the newalarm interval if used.  A {\tt Time} instant is defined for the
// ring time, used for either the one-shot newalarm time or for the next interval
// newalarm time.  A {\tt Time} instant is also defined for the previous ring
// time to keep track of newalarm intervals.  A {\tt Time} instant for stop time
// defines when newalarm intervals end.  If a one-shot newalarm is defined, only
// the ring time attribute is used, the others are not.  To keep track of
// newalarm state, two logical attributes are defined, one for ringing on/off,
// and the other for newalarm enabled/disabled.  An newalarm is enabled by default;
// if disabled by the user, it does not function at all.
//
// The primary method is to check whether it is time to set the ringer, which
// is called by the associated clock after performing a time step.  The clock
// will pass a parameter telling the newalarm check method whether the ringer is
// to be set upon crossing the ring time in the positive or negative direction.
// This is to handle both positive and negative clock timesteps.  After the
// ringer is set for interval newalarms, the check method will recalculate the
// next ring time.  This can be in the positive or negative direction, again
// depending on the parameter passed in by the clock.
//
// Other methods are defined for getting the ringing state, turning the
// ringer on/off, enabling/disabling the newalarm, and getting/setting the
// time attributes defined above.
//
// Notes:
//    TMG 4.1, 4.2:  Multiple newalarms may be instantiated and associated
//                   with a clock via clock methods
//
//-------------------------------------------------------------------------
//
// !USES:
#include "ESMCI_TimeInterval.h"
#include "ESMCI_Time.h"

 // newalarm list flags to query from clock
 enum ESMC_NewAlarmList_Flag {ESMF_NEWALARMLIST_ALL = 1,
                           ESMF_NEWALARMLIST_RINGING,   
                           ESMF_NEWALARMLIST_NEXTRINGING,
                           ESMF_NEWALARMLIST_PREVRINGING};
// type of NewAlarm list
namespace ESMCI {

 class Clock;

// !PUBLIC TYPES:
 class NewAlarm;
 typedef NewAlarm* ESMCI_NewAlarmPtr;

// !PRIVATE TYPES:
 // class configuration type:  not needed for NewAlarm

 // class definition type
class NewAlarm {
//class NewAlarm : public ESMC_Base { // TODO: inherit from ESMC_Base class
                                        // when fully aligned with F90 equiv


  private:   // corresponds to F90 module 'type ESMF_NewAlarm' members
    char              name[ESMF_MAXSTR];  // name of newalarm
                                          // TODO: inherit from ESMC_Base class
    Clock       *clock;        // associated clock
    TimeInterval ringInterval; // (TMG 4.5.2) for periodic newalarming
    TimeInterval ringDuration; // how long newalarm stays on
    Time         ringTime;     // (TMG 4.5.1) next time to ring
    Time         firstRingTime;    // the first ring time
                                        //   (save for reverse mode)
    Time         prevRingTime; // previous newalarm time 
    Time         stopTime;     // when newalarm intervals end.
    Time         ringBegin;    // note time when newalarm turns on.
    Time         ringEnd;      // save time when newalarm is turned off via
                                    //   ESMC_RingerOff().  For reverse mode.
                                    //   TODO: make array for variable
                                    //   turn off durations.
    Time         refTime;      // reference time.
    int               ringTimeStepCount;      // how long newalarm rings;
                                              //  mutually exclusive with
                                              //  ringDuration
    int               timeStepRingingCount;   // how long newalarm has been
                                              //   ringing in terms of a 
                                              //   number of time steps.

    bool              ringing;    // (TMG 4.4) currently ringing
    bool              ringingOnCurrTimeStep; // was ringing immediately after
                                             // current clock timestep.
                                             // (could have been turned off
                                             //  later due to RingerOff or
                                             //  Disable commands or
                                             //  non-sticky newalarm expiration).
    bool              ringingOnPrevTimeStep; // was ringing immediately after
                                             // previous clock timestep.
    bool              userChangedRingTime;       // true if changed via Set(),
    bool              userChangedRingInterval;   // used to determine whether
                                                 // to adjust newalarm on timeStep
                                                 // direction (sign) change
    bool              enabled;    // able to ring (TMG 4.5.3)
    bool              sticky;     // must be turned off via
                                  //   NewAlarm::ringerOff(),
                                  //  otherwise will turn self off after
                                  //  ringDuration or ringTimeStepCount.
    int               id;         // unique identifier. used for equality
                                  //    checks and to generate unique default
                                  //    names.
                                  //    TODO: inherit from ESMC_Base class
    static int        count;      // number of newalarms created. Thread-safe
                                  //   because int is atomic.
                                  //    TODO: inherit from ESMC_Base class

//    bool              pad1;       //  TODO:  align on byte boundary

//    pthread_mutex_t   newalarmMutex; // TODO: (TMG 7.5)

// !PUBLIC MEMBER FUNCTIONS:

  public:

    // NewAlarm doesn't need configuration, hence GetConfig/SetConfig
    // methods are not required

    // accessor methods

               int    set(int                nameLen,
                      const char        *name=0,
                      Clock       **clock=0,
                      Time         *ringTime=0,
                      TimeInterval *ringInterval=0,
                      Time         *stopTime=0,
                      TimeInterval *ringDuration=0,
                      int               *ringTimeStepCount=0,
                      Time         *refTime=0,
                      bool              *ringing=0,
                      bool              *enabled=0,  // (TMG 4.1, 4.7)
                      bool              *sticky=0);

              int     get(int                nameLen,
                      int               *tempNameLen,
                      char              *tempName=0,
                      Clock       **clock=0,
                      Time         *ringTime=0,
                      Time         *prevRingTime=0,
                      TimeInterval *ringInterval=0,
                      Time         *stopTime=0,
                      TimeInterval *ringDuration=0,
                      int               *ringTimeStepCount=0,
                      int               *timeStepRingingCount=0,
                      Time         *ringBegin=0,
                      Time         *ringEnd=0,
                      Time         *refTime=0,
                      bool              *ringing=0,
                      bool              *ringingOnPrevTimeStep=0,
                      bool              *enabled=0,  // (TMG 4.1, 4.7)
                      bool              *sticky=0);

              int      enable(void);    // TMG4.5.3
              int      disable(void);
              bool     isEnabled(int *rc=0) const;

              int      ringerOn(void);    // TMG4.6: manually turn on/off
              int      ringerOff(void);
              bool     isRinging(int *rc=0) const;
                                         // TMG 4.4: synchronous query for apps
              bool     willRingNext(TimeInterval *timeStep, int *rc=0) const;
              bool     wasPrevRinging(int *rc=0) const;

              int      setToSticky(void);
              int      notSticky(TimeInterval *ringDuration=0,
                             int *ringTimeStepCount=0);
              bool     isSticky(int *rc=0) const;

              bool     checkRingTime(int *rc=0);
                         // associated clock should invoke after advance:
                         // TMG4.4, 4.6
                         // Check for crossing ringTime in either positive or
                         //   negative direction
                         // Can be basis for asynchronous newalarm reporting

    bool operator==(const NewAlarm &) const; 
    bool operator!=(const NewAlarm &) const; 

    // required methods inherited and overridden from the ESMC_Base class

    // for persistence/checkpointing

    // friend to restore state
    friend NewAlarm *ESMCI_newalarmReadRestart(int, const char*, int*);
    // save state
    int writeRestart(void) const;

    // internal validation
    int validate(const char *options=0) const;

    // for testing/debugging
    int print(const char *options=0) const;

    // native C++ constructors/destructors
    NewAlarm(void);
    NewAlarm(const NewAlarm &newalarm);
    ~NewAlarm(void);

 // < declare the rest of the public interface methods here >

    // friend to allocate and initialize newalarm from heap
    friend NewAlarm *ESMCI_newalarmCreate(int, const char*, Clock*, 
                                 Time*, TimeInterval*, Time*, 
                                 TimeInterval*, int*, Time*, bool*,
                                 bool*, int*);

    // friend function to copy an newalarm
    friend NewAlarm *ESMCI_newalarmCreate(NewAlarm*, int*);

    // friend to de-allocate newalarm
    friend int ESMCI_newalarmDestroy(NewAlarm **);

    // friend function to de-allocate clock, allowing a clock's newalarm's
    // clock pointers to be nullified
    friend int ESMCI_ClockDestroy(Clock **);

// !PRIVATE MEMBER FUNCTIONS:
//
  private:
//
 // < declare private interface methods here >

    // check if time to turn on newalarm
    bool checkTurnOn(bool timeStepPositive);

    // reconstruct ringBegin during ESMF_DIRECTION_REVERSE
    int resetRingBegin(bool timeStepPositive);

    // friend class newalarm
    friend class Clock;

//
//EOP
//-------------------------------------------------------------------------

};  // end class NewAlarm

    // Note: though seemingly redundant with the friend declarations within
    // the class definition above, the following declarations are necessary
    // to appease some compilers (most notably IBM), as well as ANSI C++. 
    // These also establish defaults to match F90 optional args.

    NewAlarm *ESMCI_newalarmCreate(int nameLen,
                                 const char*        name=0,
                                 Clock*        clock=0, 
                                 Time*         ringTime=0,
                                 TimeInterval* ringInterval=0,
                                 Time*         stopTime=0, 
                                 TimeInterval* ringDuration=0,
                                 int*               ringTimeStepCount=0,
                                 Time*         refTime=0,
                                 bool*              enabled=0,
                                 bool*              sticky=0,
                                 int*               rc=0);

    // friend function to copy a newalarm
    NewAlarm *ESMCI_newalarmCreate(NewAlarm *newalarm, int *rc=0);

    // friend to de-allocate newalarm
    int ESMCI_newalarmDestroy(NewAlarm **newalarm);

    // friend to restore state
    NewAlarm *ESMCI_newalarmReadRestart(int nameLen,
                                      const char*  name=0,
                                      int*         rc=0);

 }  // namespace ESMCI
#endif // ESMC_NEWALARM_H
