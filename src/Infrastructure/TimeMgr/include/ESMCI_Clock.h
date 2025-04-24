// $Id$
//
// Earth System Modeling Framework
// Copyright (c) 2002-2025, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// ESMF Clock C++ definition include file
//
// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
//
 // these lines prevent this file from being read more than once if it
 // ends up being included multiple times

#ifndef ESMC_CLOCK_H
#define ESMC_CLOCK_H

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
// !CLASS: ESMCI::Clock - keeps track of model time
//
// !DESCRIPTION:
//
// The code in this file defines the C++ {\tt Clock} members and declares
// method signatures (prototypes).  The companion file {\tt ESMC\_Clock.C}
// contains the definitions (full code bodies) for the {\tt Clock} methods.
//
// The {\tt Clock} class encapsulates the essential ESM component requirement
// of tracking and time-stepping model time.  It also checks associated alarms
// to trigger their ringing state.
//
// The {\tt Clock} class contains {\tt Time} instants and a {\tt TimeInterval}
// to track and time step model time.  For tracking, {\tt Time} instants are
// instantiated for the current time, stop time, start time, reference time,
// and previous time.  For time stepping, a single {\tt TimeInterval} is
// instantiated.  There is also an integer counter for keeping track of the
// number of timesteps, and an array of associated alarms.  Methods are
// defined for advancing the clock (perform a time step), checking if the
// stop time is reached, synchronizing with a real-time clock, and getting
// values of the class attributes defined above. After performing the time
// step, the advance method will iterate over the alarm list and return a
// list of any active alarms.
//
// Notes:
//    TMG 3.2:  Create multiple clocks by simply instantiating this class
//              multiple times
//
//    TMG 3.3:  Component's responsibility
//
//-------------------------------------------------------------------------
//
// !USES:
#include "ESMCI_TimeInterval.h"
#include "ESMCI_Time.h"
#include "ESMCI_Alarm.h"

namespace ESMCI{

// !PUBLIC TYPES:
 class Clock;

// !PRIVATE TYPES:
 // class configuration type:  not needed for Clock

 // class definition type
 class Clock {
// class Clock : public ESMC_Base { // TODO: inherit from ESMC_Base class
                                         // when fully aligned with F90 equiv

  private:   // corresponds to F90 module 'type ESMF_Clock' members
    char         name[ESMF_MAXSTR];  // name of clock
    TimeInterval timeStep;
    TimeInterval currAdvanceTimeStep; // timeStep used in current
                                      // ClockAdvance()
    TimeInterval prevAdvanceTimeStep; // timeStep used in previous
                                      // ClockAdvance()
    Time         startTime;
    Time         stopTime;
    Time         refTime;   // reference time
    Time         currTime;  // current time
    Time         prevTime;  // previous time

   
    bool         repeat;  // If true, this is a repeating clock
    TimeInterval repeatDuration;  // Duration to repeat over if it's a repeating clock
    ESMC_I8     repeatCount;             // the number of times the clock has repeated
   
   
    ESMC_I8      advanceCount;             // number of times
                                                //   ESMCI_ClockAdvance has
                                                //   been called (number of
                                                //   time steps taken so far)

    ESMC_Direction    direction;                // forward (default) or reverse
    bool              userChangedDirection;     // used to determine whether
                                                // to adjust alarm on 
                                                // direction change

    int               alarmCount;               // number of defined alarms
    int               alarmListCapacity;        // max number of defined alarms
                                                //  before a reallocation is
                                                //  necessary
    Alarm           **alarmList;                // associated alarm array

    bool              stopTimeEnabled;  // true if optional property set

    int               id;         // unique identifier. used for equality
                                  //    checks and to generate unique default
                                  //    names.
                                  //    TODO: inherit from ESMC_Base class
    static int        count;      // number of clocks created. Thread-safe
                                  //   because int is atomic.
                                  //    TODO: inherit from ESMC_Base class

//    pthread_mutex_t clockMutex; // TODO: (TMG 7.5)

// !PUBLIC MEMBER FUNCTIONS:

  public:

    // Clock doesn't need configuration, hence GetConfig/SetConfig
    // methods are not required

    // accessor methods

    int set(int                nameLen,
                      const char        *name=0,
                      TimeInterval *timeStep=0,
                      Time         *startTime=0,
                      Time         *stopTime=0,
                      TimeInterval *runDuration=0,
                      int               *runTimeStepCount=0,
// TODO: add overload for ESMC_R8  *runTimeStepCount=0,
                      Time         *refTime=0,    // (TMG 3.1, 3.4.4)
                      Time         *currTime=0,
                      ESMC_I8      *advanceCount=0,
                      ESMC_Direction    *direction=0);

    int get(int                nameLen,
                      int               *tempNameLen,
                      char              *tempName=0,
                      TimeInterval *timeStep=0,
                      Time         *startTime=0,
                      Time         *stopTime=0,
                      TimeInterval *runDuration=0,
                      ESMC_R8      *runTimeStepCount=0,
                      Time         *refTime=0,    // (TMG 3.1, 3.4.4)
                      Time         *currTime=0, 
                      Time         *prevTime=0, 
                      TimeInterval *currSimTime=0, 
                      TimeInterval *prevSimTime=0, 
                      Calendar    **calendar=0,
                      ESMC_CalKind_Flag *calkindflag=0,
                      int               *timeZone=0,
                      ESMC_I8      *advanceCount=0, 
                      int               *alarmCount=0,
                      ESMC_Direction    *direction=0,
                      TimeInterval *repeatDuration=0,
                      ESMC_I8      *repeatCount=0
            );

    int advance(TimeInterval *timeStep=0,
                          char *ringingAlarmList1stElementPtr=0, 
                          char *ringingAlarmList2ndElementPtr=0, 
                          int  sizeofRingingAlarmList=0, 
                          int *ringingAlarmCount=0);

    // TMG3.4.1  after increment, for each alarm,
    //           calls Alarm::CheckActive()

    bool isStopTime(int *rc=0) const;           // TMG3.5.6
    int  stopTimeEnable(Time *stopTime=0); // WRF
    int  stopTimeDisable(void);                 // WRF
    bool isStopTimeEnabled(int *rc=0) const;    // WRF

    bool isDone(int *rc=0) const;           // TMG3.5.7
    bool isReverse(int *rc=0) const;        // TMG3.4.6

    int getNextTime(Time         *nextTime,
                              TimeInterval *timeStep=0);

    int getAlarm(int alarmnameLen, char *alarmname, Alarm **alarm);

    int getAlarmList(ESMC_AlarmList_Flag alarmlistflag,
                               char *AlarmList1stElementPtr, 
                               char *AlarmList2ndElementPtr,
                               int  sizeofAlarmList, 
                               int *alarmCount,
                               TimeInterval *timeStep=0);

    int syncToRealTime(void); // TMG3.4.5
    // (see Time::SyncToRealTime()

    // to suuport copying of the alarmList
    Clock& operator=(const Clock &);

    bool operator==(const Clock &) const;
    bool operator!=(const Clock &) const;

    // required methods inherited and overridden from the ESMC_Base class

    // for persistence/checkpointing

    // friend to restore state
    friend Clock *ESMCI_ClockReadRestart(int, const char*, int*);
    // save state
    int writeRestart(void) const;

    // internal validation
    int validate(const char *options=0) const;

    // for testing/debugging
    int print(const char *options=0) const;

    // native C++ constructors/destructors
    Clock(void);
    Clock(const Clock &clock);
    ~Clock(void);

 // < declare the rest of the public interface methods here >
    //

    // friend function to allocate and initialize clock from heap
    friend Clock *ESMCI_ClockCreate(int, const char*, TimeInterval*,
                                 Time*, Time*, TimeInterval*,
                                 int*, Time*, TimeInterval*, int*);
// TODO: add overload for ESMC_R8  *runTimeStepCount

    // friend function to copy a clock
    friend Clock *ESMCI_ClockCreate(Clock*, int*);

    // friend function to de-allocate clock
    friend int ESMCI_ClockDestroy(Clock **);

    // friend to allocate and initialize alarm from heap
    //   (needs access to clock current time to initialize alarm ring time)
    friend Alarm *ESMCI_alarmCreate(int, const char*, Clock*, 
                                 Time*, TimeInterval*, Time*, 
                                 TimeInterval*, int*, Time*, bool*,
                                 bool*, int*);

    // friend function to copy an alarm
    friend Alarm *ESMCI_alarmCreate(Alarm*, int*);

    // friend to de-allocate alarm
    friend int ESMCI_alarmDestroy(Alarm **);

// !PRIVATE MEMBER FUNCTIONS:
//
  private:
//
 // < declare private interface methods here >

    // called only by friend class Alarm
    int addAlarm(Alarm *alarm);    // alarmCreate(), alarmSet() (TMG 4.1, 4.2)
    int removeAlarm(Alarm *alarm); // alarmDestroy(), alarmSet()

    friend class Alarm;

//
//EOP
//-------------------------------------------------------------------------

};  // end class Clock

    // Note: though seemingly redundant with the friend declarations within
    // the class definition above, the following declarations are necessary
    // to appease some compilers (most notably IBM), as well as ANSI C++. 
    // These also establish defaults to match F90 optional args.

    // friend function to allocate and initialize clock from heap
    Clock *ESMCI_ClockCreate(int                nameLen,
                                 const char*        name=0,
                                 TimeInterval* timeStep=0,
                                 Time*         startTime=0,
                                 Time*         stopTime=0,
                                 TimeInterval *runDuration=0,
                                 int               *runTimeStepCount=0,
// TODO: add overload for ESMC_R8             *runTimeStepCount=0,
                                 Time*         refTime=0,
                                 TimeInterval *repeatDuration=0,
                                 int*               rc=0);

    // friend function to copy a clock
    Clock *ESMCI_ClockCreate(Clock *clock, int *rc=0);

    // friend function to de-allocate clock
    int ESMCI_ClockDestroy(Clock **clock);

    // friend to restore state
    Clock *ESMCI_ClockReadRestart(int nameLen,
                                      const char*  name=0,
                                      int*         rc=0);

}   // namespace ESMCI

#endif // ESMC_CLOCK_H
