// $Id: ESMC_Clock.h,v 1.46.2.5 2009/01/21 21:25:23 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research,
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
#include "ESMC_Start.h"
#include "ESMF_TimeMgr.inc"

//-------------------------------------------------------------------------
//BOP
//
// !CLASS: ESMC_Clock - keeps track of model time
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
#include "ESMC_Base.h"    // all classes inherit from the ESMC Base class.
#include "ESMC_IOSpec.h"  // IOSpec class for ReadRestart()/WriteRestart()
#include "ESMC_TimeInterval.h"
#include "ESMC_Time.h"
#include "ESMC_Alarm.h"

// !PUBLIC TYPES:
 class ESMC_Clock;

// !PRIVATE TYPES:
 // class configuration type:  not needed for Clock

 // class definition type
 class ESMC_Clock {
// class ESMC_Clock : public ESMC_Base { // TODO: inherit from ESMC_Base class
                                         // when fully aligned with F90 equiv

  private:   // corresponds to F90 module 'type ESMF_Clock' members
    char              name[ESMF_MAXSTR];  // name of clock
    ESMC_TimeInterval timeStep;
    ESMC_TimeInterval currAdvanceTimeStep; // timeStep used in current
                                           // ClockAdvance()
    ESMC_TimeInterval prevAdvanceTimeStep; // timeStep used in previous
                                           // ClockAdvance()
    ESMC_Time         startTime;
    ESMC_Time         stopTime;
    ESMC_Time         refTime;   // reference time
    ESMC_Time         currTime;  // current time
    ESMC_Time         prevTime;  // previous time

    ESMC_I8      advanceCount;             // number of times
                                                //   ESMC_ClockAdvance has
                                                //   been called (number of
                                                //   time steps taken so far)

    ESMC_Direction    direction;                // forward (default) or reverse

    int               alarmCount;               // number of defined alarms
    int               alarmListCapacity;        // max number of defined alarms
                                                //  before a reallocation is
                                                //  necessary
    ESMC_Alarm      **alarmList;                // associated alarm array

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

    int ESMC_ClockSet(int                nameLen,
                      const char        *name=0,
                      ESMC_TimeInterval *timeStep=0,
                      ESMC_Time         *startTime=0,
                      ESMC_Time         *stopTime=0,
                      ESMC_TimeInterval *runDuration=0,
                      int               *runTimeStepCount=0,
// TODO: add overload for ESMC_R8  *runTimeStepCount=0,
                      ESMC_Time         *refTime=0,    // (TMG 3.1, 3.4.4)
                      ESMC_Time         *currTime=0,
                      ESMC_I8      *advanceCount=0,
                      ESMC_Direction    *direction=0);

    int ESMC_ClockGet(int                nameLen,
                      int               *tempNameLen,
                      char              *tempName=0,
                      ESMC_TimeInterval *timeStep=0,
                      ESMC_Time         *startTime=0,
                      ESMC_Time         *stopTime=0,
                      ESMC_TimeInterval *runDuration=0,
                      ESMC_R8      *runTimeStepCount=0,
                      ESMC_Time         *refTime=0,    // (TMG 3.1, 3.4.4)
                      ESMC_Time         *currTime=0, 
                      ESMC_Time         *prevTime=0, 
                      ESMC_TimeInterval *currSimTime=0, 
                      ESMC_TimeInterval *prevSimTime=0, 
                      ESMC_Calendar    **calendar=0,
                      ESMC_CalendarType *calendarType=0,
                      int               *timeZone=0,
                      ESMC_I8      *advanceCount=0, 
                      int               *alarmCount=0,
                      ESMC_Direction    *direction=0);

    int ESMC_ClockAdvance(ESMC_TimeInterval *timeStep=0,
                          char *ringingAlarmList1stElementPtr=0, 
                          char *ringingAlarmList2ndElementPtr=0, 
                          int  sizeofRingingAlarmList=0, 
                          int *ringingAlarmCount=0);

    // TMG3.4.1  after increment, for each alarm,
    //           calls ESMC_Alarm::CheckActive()

    bool ESMC_ClockIsStopTime(int *rc=0) const;           // TMG3.5.6
    int  ESMC_ClockStopTimeEnable(ESMC_Time *stopTime=0); // WRF
    int  ESMC_ClockStopTimeDisable(void);                 // WRF
    bool ESMC_ClockIsStopTimeEnabled(int *rc=0) const;    // WRF

    bool ESMC_ClockIsDone(int *rc=0) const;           // TMG3.5.7
    bool ESMC_ClockIsReverse(int *rc=0) const;        // TMG3.4.6

    int ESMC_ClockGetNextTime(ESMC_Time         *nextTime,
                              ESMC_TimeInterval *timeStep=0);

    int ESMC_ClockGetAlarm(int nameLen, char *name, ESMC_Alarm **alarm);

    int ESMC_ClockGetAlarmList(ESMC_AlarmListType type,
                               char *AlarmList1stElementPtr, 
                               char *AlarmList2ndElementPtr,
                               int  sizeofAlarmList, 
                               int *alarmCount,
                               ESMC_TimeInterval *timeStep=0);

    int ESMC_ClockSyncToRealTime(void); // TMG3.4.5
    // (see ESMC_Time::SyncToRealTime()

    // to suuport copying of the alarmList
    ESMC_Clock& operator=(const ESMC_Clock &);

    bool operator==(const ESMC_Clock &) const;
    bool operator!=(const ESMC_Clock &) const;

    // required methods inherited and overridden from the ESMC_Base class

    // for persistence/checkpointing

    // friend to restore state
    friend ESMC_Clock *ESMC_ClockReadRestart(int, const char*, 
                                             ESMC_IOSpec*, int*);
    // save state
    int ESMC_ClockWriteRestart(ESMC_IOSpec *iospec=0) const;

    // internal validation
    int ESMC_ClockValidate(const char *options=0) const;

    // for testing/debugging
    int ESMC_ClockPrint(const char *options=0) const;

    // native C++ constructors/destructors
    ESMC_Clock(void);
    ESMC_Clock(const ESMC_Clock &clock);
    ~ESMC_Clock(void);

 // < declare the rest of the public interface methods here >
    //

    // friend function to allocate and initialize clock from heap
    friend ESMC_Clock *ESMC_ClockCreate(int, const char*, ESMC_TimeInterval*,
                                 ESMC_Time*, ESMC_Time*, ESMC_TimeInterval*,
                                 int*, ESMC_Time*, int*);
// TODO: add overload for ESMC_R8  *runTimeStepCount

    // friend function to copy a clock
    friend ESMC_Clock *ESMC_ClockCreate(ESMC_Clock*, int*);

    // friend function to de-allocate clock
    friend int ESMC_ClockDestroy(ESMC_Clock **);

    // friend to allocate and initialize alarm from heap
    //   (needs access to clock current time to initialize alarm ring time)
    friend ESMC_Alarm *ESMC_AlarmCreate(int, const char*, ESMC_Clock*, 
                                 ESMC_Time*, ESMC_TimeInterval*, ESMC_Time*, 
                                 ESMC_TimeInterval*, int*, ESMC_Time*, bool*,
                                 bool*, int*);

// !PRIVATE MEMBER FUNCTIONS:
//
  private:
//
 // < declare private interface methods here >

    // called only by friend class ESMC_Alarm
    int ESMC_ClockAddAlarm(ESMC_Alarm *alarm);  // (TMG 4.1, 4.2)

    friend class ESMC_Alarm;

//
//EOP
//-------------------------------------------------------------------------

};  // end class ESMC_Clock

    // Note: though seemingly redundant with the friend declarations within
    // the class definition above, the following declarations are necessary
    // to appease some compilers (most notably IBM), as well as ANSI C++. 
    // These also establish defaults to match F90 optional args.

    // friend function to allocate and initialize clock from heap
    ESMC_Clock *ESMC_ClockCreate(int                nameLen,
                                 const char*        name=0,
                                 ESMC_TimeInterval* timeStep=0,
                                 ESMC_Time*         startTime=0,
                                 ESMC_Time*         stopTime=0,
                                 ESMC_TimeInterval *runDuration=0,
                                 int               *runTimeStepCount=0,
// TODO: add overload for ESMC_R8             *runTimeStepCount=0,
                                 ESMC_Time*         refTime=0,
                                 int*               rc=0);

    // friend function to copy a clock
    ESMC_Clock *ESMC_ClockCreate(ESMC_Clock *clock, int *rc=0);

    // friend function to de-allocate clock
    int ESMC_ClockDestroy(ESMC_Clock **clock);

    // friend to restore state
    ESMC_Clock *ESMC_ClockReadRestart(int nameLen,
                                      const char*  name=0,
                                      ESMC_IOSpec* iospec=0,
                                      int*         rc=0);

#endif // ESMC_CLOCK_H
