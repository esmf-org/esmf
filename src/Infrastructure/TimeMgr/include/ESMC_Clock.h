// $Id: ESMC_Clock.h,v 1.19 2003/08/30 00:37:22 eschwab Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

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
 #include <ESMF_TimeMgr.inc>
 #include <pthread.h>

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
 #include <ESMC_Base.h>  // all classes inherit from the ESMC Base class.
 #include <ESMC_TimeInterval.h>
 #include <ESMC_Time.h>
 #include <ESMC_Alarm.h>

// !PUBLIC TYPES:
 class ESMC_Clock;

// !PRIVATE TYPES:
 // class configuration type:  not needed for Clock

 // class definition type
 class ESMC_Clock {
// class ESMC_Clock : public ESMC_Base { // TODO: inherit from ESMC_Base class
                                         // when fully aligned with F90 equiv

  private:   // corresponds to F90 module 'type ESMF_Clock' members
    ESMC_TimeInterval timeStep;
    ESMC_Time         startTime;
    ESMC_Time         stopTime;
    ESMC_Time         refTime;   // reference time
    ESMC_Time         currTime;  // current time
    ESMC_Time         prevTime;  // previous time

    ESMF_IKIND_I8     advanceCount;             // number of times
                                                //   ESMC_ClockAdvance has
                                                //   been called (number of
                                                //   time steps taken so far)
    int               numAlarms;                // number of defined alarms
    ESMC_Alarm       *alarmList[MAX_ALARMS];    // associated alarms

//    pthread_mutex_t clockMutex; // TODO: (TMG 7.5)

// !PUBLIC MEMBER FUNCTIONS:

  public:

    // Clock doesn't need configuration, hence GetConfig/SetConfig
    // methods are not required

    // accessor methods

    int ESMC_ClockSetup(ESMC_TimeInterval *timeStep=0,
                        ESMC_Time         *startTime=0,
                        ESMC_Time         *stopTime=0,
                        ESMC_Time         *refTime=0);   // (TMG 3.1, 3.4.4)

    int ESMC_ClockSet(ESMC_TimeInterval *timeStep=0,
                      ESMC_Time         *startTime=0,
                      ESMC_Time         *stopTime=0,
                      ESMC_Time         *refTime=0,    // (TMG 3.1, 3.4.4)
                      ESMC_Time         *currTime=0,
                      ESMF_IKIND_I8     *advanceCount=0);

    int ESMC_ClockGet(ESMC_TimeInterval *timeStep=0,
                      ESMC_Time         *startTime=0,
                      ESMC_Time         *stopTime=0,
                      ESMC_Time         *refTime=0,    // (TMG 3.1, 3.4.4)
                      ESMC_Time         *currTime=0, 
                      ESMC_Time         *prevTime=0, 
                      ESMC_Time         *currSimTime=0, 
                      ESMC_Time         *prevSimTime=0, 
                      ESMF_IKIND_I8     *advanceCount=0, 
                      int               *numAlarms=0);

    int ESMC_ClockAddAlarm(ESMC_Alarm *alarm);  // (TMG 4.1, 4.2)
    int ESMC_ClockGetAlarm(int i, ESMC_Alarm **alarm);
    int ESMC_ClockGetRingingAlarm(int i, ESMC_Alarm **alarm);
    int ESMC_ClockGetAlarmList(ESMC_Alarm ***alarmList, int *numAlarms) const;
    int ESMC_ClockGetAlarmList(ESMC_Alarm **alarmList, int *numAlarms) const;

    int ESMC_ClockAdvance(ESMC_TimeInterval *timeStep=0,
                          int *numRingingAlarms=0);
    //int ESMC_ClockAdvance(ESMC_Alarm *ringingList=0, int *numRingingAlarms=0);
    // TMG3.4.1  after increment, for each alarm,
    //           calls ESMC_Alarm::CheckActive()

    bool ESMC_ClockIsStopTime(int *rc) const;    // TMG3.5.6

    int ESMC_ClockSyncToRealTime(void); // TMG3.4.5
    // (see ESMC_Time::SyncToRealTime()

    // required methods inherited and overridden from the ESMC_Base class

    // for persistence/checkpointing
    int ESMC_ClockReadRestart(ESMC_TimeInterval *timeStep,
                              ESMC_Time         *startTime,
                              ESMC_Time         *stopTime,
                              ESMC_Time         *refTime,
                              ESMC_Time         *currTime,
                              ESMC_Time         *prevTime,
                              ESMF_IKIND_I8      advanceCount,
                              int                numAlarms,
                              ESMC_Alarm        *alarmList[]);

    int ESMC_ClockWriteRestart(ESMC_TimeInterval *timeStep,
                               ESMC_Time         *startTime,
                               ESMC_Time         *stopTime,
                               ESMC_Time         *refTime,
                               ESMC_Time         *currTime,
                               ESMC_Time         *prevTime,
                               ESMF_IKIND_I8     *advanceCount,
                               int               *numAlarms,
                               ESMC_Alarm        *alarmList[] ) const;

    // internal validation
    int ESMC_ClockValidate(const char *options=0) const;

    // for testing/debugging
    int ESMC_ClockPrint(const char *options=0) const;

    // native C++ constructors/destructors
    ESMC_Clock(void);
    ~ESMC_Clock(void);

 // < declare the rest of the public interface methods here >

// !PRIVATE MEMBER FUNCTIONS:
//
  private:
//
 // < declare private interface methods here >
//

//EOP
//-------------------------------------------------------------------------

};  // end class ESMC_Clock

#endif // ESMC_CLOCK_H
