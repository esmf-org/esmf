// $Id: ESMC_Clock.h,v 1.2 2002/10/15 23:29:53 eschwab Exp $
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
 #include <ESMC_TimeMgr.h>
 #include <ESMC_Types.h>
 #include <pthread.h>

//-------------------------------------------------------------------------
//BOP
//
// !CLASS: ESMC_Clock - keeps track of model time
//
// !DESCRIPTION:
//
// The code in this file defines the C++ Clock members and declares method
// signatures (prototypes).  The companion file ESMC_Clock.C contains
// the definitions (full code bodies) for the Clock methods.
//
// The clock class encapsulates the essential ESM component requirement of
// tracking and time-stepping model time.  It also checks associated alarms to
// trigger their ringing state.
//
// The Clock class contains TimeInstants and a TimeInterval to track and time
// step model time.  For tracking, TimeInstants are instantiated for the
// current time, stop time, start time, reference time, and previous time. For
// time stepping, a single TimeInterval is instantiated.  There is also an
// integer counter for keeping track of the number of timesteps, and an array
// of associated alarms.  Methods are defined for advancing the clock (perform
// a time step), checking if the stop time is reached, synchronizing with a
// real-time clock, and getting values of the class attributes defined above.
// After performing the time step, the advance method will iterate over the
// alarm list and return a list of any active alarms.
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
//#include <ESMC_Base.h>  // all classes inherit from the ESMC Base class.
 #include <ESMC_TimeInterval.h>
 #include <ESMC_TimeInstant.h>
 #include <ESMC_Alarm.h>

 #define MAX_ALARMS 10    // maximum alarms per clock

// !PUBLIC TYPES:
 class ESMC_Clock;

// !PRIVATE TYPES:
 // class configuration type:  not needed for Clock

 // class definition type
 class ESMC_Clock {

  private:   // corresponds to F90 module 'type ESMF_Clock' members
    ESMC_TimeInterval TimeStep;
    ESMC_TimeInstant  StartTime;
    ESMC_TimeInstant  StopTime;
    ESMC_TimeInstant  RefTime;   // reference time
    ESMC_TimeInstant  CurrTime;  // current time
    ESMC_TimeInstant  PrevTime;  // previous time

    uint32     		  AdvanceCount;
    ESMC_Alarm *AlarmList[MAX_ALARMS];    // associated alarms

    pthread_mutex_t ClockMutex; // (TMG 7.5)

// !PUBLIC MEMBER FUNCTIONS:

  public:

    // Clock is a shallow class, so only Init methods are needed
    int ESMC_ClockInit(ESMC_TimeInterval *TimeStep,
                       ESMC_TimeInstant  *StartTime,
                       ESMC_TimeInstant  *StopTime,
                       ESMC_TimeInstant  *RefTime);
	                   // (TMG 3.1, 3.4.4)

    // Clock doesn't need configuration, hence GetConfig/SetConfig
    // methods are not required

    int ESMC_ClockAddAlarm(ESMC_Alarm *Alarm);	// (TMG 4.1, 4.2)
    int ESMC_ClockGetAlarmList(ESMC_Alarm *AlarmList);	// (TMG 4.3)

    int ESMC_ClockSyncToWallClock(); // TMG3.4.5
    // (see ESMC_TimeInstant::GetRealTime() ??

    int ESMC_ClockAdvance(ESMC_Alarm *RingingList);
    // TMG3.4.1  after increment, for each alarm,
    //           calls ESMC_Alarm::CheckActive()

    bool ESMC_ClockIsStopTime(void);    // TMG3.5.6

    // accessor methods

    int ESMC_ClockGetAdvanceCount(uint32 *AdvanceCount);    // TMG3.5.1

    int ESMC_ClockGetTimeInterval(ESMC_TimeInterval *TimeInterval);
                                                                   // TMG3.5.2
    int ESMC_ClockSetTimeInterval(ESMC_TimeInterval *TimeInterval);
                                                                   // TMG3.4.2

    int ESMC_ClockGetCurrTime(ESMC_TimeInstant *CurrTime);    // TMG3.5.4
    int ESMC_ClockSetCurrTime(ESMC_TimeInstant *CurrTime);    // TMG3.4.3

    int ESMC_ClockGetStartTime(ESMC_TimeInstant *StartTime);  // TMG3.5.3
    int ESMC_ClockGetStopTime(ESMC_TimeInstant *StopTime);    // TMG3.5.3
    int ESMC_ClockGetRefTime(ESMC_TimeInstant *RefTime);      // TMG3.5.3
    int ESMC_ClockGetPrevTime(ESMC_TimeInstant *PrevTime);    // TMG3.5.4

    int ESMC_ClockGetCurrSimTime(ESMC_TimeInterval *CurrSimTime);   // TMG3.5.5
    int ESMC_ClockGetPrevSimTime(ESMC_TimeInterval *PrevSimTime);   // TMG3.5.5

    // required methods inherited and overridden from the ESMC_Base class

    // internal validation
    int ESMC_ClockValidate(const char *options) const;

    // for persistence/checkpointing
    int ESMC_ClockPrint(ESMC_TimeInterval *TimeStep,
                        ESMC_TimeInstant  *StartTime,
                        ESMC_TimeInstant  *StopTime,
                        ESMC_TimeInstant  *RefTime,
                        ESMC_TimeInstant  *CurrTime,
                        ESMC_TimeInstant  *PrevTime,
                        uint32            *AdvanceCount,
                        ESMC_Alarm        *AlarmList[] ) const;

    // for testing/debugging
    int ESMC_ClockPrint(void) const;

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
