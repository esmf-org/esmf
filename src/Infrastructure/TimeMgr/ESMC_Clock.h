// $Id: ESMC_Clock.h,v 1.2 2002/09/23 20:29:17 eschwab Exp $
#ifndef ESMC_CLOCK_H
#define ESMC_CLOCK_H

#include <ESMC_Types.h>
#include <ESMC_TimeInterval.h>
#include <ESMC_TimeInstant.h>
#include <pthread.h>

#define MAX_ALARMS 10

class ESMC_Alarm;

class ESMC_Clock
{
//-------------------------------------------------------------------------
//BOP
//
// !CLASS: ESMC_Alarm
//
// !SUPERCLASSES:
//
// !AGGREGATE CLASSES:
//	ESMC_TimeInterval (1)
//	ESMC_TimeInstant  (5)
//
// !ASSOCIATE CLASSES:
//	ESMC_Alarm (list)
//
// !FRIEND CLASSES:
//
// !PUBLIC DATA MEMBERS:
//
// !PUBLIC MEMBER FUNCTIONS:
  public:

    ESMC_Clock(void);
    ~ESMC_Clock(void);

	// (TMG 3.1, 3.4.4)
    int Init(ESMC_TimeInterval *TimeStep,
             ESMC_TimeInstant  *StartTime,
             ESMC_TimeInstant  *StopTime,
             ESMC_TimeInstant  *RefTime);

    int AddAlarm(ESMC_Alarm *Alarm);	// (TMG 4.1, 4.2)
    int GetAlarmList(ESMC_Alarm *AlarmList);	// (TMG 4.3)

    int SyncToWallClock(); // TMG3.4.5 (see ESMC_TimeInstant::GetRealTime() ??

    int Advance(ESMC_Alarm *RingingList);
    // TMG3.4.1  after increment, for each alarm,
    //           calls ESMC_Alarm::CheckActive()

    bool IsStopTime(void);    // TMG3.5.6
    int GetAdvanceCount(uint32 *AdvanceCount);    // TMG3.5.1

    int GetTimeInterval(ESMC_TimeInterval *TimeInterval);    // TMG3.5.2
    int SetTimeInterval(ESMC_TimeInterval *TimeInterval);    // TMG3.4.2

    int GetCurrTime(ESMC_TimeInstant *CurrTime);    // TMG3.5.4
    int SetCurrTime(ESMC_TimeInstant *CurrTime);    // TMG3.4.3

    int GetStartTime(ESMC_TimeInstant *StartTime);  // TMG3.5.3
    int GetStopTime(ESMC_TimeInstant *StopTime);    // TMG3.5.3
    int GetRefTime(ESMC_TimeInstant *RefTime);      // TMG3.5.3
    int GetPrevTime(ESMC_TimeInstant *PrevTime);    // TMG3.5.4

    int GetCurrSimTime(ESMC_TimeInterval *CurrSimTime);    //TMG3.5.5
    int GetPrevSimTime(ESMC_TimeInterval *PrevSimTime);    //TMG3.5.5

// !DESCRIPTION:
//    TMG 3.2:  Create multiple clocks by simply instantiating this class
//              multiple times
//
//    TMG 3.3:  Component's responsibility
//
// !BUGS:
//
// !SEE ALSO:
//
// !REVISION HISTORY:
//
//  10Jun02   Earl Schwab  Initial code.
//
//EOP
//-------------------------------------------------------------------------

  private:
    ESMC_TimeInterval TimeStep;
    ESMC_TimeInstant  StartTime;
    ESMC_TimeInstant  StopTime;
    ESMC_TimeInstant  RefTime;
    ESMC_TimeInstant  CurrTime;
    ESMC_TimeInstant  PrevTime;

    uint32     		  AdvanceCount;
    ESMC_Alarm *AlarmList[MAX_ALARMS];    // associated alarms

    pthread_mutex_t ClockMutex; // (TMG 7.5)
};

#endif // ESMC_CLOCK_H
