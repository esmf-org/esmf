/* $Id: ESMO_Alarm.h,v 1.1 2002/11/15 21:21:08 jwolfe Exp $ */

#ifndef ESMC_ALARM_H
#define ESMC_ALARM_H

#include "ESMO_BasicUtil.h"

#include "ESMO_Date.h"
#include "ESMO_Time.h"
#include "ESMO_TimeMgr.h"

/* 
!BOP
! !ROUTINE: Alarm Class
\begin{verbatim}
*/

enum AlarmType{ ESMC_ALARM_PERIODIC,
                ESMC_ALARM_MONTHLY,
                ESMC_ALARM_YEARLY };

typedef enum AlarmType ESMC_AlarmType;

struct AlarmClass{
  ESMC_AlarmType type;                /* calendar type */
  ESMC_TimeClass offset;              /* offset from start */
  ESMC_TimeClass period;              /* alarm period */
  ESMC_Bool alarmOn;                  /* true if on */
};

/* 
\end{verbatim}
!EOP
*/

typedef struct AlarmClass *ESMC_Alarm;

typedef struct AlarmClass ESMC_AlarmClass;

/*============================================================================*
 * Public methods
 *============================================================================*/

extern int ESMC_AlarmNewPeriodic(ESMC_Alarm *thisp, ESMC_Time period, ESMC_Time offset);

extern int ESMC_AlarmNewMonthly(ESMC_Alarm *thisp);

extern int ESMC_AlarmNewYearly(ESMC_Alarm *thisp);

extern int ESMC_AlarmConstructPeriodic(ESMC_Alarm this, ESMC_Time period, 
				      ESMC_Time offset);

extern int ESMC_AlarmConstructMonthly(ESMC_Alarm this);

extern int ESMC_AlarmConstructYearly(ESMC_Alarm this);

extern int ESMC_AlarmIsOn(ESMC_Alarm this, ESMC_TimeMgr timeMgr, ESMC_Bool *alarmOn);

extern int ESMC_AlarmSet(ESMC_Alarm this, ESMC_Bool alarmOn);

extern int ESMC_AlarmGetType(ESMC_Alarm this, ESMC_AlarmType *type);

extern int ESMC_AlarmDelete(ESMC_Alarm this);

#endif



















