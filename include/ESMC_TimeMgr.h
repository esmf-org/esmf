/* ESMC_TimeMgr.h */

#ifndef ESMC_TIME_MGR_H
#define ESMC_TIME_MGR_H

#include "ESMC_BasicUtil.h"
#include "ESMC_Time.h"
#include "ESMC_Date.h"

struct TimeMgrClass{
  int nstep;
  ESMC_TimeClass stepSize;
  ESMC_DateClass startDate;
  ESMC_DateClass stopDate;
  ESMC_DateClass baseDate;
  ESMC_DateClass currDate;
  ESMC_DateClass prevDate;
};

typedef struct TimeMgrClass *ESMC_TimeMgr;

typedef struct TimeMgrClass ESMC_TimeMgrClass;

/*============================================================================*
 * Public methods
 *============================================================================*/

extern int ESMC_TimeMgrNew(ESMC_TimeMgr *thisp, ESMC_Time stepSize, ESMC_Date startDate, 
			  ESMC_Date stopDate, ESMC_Date baseDate);

extern int ESMC_TimeMgrNewIS(ESMC_TimeMgr *thisp, int stepDays, int stepSecs,
			    int startCalendarDate, int startTOD,
			    int stopCalendarDate, int stopTOD,
			    int baseCalendarDate, int baseTOD,
			    ESMC_CalendarType type);

extern int ESMC_TimeMgrConstruct(ESMC_TimeMgr this, ESMC_Time stepSize, ESMC_Date startDate, 
				ESMC_Date stopDate, ESMC_Date baseDate);

extern int ESMC_TimeMgrConstructIS(ESMC_TimeMgr this, int stepDays, int stepSecs,
				  int startCalendarDate, int startTOD,
				  int stopCalendarDate, int stopTOD,
				  int baseCalendarDate, int baseTOD,
				  ESMC_CalendarType type);

extern int ESMC_TimeMgrConstructNoBase(ESMC_TimeMgr this, ESMC_Time stepSize, 
				      ESMC_Date startDate, ESMC_Date stopDate);

extern int ESMC_TimeMgrConstructNoBaseIS(ESMC_TimeMgr this, 
					int stepDays, int stepSecs,
					int startCalendarDate, int startTOD,
					int stopCalendarDate, int stopTOD,
					ESMC_CalendarType type);

extern int ESMC_TimeMgrAdvance(ESMC_TimeMgr this);

extern int ESMC_TimeMgrLastStep(ESMC_TimeMgr this, ESMC_Bool *lastStep);

extern int ESMC_TimeMgrGetNStep(ESMC_TimeMgr this, int *nstep);

extern int ESMC_TimeMgrSetNStep(ESMC_TimeMgr this, int nstep);

extern int ESMC_TimeMgrSetStepSize(ESMC_TimeMgr this, ESMC_Time stepSize);

extern int ESMC_TimeMgrSetStepSizeIS(ESMC_TimeMgr this, int days, int seconds);

extern int ESMC_TimeMgrGetStepSize(ESMC_TimeMgr this, ESMC_Time stepSize);

extern int ESMC_TimeMgrGetStepSizeIS(ESMC_TimeMgr this, int *days, int *seconds);

extern int ESMC_TimeMgrGetStartDate(ESMC_TimeMgr this, ESMC_Date startDate);

extern int ESMC_TimeMgrGetStopDate(ESMC_TimeMgr this, ESMC_Date stopDate);

extern int ESMC_TimeMgrGetBaseDate(ESMC_TimeMgr this, ESMC_Date baseDate);

extern int ESMC_TimeMgrGetCurrDate(ESMC_TimeMgr this, ESMC_Date currDate);

extern int ESMC_TimeMgrSetCurrDateIS(ESMC_TimeMgr this, int dateYYMMDD, int tod);

extern int ESMC_TimeMgrGetPrevDate(ESMC_TimeMgr this, ESMC_Date prevDate);

extern int ESMC_TimeMgrRestartWriteIS(ESMC_TimeMgr this,
				    ESMC_CalendarType *type,
				    int *nstep,
				    int *stepDays, int *stepSec,
				    int *startYYMMDD, int *startSec,
				    int *stopYYMMDD, int *stopSec,
				    int *baseYYMMDD, int *baseSec,
				    int *currYYMMDD, int *currSec);

extern int ESMC_TimeMgrRestartReadIS(ESMC_TimeMgr this,
				   ESMC_CalendarType type,
				   int nstep,
				   int stepDays, int stepSec,
				   int startYYMMDD, int startSec,
				   int stopYYMMDD, int stopSec,
				   int baseYYMMDD, int baseSec,
				   int currYYMMDD, int currSec);

extern int ESMC_TimeMgrNewRestartReadIS(ESMC_TimeMgr *thisp,
				       ESMC_CalendarType type,
				       int nstep,
				       int stepDays, int stepSec,
				       int startYYMMDD, int startSec,
				       int stopYYMMDD, int stopSec,
				       int baseYYMMDD, int baseSec,
				       int currYYMMDD, int currSec);

extern int ESMC_TimeMgrDelete(ESMC_TimeMgr this);

#endif










