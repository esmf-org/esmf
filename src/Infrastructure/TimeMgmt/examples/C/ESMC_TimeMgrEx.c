/* $Id: ESMC_TimeMgrEx.c,v 1.1 2002/11/15 17:00:17 jwolfe Exp $ */
/*============================================================================*
 * ESMC_TimeMgr C Unit Tests and Examples
 *============================================================================*/

#undef __FUNC__
#define __FUNC__ "ESMC_TimeMgrExC"

#include "ESMC.h"
#include "ESMC_TimeMgr.h"

#define START_DATE        20011128
#define START_SECS        43200

#define STOP_DATE         20031201
#define STOP_SECS         1200

#define STEP_DAYS         1
#define STEP_SECS         43200

#define NUM_ITS           5

int main(int argc, char **argv){

  int i;

  ESMC_Time stepSize;
  ESMC_TimeMgr timeMgr, timeMgr_Reconstructed;
  ESMC_Date startDate, stopDate, baseDate;
  ESMC_Date currDate, prevDate;

  ESMC_App app;

  
  int retDate, retSecs;

  ESMC_CalendarType  calType;
  int nstep, stepDays, stepSec,
    startYYMMDD, startSec,
    stopYYMMDD, stopSec,
    baseYYMMDD, baseSec,
    currYYMMDD, currSec;

  ESMC_CalendarType calType1;
  int nstep1, stepDays1, stepSec1,
    startYYMMDD1, startSec1,
    stopYYMMDD1, stopSec1,
    baseYYMMDD1, baseSec1,
    currYYMMDD1, currSec1;
  
  int test;


  ESMC_AppNew(&app);
  printf("==================================================\n");
  printf("ESMC_TimeMgr C Unit Tests and Examples\n");
  printf("==================================================\n");

  ESMC_TimeNewIS(&stepSize, STEP_DAYS, STEP_SECS);
  ESMC_DateNewIS(&startDate, ESMC_GREGORIAN, START_DATE, START_SECS);
  ESMC_DateNewIS(&stopDate, ESMC_GREGORIAN, STOP_DATE, STOP_SECS);
  ESMC_DateNewIS(&baseDate, ESMC_GREGORIAN, START_DATE, START_SECS);
  ESMC_DateNewIS(&currDate, ESMC_GREGORIAN, ESMC_TIME_UNDEFINED, ESMC_TIME_UNDEFINED);
  ESMC_DateNewIS(&prevDate, ESMC_GREGORIAN, ESMC_TIME_UNDEFINED, ESMC_TIME_UNDEFINED);

  ESMC_TimeMgrNew(&timeMgr, stepSize, startDate, stopDate, baseDate);

  for(i=0; i<NUM_ITS; i++){
    ESMC_TimeMgrAdvance(timeMgr);
  }
  
  ESMC_TimeMgrGetCurrDate(timeMgr, currDate);
  
  ESMC_DateGetIS(currDate, &retDate, &retSecs);

  printf("Ret date = %d, Ret secs = %d\n", retDate, retSecs); 

  ESMC_ERROR_TEST(((retDate==20011206) && (retSecs==0)),
              "ESMC_TimeMgrAdvance: advance a time manager by several timesteps");

  ESMC_TimeMgrSetCurrDateIS(timeMgr, START_DATE, START_SECS);
  ESMC_TimeMgrAdvance(timeMgr);
  ESMC_TimeMgrGetPrevDate(timeMgr, prevDate);
  ESMC_DateGetIS(prevDate, &retDate, &retSecs);
  ESMC_ERROR_TEST(((retDate == START_DATE) && (retSecs == START_SECS)),
              "ESMC_TimeMgrSetCurrDate: set current date (make sure prev date ok)");

  ESMC_TimeMgrRestartWriteIS(timeMgr,
			   &calType,
			   &nstep,
			   &stepDays, &stepSec,
			   &startYYMMDD, &startSec,
			   &stopYYMMDD, &stopSec,
			   &baseYYMMDD, &baseSec,
			   &currYYMMDD, &currSec
			   );
  
   ESMC_TimeMgrNewRestartReadIS(&timeMgr_Reconstructed,
                               calType,
			       nstep,
			       stepDays, stepSec,
			       startYYMMDD, startSec,
			       stopYYMMDD, stopSec,
			       baseYYMMDD, baseSec,
			       currYYMMDD, currSec
			       );

  ESMC_TimeMgrRestartWriteIS(timeMgr_Reconstructed,
			   &calType1,
			   &nstep1,
			   &stepDays1, &stepSec1,
			   &startYYMMDD1, &startSec1,
			   &stopYYMMDD1, &stopSec1,
			   &baseYYMMDD1, &baseSec1,
			   &currYYMMDD1, &currSec1
			   );

  test = (calType==calType1)
    && (nstep==nstep1)
    && (stepDays==stepDays1)
    && (stepSec==stepSec1)
    && (startYYMMDD==startYYMMDD1)
    && (stopYYMMDD==stopYYMMDD1)
    && (baseYYMMDD==baseYYMMDD1)
    && (currYYMMDD==currYYMMDD1);    

  ESMC_ERROR_TEST(test,
		"ESMC_TimeMgrRestartReadIS, ESMC_TimeMgrRestartWriteIS: write then read to get same");

  ESMC_TimeMgrDelete(timeMgr_Reconstructed);

  ESMC_TimeDelete(stepSize);
  ESMC_DateDelete(startDate);
  ESMC_DateDelete(stopDate);
  ESMC_DateDelete(baseDate);
  ESMC_DateDelete(currDate);
  ESMC_TimeMgrDelete(timeMgr);

  ESMC_AppDelete(app);

  return(ESMC_SUCCESS);
}
