/* $Id: ESMC_TimeMgr.c,v 1.1 2002/11/14 23:40:27 jwolfe Exp $ */

#include "ESMC_TimeMgr.h"
#include "ESMC_Error.h"

/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_TimeMgrNew"

int ESMC_TimeMgrNew(ESMC_TimeMgr *thisp, ESMC_Time stepSize, ESMC_Date startDate, ESMC_Date stopDate, 
		   ESMC_Date baseDate)
{
  int rc;
  
  *thisp = (ESMC_TimeMgr) malloc (sizeof(struct TimeMgrClass));

  rc = ESMC_TimeMgrConstruct(*thisp, stepSize, startDate, stopDate, baseDate);

  if (rc != ESMC_SUCCESS)
    {
      free(*thisp);
      *thisp = 0;
      return rc;
    }

  return(ESMC_SUCCESS);
}



/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_TimeMgrNewIS"

int ESMC_TimeMgrNewIS(ESMC_TimeMgr *thisp, int stepDays, int stepSecs, 
		     int startCalendarDate, int startTOD, 
		     int stopCalendarDate, int stopTOD, 
		     int baseCalendarDate, int baseTOD,
		     ESMC_CalendarType type)
{  
  int rc;

  *thisp = (ESMC_TimeMgr) malloc (sizeof(struct TimeMgrClass));

  rc = ESMC_TimeMgrConstructIS(*thisp, stepDays, stepSecs, startCalendarDate, 
			      startTOD, stopCalendarDate, stopTOD, 
			      baseCalendarDate, baseTOD, type);

  if (rc != ESMC_SUCCESS)
    {
      free(*thisp);
      *thisp = 0;
      return rc;
    }
  
  return(ESMC_SUCCESS);
}



/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_TimeMgrConstruct"

int ESMC_TimeMgrConstruct(ESMC_TimeMgr this, ESMC_Time stepSize, ESMC_Date startDate, 
			 ESMC_Date stopDate, ESMC_Date baseDate)
{  
#ifdef ESMC_DEBUG

  if((startDate->calendar.type != stopDate->calendar.type) ||
     (baseDate->calendar.type != startDate->calendar.type)){ 
    ESMC_ERRA(ESMC_ERR_ARG_OUTOFRANGE, 0, 
      "start, stop and base dates must use the same calendar");
  }
#endif

  this->nstep = 0;

  ESMC_TimeCopyConstruct(&this->stepSize, stepSize);
  ESMC_DateCopyConstruct(&this->startDate, startDate);
  ESMC_DateCopyConstruct(&this->stopDate, stopDate);
  ESMC_DateCopyConstruct(&this->baseDate, baseDate);
  ESMC_DateCopyConstruct(&this->currDate, startDate);
  ESMC_DateConstructUndefined(&this->prevDate);

  return(ESMC_SUCCESS);
}



/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_TimeMgrConstructIS"

int ESMC_TimeMgrConstructIS(ESMC_TimeMgr this, int stepDays, int stepSecs, 
			   int startCalendarDate, int startTOD, 
			   int stopCalendarDate, int stopTOD, 
			   int baseCalendarDate, int baseTOD,
			   ESMC_CalendarType type)
{  
  ESMC_TimeClass stepSize;
  ESMC_DateClass startDate, stopDate, baseDate;

  ESMC_TimeConstructIS(&stepSize, stepDays, stepSecs);
  ESMC_DateConstructIS(&startDate, type, startCalendarDate, startTOD);
  ESMC_DateConstructIS(&stopDate, type, stopCalendarDate, stopTOD);  
  ESMC_DateConstructIS(&baseDate, type, baseCalendarDate, baseTOD);  

  ESMC_TimeMgrConstruct(this, &stepSize, &startDate, &stopDate, &baseDate);

  return(ESMC_SUCCESS);
}



/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_TimeMgrConstructNoBaseIS"

int ESMC_TimeMgrConstructNoBaseIS(ESMC_TimeMgr this, int stepDays, int stepSecs, 
				 int startCalendarDate, int startTOD, 
				 int stopCalendarDate, int stopTOD, 
				 ESMC_CalendarType type)
{  
  int baseCalendarDate=startCalendarDate, baseTOD=startTOD;

  ESMC_TimeMgrConstructIS(this, stepDays, stepSecs, startCalendarDate, startTOD, 
			stopCalendarDate, stopTOD, baseCalendarDate, baseTOD, type);

  return(ESMC_SUCCESS);
}



/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_TimeMgrConstructNoBase"

int ESMC_TimeMgrConstructNoBase(ESMC_TimeMgr this, ESMC_Time stepSize, ESMC_Date startDate, 
			       ESMC_Date stopDate)


{  
  ESMC_DateClass baseDate;

  ESMC_DateCopyConstruct(&baseDate, startDate);
  ESMC_TimeMgrConstruct(this, stepSize, startDate, stopDate, &baseDate);

  return(ESMC_SUCCESS);
}



/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_TimeMgrAdvance"

int ESMC_TimeMgrAdvance(ESMC_TimeMgr this)
{  
  ESMC_DateCopy(&this->prevDate, &this->currDate);
  ESMC_DateIncrement(&this->currDate, &this->currDate, &this->stepSize);
  ++this->nstep;

  return(ESMC_SUCCESS);
}



/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_TimeMgrLastStep"

int ESMC_TimeMgrLastStep(ESMC_TimeMgr this, ESMC_Bool *lastStep)
{  
  ESMC_DateClass temp;
  
  ESMC_DateConstructUndefined(&temp);
  ESMC_DateIncrement(&this->currDate, &temp, &this->stepSize);
  ESMC_DateIsLater(&this->stopDate, &temp, lastStep);

  return(ESMC_SUCCESS);
}



/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_TimeMgrSetStepSize"

int ESMC_TimeMgrSetStepSize(ESMC_TimeMgr this, ESMC_Time stepSize)
{  
  ESMC_TimeCopy(&this->stepSize, stepSize);

  return(ESMC_SUCCESS);
}



/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_TimeMgrSetStepSizeIS"

int ESMC_TimeMgrSetStepSizeIS(ESMC_TimeMgr this, int days, int seconds)
{  
  ESMC_TimeClass temp;

  ESMC_TimeConstructIS(&temp, days, seconds);
  ESMC_TimeMgrSetStepSize(this, &temp);

  return(ESMC_SUCCESS);
}



/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_TimeMgrGetStepSize"

int ESMC_TimeMgrGetStepSize(ESMC_TimeMgr this, ESMC_Time stepSize)
{  
  ESMC_TimeCopy(stepSize, &this->stepSize);

  return(ESMC_SUCCESS);
}



/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_TimeMgrGetStepSizeIS"

int ESMC_TimeMgrGetStepSizeIS(ESMC_TimeMgr this, int *days, int *seconds)
{  
  ESMC_TimeClass temp;

  ESMC_TimeConstructUndefined(&temp);
  ESMC_TimeMgrGetStepSize(this, &temp);
  ESMC_TimeGetIS(&temp, days, seconds);

  return(ESMC_SUCCESS);
}



/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_TimeMgrGetNStep"

int ESMC_TimeMgrGetNStep(ESMC_TimeMgr this, int *nstep)
{  
  *nstep = this->nstep;

  return(ESMC_SUCCESS);
}



/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_TimeMgrSetNStep"

int ESMC_TimeMgrSetNStep(ESMC_TimeMgr this, int nstep)
{  
  this->nstep = nstep;

  return(ESMC_SUCCESS);
}



/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_TimeMgrGetStartDate"

int ESMC_TimeMgrGetStartDate(ESMC_TimeMgr this, ESMC_Date startDate)
{  
  ESMC_DateCopy(startDate, &this->startDate);

  return(ESMC_SUCCESS);
}



/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_TimeMgrGetStopDate"

int ESMC_TimeMgrGetStopDate(ESMC_TimeMgr this, ESMC_Date stopDate)
{  
  ESMC_DateCopy(stopDate, &this->stopDate);

  return(ESMC_SUCCESS);
}



/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_TimeMgrGetBaseDate"

int ESMC_TimeMgrGetBaseDate(ESMC_TimeMgr this, ESMC_Date baseDate)
{  
  ESMC_DateCopy(baseDate, &this->baseDate);

  return(ESMC_SUCCESS);
}



/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_TimeMgrGetCurrDate"

int ESMC_TimeMgrGetCurrDate(ESMC_TimeMgr this, ESMC_Date currDate)
{  
  ESMC_DateCopy(currDate, &this->currDate);

  return(ESMC_SUCCESS);
}



/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_TimeMgrSetCurrDate"

int ESMC_TimeMgrSetCurrDateIS(ESMC_TimeMgr this, int dateYYMMDD, int tod)
{  
  ESMC_CalendarType type;

  ESMC_DateGetCalendarType(&this->currDate, &type);

  ESMC_DateConstructIS(&this->currDate,
		     type,
		     dateYYMMDD,
		     tod);
  
  /* Now update the previous date to reflect this change */
  ESMC_DateDecrement(&this->currDate, &this->prevDate, &this->stepSize);

  return(ESMC_SUCCESS);
}



/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_TimeMgrGetPrevDate"

int ESMC_TimeMgrGetPrevDate(ESMC_TimeMgr this, ESMC_Date prevDate)
{  
  ESMC_DateCopy(prevDate, &this->prevDate);

  return(ESMC_SUCCESS);
}



/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_TimeMgrRestartWriteIS"
int ESMC_TimeMgrRestartWriteIS(ESMC_TimeMgr this,
			     ESMC_CalendarType *type,
			     int *nstep,
			     int *stepDays, int *stepSec,
			     int *startYYMMDD, int *startSec,
			     int *stopYYMMDD, int *stopSec,
			     int *baseYYMMDD, int *baseSec,
			     int *currYYMMDD, int *currSec)
{

  /* Retrieve all calendar dates */
  ESMC_TimeGetIS(&this->stepSize, stepDays, stepSec);
  ESMC_DateGetIS(&this->startDate, startYYMMDD, startSec);
  ESMC_DateGetIS(&this->stopDate, stopYYMMDD, stopSec);
  ESMC_DateGetIS(&this->baseDate, baseYYMMDD, baseSec);
  ESMC_DateGetIS(&this->currDate, currYYMMDD, currSec);

  /* Retrieve the calendar type */
  ESMC_DateGetCalendarType(&this->currDate, type);

  /* Retrieve nstep */
  *nstep = this->nstep;
  
  return (ESMC_SUCCESS);
}



/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_TimeMgrNewRestartReadIS"

int ESMC_TimeMgrNewRestartReadIS(ESMC_TimeMgr *thisp,
				ESMC_CalendarType type,
				int nstep,
				int stepDays, int stepSec,
				int startYYMMDD, int startSec,
				int stopYYMMDD, int stopSec,
				int baseYYMMDD, int baseSec,
				int currYYMMDD, int currSec)
{
  int rc;
  
  /* Allocate space */
  *thisp = (ESMC_TimeMgr) malloc (sizeof(struct TimeMgrClass));

  /* Initialize fields */
  rc = ESMC_TimeMgrRestartReadIS(*thisp,
				type,
				nstep,
				stepDays, stepSec,
				startYYMMDD, startSec,
				stopYYMMDD, stopSec,
				baseYYMMDD, baseSec,
				currYYMMDD, currSec);

  if (rc != ESMC_SUCCESS)
    {
      /* Initialization failed, so free memory and return error */
      free(*thisp);
      *thisp = 0;
      return rc;
    }
  
  return (ESMC_SUCCESS);
}

/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_TimeMgrRestartReadIS"

int ESMC_TimeMgrRestartReadIS(ESMC_TimeMgr this,
			    ESMC_CalendarType type,
			    int nstep,
			    int stepDays, int stepSec,
			    int startYYMMDD, int startSec,
			    int stopYYMMDD, int stopSec,
			    int baseYYMMDD, int baseSec,
			    int currYYMMDD, int currSec)
{
  int rc;

  /* Construct the basic object (nstep is set to zero by this default constructor). */
  rc = ESMC_TimeMgrConstructIS(this,
			     stepDays, stepSec,
			     startYYMMDD, startSec,
			     stopYYMMDD, stopSec,
			     baseYYMMDD, baseSec,
			     type);

  /* Set the current date.  This will also set prev date to be curr - tstep.   */
  ESMC_TimeMgrSetCurrDateIS(this, currYYMMDD, currSec);

  /* Set nstep */
  this->nstep = nstep;
  
  return rc;
}


			     
/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_TimeMgrDelete"

int ESMC_TimeMgrDelete(ESMC_TimeMgr this)
{  
  free(this);
  return(ESMC_SUCCESS);
}
/*----------------------------------------------------------------------------*/

