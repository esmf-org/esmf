/* $Id: ESMC_Alarm.c,v 1.1 2002/11/15 21:28:50 jwolfe Exp $ */

#include "ESMO_Alarm.h"

/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_AlarmNewPeriodic"

int ESMC_AlarmNewPeriodic(ESMC_Alarm *thisp, ESMC_Time period, ESMC_Time offset)
{
  int rc;

  *thisp = (ESMC_Alarm) malloc (sizeof(struct AlarmClass));

  rc = ESMC_AlarmConstructPeriodic(*thisp, period, offset);

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
#define __FUNC__ "ESMC_AlarmNewMonthly"

int ESMC_AlarmNewMonthly(ESMC_Alarm *thisp)
{
  int rc;
  
  *thisp = (ESMC_Alarm) malloc (sizeof(struct AlarmClass));

  rc = ESMC_AlarmConstructMonthly(*thisp);

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
#define __FUNC__ "ESMC_AlarmNewYearly"

int ESMC_AlarmNewYearly(ESMC_Alarm *thisp)
{
  int rc;

  *thisp = (ESMC_Alarm) malloc (sizeof(struct AlarmClass));

  rc = ESMC_AlarmConstructYearly(*thisp);

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
#define __FUNC__ "ESMC_AlarmConstructPeriodic"

int ESMC_AlarmConstructPeriodic(ESMC_Alarm this, ESMC_Time period, 
			       ESMC_Time offset)
{
  this->type = ESMC_ALARM_PERIODIC;
  ESMC_TimeCopyConstruct(&this->period, period);
  ESMC_TimeCopyConstruct(&this->offset, offset);
  this->alarmOn = ESMC_FALSE;

  return(ESMC_SUCCESS);
}



/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_AlarmConstructMonthly"

int ESMC_AlarmConstructMonthly(ESMC_Alarm this)
{
  this->type = ESMC_ALARM_MONTHLY;
  ESMC_TimeConstructUndefined(&this->period);
  ESMC_TimeConstructUndefined(&this->offset);
  this->alarmOn = ESMC_FALSE; 

  return(ESMC_SUCCESS);
}



/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_AlarmConstructYearly"

int ESMC_AlarmConstructYearly(ESMC_Alarm this)
{
  this->type = ESMC_ALARM_YEARLY;
  ESMC_TimeConstructUndefined(&this->period);
  ESMC_TimeConstructUndefined(&this->offset);
  this->alarmOn = ESMC_FALSE;

  return(ESMC_SUCCESS);
}



/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_AlarmIsOn"

int ESMC_AlarmIsOn(ESMC_Alarm this, ESMC_TimeMgr timeMgr, ESMC_Bool *alarmOn)
{
  switch(this->type){
  case(ESMC_ALARM_PERIODIC):
    *alarmOn = ESMC_FALSE;
    printf("This alarm type is not yet supported.\n");
    break;
  case(ESMC_ALARM_MONTHLY):
    *alarmOn = (timeMgr->currDate.month != timeMgr->prevDate.month) ? ESMC_TRUE : ESMC_FALSE; 
    break;
  case(ESMC_ALARM_YEARLY):
    *alarmOn = (timeMgr->currDate.year != timeMgr->prevDate.year) ? ESMC_TRUE : ESMC_FALSE;
    break;
  }

  return(ESMC_SUCCESS);
}



/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_AlarmGetType"

int ESMC_AlarmGetType(ESMC_Alarm this, ESMC_AlarmType *type)
{
  *type = this->type;

  return(ESMC_SUCCESS);
}



/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_AlarmSet"

int ESMC_AlarmSet(ESMC_Alarm this, ESMC_Bool alarmOn)
{
  this->alarmOn = alarmOn;

  return(ESMC_SUCCESS);
}



/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_AlarmDelete"

int ESMC_AlarmDelete(ESMC_Alarm this)
{
  free(this);

  return(ESMC_SUCCESS);
}













