/* $Id: ESMC_Time.c,v 1.1 2002/11/14 23:39:21 jwolfe Exp $ */

#include "ESMC_Time.h"
#include "ESMC_TimeMgmtUtil.h"
#include "ESMC_Error.h"
#include "ESMC_Constants.h"

/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_TimeNewIS"

int ESMC_TimeNewIS(ESMC_Time *thisp, int days, int seconds) 
{
  int rc;

  *thisp = (ESMC_Time) malloc (sizeof(struct TimeClass));

  rc = ESMC_TimeConstructIS(*thisp, days, seconds);

  if (rc != ESMC_SUCCESS)
    {
      free (*thisp);
      *thisp = 0;
      return rc;
    }

  return(ESMC_SUCCESS);
}



/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_TimeNewUndefined"

int ESMC_TimeNewUndefined(ESMC_Time *thisp) 
{
  int rc;

  *thisp = (ESMC_Time) malloc (sizeof(struct TimeClass));

  rc = ESMC_TimeConstructUndefined(*thisp);
  
  if (rc != ESMC_SUCCESS)
    {
      free (*thisp);
      *thisp = 0;
      return rc;
    }
  
  return(ESMC_SUCCESS);
}



/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_TimeConstructIS"

int ESMC_TimeConstructIS(ESMC_Time this, int days, int seconds)
{
 
#ifdef ESMC_DEBUG
  if(ESMC_TIME_NEGATIVE_ATTR_IS(days, seconds)){
    ESMC_ERRA2(ESMC_ERR_ARG_OUTOFRANGE, 0, 
      "days or seconds are invalid: days = %d, seconds = %d", 
      days, seconds);
  }
#endif

  if(ESMC_TIME_UNDEFINED_ATTR_IS(days, seconds)){
    ESMC_TimeConstructUndefined(this);
  }
  else{
    ESMC_TODConstructIS(&this->tod, seconds%ESMC_SID);
    this->day = days + seconds/ESMC_SID;
  }
  return(ESMC_SUCCESS);
}



/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_TimeConstruct"

int ESMC_TimeConstruct(ESMC_Time this, int days, ESMC_TOD tod)
{
  this->day = days;
  this->tod = *tod; 

  return(ESMC_SUCCESS);
}



/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_TimeConstructUndefined"

int ESMC_TimeConstructUndefined(ESMC_Time this)
{ 
  this->day = ESMC_TIME_UNDEFINED;
  ESMC_TODConstructUndefined(&this->tod);

  return(ESMC_SUCCESS);
}



/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_TimeCopyConstruct"

int ESMC_TimeCopyConstruct(ESMC_Time this, ESMC_Time orig)
{ 
  this->day = orig->day;
  this->tod = orig->tod;

  return(ESMC_SUCCESS);
}



/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_TimeCopy"

int ESMC_TimeCopy(ESMC_Time this, ESMC_Time orig)
{ 
#ifdef ESMC_DEBUG

  if((this->tod.type != orig->tod.type) &&
     (this->tod.type != ESMC_TOD_TYPE_UNDEFINED)){
    ESMC_ERRA(ESMC_ERR_ARG_OUTOFRANGE, 0, 
	    "argument TOD types are different or uninitialized");
  }

#endif

  this->day = orig->day;
  this->tod = orig->tod;

  return(ESMC_SUCCESS);
}



/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_TimeSetIS"

int ESMC_TimeSetIS(ESMC_Time this, int days, int seconds)
{
#ifdef ESMC_DEBUG
  if((ESMC_TIME_NEGATIVE_ATTR_IS(days, seconds)) || 
    (ESMC_TIME_UNDEFINED_ATTR_IS(days, seconds))){
    ESMC_ERRA2(ESMC_ERR_ARG_OUTOFRANGE, 0, 
      "days or seconds are invalid or undefined: days = %d, seconds = %d", 
      days, seconds);
  }
#endif

  if(seconds < ESMC_SID){
    ESMC_TODConstructIS(&this->tod, seconds);
    this->day = days;
  }
  else{
    ESMC_TODConstructIS(&this->tod, seconds%ESMC_SID); 
    this->day = days + seconds/ESMC_SID;
  }

  return(ESMC_SUCCESS);
}



/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_TimeGetIS"

int ESMC_TimeGetIS(ESMC_Time this, int *days, int *seconds)
{ 
  *days = this->day;
  *seconds = this->tod.sec;

  return(ESMC_SUCCESS);
}



/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_TimeGetDays"

int ESMC_TimeGetDays(ESMC_Time this, double *days)
{
  *days = (double)(this->day)+(double)(this->tod.sec)/(double)ESMC_SID;

  return(ESMC_SUCCESS);		 
}



/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_TimeIncrementIS"

int ESMC_TimeIncrementIS(ESMC_Time this, ESMC_Time incTime, int days, int seconds)
{
#ifdef ESMC_DEBUG
  if((ESMC_TIME_NEGATIVE_ATTR_IS(days, seconds)) || 
    (ESMC_TIME_UNDEFINED_ATTR_IS(days, seconds))){
    ESMC_ERRA2(ESMC_ERR_ARG_OUTOFRANGE, 0, 
    "days or seconds are invalid or undefined: days = %d, seconds = %d", 
    days, seconds);
  }
  if((ESMC_TIME_INVALID_IS(*this)) || 
    (ESMC_TIME_UNDEFINED_IS(*this))){
    ESMC_ERRA2(ESMC_ERR_ARG_OUTOFRANGE, 0, 
    "time is invalid or undefined: days = %d, seconds = %d", 
    this->day, this->tod.sec);
  }
#endif

  ESMC_TimeConstructIS(incTime,
                       this->day + days + incTime->tod.sec/ESMC_SID,
                       (this->tod.sec + seconds)%ESMC_SID
                       );

  return(ESMC_SUCCESS);		 
}



/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_TimeDiff"

int ESMC_TimeDiff(ESMC_Time earlyTime, ESMC_Time lateTime, ESMC_Time diff, 
		 ESMC_Bool *isLater)
{
  int day, sec;
  
  day = lateTime->day - earlyTime->day;
  if(day == 0){
    sec = lateTime->tod.sec - earlyTime->tod.sec;
    *isLater = (sec>=0) ? ESMC_TRUE : ESMC_FALSE;
    sec = abs(sec);
  }
  else {
    if (day < 0){
      *isLater = ESMC_FALSE;
      day *=-1;
      day--;
      sec = earlyTime->tod.sec + ESMC_SID - lateTime->tod.sec;
    }
    else{ 
      if (day > 0){
        *isLater = ESMC_TRUE;
        day--;
        sec = lateTime->tod.sec + ESMC_SID - earlyTime->tod.sec;
      }
    }
    day += sec/ESMC_SID;
    sec = sec%ESMC_SID;    
  }
  ESMC_TimeSetIS(diff, day, sec);

  return(ESMC_SUCCESS);
}



/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_TimeDecrementIS"

int ESMC_TimeDecrementIS(ESMC_Time this, ESMC_Time decTime, int days, int seconds)
{
  ESMC_TimeClass earlyTime, diff;
  ESMC_Bool isLater;

#ifdef ESMC_DEBUG
  if((ESMC_TIME_NEGATIVE_ATTR_IS(days, seconds)) || 
    (ESMC_TIME_UNDEFINED_ATTR_IS(days, seconds))){
    ESMC_ERRA2(ESMC_ERR_ARG_OUTOFRANGE, 0, 
      "days or seconds are invalid or undefined: days = %d, seconds = %d", 
      days, seconds);
  }
#endif

  ESMC_TimeConstructIS(&earlyTime, days, seconds);
  ESMC_TimeConstructUndefined(&diff);

  ESMC_TimeDiff(&earlyTime, this, &diff, &isLater);

  if(isLater == ESMC_FALSE){
    ESMC_ERRA(ESMC_ERR_ARG_OUTOFRANGE, 0, 
    "decrement is larger than time value");
  }
  else{
    ESMC_TimeCopy(decTime, &diff);
  }

  return(ESMC_SUCCESS);		 
}



/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_TimePrint"

int ESMC_TimePrint(ESMC_Time this)
{
  printf("Printing Time:\n");
  printf("day      = %d\n",  this->day);

  printf("Printing Time internal TOD:\n");
  ESMC_TODPrint(&this->tod);

  return(ESMC_SUCCESS);
}



/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_TimeDelete"

void ESMC_TimeDelete (ESMC_Time this)
{
  free(this);
  
  return;
}




















