/* $Id: ESMC_Date.c,v 1.1 2002/11/14 23:37:29 jwolfe Exp $ */

#include "ESMC_Date.h"
#include "ESMC_TimeMgmtUtil.h"
#include "ESMC_Error.h"

/*--------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_DateNewIS"

int ESMC_DateNewIS(ESMC_Date *thisp, ESMC_CalendarType type, int yearmmdd, int seconds) 
{
  int rc;

  *thisp = (ESMC_Date) malloc (sizeof(struct DateClass));

  rc = ESMC_DateConstructIS(*thisp, type, yearmmdd, seconds);

  if (rc != ESMC_SUCCESS)
    {
      free (*thisp);
      *thisp = 0;
      return rc;
    }

  return(ESMC_SUCCESS);
}



/*--------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_DateNewUndefined"

int ESMC_DateNewUndefined(ESMC_Date *thisp) 
{
  int rc;

  *thisp = (ESMC_Date) malloc (sizeof(struct DateClass));

  rc = ESMC_DateConstructUndefined(*thisp);
  
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
#define __FUNC__ "ESMC_DateConstructIS"

int ESMC_DateConstructIS(ESMC_Date this, ESMC_CalendarType type, int yearmmdd, 
  int seconds)
{
  int year, month, day;

  if((type == ESMC_CALENDAR_TYPE_UNDEFINED) || 
     (seconds == ESMC_TIME_UNDEFINED)){
    ESMC_DateConstructUndefined(this);
    /* For cases in which the user wants to set the date's calendar type only. */
    this->calendar.type = type;
    return(ESMC_SUCCESS);
  }

  year = yearmmdd/10000;
  if (yearmmdd < 0)
     yearmmdd *= -1;
  month = (yearmmdd%10000)/100;
  day = yearmmdd%100;

  ESMC_CalendarConstruct(&this->calendar, type, year);
  ESMC_TODConstructIS(&this->tod, seconds);

#ifdef ESMC_DEBUG
  if((day < 1) || (day > this->calendar.dim[month])){
    ESMC_ERRA1(ESMC_ERR_ARG_OUTOFRANGE, 0, "day = %d", day);
  }

  if((month < 1) || (month > 12)){
    ESMC_ERRA1(ESMC_ERR_ARG_OUTOFRANGE,0, "month = %d", month);
  }
#endif

  /* Compute the Julian day (absolute reference day). */

  ESMC_DATE_2_JULIAN_DAY(this->julianDay, year, month, day);

  ESMC_CalendarBuildDayOfYear(&this->calendar, month, day, &this->dayOfYear);

  this->year = year;
  this->month = month;
  this->day = day;

  return(ESMC_SUCCESS);
}



/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_DateConstructUndefined"

int ESMC_DateConstructUndefined(ESMC_Date this)
{
  this->julianDay = ESMC_TIME_UNDEFINED;
  this->dayOfYear = ESMC_TIME_UNDEFINED;

  this->year = ESMC_TIME_UNDEFINED;
  this->month = ESMC_TIME_UNDEFINED;
  this->day = ESMC_TIME_UNDEFINED;
  ESMC_CalendarConstructUndefined(&this->calendar);
  ESMC_TODConstructUndefined(&this->tod);
    
  return(ESMC_SUCCESS);
}



/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_DateCopyConstruct"

int ESMC_DateCopyConstruct(ESMC_Date this, ESMC_Date orig)
{
  this->calendar = orig->calendar;
  this->tod = orig->tod;
  this->year = orig->year;
  this->month = orig->month;
  this->day = orig->day;
  this->julianDay = orig->julianDay;
  this->dayOfYear = orig->dayOfYear;

  return(ESMC_SUCCESS);
}



/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_DateCopy"

int ESMC_DateCopy(ESMC_Date this, ESMC_Date orig)
{
#ifdef ESMC_DEBUG

  if((this->calendar.type != orig->calendar.type) &&
     (this->calendar.type != ESMC_CALENDAR_TYPE_UNDEFINED)){
    ESMC_ERRA(ESMC_ERR_ARG_OUTOFRANGE, 0, 
	    "argument calendar types are different or uninitialized");
  }

#endif

  ESMC_DateCopyConstruct(this, orig);

  return(ESMC_SUCCESS);
}



/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_DateSetIS"

int ESMC_DateSetIS (ESMC_Date this, ESMC_CalendarType type, int yearmmdd, int seconds)
{

  ESMC_DateConstructIS (this, type, yearmmdd, seconds);

  return(ESMC_SUCCESS);
}



/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_DateGetIS"

int ESMC_DateGetIS(ESMC_Date this, int *yearmmdd, int *seconds)
{
  int year;

  if (this->year < 0)
    year = -1*this->year;
  else
    year = this->year;

  *yearmmdd = year*10000 + this->month*100 + this->day;
  *seconds = this->tod.sec;

  if (this->year < 0)
    *yearmmdd *= -1;

  return(ESMC_SUCCESS);		 
}



/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_DateGetDayOfYear"

int ESMC_DateGetDayOfYear(ESMC_Date this, int *dayOfYear)
{
  *dayOfYear = this->dayOfYear;

  return(ESMC_SUCCESS);		 
}



/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_DateGetCalendarType"

int ESMC_DateGetCalendarType(ESMC_Date this, ESMC_CalendarType *type)
{
  *type = this->calendar.type;
  
  return (ESMC_SUCCESS);
}



/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_Date2Str"

int ESMC_Date2Str(ESMC_Date this, char *str)
{

  /* Implementation */

  return(ESMC_SUCCESS);		 
}



/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_DateIncrement"

int ESMC_DateIncrement(ESMC_Date this, ESMC_Date incDate, ESMC_Time time)
{
  int year, month, day;
  int ndays;

#ifdef ESMC_DEBUG
  if((this->calendar.type != incDate->calendar.type) &&
     (incDate->calendar.type != ESMC_CALENDAR_TYPE_UNDEFINED)){
    ESMC_ERRA(ESMC_ERR_ARG_OUTOFRANGE, 0, 
	    "argument calendar types are different or uninitialized");
  }
#endif

  /* Add the times of day for the original date and time increment 
     and return the result as a new time of day and a number of days. */

  ESMC_TODIncrement(&this->tod, &time->tod, &incDate->tod, &ndays);
  ndays += time->day;
  incDate->julianDay = this->julianDay + ndays;

  switch(this->calendar.type){
    case ESMC_GREGORIAN:
      ESMC_JULIAN_DAY_2_DATE(incDate->julianDay, year, month, day);
      break;
    case ESMC_NO_LEAP:
      month = this->month;  
      year = this->year+(ndays/this->calendar.diy);
      day = this->day+(ndays%this->calendar.diy);

      while(day>this->calendar.dim[month]){
        day-=this->calendar.dim[month];
        month+=1;
        if(month==13){
          ++year;
          month=1;
        }
      }
      break;
    default:
      ESMC_ERRA(ESMC_ERR_ARG_OUTOFRANGE, 0, 
	      "calendar type is not supported by this method");
  }
        
  incDate->year = year;
  incDate->month = month;
  incDate->day = day;

  ESMC_CalendarConstruct(&incDate->calendar, this->calendar.type, year);
  ESMC_CalendarBuildDayOfYear(&incDate->calendar, month, day, &incDate->dayOfYear);

  return(ESMC_SUCCESS);		 
}



/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_DateDecrement"

int ESMC_DateDecrement(ESMC_Date this, ESMC_Date decDate, ESMC_Time time)
{
  int year, month, day;
  int ndays;

#ifdef ESMC_DEBUG
  if((this->calendar.type != decDate->calendar.type) &&
     (decDate->calendar.type != ESMC_CALENDAR_TYPE_UNDEFINED)){
    ESMC_ERRA(ESMC_ERR_ARG_OUTOFRANGE, 0, 
	    "argument calendar types are different or uninitialized");
  }
#endif

  /* Add the times of day for the original date and time increment 
     and return the result as a new time of day and a number of days. */

  ESMC_TODDecrement(&this->tod, &time->tod, &decDate->tod, &ndays);
  ndays += time->day;
  decDate->julianDay = this->julianDay - ndays;

  switch(this->calendar.type){
    case ESMC_GREGORIAN:
      ESMC_JULIAN_DAY_2_DATE(decDate->julianDay, year, month, day);
      break;
    case ESMC_NO_LEAP:
      month = this->month;  
      year = this->year-(ndays/this->calendar.diy);
      day = this->day-(ndays%this->calendar.diy); /* days may be negative at this point */

      while(day<=0){
        month-=1;
        if(month==0){
          --year;
          month=12;
        }
        day+=this->calendar.dim[month];
      }
      break;
    default:
      ESMC_ERRA(ESMC_ERR_ARG_OUTOFRANGE, 0, 
	      "calendar type is not supported by this method");
  }
        
  decDate->year = year;
  decDate->month = month;
  decDate->day = day;

  ESMC_CalendarConstruct(&decDate->calendar, this->calendar.type, year);
  ESMC_CalendarBuildDayOfYear(&decDate->calendar, month, day, &decDate->dayOfYear);

  return(ESMC_SUCCESS);		 
}



/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_DateIncrementSec"

int ESMC_DateIncrementSec(ESMC_Date this, ESMC_Date incDate, int nseconds)
{
  ESMC_TimeClass time;

#ifdef ESMC_DEBUG

  if((this->calendar.type != incDate->calendar.type) &&
     (incDate->calendar.type != ESMC_CALENDAR_TYPE_UNDEFINED)){
    ESMC_ERRA(ESMC_ERR_ARG_OUTOFRANGE, 0, 
	    "argument calendar types are different or uninitialized");
  }

  if((nseconds < 0) || (nseconds == ESMC_TIME_UNDEFINED)){
    ESMC_ERRA1(ESMC_ERR_ARG_OUTOFRANGE, 0, 
    "seconds are invalid or undefined: seconds = %d", nseconds);
  }
#endif
  
  switch(this->tod.type){
    case ESMC_TOD_INT_SEC:
      ESMC_TimeConstructIS(&time, 0, nseconds);
      break;
    default:
      ESMC_ERRA(ESMC_ERR_ARG_OUTOFRANGE, 0, 
	      "time of day type is not supported by this method");
  }

  ESMC_DateIncrement(this, incDate, &time);

  return(ESMC_SUCCESS);
}



/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_DateIncrementDay"

int ESMC_DateIncrementDay (ESMC_Date this, ESMC_Date incDate, int ndays)
{
  ESMC_TimeClass time;

#ifdef ESMC_DEBUG

  if((this->calendar.type != incDate->calendar.type) &&
     (incDate->calendar.type != ESMC_CALENDAR_TYPE_UNDEFINED)){
    ESMC_ERRA(ESMC_ERR_ARG_OUTOFRANGE, 0, 
	    "argument calendar types are different or uninitialized");
  }

  if((ndays < 0) || (ndays == ESMC_TIME_UNDEFINED)){
    ESMC_ERRA1(ESMC_ERR_ARG_OUTOFRANGE, 0, 
    "days are invalid or undefined: days = %d", ndays);
  }
#endif
  
  switch(this->tod.type){
    case ESMC_TOD_INT_SEC:
      ESMC_TimeConstructIS(&time, ndays, 0);
      break;
    default:
      ESMC_ERRA(ESMC_ERR_ARG_OUTOFRANGE, 0, 
	      "time of day type is not supported by this method");
  }

  ESMC_DateIncrement(this, incDate, &time);

  return(ESMC_SUCCESS);
}



/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_DateIncrementMonth"

int ESMC_DateIncrementMonth (ESMC_Date this, ESMC_Date incDate, int nmonths)
{

#ifdef ESMC_DEBUG

  if((this->calendar.type != incDate->calendar.type) &&
     (incDate->calendar.type != ESMC_CALENDAR_TYPE_UNDEFINED)){
    ESMC_ERRA(ESMC_ERR_ARG_OUTOFRANGE, 0, 
	    "argument calendar types are different or uninitialized");
  }

  if((nmonths < 0) || (nmonths == ESMC_TIME_UNDEFINED)){
    ESMC_ERRA1(ESMC_ERR_ARG_OUTOFRANGE, 0, 
    "months are invalid or undefined: months = %d", nmonths);
  }
#endif

  ESMC_DateCopy(incDate, this);

  incDate->month += nmonths;
  
  /* Cycle to a new year if needed. */

  if(incDate->month > 12){
    incDate->year += (incDate->month)/12; 
    incDate->month = (incDate->month)%12;

    /* If in a new year, rebuild the calendar. */

    ESMC_CalendarConstruct(&incDate->calendar, incDate->calendar.type, incDate->year);
  }

  /* If the current day exceeds the number of days in the month, set the
     day of the month to the last day of the month. Seconds will remain the
     same.  Flag if this happens when in DEBUG mode. */

  if(incDate->day > incDate->calendar.dim[incDate->month]){
#ifdef DEBUG
    ESMC_ERRA(ESMC_ERR_DATE, 0,"incrementing by a month resulted in an invalid day");    
#endif
    incDate->day = incDate->calendar.dim[incDate->month];
  }

  /* Compute the new Julian day. */

  ESMC_DATE_2_JULIAN_DAY(incDate->julianDay, incDate->year, incDate->month, incDate->day);
  
  /* Compute the new day of the year */  

  ESMC_CalendarBuildDayOfYear(&incDate->calendar, incDate->month, incDate->day, 
    &incDate->dayOfYear);

  return(ESMC_SUCCESS);
}  



/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_DateIncrementYear"

int ESMC_DateIncrementYear (ESMC_Date this, ESMC_Date incDate, int nyears)
{
#ifdef ESMC_DEBUG

  if((this->calendar.type != incDate->calendar.type) &&
     (incDate->calendar.type != ESMC_CALENDAR_TYPE_UNDEFINED)){
    ESMC_ERRA(ESMC_ERR_ARG_OUTOFRANGE, 0, 
	    "argument calendar types are different or uninitialized");
  }

  if((nyears < 0) || (nyears == ESMC_TIME_UNDEFINED)){
    ESMC_ERRA1(ESMC_ERR_ARG_OUTOFRANGE, 0, 
    "years are invalid or undefined: years = %d", nyears);
  }
#endif

  ESMC_DateCopy(incDate, this);

  incDate->year+=nyears;
  
  ESMC_CalendarConstruct(&incDate->calendar, incDate->calendar.type, incDate->year);

  /* If the current day exceeds the number of days in the month, set the
     day of the month to the last day of the month. Seconds will remain the
     same.  Flag if this happens when in DEBUG mode. */

  if(incDate->day > incDate->calendar.dim[incDate->month]){
#ifdef DEBUG
    ESMC_ERRA(ESMC_ERR_DATE, 0,"incrementing by a year resulted in an invalid day");    
#endif
    incDate->day = incDate->calendar.dim[incDate->month];
  }

  /* Compute the new Julian day. */

  ESMC_DATE_2_JULIAN_DAY(incDate->julianDay, incDate->year, incDate->month, incDate->day);
  
  /* Compute the new day of the year */  

  ESMC_CalendarBuildDayOfYear(&incDate->calendar, incDate->month, incDate->day, 
    &incDate->dayOfYear);

  return(ESMC_SUCCESS);
}  



/*----------------------------------------------------------------------------*/

#undef __FUNC__
#define __FUNC__ "ESMC_DateDiff"

int ESMC_DateDiff (ESMC_Date earlyDate, ESMC_Date lateDate, ESMC_Time diff, 
		  ESMC_Bool *isLater)
{
  int extraday, earlyJulDay, lateJulDay;
  ESMC_DateClass tmpEarly, tmpLate;
  ESMC_TimeClass earlyTime, lateTime;

#ifdef ESMC_DEBUG

  /* Check that date arguments are using the same calendar. */

  if(earlyDate->calendar.type != lateDate->calendar.type)
    ESMC_ERRA(ESMC_ERR_ARG_OUTOFRANGE, 0, "dates must use the same calendar");

#endif

  switch(earlyDate->calendar.type){
    case ESMC_GREGORIAN:

      ESMC_DATE_2_JULIAN_DAY(earlyJulDay, earlyDate->year, earlyDate->month,
        earlyDate->day);    
  
      ESMC_DATE_2_JULIAN_DAY(lateJulDay, lateDate->year, lateDate->month,
        lateDate->day);

      ESMC_TimeConstruct(&earlyTime, earlyJulDay, &earlyDate->tod);
      ESMC_TimeConstruct(&lateTime, lateJulDay, &lateDate->tod);
      ESMC_TimeDiff(&earlyTime, &lateTime, diff, isLater);
      break;
    case ESMC_NO_LEAP:
      ESMC_DateIsLater(earlyDate, lateDate, isLater); 
      if(*isLater){
        ESMC_DateCopyConstruct(&tmpEarly, earlyDate);
	ESMC_DateCopyConstruct(&tmpLate, lateDate);
      }
      else{
        ESMC_DateCopyConstruct(&tmpEarly, lateDate);
	ESMC_DateCopyConstruct(&tmpLate, earlyDate);
      }
      ESMC_TODDecrement(&tmpLate.tod, &tmpEarly.tod, &diff->tod, &extraday);
      if(extraday == 1) ESMC_DateIncrementDay(&tmpEarly, &tmpEarly, 1); 
      diff->day = tmpLate.dayOfYear - tmpEarly.dayOfYear;
      if(diff->day < 0){
        diff->day += tmpLate.calendar.diy;
        ++(tmpEarly.year);
      }
      diff->day += (tmpLate.year - tmpEarly.year)*365;
      break;  
    default:
      ESMC_ERRA(ESMC_ERR_ARG_OUTOFRANGE, 0, 
	      "calendar type is not supported by this method");
  }
    
  return(ESMC_SUCCESS);
}



/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_DateIsLater"

int ESMC_DateIsLater (ESMC_Date earlyDate, ESMC_Date lateDate, ESMC_Bool *isLater)
{
  ESMC_Bool todIsLater;
#ifdef ESMC_DEBUG


  /* Check that date arguments are using the same calendar. */

  if(earlyDate->calendar.type != lateDate->calendar.type)
    ESMC_ERRA(ESMC_ERR_ARG_OUTOFRANGE, 0, "dates must use the same calendar");

#endif
  *isLater= ESMC_FALSE;

  if(lateDate->year > earlyDate->year) {
      /* Year is larger, so done */
      *isLater = ESMC_TRUE;
    } else if(lateDate->year == earlyDate->year) {
      /* Years are equal, so further testing required */
      if (lateDate->dayOfYear > earlyDate->dayOfYear) {
	  /* Year ==, Day is larger, so done. */
	  *isLater = ESMC_TRUE;
	} else if(lateDate->dayOfYear == earlyDate->dayOfYear) {
	  /* Year==, Day ==, so test time of day */
	  ESMC_TODIsLater(&earlyDate->tod, &lateDate->tod, &todIsLater);
	  if(todIsLater) {
	      /* Time of day larger, done */
	      *isLater=ESMC_TRUE;
	    }
	} /* Days == */
    } /* Years == */
  
  return(ESMC_SUCCESS);
}



/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_DatePrint"

int ESMC_DatePrint(ESMC_Date this)
{
  printf("Printing Date:\n");
  printf("year      = %d\n",  this->year);
  printf("month     = %d\n",  this->month);
  printf("day       = %d\n",  this->day);
  printf("julianDay = %d\n", this->julianDay);
  printf("dayOfYear = %d\n", this->dayOfYear); 

  printf("Printing Date internal Calendar:\n");
  ESMC_CalendarPrint(&this->calendar);

  printf("Printing Date internal TOD:\n");
  ESMC_TODPrint(&this->tod);

  return(ESMC_SUCCESS);
}



/*----------------------------------------------------------------------------*/
int ESMC_DateGetFltDayOfYear(ESMC_Date this, double *day)
{
  int iday;
  int ret = ESMC_SUCCESS;
  
  ret = ESMC_DateGetDayOfYear(this, &iday);

  *day = (double) iday;

  /* e.g. 0.1 days = 2.4 hours = 8640 seconds */
  /* Handle the integer seconds and the normal case here.  this->tod.msec
     turns out to be -1, not zero == ESMC_TIME_UNDEFINED */
  if (this->tod.msec == ESMC_TIME_UNDEFINED) {
    *day += (this->tod.sec)
      / (double) (ESMC_SID) ; 
  } else {
    *day += (this->tod.sec + (this->tod.msec / 1000.0) )
      / (double) (ESMC_SID) ; 
  }

  return ret;
}


/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_DateDelete"

int ESMC_DateDelete (ESMC_Date this)
{
  free(this);
  
  return(ESMC_SUCCESS);
}
