/* $Id: ESMC_Calendar.c,v 1.1 2002/11/15 21:29:50 jwolfe Exp $ */

#include "ESMO_Calendar.h"
#include "ESMO_Error.h"
#include "ESMO_TimeMgmtUtil.h"

/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_CalendarConstruct"

int ESMC_CalendarConstruct(ESMC_Calendar this, ESMC_CalendarType type, int year)
{
  int i, j;
  int dimStandard[13] = {0, 31,28,31, 30,31,30, 31,31,30, 31,30,31};
  
#ifdef ESMC_DEBUG
  if((type != ESMC_NO_LEAP) && (type != ESMC_GREGORIAN) && (type != ESMC_360_DAY))
      ESMC_ERRA(ESMC_ERR_ARG_OUTOFRANGE, 0, "calendar type is not valid");
#endif


  /* Set the number of days per month according to the desired calendar. 
     Initialize arrays in case this method is used to rebuild an existing
     calendar. */

  this->dim[0] = ESMC_TIME_UNDEFINED;
  this->dimRunningSum[0] = ESMC_TIME_UNDEFINED;
  for(i=1; i<13; i++){
    this->dimRunningSum[i] = 0;
  }

  if(type == ESMC_360_DAY){
    for(i=1; i<13; i++){
      this->dim[i] = 30;
    }
  } 
  else {
    for(i=1; i<13; i++){
      this->dim[i] = dimStandard[i];
    }
  }

  /* Adjust the number of days if this is a leap year */

  if((type == ESMC_GREGORIAN) && ESMC_IS_LEAP_YEAR(year)){
    this->dim[2]=29;
  }

  /* Compute and store the running sum of days per month. */

  for(i=2; i<13; i++){
    for(j=1; j<i; j++){
      this->dimRunningSum[i] += this->dim[j];
    }  
  }
  this->diy = this->dimRunningSum[12] + this->dim[12];

  this->type=type;

  return(ESMC_SUCCESS);
}



/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_CalendarConstructUndefined"

int ESMC_CalendarConstructUndefined(ESMC_Calendar this)
{
  int i;

  this->diy = ESMC_TIME_UNDEFINED;
  this->type = ESMC_CALENDAR_TYPE_UNDEFINED;    
  for(i=0; i<13; i++){
    this->dim[i] = ESMC_TIME_UNDEFINED;
    this->dimRunningSum[i] = ESMC_TIME_UNDEFINED;
  }

  return(ESMC_SUCCESS);
}



/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_CalendarBuildDayOfYear"

int ESMC_CalendarBuildDayOfYear(ESMC_Calendar this, int month, int day, 
			      int *dayOfYear)
{
  *dayOfYear = this->dimRunningSum[month]+day;
  if((this->diy == 366)&&(month > 2)) *dayOfYear += 1.;
  
  return(ESMC_SUCCESS);
}



/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_CalendarPrint"

int ESMC_CalendarPrint(ESMC_Calendar this)
{
  int i;
  char str[32];

  switch(this->type){
    case ESMC_CALENDAR_TYPE_UNDEFINED:
      strcpy(str, "ESMC_CALENDAR_TYPE_UNDEFINED");
      break;
    case ESMC_NO_LEAP:
      strcpy(str, "ESMC_NO_LEAP");
      break;
    case ESMC_GREGORIAN:
      strcpy(str, "ESMC_GREGORIAN");
      break;
    case ESMC_360_DAY:
      strcpy(str, "ESMC_360_DAY");
      break;
    default:
      strcpy(str, "UNRECOGNIZED CALENDAR TYPE");
  } 

  printf("Printing Calendar:\n");
  printf("type         = %s\n", str);
  printf("days in year = %d\n", this->diy);
  printf("days in month:");
  for(i=1; i<13; i++){
    printf(" %d", this->dim[i]);
  }
  printf("\n");
  printf("running sum of days in month:");
  for(i=1; i<13; i++){
    printf(" %d", this->dimRunningSum[i]);
  }
  printf("\n");  

  return(ESMC_SUCCESS);
}






















