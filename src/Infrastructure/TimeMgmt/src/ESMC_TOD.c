/* $Id: ESMC_TOD.c,v 1.1 2002/11/15 21:31:19 jwolfe Exp $ */

#include "ESMO_TOD.h"
#include "ESMO_TimeMgmtUtil.h"
#include "ESMO_Error.h"
#include "ESMO_Constants.h"

/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_TODConstructIS"

int ESMC_TODConstructIS(ESMC_TOD this, int seconds)
{

#ifdef ESMC_DEBUG
  if((seconds<0 || seconds >= ESMC_SID) && (seconds != ESMC_TIME_UNDEFINED)){
    ESMC_ERRA1(ESMC_ERR_ARG_OUTOFRANGE, 0, "sec = %d", seconds);
  }
#endif

  this->type = ESMC_TOD_INT_SEC;
  this->sec = seconds;
  this->msec = ESMC_TIME_UNDEFINED;

  return(ESMC_SUCCESS);
}



/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_TODConstructUndefined"

int ESMC_TODConstructUndefined(ESMC_TOD this)
{
  this->type = ESMC_TOD_TYPE_UNDEFINED;
  this->sec = ESMC_TIME_UNDEFINED;
  this->msec = ESMC_TIME_UNDEFINED;
  
  return(ESMC_SUCCESS);
}



/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_TODIncrement"

int ESMC_TODIncrement(ESMC_TOD this, ESMC_TOD inc, ESMC_TOD resultTOD, int *resultDays)
{
  int sec;

#ifdef ESMC_DEBUG
  if(this->type != inc->type){
    ESMC_ERRA(ESMC_ERR_ARG_OUTOFRANGE,0 ,"input times of day are different types");
  }
#endif

  if(this->type == ESMC_TOD_INT_SEC){
    resultTOD->type = ESMC_TOD_INT_SEC;
    sec = this->sec + inc->sec;
    resultTOD->sec = sec%ESMC_SID;
    *resultDays = sec/ESMC_SID;
  }

  return(ESMC_SUCCESS);
}



/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_TODDecrement"

int ESMC_TODDecrement(ESMC_TOD this, ESMC_TOD dec, ESMC_TOD resultTOD, int *resultDays)
{
#ifdef ESMC_DEBUG
  if(this->type != dec->type){
    ESMC_ERRA(ESMC_ERR_ARG_OUTOFRANGE,0 ,"input times of day are different types");
  }
#endif

  if(this->type == ESMC_TOD_INT_SEC){
    if(this->sec >= dec->sec){
      ESMC_TODConstructIS(resultTOD, this->sec - dec->sec);
      *resultDays = 0;
    }
    else{
      ESMC_TODConstructIS(resultTOD, (this->sec + ESMC_SID) - dec->sec);
      *resultDays = 1;
    }
  }
  return(ESMC_SUCCESS);
}



/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_TODIsLater"

int ESMC_TODIsLater(ESMC_TOD earlyTOD, ESMC_TOD lateTOD, ESMC_Bool *isLater)
{
#ifdef ESMC_DEBUG
  if(earlyTOD->type != lateTOD->type){
    ESMC_ERRA(ESMC_ERR_ARG_OUTOFRANGE, 0, "input times of day are different types");
  }
#endif

  switch(earlyTOD->type){
    case ESMC_TOD_INT_SEC:
      if(lateTOD->sec > earlyTOD->sec){
        *isLater=ESMC_TRUE;
      }
      else{
        *isLater=ESMC_FALSE;
      }
      break;
    default:
      ESMC_ERRA(ESMC_ERR_ARG_OUTOFRANGE, 0, 
	      "time of day type is not supported by this method");
  }

  return(ESMC_SUCCESS);
}



/*----------------------------------------------------------------------------*/
#undef __FUNC__
#define __FUNC__ "ESMC_TODPrint"

int ESMC_TODPrint(ESMC_TOD this)
{
  char str[32];

  switch(this->type){
    case ESMC_TOD_TYPE_UNDEFINED:
      strcpy(str, "ESMC_TOD_TYPE_UNDEFINED");
      break;
    case ESMC_TOD_INT_SEC:
      strcpy(str, "ESMC_TOD_INT_SEC");
      break;
    case ESMC_TOD_INT_MSEC:
      strcpy(str, "ESMC_TOD_INT_MSEC");
      break;
    default: 
      strcpy(str, "UNRECOGNIZED TOD TYPE");
  } 

  printf("Printing TOD:\n");
  printf("type     = %s\n", str);
  printf("seconds  = %d\n", this->sec);
  printf("mseconds = %d\n", this->msec);
    
  return(ESMC_SUCCESS);
}




















