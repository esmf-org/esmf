/* $Id: ESMC_TimeEx.c,v 1.1 2002/11/15 16:59:46 jwolfe Exp $ */
/*============================================================================*
 * ESMC_Time C Unit Tests and Examples
 *============================================================================*/

#undef __FUNC__
#define __FUNC__ "ESMC_TimeEx"

#include "ESMC.h"
#include "ESMC_BasicUtil.h"
#include "ESMC_Time.h"

#define START_DAYS   5
#define START_SECS   43200

#define STOP_DAYS    12
#define STOP_SECS    1200

#define DAY_INC      2
#define SEC_INC      1000

#define SEC_OVER_SID 100000

int main(int argc, char **argv){

  ESMC_Bool isLater;
  int retDays, retSecs;
  double retRealDays;

  ESMC_Time startTime, stopTime, retTime;
  ESMC_App app;

  ESMC_AppNew(&app);
  printf("==================================================\n");
  printf("ESMC_Time C Unit Tests and Examples\n");
  printf("==================================================\n");

  ESMC_TimeNewIS(&startTime, START_DAYS, START_SECS);
  ESMC_TimeNewIS(&stopTime, STOP_DAYS, STOP_SECS);
  ESMC_TimeNewUndefined(&retTime);

  ESMC_TimeGetIS(startTime, &retDays, &retSecs);
  printf("Ret days %d, Ret secs %d\n", retDays, retSecs);
  ESMC_ERROR_TEST(((retDays == START_DAYS) && (retSecs == START_SECS)), 
              "ESMC_TimeNewIS, ESMC_TimeGetIS:  create time and get attributes");

  ESMC_TimeGetDays(startTime, &retRealDays);
  printf("Ret real days %f\n", retRealDays);
  ESMC_ERROR_TEST((ESMC_EQUAL(retRealDays, 5.5)),
              "ESMC_TimeGetDays:  get time value as real days"); 

  ESMC_TimeIncrementIS(startTime, retTime, DAY_INC, SEC_INC);
  ESMC_TimeGetIS(retTime, &retDays, &retSecs);
  printf("Ret days = %d, Ret secs = %d\n", retDays, retSecs);
  ESMC_ERROR_TEST(((retDays==7) && (retSecs==44200)),
              "ESMC_TimeIncrementIS:  increment time, sec inc < 86400");  

  ESMC_TimeDiff(startTime, stopTime, retTime, &isLater);
  ESMC_TimeGetIS(retTime, &retDays, &retSecs);  
  printf("Ret days = %d, Ret secs = %d\n", retDays, retSecs);
  ESMC_ERROR_TEST(((retDays == 6) && (retSecs == 44400) && (isLater == ESMC_TRUE)),
              "ESMC_TimeDiff:  take time difference, isLater is true");

  ESMC_TimeDiff(stopTime, startTime, retTime, &isLater);
  ESMC_TimeGetIS(retTime, &retDays, &retSecs);  
  printf("Ret days = %d, Ret secs = %d\n", retDays, retSecs);
  ESMC_ERROR_TEST(((retDays == 6) && (retSecs == 44400) && (isLater == ESMC_FALSE)),
              "ESMC_TimeDiff: take time difference, isLater is false");

  ESMC_TimeSetIS(retTime, DAY_INC, SEC_OVER_SID);
  ESMC_TimeGetIS(retTime, &retDays, &retSecs);  
  printf("Ret days = %d, Ret secs = %d\n", retDays, retSecs);
  ESMC_ERROR_TEST(((retDays==3) && (retSecs==13600)),
              "ESMC_TimeSetIS:  set time, sec > 86400");  

  ESMC_TimeDecrementIS(stopTime, retTime, DAY_INC, SEC_OVER_SID);
  ESMC_TimeGetIS(retTime, &retDays, &retSecs);  
  printf("Ret days = %d, Ret secs = %d\n", retDays, retSecs);
  ESMC_ERROR_TEST(((retDays==8) && (retSecs==74000)),
              "ESMC_TimeDecrementIS:  decrement time, sec > 86400");  

  ESMC_TimeSetIS(retTime, 0, 3600);
  ESMC_TimeDecrementIS(retTime, retTime, 0, 3600);
  ESMC_TimeGetIS(retTime, &retDays, &retSecs);  
  printf("Ret days = %d, Ret secs = %d\n", retDays, retSecs);
  ESMC_ERROR_TEST(((retDays==0) && (retSecs==0)),
              "ESMC_TimeDecrementIS:  decrement time down to 0");  
    
  printf("\nTest Print Method\n");
  ESMC_TimePrint(startTime);

  ESMC_TimeDelete(startTime);
  ESMC_TimeDelete(stopTime);
  ESMC_TimeDelete(retTime);

  ESMC_AppDelete(app);

  return(ESMC_SUCCESS);

}
