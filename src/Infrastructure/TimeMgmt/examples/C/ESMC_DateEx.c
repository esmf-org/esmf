/* $Id: ESMC_DateEx.c,v 1.1 2002/11/15 16:59:19 jwolfe Exp $ */
/*============================================================================*
 * ESMC_Date C Unit Tests and Examples
 *============================================================================*/

#undef __FUNC__
#define __FUNC__ "ESMC_DateEx"
#include <math.h>
#include "ESMC.h"
#include "ESMC_Date.h"
#include "ESMC_Calendar.h"
#include "ESMC_Time.h"

#define START_DATE        20011128
#define START_SECS        43200

#define STOP_DATE         20041201
#define STOP_SECS         1200

#define DAY_INC           6
#define SEC_INC           43200

int main(int argc, char **argv){

  int retCalDate, retDays, retSecs;
  ESMC_Bool isLater;
  ESMC_Date startDateG, stopDateG, retDateG;
  ESMC_Date startDateN, stopDateN, retDateN;
  ESMC_Time incTime, retTime;
  ESMC_App app;
  double floatDay, calcDay;
  int retDay;

  ESMC_AppNew(&app);
  printf("==================================================\n");
  printf("ESMC_Date C Unit Tests and Examples\n");
  printf("==================================================\n");

  ESMC_DateNewIS(&startDateG, ESMC_GREGORIAN, START_DATE, START_SECS);
  ESMC_DateNewIS(&stopDateG, ESMC_GREGORIAN, STOP_DATE, STOP_SECS);
  ESMC_DateNewUndefined(&retDateG);
  ESMC_DateNewIS(&startDateN, ESMC_NO_LEAP, START_DATE, START_SECS);
  ESMC_DateNewIS(&stopDateN, ESMC_NO_LEAP, STOP_DATE, STOP_SECS);
  ESMC_DateNewUndefined(&retDateN);
  ESMC_TimeNewIS(&incTime, DAY_INC, SEC_INC);  
  ESMC_TimeNewUndefined(&retTime);

  ESMC_DateGetIS(startDateG, &retCalDate, &retSecs);
  printf("Ret date = %d, Ret secs = %d\n", retCalDate, retSecs); 
  ESMC_ERROR_TEST(((retCalDate==START_DATE) && (retSecs==START_SECS)),
              "ESMC_DateNewIS, ESMC_DateGetIS: create Gregorian date and get attr");
  
  ESMC_DateIncrementSec(startDateG, retDateG, SEC_INC);
  ESMC_DateGetIS(retDateG, &retCalDate, &retSecs);
  printf("Ret date = %d, Ret secs = %d\n", retCalDate, retSecs); 
  ESMC_ERROR_TEST(((retCalDate==20011129) && (retSecs==0)),              
  "ESMC_DateIncrementSec: increment Gregorian date by seconds");

  ESMC_DateIncrementDay(startDateG, retDateG, DAY_INC);
  ESMC_DateGetIS(retDateG, &retCalDate, &retSecs);
  printf("Ret date = %d, Ret secs = %d\n", retCalDate, retSecs); 
  ESMC_ERROR_TEST(((retCalDate==20011204) && (retSecs==43200)),
              "ESMC_DateIncrementDay: increment Gregorian date by days");

  ESMC_DateIncrement(startDateG, retDateG, incTime);
  ESMC_DateGetIS(retDateG, &retCalDate, &retSecs);
  printf("Ret date = %d, Ret secs = %d\n", retCalDate, retSecs); 
  ESMC_ERROR_TEST(((retCalDate==20011205) && (retSecs==0)),
              "ESMC_DateIncrement: increment Gregorian date");

  ESMC_DateDecrement(retDateG, retDateG, incTime);
  ESMC_DateGetIS(retDateG, &retCalDate, &retSecs);
  printf("Ret date = %d, Ret secs = %d\n", retCalDate, retSecs); 
  ESMC_ERROR_TEST(((retCalDate==20011128) && (retSecs==43200)),
              "ESMC_DateDecrement: decrement Gregorian date in place");

  ESMC_DateDiff(startDateG, stopDateG, retTime, &isLater);
  ESMC_TimeGetIS(retTime, &retDays, &retSecs);
  printf("Ret days = %d, Ret secs = %d\n", retDays, retSecs); 
  ESMC_ERROR_TEST(((retDays==1098) && (retSecs==44400) && (isLater==ESMC_TRUE)),
              "ESMC_DateDiff: difference of two Gregorian dates over leap year");

  ESMC_TimeSetIS(incTime, 1098, 44400);

  ESMC_DateDecrement(stopDateG, retDateG, incTime);
  ESMC_DateGetIS(retDateG, &retCalDate, &retSecs);
  printf("Ret date = %d, Ret secs = %d\n", retCalDate, retSecs); 
  ESMC_ERROR_TEST(((retCalDate==20011128) && (retSecs==43200)),
              "ESMC_DateDecrement: decrement Gregorian date over leap year");


  ESMC_TimeSetIS(incTime, DAY_INC, SEC_INC);

  ESMC_DateIncrement(startDateN, retDateN, incTime);
  ESMC_DateGetIS(retDateN, &retCalDate, &retSecs);
  printf("Ret date = %d, Ret secs = %d\n", retCalDate, retSecs); 
  ESMC_ERROR_TEST(((retCalDate==20011205) && (retSecs==0)),
              "ESMC_DateIncrement: increment no leap date");

  ESMC_DateDecrement(retDateN, retDateN, incTime);
  ESMC_DateGetIS(retDateN, &retCalDate, &retSecs);
  printf("Ret date = %d, Ret secs = %d\n", retCalDate, retSecs); 
  ESMC_ERROR_TEST(((retCalDate==20011128) && (retSecs==43200)),
              "ESMC_DateDecrement: decrement no leap date in place");

  ESMC_DateSetIS(startDateN, ESMC_NO_LEAP, 19700101, 0);
  ESMC_DateSetIS(stopDateN, ESMC_NO_LEAP, 19710101, 0);

  ESMC_DateDiff(startDateN, stopDateN, retTime, &isLater);
  ESMC_TimeGetIS(retTime, &retDays, &retSecs);
  printf("Ret days = %d, Ret secs = %d\n", retDays, retSecs); 
  ESMC_ERROR_TEST(((retDays==365) && (retSecs==0) && (isLater==ESMC_TRUE)),
              "ESMC_DateDiff: difference of two no leap dates over leap year");

  ESMC_DateSetIS(startDateN, ESMC_NO_LEAP, 20011128, 43200);
  ESMC_DateSetIS(stopDateN, ESMC_NO_LEAP, 20041201, 1200);

  ESMC_DateDiff(startDateN, stopDateN, retTime, &isLater);
  ESMC_TimeGetIS(retTime, &retDays, &retSecs);
  printf("Ret days = %d, Ret secs = %d\n", retDays, retSecs); 
  ESMC_ERROR_TEST(((retDays==1097) && (retSecs==44400) && (isLater==ESMC_TRUE)),
              "ESMC_DateDiff: difference of two no leap dates over leap year");

  ESMC_TimeSetIS(incTime, 1097, 44400);

  ESMC_DateDecrement(stopDateN, retDateN, incTime);
  ESMC_DateGetIS(retDateG, &retCalDate, &retSecs);
  printf("Ret date = %d, Ret secs = %d\n", retCalDate, retSecs); 
  ESMC_ERROR_TEST(((retCalDate==20011128) && (retSecs==43200)),
              "ESMC_DateDecrement: decrement no leap date over leap year");  

  ESMC_DateSetIS(retDateN, ESMC_NO_LEAP, 20031224, 3500);
  ESMC_DateGetIS(retDateN, &retCalDate, &retSecs);
  printf("Ret date = %d, Ret secs = %d\n", retCalDate, retSecs); 
  ESMC_ERROR_TEST(((retCalDate==20031224) && (retSecs==3500)),
              "ESMC_DateSet: set a no leap date");  

  ESMC_DateIsLater(startDateG, stopDateG, &isLater);
  ESMC_ERROR_TEST(isLater == ESMC_TRUE,
              "ESMC_DateIsLater: compare Gregorian dates (true result)");  

  ESMC_DateIsLater(stopDateG, startDateG, &isLater);
  ESMC_ERROR_TEST(isLater == ESMC_FALSE,
              "ESMC_DateIsLater: compare Gregorian dates (false result)");  

  printf("\nTest Print Method\n");
  ESMC_DatePrint(startDateG);

  ESMC_DateGetFltDayOfYear(startDateG, &floatDay);
  ESMC_DateGetDayOfYear(startDateG, &retDay);
  ESMC_DateGetIS(startDateG, &retCalDate, &retSecs);
  calcDay = retDay + (double) (retSecs / 86400.0);
  printf("Calendar day is:%f\n", floatDay);
  printf("Calculated day:%f\n", calcDay);
  printf("Day:%d, sec:%d\n", retDay, retSecs);
  printf("Difference:%f\n", (calcDay - floatDay));
  ESMC_ERROR_TEST((fabs(calcDay - floatDay) < 0.00001),
		"ESMC_DateGetFltDayOfYear: return date as days.seconds");
  
  ESMC_DateDelete(startDateG);
  ESMC_DateDelete(stopDateG);
  ESMC_DateDelete(retDateG);
  ESMC_DateDelete(startDateN); 
  ESMC_DateDelete(stopDateN);
  ESMC_DateDelete(retDateN);
  ESMC_TimeDelete(incTime);
  ESMC_TimeDelete(retTime);

  ESMC_AppDelete(app);

  return(ESMC_SUCCESS);

}
