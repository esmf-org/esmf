/* $Id: ESMC_Date.h,v 1.2 2001/11/15 22:56:37 dneckels Exp $ */

#ifndef ESMC_DATE_H
#define ESMC_DATE_H
           
#include "ESMC_BasicUtil.h"

#include "ESMC_Constants.h"
#include "ESMC_Calendar.h"
#include "ESMC_Time.h"
#include "ESMC_TOD.h"

struct DateClass{
  ESMC_CalendarClass calendar;

  int year;                         /* date year */         
  int month;                        /* date month */
  int day;                          /* date day */
  ESMC_TODClass tod;                  /* date time of day */           

  int julianDay;                    /* absolute day for date calculations */
  int dayOfYear;                    /* day of year (e.g. Jan 1 is day 1) */
};

typedef struct DateClass *ESMC_Date;

typedef struct DateClass ESMC_DateClass;

/*============================================================================*
 * Public methods
 *============================================================================*/

extern int ESMC_DateNewIS(ESMC_Date *thisp, ESMC_CalendarType type, int yearmmdd, int seconds);

extern int ESMC_DateNewUndefined(ESMC_Date *thisp);

extern int ESMC_DateConstructIS(ESMC_Date this, ESMC_CalendarType type, 
				int yearmmdd, int seconds);

extern int ESMC_DateConstructUndefined(ESMC_Date this);

extern int ESMC_DateCopy(ESMC_Date this, ESMC_Date orig);

extern int ESMC_DateCopyConstruct(ESMC_Date this, ESMC_Date orig);

extern int ESMC_DateSetIS(ESMC_Date this, ESMC_CalendarType type, int yearmmdd, int seconds);

extern int ESMC_DateGetIS(ESMC_Date this, int *yearmmdd, int *seconds);

extern int ESMC_DateGetCalendarType(ESMC_Date this, ESMC_CalendarType *type);

extern int ESMC_DateGetDayOfYear(ESMC_Date, int *dayOfYear);

extern int ESMC_Date2Str(ESMC_Date this, char *str);

extern int ESMC_DateIncrement(ESMC_Date this, ESMC_Date incDate, ESMC_Time time);

extern int ESMC_DateDecrement(ESMC_Date this, ESMC_Date decDate, ESMC_Time time);

extern int ESMC_DateIncrementSec(ESMC_Date this, ESMC_Date incDate, int seconds);

extern int ESMC_DateIncrementDay(ESMC_Date this, ESMC_Date incDate, int days);

extern int ESMC_DateIncrementMon(ESMC_Date this, ESMC_Date incDate, int months);

extern int ESMC_DateIncrementYear(ESMC_Date this, ESMC_Date incDate, int years);

extern int ESMC_DateDecrement(ESMC_Date this, ESMC_Date decDate, ESMC_Time time);

extern int ESMC_DateDiff(ESMC_Date earlyDate, ESMC_Date laterDate, 
			ESMC_Time diff, ESMC_Bool *isLater);

extern int ESMC_DateDiffDays(ESMC_Date earlyDate, ESMC_Date lateDate, double *days);

extern int ESMC_DateIsLater(ESMC_Date earlyDate, ESMC_Date lateDate, ESMC_Bool *isLater);

extern int ESMC_DatePrint(ESMC_Date this);

extern int ESMC_DateGetFltDayOfYear(ESMC_Date this, double *day);

extern int ESMC_DateDelete(ESMC_Date this);

#endif
