/* $Id: ESMO_Time.h,v 1.1 2002/11/15 21:26:46 jwolfe Exp $ */

#ifndef ESMC_TIME_H
#define ESMC_TIME_H

#include "ESMO_BasicUtil.h"

#include "ESMO_TOD.h"

/* 
!BOP
! !ROUTINE: Time Class
\begin{verbatim}
*/

struct TimeClass{
  int day;                   /* days in this time */
  ESMC_TODClass tod;         /* time of day */
};

/* 
\end{verbatim}
!EOP
*/

typedef struct TimeClass *ESMC_Time;

typedef struct TimeClass ESMC_TimeClass;

/*============================================================================*
 * Public methods
 *============================================================================*/

extern int ESMC_TimeNewIS(ESMC_Time *thisp, int days, int seconds);

extern int ESMC_TimeNewUndefined(ESMC_Time *thisp);

extern int ESMC_TimeConstruct(ESMC_Time this, int days, ESMC_TOD tod);

extern int ESMC_TimeConstructIS(ESMC_Time this, int days, int seconds);

extern int ESMC_TimeConstructUndefined(ESMC_Time this);

extern int ESMC_TimeCopy(ESMC_Time this, ESMC_Time orig);

extern int ESMC_TimeCopyConstruct(ESMC_Time this, ESMC_Time orig);

extern int ESMC_TimeSetIS(ESMC_Time this, int days, int seconds);

extern int ESMC_TimeGetIS(ESMC_Time this, int *days, int *seconds);

extern int ESMC_TimeGetDays(ESMC_Time this, double *days);

extern int ESMC_TimeIncrementIS(ESMC_Time this, ESMC_Time incTime, 
				   int days, int seconds);

extern int ESMC_TimeDecrementIS(ESMC_Time this, ESMC_Time decTime,
				   int days, int seconds);

extern int ESMC_TimeDiff(ESMC_Time earlyTime, ESMC_Time lateTime, ESMC_Time diff,
		        ESMC_Bool *isLater);

extern int ESMC_TimePrint(ESMC_Time this);

extern void ESMC_TimeDelete(ESMC_Time this);

#endif












