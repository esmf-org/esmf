/* ESMC_Calendar.h */

#ifndef ESMC_CALENDAR_H
#define ESMC_CALENDAR_H

#include "ESMC_BasicUtil.h"

enum CalendarType{ ESMC_CALENDAR_TYPE_UNDEFINED,
		   ESMC_NO_LEAP,
		   ESMC_GREGORIAN,
		   ESMC_360_DAY };

typedef enum CalendarType ESMC_CalendarType;

struct CalendarClass{
  ESMC_CalendarType type;             /* calendar type */
  int dim[13];                      /* number of days in each month, indexed 1:12 */
  int dimRunningSum[13];            /* running sum of days in month, indexed 1:12 */
  int diy;                          /* number of days in current year */
};

typedef struct CalendarClass *ESMC_Calendar;

typedef struct CalendarClass ESMC_CalendarClass;

/*============================================================================*
 * Public methods
 *============================================================================*/

extern int ESMC_CalendarConstruct(ESMC_Calendar this, ESMC_CalendarType type, int year);

extern int ESMC_CalendarConstructUndefined(ESMC_Calendar this);

extern int ESMC_CalendarBuildDayOfYear(ESMC_Calendar this, int month, int day,
				     int *dayOfYear);

extern int ESMC_CalendarPrint(ESMC_Calendar this);

#endif



















