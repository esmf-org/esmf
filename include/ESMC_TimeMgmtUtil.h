/* Time Management General Utilities */

#include "ESMC_BasicUtil.h"

#ifndef ESMC_TIME_MGMT_UTIL_H
#define ESMC_TIME_MGMT_UTIL_H

#define ESMC_IS_LEAP_YEAR(year)                      ((year%400 == 0)||((year%4 == 0) && \
                                                   (year%100 != 0)))

#define ESMC_DATE_2_JULIAN_DAY(jday,year,month,day)  {int temp; \
                                                   temp = ((month)-14)/12; \
                                                   (jday)=(1461 * ((year) + 4800 + temp)) / 4 + \
                                                   (367 * ((month) - 2 - 12 * temp ))/12 - \
                                                   (3 * ( ((year) + 4900 + temp )/100))/4 + \
                                                   (day) - 32075;}
                                                         
#define ESMC_JULIAN_DAY_2_DATE(jday,year,month,day)  {int tempi, tempj, templ, tempn; \
                                                   templ = (jday) + 68569; \
                                                   tempn = ( 4 * templ ) / 146097; \
                                                   templ = templ - ( 146097 * tempn + 3 ) / 4; \
                                                   tempi = ( 4000 * ( templ + 1 ) ) / 1461001; \
                                                   templ = templ - ( 1461 * tempi ) / 4 + 31; \
                                                   tempj = ( 80 * templ ) / 2447; \
                                                   (day) = templ - ( 2447 * tempj ) / 80; \
                                                   templ = tempj / 11; \
                                                   (month) = tempj + 2 - ( 12 * templ ); \
                                                   (year) = 100 * ( tempn - 49 ) + tempi + templ;}  

/*------------------------------------------------------------------------------------------*
 * An INVALID date or time contains attributes with negative values or a value of 
 * seconds that is equal to or greater than 86400.  Attributes with the value
 * ESMC_TIME_UNDEFINED are valid.
 *
 * An UNDEFINED date or time contains at least one standard value for its time of day type 
 * that is equal to ESMC_TIME_UNDEFINED.  
 *
 *------------------------------------------------------------------------------------------*/

#define ESMC_TIME_INVALID_ATTR_IS(days,secs)  ((((days)<0) && \
                                            ((days)!=ESMC_TIME_UNDEFINED))|| \
                                            ((((secs)<0) || \
				            ((secs)>=86400)) && \
					    ((secs)!=ESMC_TIME_UNDEFINED))) 

#define ESMC_TIME_UNDEFINED_ATTR_IS(days,secs)(((days) == ESMC_TIME_UNDEFINED) || \
                                            ((secs) == ESMC_TIME_UNDEFINED))

#define ESMC_TIME_NEGATIVE_ATTR_IS(days,secs) ((((days)<0) && \
                                            ((days)!=ESMC_TIME_UNDEFINED))|| \
                                            ((((secs)<0) && \
					    ((secs)!=ESMC_TIME_UNDEFINED)))) 

#define ESMC_TIME_INVALID_IS(this)            ESMC_TIME_INVALID_ATTR_IS((this).day, \
                                            (this).tod.sec)

#define ESMC_TIME_UNDEFINED_IS(this)          ESMC_TIME_UNDEFINED_ATTR_IS((this).day, \
                                            (this).tod.sec)

#define ESMC_TIME_UNDEFINED -1

#endif
