/* $Id: ESMC_DateF.c,v 1.1 2002/11/15 21:30:48 jwolfe Exp $ */

/* Fortran interface file */

#include "ESMO.h"
#include "ESMO_Date.h"

#include "ESMO_Fortran.h"

#ifdef ESMC_HAVE_FORTRAN_UNDERSCORE
#define FORTRANUNDERSCORE
#endif

#ifdef MPI_BUILD_PROFILING
#ifdef FORTRANCAPS
#define esmc_dateinitis_ PESMC_DATEINITIS
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_dateinitis_ pesmc_dateinitis__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_dateinitis_ pesmc_dateinitis
#else

#define esmc_dateinitis_ pesmc_dateinitis_
#endif
#else
#ifdef FORTRANCAPS
#define esmc_dateinitis_ ESMC_DATEINITIS
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_dateinitis_ esmc_dateinitis__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_dateinitis_ esmc_dateinitis
#endif
#endif

#ifdef MPI_BUILD_PROFILING
#ifdef FORTRANCAPS
#define esmc_datecopyinit_ PESMC_DATECOPYINIT
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_datecopyinit_ pesmc_datecopyinit__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_datecopyinit_ pesmc_datecopyinit
#else

#define esmc_datecopyinit_ pesmc_datecopyinit_
#endif
#else
#ifdef FORTRANCAPS
#define esmc_datecopyinit_ ESMC_DATECOPYINIT
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_datecopyinit_ esmc_datecopyinit__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_datecopyinit_ esmc_datecopyinit
#endif
#endif

#ifdef MPI_BUILD_PROFILING
#ifdef FORTRANCAPS
#define esmc_dateinitundefined_ PESMC_DATEINITUNDEFINED
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_dateinitundefined_ pesmc_dateinitundefined__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_dateinitundefined_ pesmc_dateinitundefined
#else

#define esmc_dateinitundefined_ pesmc_dateinitundefined_
#endif
#else
#ifdef FORTRANCAPS
#define esmc_dateinitundefined_ ESMC_DATEINITUNDEFINED
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_dateinitundefined_ esmc_dateinitundefined__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_dateinitundefined_ esmc_dateinitundefined
#endif
#endif

#ifdef MPI_BUILD_PROFILING
#ifdef FORTRANCAPS
#define esmc_datecopy_ PESMC_DATECOPY
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_datecopy_ pesmc_datecopy__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_datecopy_ pesmc_datecopy
#else

#define esmc_datecopy_ pesmc_datecopy_
#endif
#else
#ifdef FORTRANCAPS
#define esmc_datecopy_ ESMC_DATECOPY
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_datecopy_ esmc_datecopy__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_datecopy_ esmc_datecopy
#endif
#endif

#ifdef MPI_BUILD_PROFILING
#ifdef FORTRANCAPS
#define esmc_datesetis_ PESMC_DATESETIS
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_datesetis_ pesmc_datesetis__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_datesetis_ pesmc_datesetis
#else

#define esmc_datesetis_ pesmc_datesetis_
#endif
#else
#ifdef FORTRANCAPS
#define esmc_datesetis_ ESMC_DATESETIS
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_datesetis_ esmc_datesetis__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_datesetis_ esmc_datesetis
#endif
#endif

#ifdef MPI_BUILD_PROFILING
#ifdef FORTRANCAPS
#define esmc_dategetis_ PESMC_DATEGETIS
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_dategetis_ pesmc_dategetis__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_dategetis_ pesmc_dategetis
#else

#define esmc_dategetis_ pesmc_dategetis_
#endif
#else
#ifdef FORTRANCAPS
#define esmc_dategetis_ ESMC_DATEGETIS
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_dategetis_ esmc_dategetis__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_dategetis_ esmc_dategetis
#endif
#endif

#ifdef MPI_BUILD_PROFILING
#ifdef FORTRANCAPS
#define esmc_dategetdayofyear_ PESMC_DATEGETDAYOFYEAR
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_dategetdayofyear_ pesmc_dategetdayofyear__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_dategetdayofyear_ pesmc_dategetdayofyear
#else

#define esmc_dategetdayofyear_ pesmc_dategetdayofyear_
#endif
#else
#ifdef FORTRANCAPS
#define esmc_dategetdayofyear_ ESMC_DATEGETDAYOFYEAR
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_dategetdayofyear_ esmc_dategetdayofyear__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_dategetdayofyear_ esmc_dategetdayofyear
#endif
#endif

#ifdef MPI_BUILD_PROFILING
#ifdef FORTRANCAPS
#define esmc_dategetcalendartype_ PESMC_DATEGETCALENDARTYPE
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_dategetcalendartype_ pesmc_dategetcalendartype__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_dategetcalendartype_ pesmc_dategetcalendartype
#else

#define esmc_dategetcalendartype_ pesmc_dategetcalendartype_
#endif
#else
#ifdef FORTRANCAPS
#define esmc_dategetcalendartype_ ESMC_DATEGETCALENDARTYPE
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_dategetcalendartype_ esmc_dategetcalendartype__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_dategetcalendartype_ esmc_dategetcalendartype
#endif
#endif

#ifdef MPI_BUILD_PROFILING
#ifdef FORTRANCAPS
#define esmc_date2str_ PESMC_DATE2STR
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_date2str_ pesmc_date2str__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_date2str_ pesmc_date2str
#else

#define esmc_date2str_ pesmc_date2str_
#endif
#else
#ifdef FORTRANCAPS
#define esmc_date2str_ ESMC_DATE2STR
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_date2str_ esmc_date2str__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_date2str_ esmc_date2str
#endif
#endif

#ifdef MPI_BUILD_PROFILING
#ifdef FORTRANCAPS
#define esmc_dateincrement_ PESMC_DATEINCREMENT
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_dateincrement_ pesmc_dateincrement__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_dateincrement_ pesmc_dateincrement
#else

#define esmc_dateincrement_ pesmc_dateincrement_
#endif
#else
#ifdef FORTRANCAPS
#define esmc_dateincrement_ ESMC_DATEINCREMENT
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_dateincrement_ esmc_dateincrement__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_dateincrement_ esmc_dateincrement
#endif
#endif

#ifdef MPI_BUILD_PROFILING
#ifdef FORTRANCAPS
#define esmc_dateincrementsec_ PESMC_DATEINCREMENTSEC
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_dateincrementsec_ pesmc_dateincrementsec__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_dateincrementsec_ pesmc_dateincrementsec
#else

#define esmc_dateincrementsec_ pesmc_dateincrementsec_
#endif
#else
#ifdef FORTRANCAPS
#define esmc_dateincrementsec_ ESMC_DATEINCREMENTSEC
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_dateincrementsec_ esmc_dateincrementsec__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_dateincrementsec_ esmc_dateincrementsec
#endif
#endif

#ifdef MPI_BUILD_PROFILING
#ifdef FORTRANCAPS
#define esmc_dateincrementday_ PESMC_DATEINCREMENTDAY
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_dateincrementday_ pesmc_dateincrementday__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_dateincrementday_ pesmc_dateincrementday
#else

#define esmc_dateincrementday_ pesmc_dateincrementday_
#endif
#else
#ifdef FORTRANCAPS
#define esmc_dateincrementday_ ESMC_DATEINCREMENTDAY
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_dateincrementday_ esmc_dateincrementday__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_dateincrementday_ esmc_dateincrementday
#endif
#endif

#ifdef MPI_BUILD_PROFILING
#ifdef FORTRANCAPS
#define esmc_dateincrementmonth_ PESMC_DATEINCREMENTMONTH
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_dateincrementmonth_ pesmc_dateincrementmonth__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_dateincrementmonth_ pesmc_dateincrementmonth
#else

#define esmc_dateincrementmonth_ pesmc_dateincrementmonth_
#endif
#else
#ifdef FORTRANCAPS
#define esmc_dateincrementmonth_ ESMC_DATEINCREMENTMONTH
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_dateincrementmonth_ esmc_dateincrementmonth__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_dateincrementmonth_ esmc_dateincrementmonth
#endif
#endif

#ifdef MPI_BUILD_PROFILING
#ifdef FORTRANCAPS
#define esmc_dateincrementyear_ PESMC_DATEINCREMENTYEAR
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_dateincrementyear_ pesmc_dateincrementyear__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_dateincrementyear_ pesmc_dateincrementyear
#else

#define esmc_dateincrementyear_ pesmc_dateincrementyear_
#endif
#else
#ifdef FORTRANCAPS
#define esmc_dateincrementyear_ ESMC_DATEINCREMENTYEAR
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_dateincrementyear_ esmc_dateincrementyear__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_dateincrementyear_ esmc_dateincrementyear
#endif
#endif

#ifdef MPI_BUILD_PROFILING
#ifdef FORTRANCAPS
#define esmc_datediff_ PESMC_DATEDIFF
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_datediff_ pesmc_datediff__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_datediff_ pesmc_datediff
#else

#define esmc_datediff_ pesmc_datediff_
#endif
#else
#ifdef FORTRANCAPS
#define esmc_datediff_ ESMC_DATEDIFF
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_datediff_ esmc_datediff__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_datediff_ esmc_datediff
#endif
#endif

#ifdef MPI_BUILD_PROFILING
#ifdef FORTRANCAPS
#define esmc_dateislater_ PESMC_DATEISLATER
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_dateislater_ pesmc_dateislater__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_dateislater_ pesmc_dateislater
#else

#define esmc_dateislater_ pesmc_dateislater_
#endif
#else
#ifdef FORTRANCAPS
#define esmc_dateislater_ ESMC_DATEISLATER
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_dateislater_ esmc_dateislater__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_dateislater_ esmc_dateislater
#endif
#endif

#ifdef MPI_BUILD_PROFILING
#ifdef FORTRANCAPS
#define esmc_datedecrement_ PESMC_DATEDECREMENT
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_datedecrement_ pesmc_datedecrement__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_datedecrement_ pesmc_datedecrement
#else

#define esmc_datedecrement_ pesmc_datedecrement_
#endif
#else
#ifdef FORTRANCAPS
#define esmc_datedecrement_ ESMC_DATEDECREMENT
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_datedecrement_ esmc_datedecrement__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_datedecrement_ esmc_datedecrement
#endif
#endif

#ifdef MPI_BUILD_PROFILING
#ifdef FORTRANCAPS
#define esmc_dateprint_ PESMC_DATEPRINT
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_dateprint_ pesmc_dateprint__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_dateprint_ pesmc_dateprint
#else

#define esmc_dateprint_ pesmc_dateprint_
#endif
#else
#ifdef FORTRANCAPS
#define esmc_dateprint_ ESMC_DATEPRINT
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_dateprint_ esmc_dateprint__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_dateprint_ esmc_dateprint
#endif
#endif

#ifdef MPI_BUILD_PROFILING
#ifdef FORTRANCAPS
#define esmc_dategetfltdayofyear_ PESMC_DATEGETFLTDAYOFYEAR
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_dateprint_ pesmc_dategetfltdayofyear__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_dategetfltdayofyear_ pesmc_dategetfltdayofyear
#else

#define esmc_dategetfltdayofyear_ pesmc_dategetfltdayofyear_
#endif
#else
#ifdef FORTRANCAPS
#define esmc_dategetfltdayofyear_ ESMC_DATEGETFLTDAYOFYEAR
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_dategetfltdayofyear_ esmc_dategetfltdayofyear__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_dategetfltdayofyear_ esmc_dategetfltdayofyear
#endif
#endif


/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif

void   esmc_dateinitis_(ESMC_Date this, ESMC_CalendarType *type,
		          int *yearmmdd, int *tod, int *rc)
{
  *rc = ESMC_DateConstructIS(this, *type, *yearmmdd, *tod);
}



void   esmc_datecopyinit_(ESMC_Date this, ESMC_Date orig, int *rc)
{
  *rc = ESMC_DateCopyConstruct(this, orig);
}



void   esmc_dateinitundefined_(ESMC_Date this, int *rc)
{
  *rc = ESMC_DateConstructUndefined(this);
}



void  esmc_datecopy_(ESMC_Date this, ESMC_Date orig, int *rc)
{
  *rc = ESMC_DateCopy(this, orig);
}



void  esmc_datesetis_(ESMC_Date this, ESMC_CalendarType *type, int *yearmmdd, int *tod, int *rc)
{
  *rc = ESMC_DateSetIS(this, *type, *yearmmdd, *tod);
}



void esmc_dategetis_(ESMC_Date this, int *yearmmdd, int *tod, int *rc)
{
  *rc = ESMC_DateGetIS(this, yearmmdd, tod);
}



void esmc_dategetdayofyear_(ESMC_Date this, int *dayOfYear, int *rc)
{
  *rc = ESMC_DateGetDayOfYear(this, dayOfYear);
}



void esmc_dategetcalendartype_(ESMC_Date this, ESMC_CalendarType *calendarType, int *rc)
{
  *rc = ESMC_DateGetCalendarType(this, calendarType);
}



void esmc_date2str_(ESMC_Date this, char *str ESMC_MIXED_LEN(len), int *rc ESMC_END_LEN(len))
{
  char *t;

  ESMC_FIXCHAR(str, len, t);
  *rc = ESMC_Date2Str(this, t);
  ESMC_FREECHAR(str, t);
}



void esmc_dateincrement_(ESMC_Date this, ESMC_Date incDate, ESMC_Time time, int *rc)
{
  *rc = ESMC_DateIncrement(this, incDate, time);
}



void esmc_dateincrementsec_(ESMC_Date this, ESMC_Date incDate, int *nseconds, int *rc)
{
  *rc = ESMC_DateIncrementSec(this, incDate, *nseconds);
}



void esmc_dateincrementday_(ESMC_Date this, ESMC_Date incDate, int *ndays, int *rc)
{
  *rc = ESMC_DateIncrementDay(this, incDate, *ndays);
}



void esmc_dateincrementmonth_(ESMC_Date this, ESMC_Date incDate, int *nmonths, int *rc)
{
  *rc = ESMC_DateIncrementMonth(this, incDate, *nmonths);
}



void esmc_dateincrementyear_(ESMC_Date this, ESMC_Date incDate, int *nyears, int *rc)
{
  *rc = ESMC_DateIncrementYear(this, incDate, *nyears);
}



void esmc_datediff_(ESMC_Date earlyDate, ESMC_Date lateDate, ESMC_Time diff,
		  ESMC_Bool *isLater, int *rc)
{
  *rc = ESMC_DateDiff(earlyDate, lateDate, diff, isLater);
}


void esmc_dateislater_(ESMC_Date earlyDate, ESMC_Date lateDate, ESMC_Bool *isLater, int *rc)
{
  *rc = ESMC_DateIsLater(earlyDate, lateDate, isLater);
}



void esmc_datedecrement_(ESMC_Date this, ESMC_Date decDate, ESMC_Time time, int *rc)
{
  *rc = ESMC_DateDecrement(this, decDate, time);
}



void esmc_dateprint_(ESMC_Date this, int *rc)
{
  *rc = ESMC_DatePrint(this);
}



void esmc_dategetfltdayofyear_(ESMC_Date this, double *day, int *rc)
{
  *rc = ESMC_DateGetFltDayOfYear(this, day);
}



#if defined(__cplusplus)
}
#endif
