/* $Id: ESMC_AlarmF.c,v 1.1 2002/11/15 21:29:25 jwolfe Exp $ */

/* Fortran interface file */

#include "ESMO.h"
#include "ESMO_Alarm.h"

#ifdef ESMC_HAVE_FORTRAN_UNDERSCORE
#define FORTRANUNDERSCORE
#endif

#ifdef MPI_BUILD_PROFILING
#ifdef FORTRANCAPS
#define esmc_alarmnewperiodic_ PESMC_ALARMNEWPERIODIC
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_alarmnewperiodic_ pesmc_alarmnewperiodic__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_alarmnewperiodic_ pesmc_alarmnewperiodic
#else

#define esmc_alarmnewperiodic_ pesmc_alarmnewperiodic_
#endif
#else
#ifdef FORTRANCAPS
#define esmc_alarmnewperiodic_ ESMC_ALARMNEWPERIODIC
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_alarmnewperiodic_ esmc_alarmnewperiodic__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_alarmnewperiodic_ esmc_alarmnewperiodic
#endif
#endif

#ifdef MPI_BUILD_PROFILING
#ifdef FORTRANCAPS
#define esmc_alarmnewmonthly_ PESMC_ALARMNEWMONTHLY
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_alarmnewmonthly_ pesmc_alarmnewmonthly__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_alarmnewmonthly_ pesmc_alarmnewmonthly
#else

#define esmc_alarmnewmonthly_ pesmc_alarmnewmonthly_
#endif
#else
#ifdef FORTRANCAPS
#define esmc_alarmnewmonthly_ ESMC_ALARMNEWMONTHLY
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_alarmnewmonthly_ esmc_alarmnewmonthly__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_alarmnewmonthly_ esmc_alarmnewmonthly
#endif
#endif

#ifdef MPI_BUILD_PROFILING
#ifdef FORTRANCAPS
#define esmc_alarmnewyearly_ PESMC_ALARMNEWYEARLY
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_alarmnewyearly_ pesmc_alarmnewyearly__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_alarmnewyearly_ pesmc_alarmnewyearly
#else

#define esmc_alarmnewyearly_ pesmc_alarmnewyearly_
#endif
#else
#ifdef FORTRANCAPS
#define esmc_alarmnewyearly_ ESMC_ALARMNEWYEARLY
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_alarmnewyearly_ esmc_alarmnewyearly__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_alarmnewyearly_ esmc_alarmnewyearly
#endif
#endif

#ifdef MPI_BUILD_PROFILING
#ifdef FORTRANCAPS
#define esmc_alarminitperiodic_ PESMC_ALARMINITPERIODIC
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_alarminitperiodic_ pesmc_alarminitperiodic__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_alarminitperiodic_ pesmc_alarminitperiodic
#else

#define esmc_alarminitperiodic_ pesmc_alarminitperiodic_
#endif
#else
#ifdef FORTRANCAPS
#define esmc_alarminitperiodic_ ESMC_ALARMINITPERIODIC
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_alarminitperiodic_ esmc_alarminitperiodic__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_alarminitperiodic_ esmc_alarminitperiodic
#endif
#endif

#ifdef MPI_BUILD_PROFILING
#ifdef FORTRANCAPS
#define esmc_alarminitmonthly_ PESMC_ALARMINITMONTHLY
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_alarminitmonthly_ pesmc_alarminitmonthly__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_alarminitmonthly_ pesmc_alarminitmonthly
#else

#define esmc_alarminitmonthly_ pesmc_alarminitmonthly_
#endif
#else
#ifdef FORTRANCAPS
#define esmc_alarminitmonthly_ ESMC_ALARMINITMONTHLY
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_alarminitmonthly_ esmc_alarminitmonthly__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_alarminitmonthly_ esmc_alarminitmonthly
#endif
#endif

#ifdef MPI_BUILD_PROFILING
#ifdef FORTRANCAPS
#define esmc_alarminityearly_ PESMC_ALARMINITYEARLY
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_alarminityearly_ pesmc_alarminityearly__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_alarminityearly_ pesmc_alarminityearly
#else

#define esmc_alarminityearly_ pesmc_alarminityearly_
#endif
#else
#ifdef FORTRANCAPS
#define esmc_alarminityearly_ ESMC_ALARMINITYEARLY
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_alarminityearly_ esmc_alarminityearly__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_alarminityearly_ esmc_alarminityearly
#endif
#endif

#ifdef MPI_BUILD_PROFILING
#ifdef FORTRANCAPS
#define esmc_alarmison_ PESMC_ALARMISON
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_alarmison_ pesmc_alarmison__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_alarmison_ pesmc_alarmison
#else

#define esmc_alarmison_ pesmc_alarmison_
#endif
#else
#ifdef FORTRANCAPS
#define esmc_alarmison_ ESMC_ALARMISON
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_alarmison_ esmc_alarmison__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_alarmison_ esmc_alarmison
#endif
#endif

#ifdef MPI_BUILD_PROFILING
#ifdef FORTRANCAPS
#define esmc_alarmgettype_ PESMC_ALARMGETTYPE
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_alarmgettype_ pesmc_alarmgettype__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_alarmgettype_ pesmc_alarmgettype
#else

#define esmc_alarmgettype_ pesmc_alarmgettype_
#endif
#else
#ifdef FORTRANCAPS
#define esmc_alarmgettype_ ESMC_ALARMGETTYPE
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_alarmgettype_ esmc_alarmgettype__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_alarmgettype_ esmc_alarmgettype
#endif
#endif

#ifdef MPI_BUILD_PROFILING
#ifdef FORTRANCAPS
#define esmc_alarmset_ PESMC_ALARMSET
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_alarmset_ pesmc_alarmset__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_alarmset_ pesmc_alarmset
#else

#define esmc_alarmset_ pesmc_alarmset_
#endif
#else
#ifdef FORTRANCAPS
#define esmc_alarmset_ ESMC_ALARMSET
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_alarmset_ esmc_alarmset__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_alarmset_ esmc_alarmset
#endif
#endif

/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif

void   esmc_alarminitperiodic_(ESMC_Alarm this, ESMC_Time period, ESMC_Time offset, int *rc)
{
  *rc = ESMC_AlarmConstructPeriodic(this, period, offset);
}



void  esmc_alarminitmonthly_(ESMC_Alarm this, int *rc)
{
  *rc = ESMC_AlarmConstructMonthly(this);
}



void  esmc_alarminityearly_(ESMC_Alarm this, int *rc)
{
  *rc = ESMC_AlarmConstructYearly(this);
}



void esmc_alarmison_(ESMC_Alarm this, ESMC_TimeMgr timeMgr, ESMC_Bool *alarmOn, int *rc)
{
  *rc = ESMC_AlarmIsOn(this, timeMgr, alarmOn);
}



void esmc_alarmset_(ESMC_Alarm this, ESMC_Bool *alarmOn, int *rc)
{
  *rc = ESMC_AlarmSet(this, *alarmOn);
}



void esmc_alarmgettype_(ESMC_Alarm this, ESMC_AlarmType *type, int *rc)
{
  *rc = ESMC_AlarmGetType(this, type);
}













