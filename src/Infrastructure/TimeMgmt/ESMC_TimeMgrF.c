/* $Id: ESMC_TimeMgrF.c,v 1.1 2002/11/14 23:40:58 jwolfe Exp $ */

/* Fortran interface file */

#include "ESMC.h"
#include "ESMC_TimeMgr.h"

#ifdef ESMC_HAVE_FORTRAN_UNDERSCORE
#define FORTRANUNDERSCORE
#endif

#ifdef POINTER_64_BITS
#if defined(__cplusplus)
extern "C" { 
#endif 
extern void *ESMC_ToPointer();
extern int ESMC_FromPointer();
extern void ESMC_RmPointer();
#if defined(__cplusplus)
} 
#endif 

#else

#define ESMC_ToPointer(a) ((long*)(a))
#define ESMC_FromPointer(a) (long)(a)
#define ESMC_RmPointer(a)
#endif

#ifdef MPI_BUILD_PROFILING
#ifdef FORTRANCAPS
#define esmc_timemgrinitis_ PESMC_TIMEMGRINITIS
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_timemgrinitis_ pesmc_timemgrinitis__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_timemgrinitis_ pesmc_timemgrinitis
#else

#define esmc_timemgrinitis_ pesmc_timemgrinitis_
#endif
#else
#ifdef FORTRANCAPS
#define esmc_timemgrinitis_ ESMC_TIMEMGRINITIS
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_timemgrinitis_ esmc_timemgrinitis__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_timemgrinitis_ esmc_timemgrinitis
#endif
#endif

#ifdef MPI_BUILD_PROFILING
#ifdef FORTRANCAPS
#define esmc_timemgrinitnobaseis_ PESMC_TIMEMGRINITNOBASEIS
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_timemgrinitnobaseis_ pesmc_timemgrinitnobaseis__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_timemgrinitnobaseis_ pesmc_timemgrinitnobaseis
#else

#define esmc_timemgrinitnobaseis_ pesmc_timemgrinitnobaseis_
#endif
#else
#ifdef FORTRANCAPS
#define esmc_timemgrinitnobaseis_ ESMC_TIMEMGRINITNOBASEIS
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_timemgrinitnobaseis_ esmc_timemgrinitnobaseis__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_timemgrinitnobaseis_ esmc_timemgrinitnobaseis
#endif
#endif

#ifdef MPI_BUILD_PROFILING
#ifdef FORTRANCAPS
#define esmc_timemgrinit_ PESMC_TIMEMGRINIT
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_timemgrinit_ pesmc_timemgrinit__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_timemgrinit_ pesmc_timemgrinit
#else

#define esmc_timemgrinit_ pesmc_timemgrinit_
#endif
#else
#ifdef FORTRANCAPS
#define esmc_timemgrinit_ ESMC_TIMEMGRINIT
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_timemgrinit_ esmc_timemgrinit__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_timemgrinit_ esmc_timemgrinit
#endif
#endif

#ifdef MPI_BUILD_PROFILING
#ifdef FORTRANCAPS
#define esmc_timemgrinitnobase_ PESMC_TIMEMGRINITNOBASE
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_timemgrinitnobase_ pesmc_timemgrinitnobase__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_timemgrinitnobase_ pesmc_timemgrinitnobase
#else

#define esmc_timemgrinitnobase_ pesmc_timemgrinitnobase_
#endif
#else
#ifdef FORTRANCAPS
#define esmc_timemgrinitnobase_ ESMC_TIMEMGRINITNOBASE
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_timemgrinitnobase_ esmc_timemgrinitnobase__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_timemgrinitnobase_ esmc_timemgrinitnobase
#endif
#endif

#ifdef MPI_BUILD_PROFILING
#ifdef FORTRANCAPS
#define esmc_timemgradvance_ PESMC_TIMEMGRADVANCE
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_timemgradvance_ pesmc_timemgradvance__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_timemgradvance_ pesmc_timemgradvance
#else

#define esmc_timemgradvance_ pesmc_timemgradvance_
#endif
#else
#ifdef FORTRANCAPS
#define esmc_timemgradvance_ ESMC_TIMEMGRADVANCE
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_timemgradvance_ esmc_timemgradvance__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_timemgradvance_ esmc_timemgradvance
#endif
#endif

#ifdef MPI_BUILD_PROFILING
#ifdef FORTRANCAPS
#define esmc_timemgrlaststep_ PESMC_TIMEMGRLASTSTEP
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_timemgrlaststep_ pesmc_timemgrlaststep__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_timemgrlaststep_ pesmc_timemgrlaststep
#else

#define esmc_timemgrlaststep_ pesmc_timemgrlaststep_
#endif
#else
#ifdef FORTRANCAPS
#define esmc_timemgrlaststep_ ESMC_TIMEMGRLASTSTEP
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_timemgrlaststep_ esmc_timemgrlaststep__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_timemgrlaststep_ esmc_timemgrlaststep
#endif
#endif

#ifdef MPI_BUILD_PROFILING
#ifdef FORTRANCAPS
#define esmc_timemgrgetstepsizestd_ PESMC_TIMEMGRGETSTEPSIZESTD
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_timemgrgetstepsizestd_ pesmc_timemgrgetstepsizestd__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_timemgrgetstepsizestd_ pesmc_timemgrgetstepsizestd
#else

#define esmc_timemgrgetstepsizestd_ pesmc_timemgrgetstepsizestd_
#endif
#else
#ifdef FORTRANCAPS
#define esmc_timemgrgetstepsizestd_ ESMC_TIMEMGRGETSTEPSIZESTD
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_timemgrgetstepsizestd_ esmc_timemgrgetstepsizestd__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_timemgrgetstepsizestd_ esmc_timemgrgetstepsizestd
#endif
#endif

#ifdef MPI_BUILD_PROFILING
#ifdef FORTRANCAPS
#define esmc_timemgrgetstepsizeis_ PESMC_TIMEMGRGETSTEPSIZEIS
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_timemgrgetstepsizeis_ pesmc_timemgrgetstepsizeis__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_timemgrgetstepsizeis_ pesmc_timemgrgetstepsizeis
#else

#define esmc_timemgrgetstepsizeis_ pesmc_timemgrgetstepsizeis_
#endif
#else
#ifdef FORTRANCAPS
#define esmc_timemgrgetstepsizeis_ ESMC_TIMEMGRGETSTEPSIZEIS
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_timemgrgetstepsizeis_ esmc_timemgrgetstepsizeis__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_timemgrgetstepsizeis_ esmc_timemgrgetstepsizeis
#endif
#endif

#ifdef MPI_BUILD_PROFILING
#ifdef FORTRANCAPS
#define esmc_timemgrgetnstep_ PESMC_TIMEMGRGETNSTEP
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_timemgrgetnstep_ pesmc_timemgrgetnstep__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_timemgrgetnstep_ pesmc_timemgrgetnstep
#else

#define esmc_timemgrgetnstep_ pesmc_timemgrgetnstep_
#endif
#else
#ifdef FORTRANCAPS
#define esmc_timemgrgetnstep_ ESMC_TIMEMGRGETNSTEP
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_timemgrgetnstep_ esmc_timemgrgetnstep__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_timemgrgetnstep_ esmc_timemgrgetnstep
#endif
#endif

#ifdef MPI_BUILD_PROFILING
#ifdef FORTRANCAPS
#define esmc_timemgrsetnstep_ PESMC_TIMEMGRSETNSTEP
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_timemgrsetnstep_ pesmc_timemgrsetnstep__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_timemgrsetnstep_ pesmc_timemgrsetnstep
#else

#define esmc_timemgrsetnstep_ pesmc_timemgrsetnstep_
#endif
#else
#ifdef FORTRANCAPS
#define esmc_timemgrsetnstep_ ESMC_TIMEMGRSETNSTEP
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_timemgrsetnstep_ esmc_timemgrsetnstep__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_timemgrsetnstep_ esmc_timemgrsetnstep
#endif
#endif


#ifdef MPI_BUILD_PROFILING
#ifdef FORTRANCAPS
#define esmc_timemgrsetstepsizeis_ PESMC_TIMEMGRSETSTEPSIZEIS
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_timemgrsetstepsizeis_ pesmc_timemgrsetstepsizeis__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_timemgrsetstepsizeis_ pesmc_timemgrsetstepsizeis
#else

#define esmc_timemgrsetstepsizeis_ pesmc_timemgrsetstepsizeis_
#endif
#else
#ifdef FORTRANCAPS
#define esmc_timemgrsetstepsizeis_ ESMC_TIMEMGRSETSTEPSIZEIS
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_timemgrsetstepsizeis_ esmc_timemgrsetstepsizeis__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_timemgrsetstepsizeis_ esmc_timemgrsetstepsizeis
#endif
#endif

#ifdef MPI_BUILD_PROFILING
#ifdef FORTRANCAPS
#define esmc_timemgrsetstepsizestd_ PESMC_TIMEMGRSETSTEPSIZESTD
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_timemgrsetstepsizestd_ pesmc_timemgrsetstepsizestd__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_timemgrsetstepsizestd_ pesmc_timemgrsetstepsizestd
#else

#define esmc_timemgrsetstepsizestd_ pesmc_timemgrsetstepsizestd_
#endif
#else
#ifdef FORTRANCAPS
#define esmc_timemgrsetstepsizestd_ ESMC_TIMEMGRSETSTEPSIZESTD
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_timemgrsetstepsizestd_ esmc_timemgrsetstepsizestd__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_timemgrsetstepsizestd_ esmc_timemgrsetstepsizestd
#endif
#endif

#ifdef MPI_BUILD_PROFILING
#ifdef FORTRANCAPS
#define esmc_timemgrgetstartdate_ PESMC_TIMEMGRGETSTARTDATE
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_timemgrgetstartdate_ pesmc_timemgrgetstartdate__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_timemgrgetstartdate_ pesmc_timemgrgetstartdate
#else

#define esmc_timemgrgetstartdate_ pesmc_timemgrgetstartdate_
#endif
#else
#ifdef FORTRANCAPS
#define esmc_timemgrgetstartdate_ ESMC_TIMEMGRGETSTARTDATE
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_timemgrgetstartdate_ esmc_timemgrgetstartdate__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_timemgrgetstartdate_ esmc_timemgrgetstartdate
#endif
#endif

#ifdef MPI_BUILD_PROFILING
#ifdef FORTRANCAPS
#define esmc_timemgrgetstopdate_ PESMC_TIMEMGRGETSTOPDATE
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_timemgrgetstopdate_ pesmc_timemgrgetstopdate__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_timemgrgetstopdate_ pesmc_timemgrgetstopdate
#else

#define esmc_timemgrgetstopdate_ pesmc_timemgrgetstopdate_
#endif
#else
#ifdef FORTRANCAPS
#define esmc_timemgrgetstopdate_ ESMC_TIMEMGRGETSTOPDATE
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_timemgrgetstopdate_ esmc_timemgrgetstopdate__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_timemgrgetstopdate_ esmc_timemgrgetstopdate
#endif
#endif

#ifdef MPI_BUILD_PROFILING
#ifdef FORTRANCAPS
#define esmc_timemgrgetbasedate_ PESMC_TIMEMGRGETBASEDATE
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_timemgrgetbasedate_ pesmc_timemgrgetbasedate__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_timemgrgetbasedate_ pesmc_timemgrgetbasedate
#else

#define esmc_timemgrgetbasedate_ pesmc_timemgrgetbasedate_
#endif
#else
#ifdef FORTRANCAPS
#define esmc_timemgrgetbasedate_ ESMC_TIMEMGRGETBASEDATE
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_timemgrgetbasedate_ esmc_timemgrgetbasedate__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_timemgrgetbasedate_ esmc_timemgrgetbasedate
#endif
#endif

#ifdef MPI_BUILD_PROFILING
#ifdef FORTRANCAPS
#define esmc_timemgrgetcurrdate_ PESMC_TIMEMGRGETCURRDATE
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_timemgrgetcurrdate_ pesmc_timemgrgetcurrdate__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_timemgrgetcurrdate_ pesmc_timemgrgetcurrdate
#else

#define esmc_timemgrgetcurrdate_ pesmc_timemgrgetcurrdate_
#endif
#else
#ifdef FORTRANCAPS
#define esmc_timemgrgetcurrdate_ ESMC_TIMEMGRGETCURRDATE
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_timemgrgetcurrdate_ esmc_timemgrgetcurrdate__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_timemgrgetcurrdate_ esmc_timemgrgetcurrdate
#endif
#endif


#ifdef MPI_BUILD_PROFILING
#ifdef FORTRANCAPS
#define esmc_timemgrsetcurrdateis_ PESMC_TIMEMGRSETCURRDATEIS
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_timemgrsetcurrdateis_ pesmc_timemgrsetcurrdateis__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_timemgrsetcurrdateis_ pesmc_timemgrsetcurrdateis
#else

#define esmc_timemgrsetcurrdateis_ pesmc_timemgrsetcurrdateis_
#endif
#else
#ifdef FORTRANCAPS
#define esmc_timemgrsetcurrdateis_ ESMC_TIMEMGRSETCURRDATEIS
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_timemgrsetcurrdateis_ esmc_timemgrsetcurrdateis__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_timemgrsetcurrdateis_ esmc_timemgrsetcurrdateis
#endif
#endif


#ifdef MPI_BUILD_PROFILING
#ifdef FORTRANCAPS
#define esmc_timemgrgetprevdate_ PESMC_TIMEMGRGETPREVDATE
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_timemgrgetprevdate_ pesmc_timemgrgetprevdate__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_timemgrgetprevdate_ pesmc_timemgrgetprevdate
#else

#define esmc_timemgrgetprevdate_ pesmc_timemgrgetprevdate_
#endif
#else
#ifdef FORTRANCAPS
#define esmc_timemgrgetprevdate_ ESMC_TIMEMGRGETPREVDATE
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_timemgrgetprevdate_ esmc_timemgrgetprevdate__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_timemgrgetprevdate_ esmc_timemgrgetprevdate
#endif
#endif

#ifdef MPI_BUILD_PROFILING
#ifdef FORTRANCAPS
#define esmc_timemgrrestartwriteis_ PESMC_TIMEMGRRESTARTWRITEIS
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_timemgrrestartwriteis_ pesmc_timemgrrestartwriteis__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_timemgrrestartwriteis_ pesmc_timemgrrestartwriteis
#else

#define esmc_timemgrrestartwriteis_ pesmc_timemgrrestartwriteis_
#endif
#else
#ifdef FORTRANCAPS
#define esmc_timemgrrestartwriteis_ ESMC_TIMEMGRRESTARTWRITEIS
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_timemgrrestartwriteis_ esmc_timemgrrestartwriteis__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_timemgrrestartwriteis_ esmc_timemgrrestartwriteis
#endif
#endif

#ifdef MPI_BUILD_PROFILING
#ifdef FORTRANCAPS
#define esmc_timemgrrestartreadis_ PESMC_TIMEMGRRESTARTREADIS
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_timemgrrestartreadis_ pesmc_timemgrrestartreadis__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_timemgrrestartreadis_ pesmc_timemgrrestartreadis
#else

#define esmc_timemgrrestartreadis_ pesmc_timemgrrestartreadis_
#endif
#else
#ifdef FORTRANCAPS
#define esmc_timemgrrestartreadis_ ESMC_TIMEMGRRESTARTREADIS
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_timemgrrestartreadis_ esmc_timemgrrestartreadis__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_timemgrrestartreadis_ esmc_timemgrrestartreadis
#endif
#endif

#ifdef MPI_BUILD_PROFILING
#ifdef FORTRANCAPS
#define esmc_timemgrdelete_ PESMC_TIMEMGRDELETE
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_timemgrdelete_ pesmc_timemgrdelete__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_timemgrdelete_ pesmc_timemgrdelete
#else

#define esmc_timemgrdelete_ pesmc_timemgrdelete_
#endif
#else
#ifdef FORTRANCAPS
#define esmc_timemgrdelete_ ESMC_TIMEMGRDELETE
#elif defined(FORTRANDOUBLEUNDERSCORE)
#define esmc_timemgrdelete_ esmc_timemgrdelete__
#elif !defined(FORTRANUNDERSCORE)
#define esmc_timemgrdelete_ esmc_timemgrdelete
#endif
#endif

/* Definitions of Fortran Wrapper routines */
#if defined(__cplusplus)
extern "C" {
#endif


void   esmc_timemgrinit_(ESMC_TimeMgr this, ESMC_Time stepSize,
		       ESMC_Date startDate, ESMC_Date stopDate,
		       ESMC_Date baseDate, int *rc)
{
  *rc = ESMC_TimeMgrConstruct(this, stepSize, startDate, stopDate, baseDate);
}



void  esmc_timemgrinitis_(ESMC_TimeMgr this, 
			int *stepDays, int *stepSecs,
			int *startCalendarDate, int *startTOD,
			int *stopCalendarDate, int *stopTOD,
			int *baseCalendarDate, int *baseTOD,
			ESMC_CalendarType *type, int *rc)
{
  *rc = ESMC_TimeMgrConstructIS(this, *stepDays, *stepSecs, *startCalendarDate, 
			*startTOD, *stopCalendarDate, *stopTOD,
			*baseCalendarDate, *baseTOD, *type);
}



void  esmc_timemgrinitnobase_(ESMC_TimeMgr this, ESMC_Time stepSize,
			    ESMC_Date startDate, ESMC_Date stopDate, int *rc)
{
  *rc = ESMC_TimeMgrConstructNoBase(this, stepSize, startDate, stopDate);
}



void  esmc_timemgrinitnobaseis_(ESMC_TimeMgr this, 
			      int *stepDays, int *stepSecs,
			      int *startCalendarDate, int *startTOD,
			      int *stopCalendarDate, int *stopTOD,
			      int *baseCalendarDate, int *baseTOD,
			      ESMC_CalendarType *type, int *rc)
{
  *rc = ESMC_TimeMgrConstructNoBaseIS(this, *stepDays, *stepSecs, *startCalendarDate, 
			      *startTOD, *stopCalendarDate, *stopTOD, *type);
}



void  esmc_timemgradvance_(ESMC_TimeMgr this, int *rc)
{
  *rc = ESMC_TimeMgrAdvance(this);
}



void esmc_timemgrlaststep_(ESMC_TimeMgr this, ESMC_Bool *lastStep, int *rc)
{
  *rc = ESMC_TimeMgrLastStep(this, lastStep);
}



void esmc_timemgrsetstepsizestd_(ESMC_TimeMgr this, ESMC_Time stepSize, int *rc)
{
  *rc = ESMC_TimeMgrSetStepSize(this, stepSize);
}



void esmc_timemgrgetstepsizestd_(ESMC_TimeMgr this, ESMC_Time stepSize, int *rc)
{
  *rc = ESMC_TimeMgrGetStepSize(this, stepSize);
}




void esmc_timemgrsetstepsizeis_(ESMC_TimeMgr this, int *days, int *seconds, int *rc)
{
  *rc = ESMC_TimeMgrSetStepSizeIS(this, *days, *seconds);
}



void esmc_timemgrgetstepsizeis_(ESMC_TimeMgr this, int *days, int *seconds, int *rc)
{
  *rc = ESMC_TimeMgrGetStepSizeIS(this, days, seconds);
}



void esmc_timemgrgetnstep_(ESMC_TimeMgr this, int *nstep, int *rc)
{
  *rc = ESMC_TimeMgrGetNStep(this, nstep);
}



void esmc_timemgrsetnstep_(ESMC_TimeMgr this, int *nstep, int *rc)
{
  *rc = ESMC_TimeMgrSetNStep(this, *nstep);
}


void esmc_timemgrgetstartdate_(ESMC_TimeMgr this, ESMC_Date startDate, int *rc)
{
  *rc = ESMC_TimeMgrGetStartDate(this, startDate);
}



void esmc_timemgrgetstopdate_(ESMC_TimeMgr this, ESMC_Date stopDate, int *rc)
{
  *rc = ESMC_TimeMgrGetStopDate(this, stopDate);

}



void esmc_timemgrgetbasedate_(ESMC_TimeMgr this, ESMC_Date baseDate, int *rc)
{
  *rc = ESMC_TimeMgrGetBaseDate(this, baseDate);
}



void esmc_timemgrgetcurrdate_(ESMC_TimeMgr this, ESMC_Date currDate, int *rc)
{
  *rc = ESMC_TimeMgrGetCurrDate(this, currDate);
}



void esmc_timemgrsetcurrdateis_(ESMC_TimeMgr this, int *dateYYMMDD, int *tod, int *rc)
{
  *rc = ESMC_TimeMgrSetCurrDateIS(this, *dateYYMMDD, *tod);
}



void esmc_timemgrgetprevdate_(ESMC_TimeMgr this, ESMC_Date prevDate, int *rc)
{
  *rc = ESMC_TimeMgrGetPrevDate(this, prevDate);
}



void esmc_timemgrrestartwriteis_(ESMC_TimeMgr this,
			       ESMC_CalendarType *type,
			       int *nstep,
			       int *stepDays, int *stepSec,
			       int *startYYMMDD, int *startSec,
			       int *stopYYMMDD, int *stopSec,
			       int *baseYYMMDD, int *baseSec,
			       int *currYYMMDD, int *currSec,
			       int *rc)
{
  *rc = ESMC_TimeMgrRestartWriteIS(this,
				 type,
				 nstep,
				 stepDays, stepSec,
				 startYYMMDD, startSec,
				 stopYYMMDD, stopSec,
				 baseYYMMDD, baseSec,
				 currYYMMDD, currSec);
}



void esmc_timemgrrestartreadis_(ESMC_TimeMgr this,
			      ESMC_CalendarType *type,
			      int *nstep,
			      int *stepDays, int *stepSec,
			      int *startYYMMDD, int *startSec,
			      int *stopYYMMDD, int *stopSec,
			      int *baseYYMMDD, int *baseSec,
			      int *currYYMMDD, int *currSec,
			      int *rc)
{
  *rc = ESMC_TimeMgrRestartReadIS(this,
				*type,
				*nstep,
				*stepDays, *stepSec,
				*startYYMMDD, *startSec,
				*stopYYMMDD, *stopSec,
				*baseYYMMDD, *baseSec,
				*currYYMMDD, *currSec);
}

#if defined(__cplusplus)
}
#endif




















