# $Id: makefile,v 1.21 2007/04/12 21:09:30 theurich Exp $ 

ALL: build_here 

CFLAGS    = 
FFLAGS    = -free
SOURCEC	  = ESMC_Fraction_F.C ESMC_BaseTime_F.C \
            ESMC_Calendar_F.C ESMC_TimeInterval_F.C ESMC_Time_F.C \
            ESMC_Alarm_F.C ESMC_Clock_F.C

SOURCEF	  = ESMF_Fraction.F90 ESMF_BaseTime.F90 ESMF_Calendar.F90 \
            ESMF_TimeType.F90 ESMF_TimeIntervalType.F90 \
            ESMF_Time.F90 ESMF_TimeInterval.F90 \
            ESMF_AlarmType.F90 ESMF_ClockType.F90 \
            ESMF_Alarm.F90 ESMF_Clock.F90
SOURCEH	  = 
STOREH    = ESMF_TimeMgr.inc

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))
MANSEC	  = Timeobj
LIBBASE	  = libesmf
DIRS	  = 
LOCDIR	  = src/Infrastructure/TimeMgr/interface

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =


ESMC_Fraction_F.o:	ESMC_Base.h ESMC_Fraction.h makefile

ESMC_BaseTime_F.o:	ESMC_Base.h ESMC_BaseTime.h makefile

ESMC_Calendar_F.o:	ESMC_Base.h ESMC_Calendar.h makefile

ESMC_TimeInterval_F.o:	ESMC_Base.h ESMC_TimeInterval.h makefile

ESMC_Time_F.o:	ESMC_Base.h ESMC_Time.h makefile

ESMC_Alarm_F.o:	ESMC_Base.h ESMC_Alarm.h makefile

ESMC_Clock_F.o:	ESMC_Base.h ESMC_Clock.h makefile

ESMF_Fraction.o:	makefile

ESMF_BaseTime.o:	makefile

ESMF_Calendar.o:	ESMF_BaseTime.o makefile

ESMF_TimeInterval.o:	ESMF_TimeIntervalType.o ESMF_Fraction.o \
			ESMF_BaseTime.o ESMF_Time.o ESMF_Calendar.o makefile

ESMF_Time.o:	ESMF_TimeType.o ESMF_BaseTime.o ESMF_TimeInterval.o makefile

ESMF_Alarm.o:	ESMF_TimeInterval.o ESMF_Time.o \
		ESMF_AlarmType.o ESMF_ClockType.o \
		makefile

ESMF_Clock.o:	ESMF_TimeInterval.o ESMF_Time.o \
		ESMF_AlarmType.o ESMF_ClockType.o \
		makefile
