# $Id: makefile,v 1.32 2008/08/06 17:35:40 tjcnrl Exp $ 

ALL: build_here 

CFLAGS    = 
FFLAGS    = -free
SOURCEC	  = ESMC_Fraction_F.C ESMC_BaseTime_F.C \
            ESMC_Calendar_F.C ESMC_TimeInterval_F.C ESMC_Time_F.C \
            ESMC_Alarm_F.C ESMC_Clock_F.C \
            ESMC_Calendar.C ESMC_Time.C ESMC_TimeInterval.C \
            ESMC_Clock.C

SOURCEF	  = ESMF_Fraction.F90 ESMF_BaseTime.F90 ESMF_Calendar.F90 \
            ESMF_TimeType.F90 ESMF_TimeIntervalType.F90 \
            ESMF_Time.F90 ESMF_TimeInterval.F90 \
            ESMF_AlarmType.F90 ESMF_ClockType.F90 \
            ESMF_Alarm.F90 ESMF_Clock.F90
SOURCEH	  = 
STOREH    = ESMC_TimeInterval.h ESMC_Time.h ESMC_Calendar.h ESMC_Clock.h \
            ESMF_TimeMgr.inc

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
