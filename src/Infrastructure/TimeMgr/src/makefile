# $Id: makefile,v 1.10 2003/04/25 09:17:48 eschwab Exp $

ALL: lib

CFLAGS    =
FFLAGS    =
SOURCEC   = ESMC_Fraction.C ESMC_BaseTime.C ESMC_TimeInterval.C ESMC_Time.C \
            ESMC_Calendar.C ESMC_Alarm.C ESMC_Clock.C
SOURCEF   =
STOREH    = ESMC_Fraction.h ESMC_BaseTime.h ESMC_TimeInterval.h ESMC_Time.h \
            ESMC_Calendar.h ESMC_Alarm.h ESMC_Clock.h

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))
MANSEC    = TimeMgrobj
LIBBASE   = libesmf
DIRS      =
LOCDIR    = src/Infrastructure/TimeMgr/src

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =
