# $Id: makefile,v 1.8 2003/03/20 20:33:18 flanigan Exp $

ALL: lib

CFLAGS    =
FFLAGS    =
SOURCEC   = ESMC_BaseTime.C ESMC_TimeInterval.C ESMC_Time.C ESMC_Calendar.C \
            ESMC_Alarm.C ESMC_Clock.C
SOURCEF   =
STOREH    = ESMC_BaseTime.h ESMC_TimeInterval.h ESMC_Time.h ESMC_Calendar.h \
            ESMC_Alarm.h ESMC_Clock.h

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))
MANSEC    = TimeMgrobj
LIBBASE   = libesmf
DIRS      =
LOCDIR    = src/Infrastructure/TimeMgr/src
CLEANFILES = $(LIBNAME) $(addprefix doc/, $(addsuffix .tex, $(basename $(SOURCEF))))

include $(ESMF_DIR)/makefile

DIRS = 

