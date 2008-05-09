# $Id: makefile,v 1.15 2008/05/09 20:08:21 rosalind Exp $

ALL: build_here

CFLAGS    =
FFLAGS    =
SOURCEC   = ESMCI_BaseTime.C ESMC_TimeInterval.C ESMC_Time.C \
            ESMC_Calendar.C ESMC_Alarm.C ESMC_Clock.C
SOURCEF   =
STOREH    = ESMCI_BaseTime.h ESMCI_TimeInterval.h ESMCI_Time.h \
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
