# $Id: makefile,v 1.19 2008/06/12 18:08:23 rosalind Exp $

ALL: build_here

CFLAGS    =
FFLAGS    =
SOURCEC   = ESMCI_BaseTime.C ESMCI_TimeInterval.C ESMCI_Time.C \
            ESMC_Calendar.C ESMCI_Alarm.C ESMC_Clock.C
SOURCEF   =
STOREH    = ESMCI_BaseTime.h ESMCI_TimeInterval.h ESMCI_Time.h \
            ESMC_Calendar.h ESMCI_Alarm.h ESMC_Clock.h

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
