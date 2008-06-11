# $Id: makefile,v 1.17 2008/06/11 21:32:31 rosalind Exp $

ALL: build_here

CFLAGS    =
FFLAGS    =
SOURCEC   = ESMCI_BaseTime.C ESMCI_TimeInterval.C ESMCI_Time.C \
            ESMC_Calendar.C ESMC_Alarm.C ESMC_Clock.C
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
