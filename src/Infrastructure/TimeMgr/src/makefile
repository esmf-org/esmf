# $Id$

ALL: build_here

SOURCEC   = ESMCI_BaseTime.C ESMCI_TimeInterval.C ESMCI_Time.C \
            ESMCI_Calendar.C ESMCI_Alarm.C ESMCI_Clock.C
SOURCEF   =
STOREH    = ESMCI_BaseTime.h ESMCI_TimeInterval.h ESMCI_Time.h \
            ESMCI_Calendar.h ESMCI_Alarm.h ESMCI_Clock.h

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE   = libesmf
LOCDIR    = src/Infrastructure/TimeMgr/src

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

include $(ESMF_DIR)/makefile

DIRS = 
