# $Id: makefile,v 1.3 2002/12/11 16:06:37 nscollins Exp $ 
ALL: lib 

CFLAGS    =
FFLAGS    =
SOURCEC	  = \
		ESMC_Alarm.c \
		ESMC_AlarmF.c \
		ESMC_Calendar.c \
		ESMC_Date.c \
		ESMC_DateF.c \
		ESMC_TOD.c \
		ESMC_Time.c \
		ESMC_TimeF.c \
		ESMC_TimeMgr.c \
		ESMC_TimeMgrF.c 

SOURCEF	  = \
		ESMF_TimeMgmtMod.f90 \
		ESMF_TODMod.f \
		ESMF_CalendarMod.f \
		ESMF_TimeMod.f90 \
		ESMF_DateMod.f \
		ESMF_TimeMgrMod.f90 \
		ESMF_AlarmMod.f 
SOURCEH	  = \
		ESMO_Alarm.h \
		ESMO_Calendar.h \
		ESMO_Date.h \
		ESMO_TOD.h \
		ESMO_Time.h \
		ESMO_TimeMgr.h                

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))
TEXFILES  = $(addsuffix .tex, $(basename $(SOURCEF))) \
            $(addsuffix .tex, $(basename $(SOURCEH)))
MANSEC	  = Timeobj
LIBBASE	  = liboldworld
DIRS	  = examples doc
LOCDIR	  = src/Infrastructure/TimeMgmt/src
CLEANFILES = $(LIBNAME) $(addprefix doc/, $(addsuffix .tex, $(basename $(SOURCEF))))

include ${ESMF_DIR}/build/${ESMF_ARCH}/base





