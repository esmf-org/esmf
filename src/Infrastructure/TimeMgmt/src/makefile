# $Id: makefile,v 1.8 2003/02/05 17:27:14 nscollins Exp $ 
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
		ESMF_TODMod.f \
		ESMF_CalendarMod.f \
		ESMF_TimeMod.F90 \
		ESMF_DateMod.f90 \
		ESMF_TimeMgrMod.F90 \
		ESMF_AlarmMod.f \
		ESMF_TimeMgmtMod.F90 
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
RMMODFILES = $(addsuffix .mod, $(basename $(SOURCEF)))
# RM MOD FILES does generate the right list, but the case of the basename isn't
# correct - it needs to be all lower case.  try using tr first...

include ${ESMF_DIR}/build/${ESMF_ARCH}/base

lib:: 
	rm -f `echo ${RMMODFILES} | tr [:upper:]  [:lower:] `
	echo "removing TimeMgmt module files to prevent name collisions with newer TimeMgr"


