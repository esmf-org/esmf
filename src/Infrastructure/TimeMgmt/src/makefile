# $Id: makefile,v 1.14 2003/09/09 22:47:42 flanigan Exp $ 
ALL: lib 

CFLAGS    =
FFLAGS    =
SOURCEC	  = \
		ESMC_Alarm.c \
		ESMC_AlarmF.c \
		ESMC_Calendar.c \
		ESMC_Date.c \
		ESMC_DateF.c \
		ESMC_Error.c \
		ESMC_ErrorF.c \
		ESMC_TOD.c \
		ESMC_Time.c \
		ESMC_TimeF.c \
		ESMC_TimeMgr.c \
		ESMC_TimeMgrF.c 

SOURCEF	  = \
		ESMF_ErrorMod.f \
		ESMF_TODMod.f \
		ESMF_CalendarMod.f \
		ESMF_TimeMod.F90 \
		ESMF_DateMod.f90 \
		ESMF_TimeMgrMod.F90 \
		ESMF_AlarmMod.f \
		ESMF_TimeMgmtMod.F90 
SOURCEH	  = \
		ESMO_Alarm.h \
		ESMO_BasicUtil.h \
		ESMO_Calendar.h \
		ESMO_Constants.h \
		ESMO_Date.h \
		ESMO_Error.h \
		ESMO_Log.h \
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

MVMODFILES = $(addsuffix .mod, $(basename $(SOURCEF)))
# RM MOD FILES does generate the right list, but the case of the basename isn't
# correct - it needs to be all lower case.  try using tr first...

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =


lib:: 
	-@if [ ! -d ${ESMF_MODDIR}/oldworld ]; then \
          echo Making directory ${ESMF_MODDIR}/oldworld for old mod files; \
	  mkdir -p ${ESMF_MODDIR}/oldworld ; \
	fi
	-@for i in `echo ${MVMODFILES} | tr [:upper:]  [:lower:] ` ; do \
	  if [ ! -f ${ESMF_MODDIR}/oldworld/$$i ] ; then mv -f $$i ${ESMF_MODDIR}/oldworld ; fi ; \
	done
	echo "moving TimeMgmt module files to prevent name collisions with newer TimeMgr"


