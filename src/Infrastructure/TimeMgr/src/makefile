# $Id: makefile,v 1.5 2002/11/08 17:32:42 nscollins Exp $ 
ALL: lib 

CFLAGS    =
FFLAGS    =
STOREH    = 
SOURCEC	  = ESMC_Time.C ESMC_TimeInstant.C ESMC_TimeInterval.C ESMC_Clock.C ESMC_Alarm.C ESMC_Calendar.C
SOURCEF	  = 
SOURCEH	  = 
OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))
TEXFILES  = $(addsuffix .tex, $(basename $(SOURCEF)))
MANSEC	  = Timeobj
LIBBASE	  = libesmf
DIRS	  = 
LOCDIR	  = src/Infrastructure/TimeMgr/src
CLEANFILES = $(LIBNAME) $(addprefix doc/, $(addsuffix .tex, $(basename $(SOURCEF))))

include ${ESMF_DIR}/build/${ESMF_ARCH}/base
