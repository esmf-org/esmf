# $Id: makefile,v 1.5 2003/02/27 17:54:56 flanigan Exp $ 
ALL: lib 

CFLAGS    = 
FFLAGS    =
SOURCEC	  = c_ESMF_BaseTime.C c_ESMF_Time.C
SOURCEF	  = ESMF_Fraction.F90 ESMF_BaseTime.F90 \
            ESMF_Calendar.F90 ESMF_TimeInterval.F90 ESMF_Time.F90 \
            ESMF_Alarm.F90 ESMF_Clock.F90
SOURCEH	  = 
OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))
TEXFILES  = $(addsuffix .tex, $(basename $(SOURCEF)))
MANSEC	  = Timeobj
LIBBASE	  = libesmf
DIRS	  = 
LOCDIR	  = src/Infrastructure/TimeMgr/interface
CLEANFILES = $(LIBNAME) $(addprefix doc/, $(addsuffix .tex, $(basename $(SOURCEF))))

include $(ESMF_DIR)/makefile

DIRS = 

c_ESMF_BaseTime.o:	ESMC_BaseTime.h makefile

c_ESMF_Time.o:	ESMC_Time.h makefile

ESMF_Fraction.o:	makefile

ESMF_BaseTime.o:	ESMF_Base.o makefile

ESMF_Calendar.o:	ESMF_Time.o makefile

ESMF_TimeInterval.o:	ESMF_Fraction.o ESMF_BaseTime.o makefile

ESMF_Time.o:	ESMF_BaseTime.o ESMF_TimeInterval.o makefile

ESMF_Alarm.o:	ESMF_TimeInterval.o ESMF_Time.o	makefile

ESMF_Clock.o:	ESMF_TimeInterval.o ESMF_Time.o ESMF_Alarm.o makefile
