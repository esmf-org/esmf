# $Id: makefile,v 1.6 2003/03/14 05:19:12 eschwab Exp $ 
ALL: lib 

CFLAGS    = 
FFLAGS    =
SOURCEC	  = ESMC_Calendar_F.C
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

ESMC_Time_F.o:	ESMC_Time.h makefile

ESMF_Fraction.o:	makefile

ESMF_BaseTime.o:	ESMF_Base.o makefile

ESMF_Calendar.o:	ESMF_Time.o makefile

ESMF_TimeInterval.o:	ESMF_Fraction.o ESMF_BaseTime.o makefile

ESMF_Time.o:	ESMF_BaseTime.o ESMF_TimeInterval.o makefile

ESMF_Alarm.o:	ESMF_TimeInterval.o ESMF_Time.o	makefile

ESMF_Clock.o:	ESMF_TimeInterval.o ESMF_Time.o ESMF_Alarm.o makefile
