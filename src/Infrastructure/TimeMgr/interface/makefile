# $Id: makefile,v 1.12 2003/03/22 05:46:48 eschwab Exp $ 
ALL: lib 

CFLAGS    = 
FFLAGS    =
SOURCEC	  = ESMC_BaseTime_F.C ESMC_Time_F.C ESMC_Alarm_F.C
#SOURCEC	  = ESMC_Fraction_F.C ESMC_BaseTime_F.C \
            ESMC_Calendar_F.C ESMC_TimeInterval_F.C ESMC_Time_F.C \
            ESMC_Alarm_F.C ESMC_Clock_F.C

SOURCEF	  = ESMF_Fraction.F90 ESMF_BaseTime.F90 \
            ESMF_Calendar.F90 ESMF_TimeInterval.F90 ESMF_Time.F90 \
            ESMF_Alarm.F90 ESMF_Clock.F90
SOURCEH	  = 
OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))
MANSEC	  = Timeobj
LIBBASE	  = libesmf
DIRS	  = 
LOCDIR	  = src/Infrastructure/TimeMgr/interface
CLEANFILES = $(LIBNAME) $(addprefix doc/, $(addsuffix .tex, $(basename $(SOURCEF))))

include $(ESMF_DIR)/makefile

DIRS = 

ESMC_Fraction_F.o:	ESMC.h ESMC_Base.h ESMC_Fraction.h makefile

ESMC_BaseTime_F.o:	ESMC.h ESMC_Base.h ESMC_BaseTime.h makefile

ESMC_Calendar_F.o:	ESMC.h ESMC_Base.h ESMC_Calendar.h makefile

ESMC_TimeInterval_F.o:	ESMC.h ESMC_Base.h ESMC_TimeInterval.h makefile

ESMC_Time_F.o:	ESMC.h ESMC_Base.h ESMC_Time.h makefile

ESMC_Alarm_F.o:	ESMC.h ESMC_Base.h ESMC_Alarm.h makefile

ESMC_Clock_F.o:	ESMC.h ESMC_Base.h ESMC_Clock.h makefile

ESMF_Fraction.o:	makefile

ESMF_BaseTime.o:	ESMF_Base.o makefile

ESMF_Calendar.o:	ESMF_Base.o ESMF_BaseTime.o makefile

ESMF_TimeInterval.o:	ESMF_Base.o ESMF_Fraction.o \
			ESMF_BaseTime.o ESMF_Calendar.o makefile

ESMF_Time.o:	ESMF_BaseTime.o ESMF_TimeInterval.o makefile

ESMF_Alarm.o:	ESMF_Base.o ESMF_TimeInterval.o ESMF_Time.o \
		makefile

ESMF_Clock.o:	ESMF_Base.o ESMF_TimeInterval.o ESMF_Time.o \
		ESMF_Alarm.o makefile
