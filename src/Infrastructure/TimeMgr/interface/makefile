# $Id: makefile,v 1.10 2003/03/20 20:33:18 flanigan Exp $ 
ALL: lib 

CFLAGS    = 
FFLAGS    =
SOURCEC	  = ESMC_BaseTime_F.C ESMC_Alarm_F.C
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

ESMF_BaseTime.o:	ESMF_BaseMod.mod makefile

ESMF_Calendar.o:	ESMF_BaseMod.mod ESMF_BaseTimeMod.mod makefile

ESMF_TimeInterval.o:	ESMF_BaseMod.mod ESMF_FractionMod.mod \
			ESMF_BaseTimeMod.mod ESMF_CalendarMod.mod makefile

ESMF_Time.o:	ESMF_BaseTimeMod.mod ESMF_TimeIntervalMod.mod makefile

ESMF_Alarm.o:	ESMF_BaseMod.mod ESMF_TimeIntervalMod.mod ESMF_TimeMod.mod \
		makefile

ESMF_Clock.o:	ESMF_BaseMod.mod ESMF_TimeIntervalMod.mod ESMF_TimeMod.mod \
		ESMF_AlarmMod.mod makefile
