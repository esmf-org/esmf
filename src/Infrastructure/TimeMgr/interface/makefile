# $Id: makefile,v 1.2 2002/10/08 21:16:15 jwolfe Exp $ 
ALL: lib 

CFLAGS    = 
FFLAGS    =
SOURCEC	  = c_ESMF_Time.cc c_ESMF_TimeInstant.cc
SOURCEF	  = ESMF_Alarm.F90 ESMF_Calendar.F90 ESMF_Clock.F90 ESMF_Fraction.F90 \
            ESMF_Time.F90 ESMF_TimeInstant.F90 ESMF_TimeInterval.F90 ESMF_Types.F90
SOURCEH	  = 
OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))
TEXFILES  = $(addsuffix .tex, $(basename $(SOURCEF)))
MANSEC	  = Timeobj
LIBBASE	  = libesmf
DIRS	  = 
LOCDIR	  = src/Infrastructure/TimeMgr/interface
CLEANFILES = $(LIBNAME) $(addprefix doc/, $(addsuffix .tex, $(basename $(SOURCEF))))

include ${ESMF_DIR}/build/${ESMF_ARCH}/base


c_ESMF_Time.o:		

c_ESMF_TimeInstant.o:	

ESMF_Alarm.o:		

ESMF_Calendar.o:	

ESMF_Clock.o:		

ESMF_Fraction.o:	

ESMF_Time.o:		

ESMF_TimeInstant.o:	

ESMF_TimeInterval.o:	

ESMF_Types.o:		

# DO NOT DELETE
