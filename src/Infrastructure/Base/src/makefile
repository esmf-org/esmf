# $Id: makefile,v 1.24 2004/12/03 20:47:45 nscollins Exp $ 

# default rule if no explicit target is given to make
ALL: build_here

CFLAGS    += -I$(ESMF_DIR)/src/Infrastructure/LogErr/include
FFLAGS    =

SOURCEC	  = ESMC_Base.C ESMC_Test.C ESMC_Fraction.C
SOURCEF	  = ESMF_BaseTypes.F90 ESMF_VMTypes.F90 ESMF_VM.F90 \
            ESMF_LogErr.F90 ESMF_Test.F90
STOREH    = ESMF_Macros.inc ESMF_Version.inc ESMF.h ESMC_Macros.h \
            ESMC_Base.h ESMC_Test.h ESMF_StdCppMacros.h ESMC_Start.h \
            ESMC_F90Interface.h ESMF_ErrReturnCodes.inc ESMF_LogMacros.inc \
	    ESMF_LogConstants.inc ESMC_Fraction.h

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))
TEXFILES  = $(addsuffix _fapi.tex, $(basename $(AUTOGEN)))

MANSEC	  = Baseobj
LIBBASE	  = libesmf
DIRS	  = 
LOCDIR	  = src/Infrastructure/Base/src

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = 
CLEANFILES  = $(AUTOGEN) $(addprefix ../doc/, $(TEXFILES) )
CLOBBERDIRS = 

ESMF_LogErr.o: ESMF_BaseTypes.o

ESMF_VMTypes.o: ESMF_BaseTypes.o

ESMF_VM.o: ESMF_VMTypes.o

