# $Id: makefile,v 1.5 2006/12/12 23:21:10 donstark Exp $ 

# default rule if no explicit target is given to make
ALL: build_here

CFLAGS    += -I$(ESMF_DIR)/src/Infrastructure/LogErr/include ${MPI_INCLUDE}
FFLAGS    =

SOURCEC	  = ESMC_Util.C ESMC_Fraction.C ESMC_F90Interface.C
SOURCEF	  = ESMF_InitMacros.F90 \
            ESMF_UtilTypes.F90 \
            ESMF_LogErr.F90    
            
STOREH    = ESMF_Macros.inc ESMF_Version.inc ESMF.h ESMC_Macros.h \
            ESMC_Util.h ESMF_StdCppMacros.h ESMC_Start.h \
            ESMC_F90Interface.h ESMF_ErrReturnCodes.inc ESMF_LogMacros.inc \
	    ESMF_LogConstants.inc ESMC_Fraction.h ESMF_TypeKindTemplate.h \
            ESMF_InitMacros.inc 

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))
TEXFILES  = $(addsuffix _fapi.tex, $(basename $(AUTOGEN)))

MANSEC	  = Utilobj
LIBBASE	  = libesmf
DIRS	  = 
LOCDIR	  = src/Infrastructure/Util/src

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = 
CLEANFILES  = $(AUTOGEN) $(addprefix ../doc/, $(TEXFILES) )
CLOBBERDIRS = 

ESMF_LogErr.o: ESMF_UtilTypes.o ESMF_InitMacros.o
ESMF_UtilTypes.o: ESMF_InitMacros.o
