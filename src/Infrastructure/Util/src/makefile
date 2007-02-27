# $Id: makefile,v 1.9 2007/02/27 22:41:36 theurich Exp $ 

# default rule if no explicit target is given to make
ALL: build_here

CFLAGS    += -I$(ESMF_DIR)/src/Infrastructure/LogErr/include ${MPI_INCLUDE}
FFLAGS    =

# files which are generated from .cpp files
AUTOGEN   = ESMF_FortranWordsize.F90 ESMF_TypeKindGet.F90

SOURCEC	  = ESMC_Util.C ESMC_Fraction.C ESMC_F90Interface.C
SOURCEF	  = ESMF_UtilTypes.F90 \
            ESMF_LogErr.F90  \
            $(AUTOGEN) \
            ESMF_InitMacros.F90
            
STOREH    = ESMF_Macros.inc ESMF_Version.inc ESMF.h ESMC_Macros.h \
            ESMC_Util.h ESMF_StdCppMacros.h ESMC_Start.h \
            ESMC_F90Interface.h ESMF_ErrReturnCodes.inc ESMF_LogMacros.inc \
	    ESMF_LogConstants.inc ESMC_Fraction.h ESMF_TypeKindTemplate.h \
            ESMF_InitMacros.inc \
            ESMF_TypeKindGetMacros.h

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

ESMF_InitMacros.o: ESMF_LogErr.o ESMF_UtilTypes.o 
ESMF_LogErr.o: ESMF_UtilTypes.o 
ESMF_FortranWordsize.o: ESMF_FortranWordsize.cpp ESMF_TypeKindTemplate.h ESMF_FortranWordsizeMacros.h
ESMF_TypeKindGet.o: ESMF_TypeKindGet.cpp ESMF_TypeKindTemplate.h ESMF_TypeKindGet.h
