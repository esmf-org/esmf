# $Id: makefile,v 1.17.2.2 2008/05/06 17:19:18 theurich Exp $ 

# default rule if no explicit target is given to make
ALL: build_here

CFLAGS    += -I$(ESMF_DIR)/src/Infrastructure/LogErr/include ${MPI_INCLUDE}
FFLAGS    =

# files which are generated from .cpp files
AUTOGEN   = ESMF_FortranWordsize.F90 ESMF_TypeKindGet.F90

SOURCEC	  = ESMC_Util.C ESMC_Fraction.C ESMCI_F90Interface.C
SOURCEF	  = ESMF_UtilTypes.F90 \
            ESMF_LogErr.F90  \
            ESMF_FortranWordsizeHelper.F90 \
            $(AUTOGEN) \
            ESMF_InitMacros.F90
            
STOREH    = ESMC_Arg.h ESMCI_Arg.h ESMF_Macros.inc ESMF.h ESMC_Macros.h \
            ESMC_Util.h ESMCI_Util.h ESMF_TypeKindRankMacros.hcppF90 \
            ESMC_Start.h \
            ESMC_F90Interface.h ESMCI_F90Interface.h ESMF_ErrReturnCodes.inc \
            ESMF_LogMacros.inc ESMF_LogConstants.inc ESMC_Fraction.h \
            ESMF_TypeKindMacros.hcppF90 ESMF_InitMacros.inc

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
ESMF_FortranWordsize.o: ESMF_FortranWordsize.cppF90 ESMF_FortranWordsizeHelper.o ESMF_TypeKindMacros.hcppF90
ESMF_TypeKindGet.o: ESMF_TypeKindGet.cppF90 ESMF_TypeKindMacros.hcppF90
