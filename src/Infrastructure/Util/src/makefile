# $Id: makefile,v 1.28 2008/10/19 03:47:03 eschwab Exp $ 

ALL: build_here

AUTOGEN   = ESMF_FortranWordsize.F90 ESMF_TypeKindGet.F90

SOURCEC	  = ESMC_Util.C ESMCI_Fraction.C ESMCI_F90Interface.C
SOURCEF	  = ESMF_UtilTypes.F90 \
            ESMF_LogErr.F90  \
            $(AUTOGEN) \
            ESMF_InitMacros.F90 \
            ESMF_FIOUtil.F90
            
STOREH    = ESMC_Arg.h ESMCI_Arg.h ESMF_Macros.inc ESMF.h ESMC_Macros.h \
            ESMC_Util.h ESMCI_Util.h ESMF_TypeKindRankMacros.hcppF90 \
            ESMC_Start.h \
            ESMCI_F90Interface.h ESMF_ErrReturnCodes.inc \
            ESMF_LogMacros.inc ESMF_LogConstants.inc ESMCI_Fraction.h \
            ESMF_TypeKindMacros.hcppF90 ESMF_InitMacros.inc \
            ESMC_LogMacros.inc

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))
TEXFILES  = $(addsuffix _fapi.tex, $(basename $(AUTOGEN)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/Util/src

CLEANDIRS   = 
CLEANFILES  = $(AUTOGEN) $(addprefix ../doc/, $(TEXFILES) )
CLOBBERDIRS = 

include $(ESMF_DIR)/makefile

DIRS = 

