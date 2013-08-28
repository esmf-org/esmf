# $Id$ 

ALL: build_here

AUTOGEN   = ESMF_FortranWordsize.F90 ESMF_TypeKindGet.F90 \
            ESMF_UtilSort.F90

SOURCEC	  = ESMCI_Util.C ESMCI_Fraction.C ESMCI_F90Interface.C \
	    ESMCI_CoordSys.C
SOURCEF	  = ESMF_UtilTypes.F90 \
            ESMF_LogErr.F90  \
            $(AUTOGEN) \
            ESMF_InitMacros.F90 \
            ESMF_IOUtil.F90
            
STOREH    = ESMCI_Arg.h ESMF_Macros.inc ESMF.h ESMCI_Util.h \
            ESMF_TypeKindRankMacros.hcppF90 ESMCI_Macros.h \
            ESMCI_F90Interface.h ESMF_ErrReturnCodes.inc \
            ESMF_LogMacros.inc ESMF_LogConstants.inc ESMCI_Fraction.h \
            ESMF_TypeKindMacros.hcppF90 ESMF_InitMacros.inc ESMCI_CoordSys.h

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

