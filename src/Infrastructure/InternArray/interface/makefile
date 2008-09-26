# $Id: makefile,v 1.7 2008/09/26 05:18:07 cdeluca Exp $

ALL: build_here 

AUTOGEN   = ESMF_InternArrayCreate.F90 ESMF_InternArrayGet.F90
SOURCEC	  = ESMC_InternArrayDataMap.C ESMC_InternArray_F.C
SOURCEF	  = ESMF_InternArrayDataMap_C.F90 ESMF_InternArray.F90 $(AUTOGEN) ESMF_Alloc_C.F90
SOURCEH	  =

# List all .h files which should be copied to common include dir
STOREH    = ESMC_InternArrayDataMap.h

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))
TEXFILES  = $(addsuffix _fapi.tex, $(basename $(AUTOGEN)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/InternArray/interface

CLEANDIRS   =
CLEANFILES  = $(AUTOGEN) $(addprefix ../doc/, $(TEXFILES) )
CLOBBERDIRS =

include $(ESMF_DIR)/makefile

# on the mac, you have to give this additional flag so the file uses > 16 bit
# offsets internally in the file.
ifeq ($(ESMF_COMPILER),absoft)
ESMF_F90COMPILEOPTS += -N11
endif

DIRS	  = 

