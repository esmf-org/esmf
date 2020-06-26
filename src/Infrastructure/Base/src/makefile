# $Id$

ALL: build_here

SOURCEC	  = ESMCI_Base.C ESMCI_Info.C ESMC_InfoCDef.C \
            ESMC_InfoCDefGeneric.C
SOURCEF	  =
STOREH    = ESMCI_Base.h \
            nlohmann/json/json.hpp \
            ESMCI_Info.h

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))
TEXFILES  = $(addsuffix _fapi.tex, $(basename $(AUTOGEN)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/Base/src

CLEANDIRS   =
CLEANFILES  = $(AUTOGEN) $(addprefix ../doc/, $(TEXFILES) )
CLOBBERDIRS =

include $(ESMF_DIR)/makefile

DIRS =
