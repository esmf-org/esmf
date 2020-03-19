# $Id$

ALL: build_here

SOURCEC	  = ESMC_InfoDescribeCDef.C ESMC_InfoCacheCDef.C
SOURCEF	  =
STOREH    =

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))
TEXFILES  = $(addsuffix _fapi.tex, $(basename $(AUTOGEN)))

LIBBASE	  = libesmf
LOCDIR	  = src/Superstructure/InfoAPI/src

CLEANDIRS   =
CLEANFILES  = $(AUTOGEN) $(addprefix ../doc/, $(TEXFILES) )
CLOBBERDIRS =

include $(ESMF_DIR)/makefile

DIRS =
