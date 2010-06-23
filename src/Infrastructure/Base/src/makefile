# $Id: makefile,v 1.33 2010/06/23 05:52:17 theurich Exp $ 

ALL: build_here

SOURCEC	  = ESMCI_Base.C
SOURCEF	  = 
STOREH    = ESMCI_Base.h 

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

