# $Id: makefile,v 1.5 2008/10/09 14:28:15 rokuingh Exp $ 

# default rule if no explicit target is given to make
ALL: build_here

SOURCEC	  = ESMCI_Attribute.C
SOURCEF	  = 
STOREH    = ESMCI_Attribute.h 

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))
TEXFILES  = $(addsuffix _fapi.tex, $(basename $(AUTOGEN)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/Attribute/src

CLEANDIRS   = 
CLEANFILES  = $(AUTOGEN) $(addprefix ../doc/, $(TEXFILES) )
CLOBBERDIRS = 

include $(ESMF_DIR)/makefile

DIRS = 

