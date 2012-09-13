# $Id: makefile,v 1.7 2012/09/13 21:57:34 rokuingh Exp $ 

# default rule if no explicit target is given to make
ALL: build_here

SOURCEC	  = ESMCI_Attribute.C ESMCI_AttributeUpdate.C ESMCI_AttributeWrite.C
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

