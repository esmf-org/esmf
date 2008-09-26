# $Id: makefile,v 1.4 2008/09/26 05:53:53 cdeluca Exp $ 

# default rule if no explicit target is given to make
ALL: build_here

SOURCEC	  = ESMC_Attribute.C
SOURCEF	  = 
STOREH    = ESMC_Attribute.h 

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

