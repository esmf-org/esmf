# $Id: makefile,v 1.31 2008/09/26 04:16:42 cdeluca Exp $ 

# default rule if no explicit target is given to make
ALL: build_here

SOURCEC	  = ESMC_Base.C
SOURCEF	  = 
STOREH    = ESMC_Base.h 

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))
TEXFILES  = $(addsuffix _fapi.tex, $(basename $(AUTOGEN)))

LIBBASE	  = libesmf
DIRS	  = 
LOCDIR	  = src/Infrastructure/Base/src

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = 
CLEANFILES  = $(AUTOGEN) $(addprefix ../doc/, $(TEXFILES) )
CLOBBERDIRS = 

