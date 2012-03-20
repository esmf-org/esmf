# $Id: makefile,v 1.3 2012/03/20 23:06:38 rokuingh Exp $ 

ALL: build_here 

AUTOGEN	  = ESMF_Attribute.F90
SOURCEC	  = 
SOURCEF	  = ESMF_AttributeInternals.F90 $(AUTOGEN)
SOURCEH	  = 
TEXFILES  = $(addsuffix _fapi.tex, $(basename $(AUTOGEN)))

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Superstructure/AttributeAPI/interface

CLEANDIRS   =
CLEANFILES  = $(AUTOGEN)
CLOBBERDIRS =

include $(ESMF_DIR)/makefile

DIRS = 






