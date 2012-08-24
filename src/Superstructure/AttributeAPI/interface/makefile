# $Id: makefile,v 1.4 2012/08/24 00:32:13 rokuingh Exp $ 

ALL: build_here 

AUTOGEN	  = ESMF_Attribute.F90
SOURCEC	  = 
SOURCEF	  = ESMF_AttributeInternals.F90 $(AUTOGEN) ESMF_Attribute_C.F90
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






