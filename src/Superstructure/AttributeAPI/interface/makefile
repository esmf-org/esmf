# $Id$ 

ALL: build_here 

AUTOGEN	  = ESMF_AttributeInternals.F90 ESMF_Attribute.F90
SOURCEC	  = 
SOURCEF	  = ESMF_Info.F90 $(AUTOGEN) ESMF_Attribute_C.F90
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






