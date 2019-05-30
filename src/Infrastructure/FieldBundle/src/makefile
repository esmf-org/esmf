# $Id$ 


ALL: build_here 

AUTOGEN   = ESMF_FieldBundle.F90

SOURCEC	  = 
SOURCEF	  = $(AUTOGEN)
SOURCEH	  = 

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))
TEXFILES  = $(addsuffix _fapi.tex, $(basename $(AUTOGEN)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/FieldBundle/src

CLEANDIRS   =
CLEANFILES  = $(AUTOGEN) $(addprefix ../doc/, $(TEXFILES) )
CLOBBERDIRS =

include $(ESMF_DIR)/makefile

#ESMF_CPPDEFAULT             = gcc -E -P -x c -C -w

DIRS	  = 

