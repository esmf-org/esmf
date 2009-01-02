# $Id: makefile,v 1.31 2009/01/02 19:48:56 feiliu Exp $ 

ALL: build_here 

AUTOGEN   = ESMF_FieldGet.F90 ESMF_FieldSet.F90 ESMF_FieldSetCo.F90 ESMF_FieldCreateGe.F90 \
	ESMF_FieldCreateGr.F90 ESMF_FieldCreateLo.F90 ESMF_FieldCreateMe.F90 ESMF_FieldCreate.F90
SOURCEC	  = 
SOURCEF	  = ESMF_Field.F90 ESMF_FieldPr.F90 ESMF_FieldCreateBa.F90 $(AUTOGEN) ESMF_FieldRegrid.F90
SOURCEH	  = 

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))
TEXFILES  = $(addsuffix _fapi.tex, $(basename $(AUTOGEN)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/Field/src

CLEANDIRS   =
CLEANFILES  = $(AUTOGEN) $(addprefix ../doc/, $(TEXFILES) )
CLOBBERDIRS =

include $(ESMF_DIR)/makefile

# on AIX with the new XLF compiler (version 10.1) there is a
# problem compiling the local files with optimization ->
# solution: turn optimization off for AIX
ifeq ($(ESMF_OS),AIX)
ESMF_OPTLEVEL = 0
endif

DIRS	  = 

