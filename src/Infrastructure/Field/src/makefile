# $Id: makefile,v 1.34 2009/01/08 15:38:14 theurich Exp $ 

ALL: build_here 

AUTOGEN   = ESMF_FieldGet.F90 ESMF_FieldSet.F90 ESMF_FieldSetCo.F90 ESMF_FieldCreate.F90
SOURCEC	  = 
SOURCEF	  = ESMF_Field.F90 ESMF_FieldPr.F90 $(AUTOGEN) ESMF_FieldRegrid.F90
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
# solution: turn optimization off
ifeq ($(ESMF_OS),AIX)
ESMF_OPTLEVEL = 0
endif

# on IBM BGL with the XLF compiler there is a
# problem compiling the local files with optimization ->
# solution: turn optimization off
ifeq ($(ESMF_OS),Linux)
ifeq ($(ESMF_COMPILER),xlf)
ESMF_OPTLEVEL = 0
endif
endif

DIRS	  = 

