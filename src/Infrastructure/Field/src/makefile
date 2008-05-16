# $Id: makefile,v 1.23.2.4 2008/05/16 21:58:39 theurich Exp $ 

#
ALL: build_here 

AUTOGEN   = ESMF_FieldGet.F90 ESMF_FieldSet.F90 ESMF_FieldCreate.F90
CFLAGS    =
FFLAGS    =
SOURCEC	  = 
SOURCEF	  = ESMF_Field.F90 ESMF_FieldPr.F90 $(AUTOGEN)
SOURCEH	  = 

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))
TEXFILES  = $(addsuffix _fapi.tex, $(basename $(AUTOGEN)))

MANSEC	  = Fieldobj
LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/Field/src

include $(ESMF_DIR)/makefile

# on AIX with the new XLF compiler (version 10.1) there is a
# problem compiling the local files with optimization ->
# solution: turn optimization off for AIX
ifeq ($(ESMF_OS),AIX)
ESMF_OPTLEVEL = 0
endif

DIRS	  = 

CLEANDIRS   =
CLEANFILES  = $(AUTOGEN) $(addprefix ../doc/, $(TEXFILES) )
CLOBBERDIRS =

