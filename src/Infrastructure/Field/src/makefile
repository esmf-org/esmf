# $Id: makefile,v 1.20 2007/08/30 05:06:32 cdeluca Exp $ 

#
ALL: build_here 

AUTOGEN   = ESMF_FieldGet.F90 ESMF_FieldSet.F90 ESMF_FieldCreate.F90
CFLAGS    =
FFLAGS    =
SOURCEC	  = 
SOURCEF	  = ESMF_Field.F90 
# TODO:FIELDINTEGRATION Turn other Field methods back on.
# $(AUTOGEN)
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


ESMF_FieldGet.o: ESMF_Field.o ESMF_FieldGet.cpp ESMF_FieldGetMacros.h

ESMF_FieldSet.o: ESMF_Field.o ESMF_FieldSet.cpp ESMF_FieldSetMacros.h

ESMF_FieldCreate.o: ESMF_Field.o ESMF_FieldGet.o ESMF_FieldSet.o \
                    ESMF_FieldCreate.cpp ESMF_FieldCreateMacros.h


