# $Id: makefile,v 1.17 2004/08/03 19:33:46 nscollins Exp $ 

#
ALL: build_here 

AUTOGEN   = ESMF_FieldGet.F90 ESMF_FieldSet.F90 ESMF_FieldCreate.F90
CFLAGS    =
FFLAGS    =
SOURCEC	  = 
SOURCEF	  = ESMF_Field.F90 $(AUTOGEN)
SOURCEH	  = 
CPPFILES  = $(AUTOGEN)

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))
TEXFILES  = $(addsuffix _fapi.tex, $(basename $(AUTOGEN)))


MANSEC	  = Fieldobj
LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/Field/src

include $(ESMF_DIR)/makefile

DIRS	  = 

CLEANDIRS   =
CLEANFILES  = $(AUTOGEN) $(addprefix ../doc/, $(TEXFILES) )
CLOBBERDIRS =


ESMF_FieldGet.o: ESMF_Field.o ESMF_FieldGet.cpp ESMF_FieldGetMacros.h

ESMF_FieldSet.o: ESMF_Field.o ESMF_FieldSet.cpp ESMF_FieldSetMacros.h

ESMF_FieldCreate.o: ESMF_Field.o ESMF_FieldGet.o ESMF_FieldSet.o \
                    ESMF_FieldCreate.cpp ESMF_FieldCreateMacros.h


