# $Id: makefile,v 1.15 2004/05/10 15:43:47 nscollins Exp $ 

#
ALL: build_here 

CFLAGS    =
FFLAGS    =
GENC      =
GENF      = ESMF_FieldGet.F90 ESMF_FieldSet.F90 ESMF_FieldCreate.F90
SOURCEC	  = 
SOURCEF	  = ESMF_Field.F90 ESMF_FieldGet.F90 ESMF_FieldSet.F90 ESMF_FieldCreate.F90
SOURCEH	  = 
OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

MANSEC	  = Fieldobj
LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/Field/src

TEXFILES_TO_MAKE = ${GENF}

include $(ESMF_DIR)/makefile

DIRS	  = 

CLEANDIRS   =
CLEANFILES  = ESMF_FieldGet.F90 ESMF_FieldSet.F90 ESMF_FieldCreate.F90
CLOBBERDIRS =


ESMF_FieldGet.o: ESMF_Field.o ESMF_FieldGet.cpp ESMF_FieldGetMacros.h

ESMF_FieldSet.o: ESMF_Field.o ESMF_FieldSet.cpp ESMF_FieldSetMacros.h

ESMF_FieldCreate.o: ESMF_Field.o ESMF_FieldGet.o ESMF_FieldSet.o ESMF_FieldCreate.cpp \
                    ESMF_FieldCreateMacros.h


