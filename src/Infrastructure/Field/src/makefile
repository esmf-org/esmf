# $Id: makefile,v 1.12 2004/04/19 22:01:52 nscollins Exp $ 

#
ALL: build_here 

CFLAGS    =
FFLAGS    =
SOURCEC	  = 
SOURCEF	  = ESMF_Field.F90 ESMF_FieldCreate.F90 ESMF_FieldGet.F90 ESMF_FieldSet.F90
SOURCEH	  = 
OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

MANSEC	  = Fieldobj
LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/Field/src


include $(ESMF_DIR)/makefile

DIRS	  = 

CLEANDIRS   =
CLEANFILES  = ESMF_FieldGet.F90 ESMF_FieldSet.F90 ESMF_FieldCreate.F90
CLOBBERDIRS =


ESMF_FieldGet.o: ESMF_Field.o 

ESMF_FieldGet.F90: ESMF_FieldGet.cpp ESMF_FieldGetMacros.h ESMF_Field.o

ESMF_FieldSet.o: ESMF_Field.o

ESMF_FieldSet.F90: ESMF_FieldSet.cpp ESMF_FieldSetMacros.h ESMF_Field.o

ESMF_FieldCreate.o: ESMF_Field.o ESMF_FieldGet.o ESMF_FieldSet.o 

ESMF_FieldCreate.F90: ESMF_FieldCreate.cpp ESMF_FieldCreateMacros.h \
                      ESMF_Field.o ESMF_FieldGet.o ESMF_FieldSet.o 


