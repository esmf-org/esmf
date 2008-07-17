# $Id: makefile,v 1.10.2.2 2008/07/17 21:41:36 feiliu Exp $ 

ALL: build_here 

CFLAGS    =
FFLAGS    =
SOURCEC	  = ESMCI_Field_F.C
SOURCEF	  = 
! TODO:FIELDINTEGRATION Restore ESMF_Field_C.F90
# ESMF_Field_C.F90 
STOREH	  = 

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))
MANSEC	  = Fieldobj
LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/Field/interface


include $(ESMF_DIR)/makefile

DIRS	  = 

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =




