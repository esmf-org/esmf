# $Id: makefile,v 1.10.2.1 2008/03/27 23:36:43 theurich Exp $ 

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




