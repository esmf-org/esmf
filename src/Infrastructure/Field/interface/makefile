# $Id: makefile,v 1.12 2008/09/26 05:18:05 cdeluca Exp $ 

ALL: build_here 

SOURCEC	  = ESMCI_Field_F.C
SOURCEF	  = 
# TODO:FIELDINTEGRATION Restore ESMF_Field_C.F90
# ESMF_Field_C.F90 

# List all .h files which should be copied to a common include dir
STOREH	  = 

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/Field/interface

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

include $(ESMF_DIR)/makefile

DIRS	  = 



