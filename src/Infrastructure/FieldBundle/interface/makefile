# $Id: makefile,v 1.4 2008/09/26 05:18:05 cdeluca Exp $ 

ALL: build_here 

SOURCEC	  = ESMCI_FieldBundle_F.C
SOURCEF	  = ESMF_FieldBundle_C.F90

# List all .h files which should be copied to common include dir
STOREH	  = 

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/FieldBundle/interface

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

include $(ESMF_DIR)/makefile

DIRS	  = 




