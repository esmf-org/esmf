# $Id: makefile,v 1.13 2009/09/21 20:38:33 feiliu Exp $ 

ALL: build_here 

SOURCEC	  = ESMCI_Field_F.C ESMC_Field.C
SOURCEF	  = ESMF_Field_C.F90 

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



