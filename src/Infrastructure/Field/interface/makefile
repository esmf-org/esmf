# $Id: makefile,v 1.16 2009/10/24 05:20:08 theurich Exp $ 

ALL: build_here 

SOURCEC	  = ESMCI_Field_F.C ESMCI_Field.C ESMC_Field.C
SOURCEF	  = ESMF_Field_C.F90 

# List all .h files which should be copied to a common include dir
STOREH	  = ESMC_Field.h ESMCI_Field.h

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/Field/interface

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

include $(ESMF_DIR)/makefile

DIRS	  = 



