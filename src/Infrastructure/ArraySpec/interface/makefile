# $Id: makefile,v 1.5 2008/09/26 05:18:03 cdeluca Exp $

ALL: build_here

SOURCEC	  = ESMCI_ArraySpec.C ESMC_ArraySpec.C
SOURCEF	  =  ESMF_ArraySpec_C.F90
SOURCEH	  = 

# List all .h files which should be copied to common include dir
STOREH	  =  ESMCI_ArraySpec.h ESMC_ArraySpec.h

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/ArraySpec/interface

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

include $(ESMF_DIR)/makefile

DIRS	  = 