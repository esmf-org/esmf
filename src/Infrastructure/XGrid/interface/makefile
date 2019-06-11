# $Id$ 

ALL: build_here 

SOURCEC	  = ESMCI_XGrid_F.C ESMCI_XGrid.C ESMC_XGrid.C
SOURCEF	  = ESMF_XGrid_C.F90

# List all .h files which should be copied to a common include dir
STOREH	  = ESMC_XGrid.h

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/XGrid/interface

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

include $(ESMF_DIR)/makefile

DIRS	  = 
