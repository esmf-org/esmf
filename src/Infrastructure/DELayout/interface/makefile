# $Id$

ALL: build_here

SOURCEC	  = ESMCI_DELayout_F.C
SOURCEF	  = ESMF_DELayout.F90
SOURCEH	  = 

# List all .h files which should be copied to common include dir
STOREH	  = 

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/DELayout/interface

CLEANDIRS   =
CLEANFILES  = 
CLOBBERDIRS =

include ${ESMF_DIR}/makefile

DIRS	  = 
