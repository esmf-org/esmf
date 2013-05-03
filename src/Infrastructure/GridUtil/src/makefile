# $Id$

ALL: build_here 

SOURCEC	  = ESMCI_GridToMesh.C
SOURCEF	  =
SOURCEH	  = 

# List all .h files which should be copied to common include dir
STOREH	  = ESMCI_GridToMesh.h ESMCI_Ptypes.h

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/GridUtil/src

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

include ${ESMF_DIR}/makefile

DIRS =

