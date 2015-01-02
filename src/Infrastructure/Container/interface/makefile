# $Id$

ALL: build_here

AUTOGEN   = 
SOURCEC	  = ESMCI_Container_F.C
SOURCEF	  = $(AUTOGEN) ESMF_Container.F90
SOURCEH	  = 

# List all .h files which should be copied to common include dir
STOREH	  = ESMCI_Container.h

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/Container/interface

CLEANDIRS   =
CLEANFILES  = $(AUTOGEN)
CLOBBERDIRS =

include ${ESMF_DIR}/makefile

DIRS	  = 
