# $Id: makefile,v 1.4 2008/09/26 05:18:03 cdeluca Exp $

ALL: build_here

SOURCEC	  = ESMCI_ArrayBundle_F.C
SOURCEF	  = ESMF_ArrayBundle.F90
SOURCEH	  = 

# List all .h files which should be copied to common include dir
STOREH	  = 

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/ArrayBundle/interface

CLEANDIRS   =
CLEANFILES  = 
CLOBBERDIRS =

include ${ESMF_DIR}/makefile

DIRS	  = 
