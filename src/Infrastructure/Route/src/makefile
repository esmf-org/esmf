# $Id: makefile,v 1.15 2009/07/28 17:34:44 theurich Exp $

ALL: build_here 

SOURCEC	  = ESMCI_RHandle.C
SOURCEF	  = 
SOURCEH	  = 

# List all .h files which should be copied to common include dir
STOREH	  = ESMCI_RHandle.h

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/Route/src

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

include ${ESMF_DIR}/makefile

DIRS =

