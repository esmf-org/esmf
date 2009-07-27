# $Id: makefile,v 1.14 2009/07/27 23:23:34 theurich Exp $

ALL: build_here 

SOURCEC	  = ESMC_RHandle.C
SOURCEF	  = 
SOURCEH	  = 

# List all .h files which should be copied to common include dir
STOREH	  = ESMC_RHandle.h

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/Route/src

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

include ${ESMF_DIR}/makefile

DIRS =

