# $Id: makefile,v 1.1 2010/02/25 19:59:50 oehmke Exp $

ALL: build_here 

SOURCEC	  = 
SOURCEF	  = ESMF_GeomBase.F90
SOURCEH	  = 

# List all .h files which should be copied to common include dir
STOREH	  = 

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/GeomBase/src

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

include ${ESMF_DIR}/makefile

DIRS =

