# $Id: makefile,v 1.7 2009/02/26 06:51:37 theurich Exp $

ALL: build_here 

SOURCEC	  = ESMC_GridToMesh.C
SOURCEF	  = ESMF_GeomBase.F90
SOURCEH	  = 

# List all .h files which should be copied to common include dir
STOREH	  = 

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/GridUtil/src

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

include ${ESMF_DIR}/makefile

DIRS =

