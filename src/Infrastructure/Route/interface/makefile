# $Id: makefile,v 1.11 2009/07/27 23:23:34 theurich Exp $

ALL: build_here 

SOURCEC	  = ESMC_RHandle_F.C
SOURCEF	  = ESMF_RHandle.F90
SOURCEH	  = 

# list all .h files which should be copied to common include dir
STOREH	  = 

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/Route/interface

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

include ${ESMF_DIR}/makefile

DIRS =

