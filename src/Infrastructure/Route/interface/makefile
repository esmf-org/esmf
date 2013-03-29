# $Id$

ALL: build_here 

SOURCEC	  = ESMCI_RHandle_F.C ESMC_RHandle.C
SOURCEF	  = ESMF_RHandle.F90
SOURCEH	  = 

# list all .h files which should be copied to common include dir
STOREH	  = ESMC_RHandle.h

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/Route/interface

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

include ${ESMF_DIR}/makefile

DIRS =

