# $Id$

ALL: build_here

SOURCEC	  = ESMCI_HConfig_F.C
SOURCEF	  = ESMF_HConfig.F90
SOURCEH	  =

# List all .h files which should be copied to common include dir
#STOREH	  = ESMC_HConfig.h

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/HConfig/interface

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

include ${ESMF_DIR}/makefile

DIRS	  =
