# $Id: makefile,v 1.12 2008/09/26 05:18:12 cdeluca Exp $

ALL: build_here 

SOURCEC	  = 
SOURCEF	  = ESMF_Init.F90 ESMF.F90 

# list all .h files which should be copied to common include dir
STOREH	  = 

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Superstructure/ESMFMod/src

CLEANDIRS  =
CLEANFILES =
CLOBBERDIS =

include ${ESMF_DIR}/makefile

DIRS	  = 

