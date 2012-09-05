# $Id: makefile,v 1.1 2012/09/05 16:59:33 theurich Exp $

ALL: build_here 

SOURCEC	  = 
SOURCEF	  = ESMF_RegridWeightGen.F90 

# list all .h files which should be copied to common include dir
STOREH	  = 

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Superstructure/PreESMFMod/src

CLEANDIRS  =
CLEANFILES =
CLOBBERDIS =

include ${ESMF_DIR}/makefile

DIRS	  = 

