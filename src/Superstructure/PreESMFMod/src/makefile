# $Id$

ALL: build_here 

SOURCEC	  = 
SOURCEF	  = ESMF_RegridWeightGenCheck.F90 ESMF_RegridWeightGen.F90 ESMF_FileRegrid.F90

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

