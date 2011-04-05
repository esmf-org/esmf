# $Id: makefile,v 1.1 2011/04/05 15:10:28 theurich Exp $

ALL: build_here 

SOURCEC	  = 
SOURCEF	  = ESMF_AttachMethods.F90 

# list all .h files which should be copied to common include dir
STOREH	  = 

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Superstructure/AttachMethods/src

CLEANDIRS  =
CLEANFILES =
CLOBBERDIS =

include ${ESMF_DIR}/makefile

DIRS	  = 

