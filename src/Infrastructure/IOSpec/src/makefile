# $Id: makefile,v 1.2 2004/03/18 16:24:26 nscollins Exp $

ALL: build_here

CFLAGS    =
FFLAGS    =
SOURCEC   = 
SOURCEF   = ESMF_IOSpec.F90
STOREH    = 

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

MANSEC    = IOSpecobj
LIBBASE   = libesmf
DIRS      =
LOCDIR    = src/Infrastructure/IOSpec/src

include $(ESMF_DIR)/makefile

DIRS =

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =
