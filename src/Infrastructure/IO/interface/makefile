# $Id: makefile,v 1.9 2009/02/18 06:49:04 eschwab Exp $

ALL: build_here 

SOURCEC	  = 
SOURCEF	  = ESMF_IO.F90
SOURCEH	  = 

# List all .h files which should be copied to common include dir
STOREH    = 

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/IO/interface

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

include $(ESMF_DIR)/makefile

DIRS = 
