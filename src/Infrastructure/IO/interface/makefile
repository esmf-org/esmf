# $Id: makefile,v 1.10 2009/03/18 05:45:14 eschwab Exp $

ALL: build_here 

SOURCEC	  = ESMCI_IO_F.C
# TODO      ESMC_IO.C
SOURCEF	  = 
SOURCEH	  = 

# List all .h files which should be copied to common include dir
STOREH    = 
# TODO ESMC_IO.h

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/IO/interface

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

include $(ESMF_DIR)/makefile

DIRS = 
