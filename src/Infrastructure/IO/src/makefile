# $Id: makefile,v 1.20 2009/03/18 05:42:20 eschwab Exp $

ALL: build_here

SOURCEC   = ESMCI_IO.C
SOURCEF   =
STOREH    = ESMCI_IO.h

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE   = libesmf
LOCDIR    = src/Infrastructure/IO/src

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

include $(ESMF_DIR)/makefile

DIRS = 
