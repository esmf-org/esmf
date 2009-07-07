# $Id: makefile,v 1.21 2009/07/07 06:00:17 eschwab Exp $

ALL: build_here

SOURCEC   = ESMCI_IO.C ESMCI_IO_NetCDF.C
SOURCEF   =
STOREH    = ESMCI_IO.h ESMCI_IO_NetCDF.h

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE   = libesmf
LOCDIR    = src/Infrastructure/IO/src

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

include $(ESMF_DIR)/makefile

DIRS = 
