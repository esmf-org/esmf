# $Id: makefile,v 1.22 2009/09/09 05:48:42 eschwab Exp $

ALL: build_here

SOURCEC   = ESMCI_IO_XML.C ESMCI_IO_NetCDF.C
SOURCEF   =
STOREH    = ESMCI_IO_XML.h ESMCI_IO_NetCDF.h

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE   = libesmf
LOCDIR    = src/Infrastructure/IO/src

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

include $(ESMF_DIR)/makefile

DIRS = 
