# $Id: makefile,v 1.24 2011/05/18 15:50:29 samsoncheung Exp $

ALL: build_here

SOURCEC   = ESMCI_SAX2ReadHandler.C ESMCI_SAX2WriteHandler.C ESMCI_IO_XML.C ESMCI_IO_NetCDF.C ESMCI_IO.C
SOURCEF   =
STOREH    = ESMCI_SAX2ReadHandler.h ESMCI_SAX2WriteHandler.h ESMCI_IO_XML.h ESMCI_IO_NetCDF.h ESMCI_IO.h

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE   = libesmf
LOCDIR    = src/Infrastructure/IO/src

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

include $(ESMF_DIR)/makefile

DIRS = 
