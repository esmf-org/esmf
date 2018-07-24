# $Id$

ALL: build_here

SOURCEC   = ESMCI_SAX2ReadHandler.C ESMCI_SAX2WriteHandler.C ESMCI_IO_XML.C ESMCI_IO_NetCDF.C ESMCI_IO.C ESMCI_IO_Handler.C ESMCI_IO_Gridspec.C ESMCI_IO_Scrip.C ESMCI_IO_YAML.C
SOURCEF   =
STOREH    = ESMCI_SAX2ReadHandler.h ESMCI_SAX2WriteHandler.h ESMCI_IO_XML.h ESMCI_IO_NetCDF.h ESMCI_IO.h ESMCI_IO_Handler.h ESMCI_IO_Gridspec.h ESMCI_IO_Scrip.h ESMCI_IO_YAML.h

ifeq ($(ESMF_PIO),internal)
SOURCEC  += ESMCI_PIO_Handler.C
# PIO_Handler not called directly (and would require PIO includes as well)
#STOREH   += ESMCI_PIO_Handler.h
endif

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE   = libesmf
LOCDIR    = src/Infrastructure/IO/src

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

include $(ESMF_DIR)/makefile

ifeq ($(ESMF_PIO),internal)
ESMF_CXXCOMPILEPATHSLOCAL += -I$(ESMF_DIR)/$(LOCDIR)/../PIO
endif

DIRS = 
