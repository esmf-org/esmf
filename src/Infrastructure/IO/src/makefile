# $Id$

ALL: build_here

SOURCEC   = ESMCI_IO_NetCDF.C ESMCI_IO.C ESMCI_IO_Handler.C ESMCI_IO_Gridspec.C ESMCI_IO_Scrip.C ESMCI_IO_YAML.C
SOURCEF   =
STOREH    = ESMCI_IO_NetCDF.h ESMCI_IO.h ESMCI_IO_Handler.h ESMCI_IO_Gridspec.h ESMCI_IO_Scrip.h ESMCI_IO_YAML.h

ifdef ESMF_PIO
  SOURCEC  += ESMCI_PIO_Handler.C
  STOREH   += ESMCI_PIO_Handler.h
endif

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE   = libesmf
LOCDIR    = src/Infrastructure/IO/src

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

include $(ESMF_DIR)/makefile

DIRS =
