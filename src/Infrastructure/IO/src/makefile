# $Id$

ALL: build_here

SOURCEC   = ESMCI_IO_NetCDF.C ESMCI_IO.C ESMCI_IO_Handler.C ESMCI_IO_Gridspec.C ESMCI_IO_Scrip.C ESMCI_IO_YAML.C
SOURCEF   =
STOREH    = ESMCI_IO_NetCDF.h ESMCI_IO.h ESMCI_IO_Handler.h ESMCI_IO_Gridspec.h ESMCI_IO_Scrip.h ESMCI_IO_YAML.h

ifdef $(ESMF_PIO)
  ifneq ($(ESMF_PIO),'none')
    SOURCEC  += ESMCI_PIO_Handler.C
    # PIO_Handler not called directly (and would require PIO includes as well)
    STOREH   += ESMCI_PIO_Handler.h
  endif
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
