# $Id: makefile,v 1.11 2009/07/07 06:00:17 eschwab Exp $

ALL: build_here 

SOURCEC	  = ESMCI_IO_F.C ESMCI_IO_NetCDF_F.C
# TODO      ESMC_IO.C ESMC_IO_NetCDF.C (C interface)
SOURCEF	  = ESMF_IO_NetCDF.F90
SOURCEH	  = 

# List all .h files which should be copied to common include dir
STOREH    = 
# TODO ESMC_IO.h ESMC_IO_NetCDF.h (C interface)

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/IO/interface

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

include $(ESMF_DIR)/makefile

DIRS = 
