# $Id: makefile,v 1.14 2011/05/18 15:48:37 samsoncheung Exp $

ALL: build_here 

SOURCEC	  = ESMCI_IO_XML_F.C ESMCI_IO_NetCDF_F.C ESMC_IOScrip2ESMF.C
# TODO      ESMC_IO_XML.C ESMC_IO_NetCDF.C (C interface)
SOURCEF	  = ESMF_IO_NetCDF.F90 ESMF_IOScrip.F90 ESMF_IO_C.F90
SOURCEH	  = 

# List all .h files which should be copied to common include dir
STOREH    = 
# TODO ESMC_IO_XML.h ESMC_IO_NetCDF.h (C interface)

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/IO/interface

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

include $(ESMF_DIR)/makefile

DIRS = 
