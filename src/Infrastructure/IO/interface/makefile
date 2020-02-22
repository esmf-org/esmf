# $Id$

ALL: build_here 

AUTOGEN = ESMF_IO_NCPutGet.F90
SOURCEC	  = ESMCI_IO_NetCDF_F.C ESMC_IOScrip2ESMF.C
SOURCEC  += ESMCI_IO_F.C ESMC_IO_Gridspec.C ESMC_IO_Scrip.C
SOURCEC  += ESMCI_IO_YAML_F.C
# TODO      ESMC_IO_XML.C ESMC_IO_NetCDF.C (C interface)
SOURCEF	  = ESMF_IO_NetCDF.F90 ESMF_IOUGrid.F90 ESMF_IOGridspec.F90 
SOURCEF  += ESMF_IOGridmosaic.F90 ESMF_IOScrip.F90 $(AUTOGEN) 
SOURCEF  += ESMF_IOFileTypeCheck.F90
SOURCEF  += ESMF_IO.F90 ESMF_IO_Gridspec_C.F90 ESMF_IO_Scrip_C.F90
SOURCEF  += ESMF_IO_YAML.F90
SOURCEH	  = 

# List all .h files which should be copied to common include dir
STOREH    = 
# TODO ESMC_IO_XML.h ESMC_IO_NetCDF.h (C interface)

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/IO/interface

CLEANDIRS   =
CLEANFILES  = $(AUTOGEN)
CLOBBERDIRS =

include $(ESMF_DIR)/makefile

DIRS = 
