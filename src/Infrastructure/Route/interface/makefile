# $Id: makefile,v 1.10 2008/09/26 05:18:08 cdeluca Exp $

ALL: build_here 

SOURCEC	  = ESMC_XPacket_F.C ESMC_RTable_F.C ESMC_CommTable_F.C ESMC_Route_F.C \
            ESMC_RHandle_F.C
SOURCEF	  = ESMF_XPacket.F90 ESMF_RTable.F90 ESMF_CommTable.F90 ESMF_Route.F90 \
            ESMF_RHandle.F90
SOURCEH	  = 

# list all .h files which should be copied to common include dir
STOREH	  = 

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/Route/interface

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

include ${ESMF_DIR}/makefile

DIRS =

