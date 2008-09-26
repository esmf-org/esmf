# $Id: makefile,v 1.13 2008/09/26 05:53:54 cdeluca Exp $

ALL: build_here 

SOURCEC	  = ESMC_XPacket.C ESMC_RTable.C ESMC_CommTable.C ESMC_Route.C \
            ESMC_TValues.C ESMC_RHandle.C
SOURCEF	  = 
SOURCEH	  = 

# List all .h files which should be copied to common include dir
STOREH	  = ESMC_CommTable.h  ESMC_RHandle.h  ESMC_Route.h  ESMC_RTable.h  ESMC_TValues.h  ESMC_XPacket.h

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/Route/src

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

include ${ESMF_DIR}/makefile

DIRS =

