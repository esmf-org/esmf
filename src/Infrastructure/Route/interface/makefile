# $Id: makefile,v 1.9 2008/09/02 22:35:21 theurich Exp $

# leave alone.
ALL: build_here 

# if you have C files in this directory, list them all on the line below.
SOURCEC	  = ESMC_XPacket_F.C ESMC_RTable_F.C ESMC_CommTable_F.C ESMC_Route_F.C \
            ESMC_RHandle_F.C
# if you have F90 files in this directory, list them all on the line below.
SOURCEF	  = ESMF_XPacket.F90 ESMF_RTable.F90 ESMF_CommTable.F90 ESMF_Route.F90 \
            ESMF_RHandle.F90
# if you have .h include files in this dir, list them all on the line below.
SOURCEH	  = 
# list all .h files which should be copied to common include dir
STOREH	  = 

# leave these alone.
OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

# is MANSEC something about documentation?
MANSEC	  = Routeobj
LIBBASE	  = libesmf

# the following line must be set to the current subdir name relative
# to the top level esmf directory. (e.g. Base/src, TimeMgr/interface, etc)
LOCDIR	  = src/Infrastructure/Route/interface

include ${ESMF_DIR}/makefile

DIRS =

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

