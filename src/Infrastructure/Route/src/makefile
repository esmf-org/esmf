# $Id: makefile,v 1.11 2007/04/12 18:08:24 theurich Exp $

# leave alone.
ALL: build_here 

# Don't set these unless absolutely necessary - they've got to be
# architecture and compiler independent and should be inherited
# from the upper level makefile if possible.
CFLAGS    = ${MPI_INCLUDE}
FFLAGS    =

# If you have C files in this directory, list them all on the line below.
SOURCEC	  = ESMC_XPacket.C ESMC_RTable.C ESMC_CommTable.C ESMC_Route.C \
            ESMC_TValues.C ESMC_RHandle.C
# If you have F90 files in this directory, list them all on the line below.
SOURCEF	  = 
# If you have .h include files in this dir, list them all on the line below.
SOURCEH	  = 
# List all .h files which should be copied to common include dir
STOREH	  = ESMC_CommTable.h  ESMC_RHandle.h  ESMC_Route.h  ESMC_RTable.h  ESMC_TValues.h  ESMC_XPacket.h

# Leave these alone.
OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

MANSEC	  = Routeobj
LIBBASE	  = libesmf

# The following line must be set to the current subdir name relative
# to the top level esmf directory. (e.g. Base/src, TimeMgr/interface, etc)
LOCDIR	  = src/Infrastructure/Route/src


include ${ESMF_DIR}/makefile

DIRS =

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

