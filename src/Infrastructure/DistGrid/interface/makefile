# $Id: makefile,v 1.12 2008/03/05 05:50:41 theurich Exp $

# leave alone.
ALL: build_here

# If you have C files in this directory, list them all on the line below.
SOURCEC	  = ESMCI_DistGrid_F.C ESMC_DistGrid.C
# If you have F90 files in this directory, list them all on the line below.
SOURCEF	  = ESMF_DistGrid.F90
# If you have .h include files in this dir, list them all on the line below.
SOURCEH	  = 
# List all .h files which should be copied to common include dir
STOREH	  = ESMC_DistGrid.h

# Leave these alone.
OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

MANSEC	  = DistGridobj
LIBBASE	  = libesmf

# The following line must be set to the current subdir name relative
# to the top level esmf directory. (e.g. Base/src, TimeMgr/interface, etc)
LOCDIR	  = src/Infrastructure/DistGrid/interface

CLEANDIRS   =
CLEANFILES  = 
CLOBBERDIRS =

include ${ESMF_DIR}/makefile

# Leave the following line blank.
DIRS	  = 
