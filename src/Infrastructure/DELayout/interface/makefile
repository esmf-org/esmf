# $Id: makefile,v 1.8 2008/09/26 04:16:47 cdeluca Exp $

# leave alone.
ALL: build_here

# If you have C files in this directory, list them all on the line below.
SOURCEC	  = ESMCI_DELayout_F.C
# If you have F90 files in this directory, list them all on the line below.
SOURCEF	  = ESMF_DELayout.F90
# If you have .h include files in this dir, list them all on the line below.
SOURCEH	  = 
# List all .h files which should be copied to common include dir
STOREH	  = 

# Leave these alone.
OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf

# The following line must be set to the current subdir name relative
# to the top level esmf directory. (e.g. Base/src, TimeMgr/interface, etc)
LOCDIR	  = src/Infrastructure/DELayout/interface

CLEANDIRS   =
CLEANFILES  = 
CLOBBERDIRS =

include ${ESMF_DIR}/makefile

# Leave the following line blank.
DIRS	  = 
