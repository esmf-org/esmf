# $Id: makefile,v 1.4 2008/09/02 22:35:19 theurich Exp $

# Leave alone.
ALL: build_here 

# If you have C files in this directory, list them all on the line below.
SOURCEC	  = ESMC_InternArray.C 
# If you have F90 files in this directory, list them all on the line below.
SOURCEF	  = ESMF_InternArrayDataMap.F90
# If you have .h include files in this dir, list them all on the line below.
STOREH	  = ESMC_InternArray.h

# Leave these alone.
OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

MANSEC	  = InternArrayobj
LIBBASE	  = libesmf



# The following line must be set to the current subdir name relative
# to the top level esmf directory. (e.g. Base/src, TimeMgr/interface, etc)
LOCDIR	  = src/Infrastructure/InternArray/src

include $(ESMF_DIR)/makefile

# Leave the following line blank.
DIRS	  = 

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

