# $Id: makefile,v 1.9 2004/08/03 19:33:49 nscollins Exp $

# leave alone.
ALL: build_here 

# Don't set these unless absolutely necessary - they've got to be
# architecture and compiler independent and should be inherited
# from the upper level makefile if possible.
CFLAGS    =
FFLAGS    =

# If you have C files in this directory, list them all on the line below.
SOURCEC	  = 
# If you have F90 files in this directory, list them all on the line below.
SOURCEF	  = ESMF_PhysCoord.F90 ESMF_PhysGrid.F90
# If you have .h include files in this dir, list them all on the line below.
SOURCEH	  = 

# Leave these alone.
OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

MANSEC	  = PhysGridobj
LIBBASE	  = libesmf


# The following line must be set to the current subdir name relative
# to the top level esmf directory. (e.g. Base/src, TimeMgr/interface, etc)
LOCDIR	  = src/Infrastructure/PhysGrid/src

include $(ESMF_DIR)/makefile

# Leave the following line blank.
DIRS	  = 

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =



# If you have class files which need to be compiled in a particular
# order, list the dependencies below, with no rules following.
#
# e.g.:
#
# ESMF_Field.o:   ESMF_DataArray.o
#
# ESMF_Bundle.o:  ESMF_Field.o
#




