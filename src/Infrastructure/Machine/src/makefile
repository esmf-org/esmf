# $Id: makefile,v 1.3 2004/02/13 15:37:15 nscollins Exp $

# leave alone.
ALL: build_here 

# don't set these unless absolutely necessary - they've got to be
#  architecture and compiler independent and should be inherited
#  from the upper level makefile if possible.
#  TODO:  move platform define to central esmf/build
CFLAGS    += -D${ESMF_ARCH} ${MPI_INCLUDE}
FFLAGS    =

# if you have C files in this directory, list them all on the line below.
SOURCEC	  = ESMC_Machine.C 
# if you have F90 files in this directory, list them all on the line below.
SOURCEF	  = 
# if you have private .h include files in this dir, list them on the line below.
SOURCEH	  = 
# if you have public .h include files in this dir, list them on the line below.
STOREH	  = ESMC_Machine.h

# leave these alone.
OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

MANSEC	  = Machobj
LIBBASE	  = libesmf

# the following line must be set to the current subdir name relative
# to the top level esmf directory. (e.g. Base/src, TimeMgr/interface, etc)
LOCDIR	  = src/Infrastructure/Machine/src

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

