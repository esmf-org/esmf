# $Id: makefile,v 1.5 2004/03/23 18:30:30 nscollins Exp $

# Leave alone.
ALL: build_here 

# Don't set these unless absolutely necessary - they've got to be
# architecture and compiler independent and should be inherited
# from the upper level makefile if possible.
CFLAGS    += ${MPI_INCLUDE}
FFLAGS    =

# If you have C files in this directory, list them all on the line below.
SOURCEC	  = 
# If you have F90 files in this directory, list them all on the line below.
SOURCEF	  = ESMF_State.F90  ESMF_StateGet.F90
# If you have .h include files in this dir, list them all on the line below.
SOURCEH	  = 
# If you have public .h files in this dir, list them here
STOREH    = ESMC_State.h

# Leave these alone.
OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

MANSEC	  = Stateobj
LIBBASE	  = libesmf


# The following line must be set to the current subdir name relative
# to the top level esmf directory. (e.g. Base/src, TimeMgr/interface, etc)
LOCDIR	  = src/Superstructure/State/src

include $(ESMF_DIR)/makefile

# leave the following line blank.
DIRS	  = 

CLEANDIRS   =
CLEANFILES  = ESMF_StateGet.F90
CLOBBERDIRS =

ESMF_StateGet.o: ESMF_StateGet.cpp ESMF_StateMacros.h ESMF_State.o

