# $Id: makefile,v 1.14 2005/03/01 21:02:54 nscollins Exp $

ALL: build_here

CFLAGS    =
FFLAGS    = 
SOURCEC   = 
SOURCEF   = wrf_io.F field_routines.F module_wrf_error.F wrf_abort.F
STOREH    = 

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

MANSEC    = IOobj
LIBBASE   = libesmf

# This needs to be the path of this directory relative to the
# top level esmf directory.
LOCDIR    = src/Infrastructure/IO/src

# Leave this line alone.
include $(ESMF_DIR)/makefile

# The DIRS line needs to contain all subdirectories which exist 
# directly below this directory, and have either library,
# example/test code, or documents which need to be generated.
DIRS =

CLEANDIRS   =
CLEANFILES  = 
CLOBBERDIRS =

