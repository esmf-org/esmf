# $Id: makefile,v 1.15.2.1 2005/10/25 22:06:02 jwolfe Exp $

ALL: build_here

CFLAGS    =
FFLAGS    =
SOURCEC   = 
ifeq ($(ESMF_NO_IOCODE),true)
SOURCEF   = wrf_io.F90
else
SOURCEF   = wrf_io.F90 field_routines.F90 module_wrf_error.F wrf_abort.F
endif
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

