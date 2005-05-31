# $Id: makefile,v 1.2 2005/05/31 17:27:16 nscollins Exp $

ALL:  build_here

# This needs to be the path of this directory relative to the
# top level esmf directory.
LOCDIR	  = src/Infrastructure/Util/

# Leave this line alone.
include $(ESMF_DIR)/makefile

# This directory needs to be compiled first and contains generic utility
# routines which are used by all parts of the framework; e.g. string
# handling routines, getting time-of-day from the system for a timestamp, etc.

# The DIRS line needs to contain all subdirectories which exist 
# directly below this directory, and have either library,
# example/test code, or documents which need to be generated.
DIRS	  = src interface tests examples doc

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =


