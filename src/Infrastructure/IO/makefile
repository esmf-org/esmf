# $Id: makefile,v 1.22 2012/02/08 19:57:40 theurich Exp $ 

ALL: build_here

# This needs to be the path of this directory relative to the
# top level esmf directory.
LOCDIR	  = src/Infrastructure/IO/

# Leave this line alone.
include $(ESMF_DIR)/makefile

# The DIRS line needs to contain all subdirectories which exist 
# directly below this directory, and have either library,
# example/test code, or documents which need to be generated.
DIRS      = 
ifeq ($(ESMF_PIO),internal)
DIRS     += PIO
endif
DIRS     += src interface tests doc

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

