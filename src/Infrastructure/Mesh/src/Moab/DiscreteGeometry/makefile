# $Id$

# leave alone.
ALL: build_here 

ESMF_CXXCOMPILECPPFLAGS += -DIS_BUILDING_MB -DHAVE_VSNPRINTF -DUSE_MPI
# -DLINUX selects MOAB's Unix code paths (dlfcn.h, dirent.h in Core.cpp).
# Those headers do not exist on MinGW; leave LINUX undefined there so MOAB
# uses its POSIX/Windows-safe fallbacks. Kept for Linux and Darwin.
ifneq ($(ESMF_OS),MinGW)
ESMF_CXXCOMPILECPPFLAGS += -DLINUX
endif

SOURCEC = \
  DGMSolver.cpp HiReconstruction.cpp

SOURCEF =

SOURCEH =

# List all .h files which should be copied to common include dir
STOREH    =

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE   = libesmf
LOCDIR    = src/Infrastructure/Mesh/src/Moab/DiscreteGeometry

ESMF_CXXCOMPILECPPFLAGS += \
                           -I$(ESMF_DIR)/$(LOCDIR)/.. \
                           -I$(ESMF_DIR)/$(LOCDIR)/../parallel


CLEANDIRS   =
CLEANFILES  = 
CLOBBERDIRS =

include $(ESMF_DIR)/makefile

DIRS        = 
