# $Id$

# leave alone.
ALL: build_here 

ESMF_CXXCOMPILECPPFLAGS += -DIS_BUILDING_MB -DHAVE_VSNPRINTF -DLINUX -DUSE_MPI

SOURCEC = NestedRefine.cpp
SOURCEF =

SOURCEH =

# List all .h files which should be copied to common include dir
STOREH	  =

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/Mesh/src/Moab/RefineMesh

ESMF_CXXCOMPILECPPFLAGS += -I$(ESMF_DIR)/$(LOCDIR)/.. \
                           -I$(ESMF_DIR)/$(LOCDIR)/../parallel

CLEANDIRS   =
CLEANFILES  = 
CLOBBERDIRS =

include $(ESMF_DIR)/makefile

DIRS	  =
