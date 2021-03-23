# $Id$

# leave alone.
ALL: build_here 

ESMF_CXXCOMPILECPPFLAGS += -DIS_BUILDING_MB -DHAVE_VSNPRINTF -DLINUX -DUSE_MPI

SOURCEC = ElemEvaluator.cpp LinearHex.cpp LinearQuad.cpp \
  LinearTet.cpp LinearTri.cpp QuadraticHex.cpp 
	
	# SpectralHex.cpp SpectralQuad.cpp

SOURCEF =

SOURCEH =

# List all .h files which should be copied to common include dir
STOREH	  =

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/Mesh/src/Moab/LocalDiscretization

ESMF_CXXCOMPILECPPFLAGS += -I$(ESMF_DIR)/$(LOCDIR)/..

CLEANDIRS   =
CLEANFILES  = 
CLOBBERDIRS =

include $(ESMF_DIR)/makefile

DIRS	  =
