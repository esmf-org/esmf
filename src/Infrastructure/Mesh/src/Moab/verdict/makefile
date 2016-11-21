# $Id$

# leave alone.
ALL: build_here 

ESMF_CXXCOMPILECPPFLAGS += -DIS_BUILDING_MB -DHAVE_VSNPRINTF -DLINUX -DUSE_MPI

SOURCEC = V_EdgeMetric.cpp VerdictVector.cpp VerdictWrapper.cpp \
  V_GaussIntegration.cpp V_HexMetric.cpp V_KnifeMetric.cpp \
  V_PyramidMetric.cpp V_QuadMetric.cpp V_TetMetric.cpp \
  V_TriMetric.cpp V_WedgeMetric.cpp

SOURCEF =

SOURCEH =

# List all .h files which should be copied to common include dir
STOREH	  =

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/Mesh/src/Moab/verdict

ESMF_CXXCOMPILECPPFLAGS += -I$(ESMF_DIR)/$(LOCDIR)/..

CLEANDIRS   =
CLEANFILES  = 
CLOBBERDIRS =

include $(ESMF_DIR)/makefile

DIRS	  =
