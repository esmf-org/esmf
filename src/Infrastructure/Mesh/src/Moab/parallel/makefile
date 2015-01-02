# $Id$

# leave alone.
ALL: build_here 

ESMF_CXXCOMPILECPPFLAGS += -I. -Iio -Iparallel \
	-DIS_BUILDING_MB -DHAVE_VSNPRINTF -DLINUX -DUSE_MPI

SOURCEC = \
	gs.cpp            ParallelMergeMesh.cpp  SharedSetData.cpp \
	ParallelComm.cpp  ProcConfig.cpp         TupleList.cpp \
	ParallelData.cpp  ReadParallel.cpp
#	WriteHDF5Parallel.cpp

SOURCEF =

SOURCEH =

# List all .h files which should be copied to common include dir
STOREH	  =

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/Mesh/src/Moab/parallel

ESMF_CXXCOMPILECPPFLAGS += \
	-I$(ESMF_DIR)/$(LOCDIR)/. \
	-I$(ESMF_DIR)/$(LOCDIR)/.. \
	-I$(ESMF_DIR)/$(LOCDIR)/../io \
	-I$(ESMF_DIR)/$(LOCDIR)/../io/mhdf/include

CLEANDIRS   =
CLEANFILES  = 
CLOBBERDIRS =

include $(ESMF_DIR)/makefile
