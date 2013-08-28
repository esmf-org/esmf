# $Id$

# leave alone.
ALL: build_here 

ESMF_CXXCOMPILECPPFLAGS += -I. -Iio -Iparallel \
	-DIS_BUILDING_MB -DHAVE_VSNPRINTF -DLINUX -DUSE_MPI

SOURCEC = \
	ExoIIUtil.cpp FileTokenizer.cpp GmshUtil.cpp \
	IODebugTrack.cpp ReadABAQUS.cpp ReadGmsh.cpp \
	ReadIDEAS.cpp ReadMCNP5.cpp \
	ReadNASTRAN.cpp ReadSmf.cpp ReadSms.cpp \
	ReadSTL.cpp ReadTemplate.cpp ReadTetGen.cpp ReadTxt.cpp ReadVtk.cpp \
	SMF_State.cpp Tqdcfr.cpp VtkUtil.cpp WriteAns.cpp \
	WriteGmsh.cpp WriteGMV.cpp \
	WriteSmf.cpp WriteSTL.cpp WriteTemplate.cpp WriteVtk.cpp

ifdef $(ESMF_NETCDF)
SOURCEC +=  ReadGCRM.cpp ReadNC.cpp ReadNCDF.cpp
endif

# Not currently compiled:
#	DamselUtil.cpp HDF5Common.cpp
#       ReadCCMIO.cpp ReadCGM.cpp ReadDamsel.cpp
#       ReadHDF5.cpp ReadHDF5Dataset.cpp ReadHDF5VarLen.cpp
#       WriteCCMIO.cpp WriteDamsel.cpp
#       WriteHDF5.cpp WriteNCDF.cpp WriteSLAC.cpp 

SOURCEF =

SOURCEH =

# List all .h files which should be copied to common include dir
STOREH	  =

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/Mesh/src/Moab/io

ESMF_CXXCOMPILECPPFLAGS += \
	-I$(ESMF_DIR)/$(LOCDIR)/. \
	-I$(ESMF_DIR)/$(LOCDIR)/.. \
	-I$(ESMF_DIR)/$(LOCDIR)/../parallel

CLEANDIRS   =
CLEANFILES  = 
CLOBBERDIRS =

include $(ESMF_DIR)/makefile

DIRS	  =
