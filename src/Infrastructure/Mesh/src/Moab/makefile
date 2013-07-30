# $Id$

# leave alone.
ALL: build_here 

ESMF_CXXCOMPILECPPFLAGS += -I. -Iio -Iparallel \
	-DIS_BUILDING_MB -DHAVE_VSNPRINTF -DLINUX -DUSE_MPI

SOURCEC = \
	AdaptiveKDTree.cpp AEntityFactory.cpp AffineXform.cpp AxisBox.cpp \
	BitPage.cpp BitTag.cpp BSPTree.cpp BSPTreePoly.cpp \
	CartVect.cpp CN.cpp Core.cpp \
	DebugOutput.cpp DenseTag.cpp DualTool.cpp \
	EntitySequence.cpp \
	Factory.cpp FBEngine.cpp FileOptions.cpp \
	GeomTopoTool.cpp GeomUtil.cpp \
	HigherOrderFactory.cpp HomXform.cpp \
	MergeMesh.cpp MeshSet.cpp MeshSetSequence.cpp MeshTag.cpp \
	MeshTopoUtil.cpp \
	OrientedBox.cpp OrientedBoxTreeTool.cpp \
	PolyElementSeq.cpp ProgOptions.cpp \
	Range.cpp RangeSeqIntersectIter.cpp ReaderWriterSet.cpp \
	ReadUtil.cpp ReorderTool.cpp \
	ScdElementData.cpp ScdInterface.cpp ScdVertexData.cpp SequenceData.cpp \
	SequenceManager.cpp SetIterator.cpp Skinner.cpp SmoothCurve.cpp \
	SmoothFace.cpp SparseTag.cpp SpectralMeshTool.cpp StructuredElementSeq.cpp \
	SweptElementData.cpp SweptElementSeq.cpp SweptVertexData.cpp SysUtil.cpp \
	TagInfo.cpp Types.cpp TypeSequenceManager.cpp \
	UnstructuredElemSeq.cpp Util.cpp \
	VarLenDenseTag.cpp VarLenSparseTag.cpp VertexSequence.cpp \
	WriteUtil.cpp

# Not currently compiled:

SOURCEF =

SOURCEH =

# List all .h files which should be copied to common include dir
STOREH	  =

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/Mesh/src/Moab

ESMF_CXXCOMPILECPPFLAGS += \
	-I$(ESMF_DIR)/$(LOCDIR)/. \
	-I$(ESMF_DIR)/$(LOCDIR)/io/. \
	-I$(ESMF_DIR)/$(LOCDIR)/moab/. \
	-I$(ESMF_DIR)/$(LOCDIR)/parallel/.

CLEANDIRS   =
CLEANFILES  = 
CLOBBERDIRS =

include $(ESMF_DIR)/makefile

DIRS	  = io parallel
