# $Id$

# leave alone.
ALL: build_here 

ESMF_CXXCOMPILECPPFLAGS += -DIS_BUILDING_MB -DHAVE_VSNPRINTF -DLINUX -DUSE_MPI

SOURCEC = \
  AdaptiveKDTree.cpp AEntityFactory.cpp AffineXform.cpp AxisBox.cpp \
  BitPage.cpp BitTag.cpp BoundBox.cpp BSPTree.cpp BSPTreePoly.cpp BVHTree.cpp \
  CartVect.cpp CN.cpp Core.cpp \
  DebugOutput.cpp DenseTag.cpp DualTool.cpp \
  EntitySequence.cpp ErrorHandler.cpp ErrorOutput.cpp \
  Factory.cpp FBEngine.cpp FileOptions.cpp \
  GeomQueryTool.cpp GeomTopoTool.cpp GeomUtil.cpp \
  HalfFacetRep.cpp HigherOrderFactory.cpp HomXform.cpp \
  iMOAB.cpp \
  LloydSmoother.cpp\
  MergeMesh.cpp MeshGeneration.cpp MeshSet.cpp MeshSetSequence.cpp MeshTag.cpp \
  MeshTopoUtil.cpp \
  NestedRefine.cpp \
  OrientedBox.cpp OrientedBoxTreeTool.cpp \
  PolyElementSeq.cpp ProgOptions.cpp \
  Range.cpp RangeSeqIntersectIter.cpp ReaderWriterSet.cpp \
  ReadUtil.cpp ReorderTool.cpp \
  ScdElementData.cpp ScdInterface.cpp ScdVertexData.cpp SequenceData.cpp \
  SequenceManager.cpp SetIterator.cpp Skinner.cpp SmoothCurve.cpp \
  SmoothFace.cpp SparseTag.cpp SpatialLocator.cpp \
  SpectralMeshTool.cpp StructuredElementSeq.cpp \
  SweptElementData.cpp SweptElementSeq.cpp SweptVertexData.cpp SysUtil.cpp \
  TagInfo.cpp Tree.cpp TupleList.cpp Types.cpp TypeSequenceManager.cpp \
  UnstructuredElemSeq.cpp Util.cpp \
  VarLenDenseTag.cpp VarLenSparseTag.cpp VertexSequence.cpp \
  WriteUtil.cpp


SOURCEF =

SOURCEH =

# List all .h files which should be copied to common include dir
STOREH    =

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE   = libesmf
LOCDIR    = src/Infrastructure/Mesh/src/Moab

ESMF_CXXCOMPILECPPFLAGS += \
                           -I$(ESMF_DIR)/$(LOCDIR)/DiscreteGeometry \
                           -I$(ESMF_DIR)/$(LOCDIR)/IntxMesh \
                           -I$(ESMF_DIR)/$(LOCDIR)/io \
                           -I$(ESMF_DIR)/$(LOCDIR)/io/mhdf/include \
                           -I$(ESMF_DIR)/$(LOCDIR)/LocalDiscretization \
                           -I$(ESMF_DIR)/$(LOCDIR)/parallel \
                           -I$(ESMF_DIR)/$(LOCDIR)/verdict

#                           -I$(ESMF_DIR)/$(LOCDIR)/../Zoltan/.

CLEANDIRS   =
CLEANFILES  = 
CLOBBERDIRS =

include $(ESMF_DIR)/makefile

DIRS        = DiscreteGeometry IntxMesh io LocalDiscretization parallel verdict

# not building files in HYPRE, lotte, mesquite, Remapping directories