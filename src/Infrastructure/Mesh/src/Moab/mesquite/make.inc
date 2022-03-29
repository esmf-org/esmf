MSQ_SRCS=
MSQ_HDRS=
MSQ_INCLUDES =

MSQ_INCLUDES += -I$(top_srcdir)/src/mesquite/include \
                -I$(top_builddir)/src/mesquite/include \
                -I$(top_srcdir)/src/mesquite/Mesh

if HAVE_MPI
  MPI_SRCS = Mesh/ParallelHelper.cpp
  MPI_INCS = Mesh/ParallelHelper.hpp
else
  MPI_SRCS = Mesh/ParallelHelperNoMPIStub.cpp
  MPI_INCS = Mesh/ParallelHelperNoMPIStub.hpp
endif

MSQ_SRCS += \
	Mesh/ArrayMesh.cpp \
	Mesh/ElementPatches.cpp \
	Mesh/ExtraData.cpp \
	Mesh/GlobalPatch.cpp \
	Mesh/IdealElements.cpp \
	Mesh/MeshDecorator.cpp \
	Mesh/MeshImpl.cpp \
	Mesh/MeshImplData.cpp \
	Mesh/MeshImplTags.cpp \
	Mesh/MeshUtil.cpp \
	Mesh/MeshWriter.cpp \
	Mesh/MsqMeshEntity.cpp \
	Mesh/MsqVertex.cpp \
	Mesh/ParallelMeshImpl.cpp \
	Mesh/PatchData.cpp \
	Mesh/PatchIterator.cpp \
	Mesh/PatchSet.cpp \
	Mesh/TagVertexMesh.cpp \
	Mesh/TopologyInfo.cpp \
	Mesh/VertexPatches.cpp \
	$(MPI_SRCS) 

MSQ_HDRS += \
	Mesh/ArrayMesh.hpp \
	Mesh/ElementPatches.hpp \
	Mesh/ExtraData.hpp \
	Mesh/ExtraDataUser.hpp \
	Mesh/GlobalPatch.hpp \
	Mesh/IdealElements.hpp \
	Mesh/MeshDecorator.hpp \
	Mesh/MeshImpl.hpp \
	Mesh/MeshImplData.hpp \
	Mesh/MeshImplTags.hpp \
	Mesh/MeshUtil.hpp \
	Mesh/MeshWriter.hpp \
	Mesh/MsqFreeVertexIndexIterator.hpp \
	Mesh/MsqMeshEntity.hpp \
	Mesh/MsqVertex.hpp \
	Mesh/ParallelMeshImpl.hpp \
	Mesh/PatchData.hpp \
	Mesh/PatchIterator.hpp \
	Mesh/PatchSet.hpp \
	Mesh/TagVertexMesh.hpp \
	Mesh/TopologyInfo.hpp \
	Mesh/VertexPatches.hpp \
	$(MPI_INCS) 

MSQ_INCLUDES += -I$(top_srcdir)/src/mesquite/Control

MSQ_SRCS += \
	Control/Instruction.cpp \
	Control/InstructionQueue.cpp \
	Control/IQInterface.cpp \
	Control/TerminationCriterion.cpp \
        Control/Settings.cpp \
        Control/SlaveBoundaryVertices.cpp 

MSQ_HDRS += \
	Control/Instruction.hpp \
	Control/InstructionQueue.hpp \
	Control/IQInterface.hpp \
	Control/TerminationCriterion.hpp \
        Control/Settings.hpp  \
        Control/SlaveBoundaryVertices.hpp \
        Control/VertexSlaver.hpp 

MSQ_INCLUDES += -I$(top_srcdir)/src/mesquite/Wrappers

MSQ_SRCS += \
	Wrappers/DeformingDomainWrapper.cpp \
	Wrappers/LaplaceWrapper.cpp \
	Wrappers/ShapeImprovementWrapper.cpp \
	Wrappers/ShapeImprover.cpp \
	Wrappers/SizeAdaptShapeWrapper.cpp \
	Wrappers/PaverMinEdgeLengthWrapper.cpp \
	Wrappers/UntangleWrapper.cpp \
	Wrappers/ViscousCFDTetShapeWrapper.cpp \
	Wrappers/Wrapper.cpp

MSQ_HDRS += \
	Wrappers/DeformingDomainWrapper.hpp \
	Wrappers/LaplaceWrapper.hpp \
	Wrappers/ShapeImprovementWrapper.hpp \
	Wrappers/ShapeImprover.hpp \
	Wrappers/SizeAdaptShapeWrapper.hpp \
	Wrappers/PaverMinEdgeLengthWrapper.hpp \
	Wrappers/UntangleWrapper.hpp \
	Wrappers/ViscousCFDTetShapeWrapper.hpp \
	Wrappers/Wrapper.hpp

MSQ_INCLUDES += -I$(top_srcdir)/src/mesquite/MappingFunction

MSQ_SRCS += \
	MappingFunction/MappingFunction.cpp \
	MappingFunction/NodeSet.cpp \
        MappingFunction/Sample.cpp

MSQ_HDRS += \
	MappingFunction/MappingFunction.hpp \
	MappingFunction/NodeSet.hpp  \
        MappingFunction/Sample.hpp

MSQ_INCLUDES += -I$(top_srcdir)/src/mesquite/MappingFunction/Lagrange

MSQ_SRCS += \
	MappingFunction/Lagrange/HexLagrangeShape.cpp \
	MappingFunction/Lagrange/QuadLagrangeShape.cpp \
	MappingFunction/Lagrange/TetLagrangeShape.cpp \
	MappingFunction/Lagrange/TriLagrangeShape.cpp 

MSQ_HDRS += \
	MappingFunction/Lagrange/HexLagrangeShape.hpp \
	MappingFunction/Lagrange/QuadLagrangeShape.hpp \
	MappingFunction/Lagrange/TetLagrangeShape.hpp \
	MappingFunction/Lagrange/TriLagrangeShape.hpp 

MSQ_INCLUDES += -I$(top_srcdir)/src/mesquite/MappingFunction/Linear

MSQ_SRCS += \
	MappingFunction/Linear/LinearHexahedron.cpp \
	MappingFunction/Linear/LinearPrism.cpp \
	MappingFunction/Linear/LinearPyramid.cpp \
	MappingFunction/Linear/LinearQuadrilateral.cpp \
	MappingFunction/Linear/LinearTetrahedron.cpp \
	MappingFunction/Linear/LinearTriangle.cpp 

MSQ_HDRS += \
	MappingFunction/Linear/LinearHexahedron.hpp \
	MappingFunction/Linear/LinearPrism.hpp \
	MappingFunction/Linear/LinearPyramid.hpp \
	MappingFunction/Linear/LinearQuadrilateral.hpp \
	MappingFunction/Linear/LinearTetrahedron.hpp \
	MappingFunction/Linear/LinearTriangle.hpp 

MSQ_INCLUDES += -I$(top_srcdir)/src/mesquite/Misc

if ENABLE_imesh
  MSQ_ITAPS_COMMON_SRC = Misc/MsqIBase.cpp \
                         Misc/MsqIBase.hpp
else
  MSQ_ITAPS_COMMON_SRC =
endif

MSQ_SRCS += \
	Misc/BoundedCylinderDomain.cpp \
	Misc/ConicDomain.cpp \
	Misc/CylinderDomain.cpp \
	Misc/DomainClassifier.cpp \
	Misc/DomainUtil.cpp \
	Misc/DomainUtil.hpp \
	Misc/EdgeIterator.cpp \
	Misc/Exponent.cpp \
	Misc/FileTokenizer.cpp \
	Misc/MeshDomain1D.cpp \
	Misc/MeshTransform.cpp \
	Misc/MsqDebug.cpp \
	Misc/MsqError.cpp \
	Misc/MsqFPE.cpp \
	Misc/MsqGeomPrim.cpp \
	Misc/MsqHessian.cpp \
	Misc/MsqInterrupt.cpp \
	Misc/MsqTimer.cpp \
	Misc/PlanarDomain.cpp \
	Misc/SimpleStats.cpp \
	Misc/SphericalDomain.cpp \
	Misc/Vector3D.cpp \
	Misc/VtkTypeInfo.cpp \
	Misc/XYPlanarDomain.cpp \
	Misc/XYRectangle.cpp \
        $(MSQ_ITAPS_COMMON_SRC)


MSQ_HDRS += \
	Misc/BoundedCylinderDomain.hpp \
	Misc/Bits.hpp \
	Misc/ConicDomain.hpp \
	Misc/CylinderDomain.hpp \
	Misc/DomainClassifier.hpp \
	Misc/EdgeIterator.hpp \
	Misc/Exponent.hpp \
	Misc/FileTokenizer.hpp \
	Misc/Matrix3D.hpp \
	Misc/MeshDomain1D.hpp \
	Misc/MeshTransform.hpp \
	Misc/MsqDebug.hpp \
	Misc/MsqError.hpp \
	Misc/MsqFPE.hpp \
	Misc/MsqGeomPrim.hpp \
	Misc/MsqHessian.hpp \
	Misc/MsqInterrupt.hpp \
	Misc/MsqMatrix.hpp \
	Misc/MsqTimer.hpp \
	Misc/PlanarDomain.hpp \
	Misc/SimpleStats.hpp \
	Misc/SphericalDomain.hpp \
	Misc/SymMatrix3D.hpp \
	Misc/Vector3D.hpp \
	Misc/VtkTypeInfo.hpp \
	Misc/XYPlanarDomain.hpp \
	Misc/XYRectangle.hpp

MSQ_INCLUDES += -I$(top_srcdir)/src/mesquite/ObjectiveFunction

MSQ_SRCS += \
	ObjectiveFunction/CompositeOFAdd.cpp \
	ObjectiveFunction/CompositeOFMultiply.cpp \
	ObjectiveFunction/CompositeOFScalarAdd.cpp \
	ObjectiveFunction/CompositeOFScalarMultiply.cpp \
	ObjectiveFunction/LInfTemplate.cpp \
	ObjectiveFunction/LPtoPTemplate.cpp \
	ObjectiveFunction/MaxTemplate.cpp \
	ObjectiveFunction/OFEvaluator.cpp \
	ObjectiveFunction/ObjectiveFunction.cpp \
	ObjectiveFunction/ObjectiveFunctionTemplate.cpp \
	ObjectiveFunction/PatchPowerMeanP.cpp \
	ObjectiveFunction/PMeanPTemplate.cpp \
	ObjectiveFunction/StdDevTemplate.cpp \
	ObjectiveFunction/VarianceTemplate.cpp

MSQ_HDRS += \
	ObjectiveFunction/CompositeOFAdd.hpp \
	ObjectiveFunction/CompositeOFMultiply.hpp \
	ObjectiveFunction/CompositeOFScalarAdd.hpp \
	ObjectiveFunction/CompositeOFScalarMultiply.hpp \
	ObjectiveFunction/LInfTemplate.hpp \
	ObjectiveFunction/LPtoPTemplate.hpp \
	ObjectiveFunction/MaxTemplate.hpp \
	ObjectiveFunction/OFEvaluator.hpp \
	ObjectiveFunction/ObjectiveFunction.hpp \
	ObjectiveFunction/ObjectiveFunctionTemplate.hpp \
	ObjectiveFunction/PatchPowerMeanP.hpp \
	ObjectiveFunction/PMeanPTemplate.hpp \
	ObjectiveFunction/StdDevTemplate.hpp \
	ObjectiveFunction/VarianceTemplate.hpp

MSQ_INCLUDES += -I$(top_srcdir)/src/mesquite/QualityAssessor

MSQ_SRCS += QualityAssessor/QualityAssessor.cpp 

MSQ_HDRS += QualityAssessor/QualityAssessor.hpp 

MSQ_INCLUDES += -I$(top_srcdir)/src/mesquite/QualityImprover

MSQ_SRCS += \
	QualityImprover/PatchSetUser.cpp \
	QualityImprover/QualityImprover.cpp \
	QualityImprover/VertexMover.cpp

MSQ_HDRS += \
	QualityImprover/NullImprover.hpp \
	QualityImprover/PatchSetUser.hpp \
	QualityImprover/QualityImprover.hpp \
	QualityImprover/VertexMover.hpp 

MSQ_INCLUDES += -I$(top_srcdir)/src/mesquite/QualityImprover/OptSolvers

MSQ_SRCS += QualityImprover/OptSolvers/ConjugateGradient.cpp \
	    QualityImprover/OptSolvers/FeasibleNewton.cpp \
	    QualityImprover/OptSolvers/NonSmoothDescent.cpp \
	    QualityImprover/OptSolvers/QuasiNewton.cpp \
	    QualityImprover/OptSolvers/SteepestDescent.cpp \
	    QualityImprover/OptSolvers/NonGradient.cpp \
	    QualityImprover/OptSolvers/TrustRegion.cpp

MSQ_HDRS += QualityImprover/OptSolvers/ConjugateGradient.hpp \
	    QualityImprover/OptSolvers/FeasibleNewton.hpp \
	    QualityImprover/OptSolvers/NonSmoothDescent.hpp \
	    QualityImprover/OptSolvers/QuasiNewton.hpp \
	    QualityImprover/OptSolvers/SteepestDescent.hpp \
	    QualityImprover/OptSolvers/NonGradient.hpp \
	    QualityImprover/OptSolvers/TrustRegion.hpp 

MSQ_INCLUDES += -I$(top_srcdir)/src/mesquite/QualityImprover/Relaxation

MSQ_SRCS += \
	QualityImprover/Relaxation/Randomize.cpp \
	QualityImprover/Relaxation/RelaxationSmoother.cpp \
	QualityImprover/Relaxation/LaplacianSmoother.cpp \
	QualityImprover/Relaxation/SmartLaplacianSmoother.cpp 

MSQ_HDRS += \
	QualityImprover/Relaxation/Randomize.hpp \
	QualityImprover/Relaxation/RelaxationSmoother.hpp \
	QualityImprover/Relaxation/LaplacianSmoother.hpp \
	QualityImprover/Relaxation/SmartLaplacianSmoother.hpp 

MSQ_INCLUDES += -I$(top_srcdir)/src/mesquite/QualityMetric \
                -I$(top_srcdir)/src/mesquite/QualityMetric/Debug

MSQ_SRCS += \
	QualityMetric/Debug/CompareQM.cpp \
	QualityMetric/Debug/NumericalQM.cpp \
	QualityMetric/AddQualityMetric.cpp \
	QualityMetric/AveragingQM.cpp \
	QualityMetric/EdgeQM.cpp \
	QualityMetric/ElemSampleQM.cpp \
	QualityMetric/ElementMaxQM.cpp \
	QualityMetric/ElementAvgQM.cpp \
	QualityMetric/ElementPMeanP.cpp \
	QualityMetric/ElementQM.cpp \
	QualityMetric/MultiplyQualityMetric.cpp \
	QualityMetric/PMeanPMetric.cpp \
	QualityMetric/PowerQualityMetric.cpp \
	QualityMetric/QualityMetric.cpp \
	QualityMetric/ScalarAddQualityMetric.cpp \
	QualityMetric/ScalarMultiplyQualityMetric.cpp \
	QualityMetric/VertexMaxQM.cpp \
	QualityMetric/VertexPMeanP.cpp \
	QualityMetric/VertexQM.cpp 

MSQ_HDRS += \
	QualityMetric/Debug/CompareQM.hpp \
	QualityMetric/Debug/NumericalQM.hpp \
	QualityMetric/AddQualityMetric.hpp \
	QualityMetric/AveragingQM.hpp \
	QualityMetric/EdgeQM.hpp \
	QualityMetric/ElemSampleQM.hpp \
	QualityMetric/ElementMaxQM.hpp \
	QualityMetric/ElementAvgQM.hpp \
	QualityMetric/ElementPMeanP.hpp \
	QualityMetric/ElementQM.hpp \
	QualityMetric/MultiplyQualityMetric.hpp \
	QualityMetric/PMeanPMetric.hpp \
	QualityMetric/PowerQualityMetric.hpp \
	QualityMetric/QualityMetric.hpp \
	QualityMetric/ScalarAddQualityMetric.hpp \
	QualityMetric/ScalarMultiplyQualityMetric.hpp \
	QualityMetric/VertexMaxQM.hpp \
	QualityMetric/VertexPMeanP.hpp \
	QualityMetric/VertexQM.hpp 

MSQ_INCLUDES += -I$(top_srcdir)/src/mesquite/QualityMetric/Shape

MSQ_SRCS += \
	QualityMetric/Shape/AspectRatioGammaQualityMetric.cpp \
	QualityMetric/Shape/ConditionNumberQualityMetric.cpp \
	QualityMetric/Shape/IdealWeightInverseMeanRatio.cpp \
	QualityMetric/Shape/IdealWeightMeanRatio.cpp \
	QualityMetric/Shape/VertexConditionNumberQualityMetric.cpp 

MSQ_HDRS += \
	QualityMetric/Shape/AspectRatioGammaQualityMetric.hpp \
	QualityMetric/Shape/ConditionNumberFunctions.hpp \
	QualityMetric/Shape/ConditionNumberQualityMetric.hpp \
	QualityMetric/Shape/IdealWeightInverseMeanRatio.hpp \
	QualityMetric/Shape/IdealWeightMeanRatio.hpp \
	QualityMetric/Shape/MeanRatioFunctions.hpp \
	QualityMetric/Shape/VertexConditionNumberQualityMetric.hpp 

MSQ_INCLUDES += -I$(top_srcdir)/src/mesquite/QualityMetric/Smoothness

MSQ_SRCS += \
	QualityMetric/Smoothness/EdgeLengthQualityMetric.cpp \
	QualityMetric/Smoothness/EdgeLengthRangeQualityMetric.cpp 

MSQ_HDRS += \
	QualityMetric/Smoothness/EdgeLengthQualityMetric.hpp \
	QualityMetric/Smoothness/EdgeLengthRangeQualityMetric.hpp 

MSQ_INCLUDES += -I$(top_srcdir)/src/mesquite/QualityMetric/TMP

MSQ_SRCS += QualityMetric/TMP/AffineMapMetric.cpp \
            QualityMetric/TMP/TargetMetricUtil.cpp \
            QualityMetric/TMP/TargetMetricUtil.hpp \
            QualityMetric/TMP/AWQualityMetric.cpp \
            QualityMetric/TMP/TQualityMetric.cpp \
            QualityMetric/TMP/TMPQualityMetric.cpp 

MSQ_HDRS += QualityMetric/TMP/AffineMapMetric.hpp \
            QualityMetric/TMP/AWQualityMetric.hpp \
            QualityMetric/TMP/TQualityMetric.hpp \
            QualityMetric/TMP/TMPQualityMetric.hpp

MSQ_INCLUDES += -I$(top_srcdir)/src/mesquite/QualityMetric/Untangle

MSQ_SRCS += QualityMetric/Untangle/UntangleBetaQualityMetric.cpp 

MSQ_HDRS += QualityMetric/Untangle/UntangleBetaQualityMetric.hpp 

MSQ_INCLUDES += -I$(top_srcdir)/src/mesquite/QualityMetric/Volume

MSQ_SRCS += QualityMetric/Volume/EdgeLengthMetric.cpp \
	    QualityMetric/Volume/LocalSizeQualityMetric.cpp \
	    QualityMetric/Volume/SizeMetric.cpp

MSQ_HDRS += QualityMetric/Volume/EdgeLengthMetric.hpp \
	    QualityMetric/Volume/LocalSizeQualityMetric.hpp \
	    QualityMetric/Volume/SizeMetric.hpp

MSQ_INCLUDES += -I$(top_srcdir)/src/mesquite/TargetCalculator

MSQ_SRCS += \
	TargetCalculator/CachingTargetCalculator.cpp \
	TargetCalculator/IdealShapeTarget.cpp \
	TargetCalculator/InverseMetricWeight.cpp \
	TargetCalculator/JacobianCalculator.cpp \
	TargetCalculator/LambdaTarget.cpp \
	TargetCalculator/LambdaConstant.cpp \
	TargetCalculator/LVQDTargetCalculator.cpp \
	TargetCalculator/MetricWeight.cpp \
	TargetCalculator/RefMeshTargetCalculator.cpp \
	TargetCalculator/ReferenceMesh.cpp \
	TargetCalculator/RefSizeTargetCalculator.cpp \
	TargetCalculator/RemainingWeight.cpp \
	TargetCalculator/TargetCalculator.cpp \
	TargetCalculator/TargetReader.cpp \
	TargetCalculator/TargetWriter.cpp \
	TargetCalculator/TetDihedralWeight.cpp \
	TargetCalculator/WeightCalculator.cpp \
	TargetCalculator/WeightReader.cpp 

MSQ_HDRS += \
	TargetCalculator/CachingTargetCalculator.hpp \
	TargetCalculator/IdealShapeTarget.hpp \
	TargetCalculator/InverseMetricWeight.hpp \
	TargetCalculator/JacobianCalculator.hpp \
	TargetCalculator/LambdaTarget.hpp \
	TargetCalculator/LambdaConstant.hpp \
	TargetCalculator/LVQDTargetCalculator.hpp \
	TargetCalculator/MetricWeight.hpp \
	TargetCalculator/RefMeshTargetCalculator.hpp \
	TargetCalculator/ReferenceMesh.hpp \
	TargetCalculator/RefSizeTargetCalculator.hpp \
	TargetCalculator/RemainingWeight.hpp \
	TargetCalculator/TargetCalculator.hpp \
	TargetCalculator/TargetReader.hpp \
	TargetCalculator/TargetWriter.hpp \
	TargetCalculator/TetDihedralWeight.hpp \
	TargetCalculator/WeightCalculator.hpp \
	TargetCalculator/WeightReader.hpp 

MSQ_INCLUDES += -I$(top_srcdir)/src/mesquite/TargetMetric

MSQ_SRCS += TargetMetric/AWMetric.cpp \
            TargetMetric/TMetric.cpp \
	    TargetMetric/TMPCommon.hpp \
	    TargetMetric/TMPDerivs.hpp \
            TargetMetric/TMetricBarrier.cpp \
            TargetMetric/TMetricNonBarrier.cpp \
            TargetMetric/AWMetricBarrier.cpp \
            TargetMetric/AWMetricNonBarrier.cpp 

MSQ_HDRS += TargetMetric/AWMetric.hpp \
            TargetMetric/TMetric.hpp \
            TargetMetric/TMetricBarrier.hpp \
            TargetMetric/TMetricNonBarrier.hpp \
            TargetMetric/AWMetricBarrier.hpp \
            TargetMetric/AWMetricNonBarrier.hpp

MSQ_INCLUDES += -I$(top_srcdir)/src/mesquite/TargetMetric/Misc

MSQ_SRCS += TargetMetric/Misc/InvTransBarrier.cpp \
	    TargetMetric/Misc/TOffset.cpp \
	    TargetMetric/Misc/TMixed.cpp \
	    TargetMetric/Misc/TPower2.cpp \
	    TargetMetric/Misc/TScale.cpp \
	    TargetMetric/Misc/TSquared.cpp \
	    TargetMetric/Misc/TSum.cpp

MSQ_HDRS += TargetMetric/Misc/InvTransBarrier.hpp \
	    TargetMetric/Misc/TOffset.hpp \
	    TargetMetric/Misc/TMixed.hpp \
	    TargetMetric/Misc/TPower2.hpp \
	    TargetMetric/Misc/TScale.hpp \
	    TargetMetric/Misc/TSquared.hpp \
	    TargetMetric/Misc/TSum.hpp 

MSQ_INCLUDES += -I$(top_srcdir)/src/mesquite/TargetMetric/Shape

MSQ_SRCS += TargetMetric/Shape/AWShape2DB1.cpp \
	    TargetMetric/Shape/AWShape2DNB1.cpp \
	    TargetMetric/Shape/AWShape2DNB2.cpp \
	    TargetMetric/Shape/TInverseMeanRatio.cpp \
	    TargetMetric/Shape/TShape2DNB2.cpp \
	    TargetMetric/Shape/TShape3DB2.cpp \
	    TargetMetric/Shape/TShapeB1.cpp \
	    TargetMetric/Shape/TShapeNB1.cpp 

MSQ_HDRS += TargetMetric/Shape/AWShape2DB1.hpp \
	    TargetMetric/Shape/AWShape2DNB1.hpp \
	    TargetMetric/Shape/AWShape2DNB2.hpp \
	    TargetMetric/Shape/TInverseMeanRatio.hpp \
	    TargetMetric/Shape/TShape2DNB2.hpp \
	    TargetMetric/Shape/TShape3DB2.hpp \
	    TargetMetric/Shape/TShapeB1.hpp \
	    TargetMetric/Shape/TShapeNB1.hpp 

MSQ_INCLUDES += -I$(top_srcdir)/src/mesquite/TargetMetric/ShapeOrient

MSQ_SRCS += TargetMetric/ShapeOrient/AWShapeOrientNB1.cpp \
	    TargetMetric/ShapeOrient/TShapeOrientB1.cpp \
	    TargetMetric/ShapeOrient/TShapeOrientB2.cpp \
	    TargetMetric/ShapeOrient/TShapeOrientNB1.cpp \
	    TargetMetric/ShapeOrient/TShapeOrientNB2.cpp

MSQ_HDRS += TargetMetric/ShapeOrient/AWShapeOrientNB1.hpp \
	    TargetMetric/ShapeOrient/TShapeOrientB1.hpp \
	    TargetMetric/ShapeOrient/TShapeOrientB2.hpp \
	    TargetMetric/ShapeOrient/TShapeOrientNB1.hpp \
	    TargetMetric/ShapeOrient/TShapeOrientNB2.hpp

MSQ_INCLUDES += -I$(top_srcdir)/src/mesquite/TargetMetric/ShapeSize

MSQ_SRCS += TargetMetric/ShapeSize/AWShapeSizeB1.cpp \
	    TargetMetric/ShapeSize/TShapeSize2DB2.cpp \
	    TargetMetric/ShapeSize/TShapeSize2DNB1.cpp \
	    TargetMetric/ShapeSize/TShapeSize2DNB2.cpp \
	    TargetMetric/ShapeSize/TShapeSize3DB2.cpp \
	    TargetMetric/ShapeSize/TShapeSize3DB4.cpp \
	    TargetMetric/ShapeSize/TShapeSize3DNB1.cpp \
	    TargetMetric/ShapeSize/TShapeSizeB1.cpp \
	    TargetMetric/ShapeSize/TShapeSizeB3.cpp \
	    TargetMetric/ShapeSize/TShapeSizeNB3.cpp

MSQ_HDRS += TargetMetric/ShapeSize/AWShapeSizeB1.hpp \
	    TargetMetric/ShapeSize/TShapeSize2DB2.hpp \
	    TargetMetric/ShapeSize/TShapeSize2DNB1.hpp \
	    TargetMetric/ShapeSize/TShapeSize2DNB2.hpp \
	    TargetMetric/ShapeSize/TShapeSize3DB2.hpp \
	    TargetMetric/ShapeSize/TShapeSize3DB4.hpp \
	    TargetMetric/ShapeSize/TShapeSize3DNB1.hpp \
	    TargetMetric/ShapeSize/TShapeSizeB1.hpp \
	    TargetMetric/ShapeSize/TShapeSizeB3.hpp \
	    TargetMetric/ShapeSize/TShapeSizeNB3.hpp

MSQ_INCLUDES += -I$(top_srcdir)/src/mesquite/TargetMetric/ShapeSizeOrient

MSQ_SRCS += TargetMetric/ShapeSizeOrient/AWShapeSizeOrientNB1.cpp \
            TargetMetric/ShapeSizeOrient/TShapeSizeOrientB1.cpp \
            TargetMetric/ShapeSizeOrient/TShapeSizeOrientB2.cpp \
            TargetMetric/ShapeSizeOrient/TShapeSizeOrientNB1.cpp
            
MSQ_HDRS += TargetMetric/ShapeSizeOrient/AWShapeSizeOrientNB1.hpp \
            TargetMetric/ShapeSizeOrient/TShapeSizeOrientB1.hpp \
            TargetMetric/ShapeSizeOrient/TShapeSizeOrientB2.hpp \
            TargetMetric/ShapeSizeOrient/TShapeSizeOrientNB1.hpp

MSQ_INCLUDES += -I$(top_srcdir)/src/mesquite/TargetMetric/Size

MSQ_SRCS += TargetMetric/Size/AWSizeB1.cpp \
	    TargetMetric/Size/AWSizeNB1.cpp \
	    TargetMetric/Size/TSizeB1.cpp \
	    TargetMetric/Size/TSizeNB1.cpp \
	    TargetMetric/Size/TTau.cpp 

MSQ_HDRS += TargetMetric/Size/AWSizeB1.hpp \
	    TargetMetric/Size/AWSizeNB1.hpp \
	    TargetMetric/Size/TSizeB1.hpp \
	    TargetMetric/Size/TSizeNB1.hpp \
	    TargetMetric/Size/TTau.hpp 

MSQ_INCLUDES += -I$(top_srcdir)/src/mesquite/TargetMetric/Untangle

MSQ_SRCS += TargetMetric/Untangle/AWUntangleBeta.cpp \
            TargetMetric/Untangle/TUntangle1.cpp \
            TargetMetric/Untangle/TUntangleBeta.cpp \
	    TargetMetric/Untangle/TUntangleMu.cpp

MSQ_HDRS += TargetMetric/Untangle/AWUntangleBeta.hpp \
            TargetMetric/Untangle/TUntangle1.hpp \
            TargetMetric/Untangle/TUntangleBeta.hpp \
	    TargetMetric/Untangle/TUntangleMu.hpp

