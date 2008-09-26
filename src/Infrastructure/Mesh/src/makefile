# $Id: makefile,v 1.11 2008/09/26 05:53:54 cdeluca Exp $

# leave alone.
ALL: build_here 

# Workaround an MPICH(2) issue
ESMF_CXXCOMPILECPPFLAGS += -DMPICH_IGNORE_CXX_SEEK

SOURCEC	  = \
           ESMC_Attr.C \
           ESMC_BBox.C \
           ESMC_CommReg.C \
           ESMC_CommRel.C \
           ESMC_Context.C \
           ESMC_DDir.C \
           ESMC_Exception.C \
           ESMC_Extrapolation.C \
           ESMC_FieldReg.C \
           ESMC_GeomRendezvous.C \
           ESMC_GlobalIds.C \
           ESMC_HAdapt.C \
           ESMC_Interp.C \
           ESMC_IOField.C \
           ESMC_Kernel.C \
           ESMC_Mapping.C \
           ESMC_MasterElement.C \
           ESMC_MCoord.C \
           ESMC_MEFamily.C \
           ESMC_MEField.C \
           ESMC_MEImprint.C \
           ESMC_Mesh.C \
           ESMC_MeshDB.C \
           ESMC_MeshExodus.C \
           ESMC_MeshField.C \
           ESMC_MeshGen.C \
           ESMC_MeshllField.C \
           ESMC_MeshNC.C \
           ESMC_MeshObj.C \
           ESMC_MeshObjConn.C \
           ESMC_MeshObjPack.C \
           ESMC_MeshObjTopo.C \
           ESMC_MeshPartition.C \
           ESMC_MeshPNC.C \
           ESMC_MeshRead.C \
           ESMC_MeshRefine.C \
           ESMC_MeshSkin.C \
           ESMC_MeshUtils.C \
           ESMC_MeshVTK.C \
           ESMC_MEValues.C \
           ESMC_Migrator.C \
           ESMC_OctBox3d.C \
           ESMC_ParEnv.C \
           ESMC_ParLog.C \
           ESMC_PatchRecovery.C \
           ESMC_Polynomial.C \
           ESMC_Quadrature.C \
           ESMC_Rebalance.C \
           ESMC_RefineTopo.C \
           ESMC_Search.C \
           ESMC_SFuncAdaptor.C \
           ESMC_ShapeFunc.C \
           ESMC_ShapeLagrange.C \
           ESMC_SmallAlloc.C \
           ESMC_SparseMsg.C \
           ESMC_WMat.C \
           ESMC_WriteWeights.C \
           ESMC_WriteWeightsPar.C 

SOURCEF	  = 
SOURCEH	  = 

# List all .h files which should be copied to common include dir
STOREH	  = 

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/Mesh/src

CLEANDIRS   =
CLEANFILES  = 
CLOBBERDIRS =

include ${ESMF_DIR}/makefile

DIRS	  = Zoltan
