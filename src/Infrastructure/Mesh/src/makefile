# $Id: makefile,v 1.8 2007/12/06 20:37:09 dneckels Exp $

# leave alone.
ALL: build_here 

# Don't set these unless absolutely necessary - they've got to be
# architecture and compiler independent and should be inherited
# from the upper level makefile if possible.
CFLAGS    =
FFLAGS    =

# If you have C files in this directory, list them all on the line below.
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
           ESMC_ShapeHierarchic.C \
           ESMC_ShapeLagrange.C \
           ESMC_SmallAlloc.C \
           ESMC_SparseMsg.C \
           ESMC_WMat.C \
           ESMC_WriteWeights.C \
           ESMC_WriteWeightsPar.C 

# If you have F90 files in this directory, list them all on the line below.
SOURCEF	  = 
# If you have .h include files in this dir, list them all on the line below.
SOURCEH	  = 
# List all .h files which should be copied to common include dir
STOREH	  = 

# Leave these alone.
OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

MANSEC	  = mesh
LIBBASE	  = libesmf

# The following line must be set to the current subdir name relative
# to the top level esmf directory. (e.g. Base/src, TimeMgr/interface, etc)
LOCDIR	  = src/Infrastructure/Mesh/src

CLEANDIRS   =
CLEANFILES  = 
CLOBBERDIRS =

include ${ESMF_DIR}/makefile

# Leave the following line blank.
DIRS	  = Zoltan



# If you have class files which need to be compiled in a particular
# order, list the dependencies below, with no rules following.
#
# e.g.:
#




