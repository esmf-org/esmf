# $Id$

# leave alone.
ALL: build_here 

# Workaround an MPICH2 issue
ESMF_CXXCOMPILECPPFLAGS += -DMPICH_IGNORE_CXX_SEEK

SOURCEC	  = \
           ESMCI_Attr.C \
           ESMCI_BBox.C \
           ESMCI_CommReg.C \
           ESMCI_CommRel.C \
           ESMCI_Context.C \
           ESMCI_DDir.C \
           ESMCI_Exception.C \
           ESMCI_FieldReg.C \
           ESMCI_GeomRendezvous.C \
           ESMCI_GlobalIds.C \
           ESMCI_HAdapt.C \
           ESMCI_IOField.C \
           ESMCI_Kernel.C \
           ESMCI_MasterElement.C \
           ESMCI_MCoord.C \
           ESMCI_MEFamily.C \
           ESMCI_MEField.C \
           ESMCI_MEImprint.C \
           ESMCI_MeshDB.C \
           ESMCI_MeshExodus.C \
           ESMCI_MeshField.C \
           ESMCI_MeshGen.C \
           ESMCI_MeshllField.C \
           ESMCI_MeshMerge.C \
           ESMCI_MeshNC.C \
           ESMCI_MeshObj.C \
           ESMCI_MeshObjConn.C \
           ESMCI_MeshObjPack.C \
           ESMCI_MeshObjTopo.C \
           ESMCI_MeshPartition.C \
           ESMCI_MeshPNC.C \
           ESMCI_MeshRead.C \
           ESMCI_MeshRefine.C \
           ESMCI_MeshSkin.C \
           ESMCI_MeshUtils.C \
           ESMCI_MeshVTK.C \
           ESMCI_MEValues.C \
           ESMCI_Migrator.C \
           ESMCI_ParEnv.C \
           ESMCI_ParLog.C \
					 ESMCI_Phedra.C \
           ESMCI_Polynomial.C \
           ESMCI_Quadrature.C \
           ESMCI_Rebalance.C \
           ESMCI_RefineTopo.C \
           ESMCI_SFuncAdaptor.C \
           ESMCI_ShapeLagrange.C \
           ESMCI_SmallAlloc.C \
           ESMCI_SparseMsg.C \
           ESMCI_WriteWeights.C \
           ESMCI_WriteWeightsPar.C \
           ESMCI_SpaceDir.C \
           ESMCI_FindPnts.C \
           ESMCI_SM.C \

SOURCEF	  = ESMF_SolverUtil_C.F90 \

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/Mesh/src/Legacy

CLEANDIRS   =
CLEANFILES  = 
CLOBBERDIRS =

include ${ESMF_DIR}/makefile

ifeq ($(ESMF_MOAB),internal)
ESMF_CXXCOMPILECPPFLAGS += -I$(ESMF_DIR)/$(LOCDIR)/.. \
                           -I$(ESMF_DIR)/$(LOCDIR)/../Moab \
													 -I$(ESMF_DIR)/$(LOCDIR)/../Moab/parallel \
                           -I$(ESMF_DIR)/$(LOCDIR)/../Moab/LocalDiscretization
endif

DIRS	  = Legacy Regridding Zoltan
ifeq ($(ESMF_LAPACK),internal)
DIRS	 += Lapack BLAS
STOREH   += ESMF_LapackBlas.inc
endif
ifeq ($(ESMF_MOAB),internal)
DIRS     += Moab
endif
