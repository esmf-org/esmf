# $Id: makefile,v 1.83 2009/10/28 01:41:02 theurich Exp $ 

ALL: build_system_tests

LOCDIR   = src/system_tests

include $(ESMF_DIR)/makefile

# arranged roughly in order of complexity
DIRS      = ESMF_CompCreate \
            ESMF_CompFortranAndC \
            ESMF_RecursiveComponent \
            ESMF_ArrayScatterGather \
            ESMF_Attribute \
            ESMF_ArraySparseMatMul \
            ESMF_ArrayRedist \
            ESMF_ArrayRedistMPMD \
            ESMF_ArrayRedistSharedObj \
            ESMF_ArrayRedist3D \
            ESMF_DirectCoupling \
            ESMF_ArrayBundleSparseMatMul \
            ESMF_ArrayBundleRedist \
            ESMF_FieldSparseMatMul \
            ESMF_ConcurrentComponent \
            ESMF_FieldConcurrentComp \
            ESMF_FieldBundleSMM \
            ESMF_FieldRedist \
            ESMF_FieldRedistBlk2Blk \
            ESMF_FieldRedistBlk2Arb \
            ESMF_FieldRedistArb2Arb \
            ESMF_FieldBundleRedistBlk2Blk \
            ESMF_FieldBundleRedistBlk2Arb \
            ESMF_FieldBundleRedistArb2Arb \
            ESMF_FieldRegrid \
            ESMF_FieldRegridDisjoint \
            ESMF_FieldRegridOverlap \
            ESMF_FieldRegridMesh \
            ESMF_FieldLSSMM \
            ESMF_FieldMeshSMM \
	    ESMF_SequentialEnsemble \
	    ESMF_ConcurrentEnsemble \
	    ESMF_ArrayRedistOpenMP 
# Tests temporarily disabled, re-enable once capability is implemented
#           ESMF_RowReduce \          cover by Array comms
#           ESMF_MergeCoupling \
#           ESMF_FieldRegridConserv \ cover by Regrid
#           ESMF_FieldRegridMulti \   cover by Regrid
#           ESMF_FieldRegridOrder \   cover by Regrid
#           ESMF_FlowWithCoupling \
#           ESMF_FlowComp \
#           ESMF_FieldHalo \
#           ESMF_FieldHaloPer \


CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

# DO NOT DELETE
