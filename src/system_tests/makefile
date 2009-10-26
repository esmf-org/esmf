# $Id: makefile,v 1.82 2009/10/26 17:25:58 oehmke Exp $ 

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
# Tests temporarily disabled
# Tests not yet ready
#           ESMF_RowReduce \        gjt: currently the code depends on DELayout comms, but it should be covered/converted to newArray
#           ESMF_MergeCoupling \
#           ESMF_CplOnExclDEs \
# Removed for new Regrid construction. cmd 8/07.
#           ESMF_FieldRegridConserv \
#           ESMF_FieldRegridMulti \
#           ESMF_FieldRegridOrder \
#           ESMF_FieldExcl \
# TODO:FIELDINTEGRATION Restore when Field and FieldBundle comms are restored. 
#           ESMF_FlowWithCoupling \
#           ESMF_FlowComp \
#           ESMF_FieldHalo \
#           ESMF_FieldHaloPer \


CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

# DO NOT DELETE
