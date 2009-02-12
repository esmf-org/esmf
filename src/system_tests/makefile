# $Id: makefile,v 1.58 2009/02/12 20:31:45 svasquez Exp $ 

ALL: build_system_tests

LOCDIR   = src/system_tests

include $(ESMF_DIR)/makefile

# arranged roughly in order of complexity
DIRS      = ESMF_CompCreate \
            ESMF_CompFortranAndC \
            ESMF_Attribute \
	    ESMF_SeqEnsemEx \
	    ESMF_ConcurrentEnsemble \
            ESMF_RecursiveComponent \
            ESMF_ArrayScatterGather \
            ESMF_ArraySparseMatMul \
            ESMF_ArrayRedist \
            ESMF_ArrayRedistMPMD \
            ESMF_ArrayRedist3D \
            ESMF_DirectCoupling \
            ESMF_ArrayBundleSparseMatMul \
            ESMF_DistDir \
            ESMF_FieldSparseMatMul \
            ESMF_ConcurrentComponent \
            ESMF_FieldConcurrentComp \
            ESMF_FieldBundleSMM \
            ESMF_FieldRedist \
            ESMF_FieldRedistBlk2Blk \
            ESMF_FieldBundleRedistBlk2Blk


# Tests not yet ready
#           ESMF_RowReduce \        gjt: currently the code depends on DELayout comms, but it should be covered/converted to newArray
#           ESMF_MergeCoupling \
#           ESMF_CplOnExclDEs \
# Removed for new Regrid construction. cmd 8/07.
#           ESMF_FieldRegrid \
#           ESMF_FieldRegridConserv \
#           ESMF_FieldRegridMulti \
#           ESMF_FieldRegridOrder \
#           ESMF_FieldExcl \
# TODO:FIELDINTEGRATION Restore when Field and FieldBundle comms are restored. 
#           ESMF_FlowWithCoupling \
#           ESMF_FlowComp \
#           ESMF_FieldHalo \
#           ESMF_FieldHaloPer \
#           ESMF_FieldBundleRedistArb2Arb \
#           ESMF_FieldBundleRedistBlk2Arb \
#           ESMF_FieldRedistArb2Arb \
#           ESMF_FieldRedistBlk2Arb \


CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

# DO NOT DELETE
