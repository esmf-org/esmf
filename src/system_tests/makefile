# $Id: makefile,v 1.42 2008/04/07 06:46:18 theurich Exp $ 

ALL: build_system_tests

LOCDIR   = src/system_tests

include $(ESMF_DIR)/makefile

# arranged roughly in order of complexity
DIRS      = ESMF_CompCreate \
            ESMF_Attribute \
	    ESMF_SeqEnsemEx \
            ESMF_RecursiveComponent \
            ESMF_ArrayScatterGather \
            ESMF_ArraySparseMatMul \
            ESMF_ArrayRedist \
            ESMF_ArrayRedistMPMD \
            ESMF_DirectCoupling \
            ESMF_DistDir \
            ESMF_FieldSparseMatMul

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
#           ESMF_FieldRedist \
#           ESMF_FieldHalo \
#           ESMF_FieldHaloPer \
#           ESMF_FieldBundleRedistArb2Arb \
#           ESMF_FieldBundleRedistBlk2Arb \
#	    ESMF_FieldBundleRedistBlk2Blk \
#           ESMF_FieldRedistArb2Arb \
#           ESMF_FieldRedistBlk2Arb \
#           ESMF_FieldRedistBlk2Blk \
#           ESMF_SimpleCoupling \




CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

# DO NOT DELETE
