# $Id: makefile,v 1.29 2007/08/20 22:57:41 cdeluca Exp $ 

ALL: build_system_tests

LOCDIR   = src/system_tests

include $(ESMF_DIR)/makefile

# arranged roughly in order of complexity
DIRS     = \
           ESMF_CompCreate \
           ESMF_ArraySparseMatMul \
           ESMF_FieldRedist \
           ESMF_FieldHalo \
           ESMF_FieldHaloPer \
           ESMF_BundleRedistArb2Arb \
           ESMF_BundleRedistBlk2Arb \
           ESMF_BundleRedistBlk2Blk \
           ESMF_FieldRedistArb2Arb \
           ESMF_FieldRedistBlk2Arb \
           ESMF_FieldRedistBlk2Blk \
           ESMF_SimpleCoupling \
           ESMF_FlowComp \
           ESMF_FlowWithCoupling \
           ESMF_DistDir

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


CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

# DO NOT DELETE
