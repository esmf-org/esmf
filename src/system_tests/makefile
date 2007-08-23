# $Id: makefile,v 1.30 2007/08/23 17:16:08 cdeluca Exp $ 

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
# Removed while inserting new grid into components. cmd 8/07.
#           ESMF_FlowWithCoupling \
#           ESMF_FlowComp \

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

# DO NOT DELETE
