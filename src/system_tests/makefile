# $Id: makefile,v 1.20.4.2 2006/08/30 15:33:11 samsoncheung Exp $ 

ALL: build_system_tests

LOCDIR   = src/system_tests

include $(ESMF_DIR)/makefile

# arranged roughly in order of complexity
DIRS     = \
           ESMF_CompCreate \
           ESMF_FieldRedist \
           ESMF_FieldHalo \
           ESMF_FieldHaloPer \
           ESMF_FieldRegrid \
           ESMF_FieldRegridConserv \
           ESMF_FieldRegridMulti \
           ESMF_FieldRegridOrder \
           ESMF_BundleArb2ArbRedist \
           ESMF_BundleBlk2ArbRedist \
           ESMF_BundleBlk2BlkRedist \
           ESMF_FieldArb2ArbRedist \
           ESMF_FieldBlk2ArbRedist \
           ESMF_FieldBlk2BlkRedist \
           ESMF_FieldExcl \
           ESMF_SimpleCoupling \
           ESMF_FlowComp \
           ESMF_FlowWithCoupling

# tests not yet ready
#           ESMF_RowReduce \        gjt: currently the code depends on DELayout comms, but it should be covered/converted to newArray
#           ESMF_MergeCoupling \
#           ESMF_CplOnExclDEs \


CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

# DO NOT DELETE
