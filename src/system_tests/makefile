# $Id: makefile,v 1.20.4.1 2006/04/26 17:31:20 theurich Exp $ 

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
           ESMF_ArbitraryDistribution \
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
