# $Id: makefile,v 1.17 2004/11/01 17:14:42 theurich Exp $ 

ALL: build_system_tests

LOCDIR   = src/system_tests

include $(ESMF_DIR)/makefile

# arranged roughly in order of complexity
DIRS     = ESMF_RowReduce \
           ESMF_CompCreate \
           ESMF_FieldRedist \
           ESMF_FieldHalo \
           ESMF_FieldHaloPer \
           ESMF_FieldRegrid \
           ESMF_FieldRegridConserv \
           ESMF_FieldRegridMulti \
           ESMF_FieldRegridOrder \
           ESMF_SimpleCoupling \
           ESMF_FlowComp \
           ESMF_FlowWithCoupling

# tests not yet ready
#           ESMF_MergeCoupling \
#           ESMF_CplOnExclDEs \
# the next one exercises the pthread mechanism in VM
#           ESMF_FieldExcl \


CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

# DO NOT DELETE
