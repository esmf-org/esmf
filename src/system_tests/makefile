# $Id: makefile,v 1.18 2004/11/01 23:41:21 nscollins Exp $ 

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
           ESMF_FieldExcl \
           ESMF_SimpleCoupling \
           ESMF_FlowComp \
           ESMF_FlowWithCoupling

# tests not yet ready
#           ESMF_MergeCoupling \
#           ESMF_CplOnExclDEs \


CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

# DO NOT DELETE
