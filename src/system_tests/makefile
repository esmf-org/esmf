# $Id: makefile,v 1.16 2004/10/13 22:26:33 nscollins Exp $ 

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
# the next one runs, but you must comment out the
# CPPFLAGS += -DVM_DONT_SPAWN_PTHREADS 
# line in $ESMF_DIR/build/common.mk before
# you compile the framework and the test.
#           ESMF_FieldExcl \


CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

# DO NOT DELETE
