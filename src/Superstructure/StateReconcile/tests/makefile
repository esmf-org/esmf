# $Id$ 

ALL:  build_unit_tests
run:  run_unit_tests
run_uni: run_unit_tests_uni

LOCDIR	  = src/Superstructure/State/tests

.NOTPARALLEL:
TESTS_BUILD   =	$(ESMF_TESTDIR)/ESMF_StateReconcileUTest \
                $(ESMF_TESTDIR)/ESMF_StateReconcileProxyUTest

TESTS_RUN     =	RUN_ESMF_StateReconcileUTest \
                RUN_ESMF_StateReconcileProxyUTest

TESTS_RUN_UNI =	RUN_ESMF_StateReconcileUTestUNI

include $(ESMF_DIR)/makefile

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =

DIRS        =

#       
# StateReconcile Unit Test
#

RUN_ESMF_StateReconcileUTest:
	$(MAKE) TNAME=StateReconcile NP=4 ftest

RUN_ESMF_StateReconcileUTestUNI:
	$(MAKE) TNAME=StateReconcile NP=1 ftest

#       
# StateReconcileProxy Unit Test
#

RUN_ESMF_StateReconcileProxyUTest:
	$(MAKE) TNAME=StateReconcileProxy NP=4 ftest
