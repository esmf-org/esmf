# $Id$ 

ALL:      build_unit_tests
run:      run_unit_tests
run_uni:  run_unit_tests_uni

LOCDIR	  = src/Infrastructure/ArrayBundle/tests

.NOTPARALLEL:
TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_ArrayBundleCreateUTest \
                $(ESMF_TESTDIR)/ESMF_ArrayBundleIOUTest


TESTS_RUN     = RUN_ESMF_ArrayBundleCreateUTest \
                RUN_ESMF_ArrayBundleIOUTest

TESTS_RUN_UNI = RUN_ESMF_ArrayBundleCreateUTestUNI


include ${ESMF_DIR}/makefile

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =

DIRS        = 

#
# unit test targets
#

RUN_ESMF_ArrayBundleCreateUTest:
	$(MAKE) TNAME=ArrayBundleCreate NP=4 ftest

RUN_ESMF_ArrayBundleCreateUTestUNI:
	$(MAKE) TNAME=ArrayBundleCreate NP=1 ftest

RUN_ESMF_ArrayBundleIOUTest:
	$(MAKE) TNAME=ArrayBundleIO NP=4 ftest

