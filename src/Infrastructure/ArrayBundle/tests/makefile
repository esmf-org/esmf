# $Id: makefile,v 1.4 2008/09/26 21:11:36 cdeluca Exp $ 

ALL:      build_unit_tests
run:      run_unit_tests
run_uni:  run_unit_tests_uni

LOCDIR	  = src/Infrastructure/ArrayBundle/tests

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_ArrayBundleCreateUTest


TESTS_RUN     = RUN_ESMF_ArrayBundleCreateUTest

TESTS_RUN_UNI =


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
