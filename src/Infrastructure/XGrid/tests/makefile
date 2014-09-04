# $Id$ 

ALL:      build_unit_tests
run:      run_unit_tests
run_uni:  run_unit_tests_uni

LOCDIR	  = src/Infrastructure/XGrid/tests

.NOTPARALLEL:
TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_XGridUTest $(ESMF_TESTDIR)/ESMF_XGridMaskingUTest

TESTS_RUN     = RUN_ESMF_XGridUTest RUN_ESMF_XGridMaskingUTest

TESTS_RUN_UNI = RUN_ESMF_XGridUTestUNI RUN_ESMF_XGridMaskingUTestUNI

include ${ESMF_DIR}/makefile

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD) $(CPFILES)
CLOBBERDIRS =

DIRS        = 


#
# XGrid
#

RUN_ESMF_XGridUTest:
	$(MAKE) TNAME=XGrid NP=2 ftest

RUN_ESMF_XGridUTestUNI:
	$(MAKE) TNAME=XGrid NP=1 ftest

RUN_ESMF_XGridMaskingUTest:
	$(MAKE) TNAME=XGridMasking NP=2 ftest

RUN_ESMF_XGridMaskingUTestUNI:
	$(MAKE) TNAME=XGridMasking NP=1 ftest
