# $Id$ 

ALL:      build_unit_tests
run:      run_unit_tests
run_uni:  run_unit_tests_uni

LOCDIR	  = src/Infrastructure/HConfig/tests

.NOTPARALLEL:
TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_HConfigUTest

TESTS_RUN     = RUN_ESMF_HConfigUTest

TESTS_RUN_UNI = RUN_ESMF_HConfigUTestUNI


include ${ESMF_DIR}/makefile

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =

DIRS        = 

#
# unit test targets
#

RUN_ESMF_HConfigUTest:
	cp -f sample.rc $(ESMF_TESTDIR)
	cp -f sample.yaml $(ESMF_TESTDIR)
	$(MAKE) TNAME=HConfig NP=4 ftest

RUN_ESMF_HConfigUTestUNI:
	cp -f sample.yaml $(ESMF_TESTDIR)
	cp -f sample.yaml $(ESMF_TESTDIR)
	$(MAKE) TNAME=HConfig NP=1 ftest
