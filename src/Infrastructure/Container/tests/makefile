# $Id$ 

ALL:      build_unit_tests
run:      run_unit_tests
run_uni:  run_unit_tests_uni

LOCDIR	  = src/Infrastructure/Container/tests

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_ContainerUTest

TESTS_RUN     = RUN_ESMF_ContainerUTest

TESTS_RUN_UNI = RUN_ESMF_ContainerUTestUNI


include ${ESMF_DIR}/makefile

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =

DIRS        = 

#
# unit test targets
#

RUN_ESMF_ContainerUTest:
	$(MAKE) TNAME=Container NP=4 ftest

RUN_ESMF_ContainerUTestUNI:
	$(MAKE) TNAME=Container NP=1 ftest

