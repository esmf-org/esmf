# $Id$ 

ALL:      build_unit_tests
run:      run_unit_tests
run_uni:  run_unit_tests_uni

LOCDIR	  = src/addon/NUOPC/tests

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_NUOPC_UTest

TESTS_RUN     = RUN_ESMF_NUOPC_UTest

TESTS_RUN_UNI = RUN_ESMF_NUOPC_UTestUNI


include ${ESMF_DIR}/makefile

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD) *.nc
CLOBBERDIRS =

DIRS        = 

#
# NUOPC unit test
#
RUN_ESMF_NUOPC_UTest:
	$(MAKE) TNAME=NUOPC_ NP=4 ftest

RUN_ESMF_NUOPC_UTestUNI:
	$(MAKE) TNAME=NUOPC_ NP=1 ftest

