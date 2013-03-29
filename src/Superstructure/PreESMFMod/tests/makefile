# $Id$ 

ALL:      build_unit_tests
run:      run_unit_tests
run_uni:  run_unit_tests_uni

LOCDIR	  = src/Superstructure/PreESMFMod/tests

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_RegridWeightGenUTest

TESTS_RUN     = RUN_ESMF_RegridWeightGenUTest

TESTS_RUN_UNI = RUN_ESMF_RegridWeightGenUTestUNI

include ${ESMF_DIR}/makefile

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =

DIRS        = 

#
# unit test targets
#

# --- RegridWeightGen

RUN_ESMF_RegridWeightGenUTest:
	mkdir -p $(ESMF_TESTDIR)/data
	cp -f data/*.nc $(ESMF_TESTDIR)/data
	chmod u+rw $(ESMF_TESTDIR)/data/*
	$(MAKE) TNAME=RegridWeightGen NP=4 ftest

RUN_ESMF_RegridWeightGenUTestUNI:
	mkdir -p $(ESMF_TESTDIR)/data
	cp -f data/*.nc $(ESMF_TESTDIR)/data
	chmod u+rw $(ESMF_TESTDIR)/data/*
	$(MAKE) TNAME=RegridWeightGen NP=1 ftest



