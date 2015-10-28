# $Id$ 

ALL:      build_unit_tests
run:      run_unit_tests
run_uni:  run_unit_tests_uni

LOCDIR	  = src/Superstructure/PreESMFMod/tests

.NOTPARALLEL:
TESTS_BUILD   = \
		$(ESMF_TESTDIR)/ESMF_RegridWeightGenUTest \
		$(ESMF_TESTDIR)/ESMF_FileRegridUTest

TESTS_RUN     = \
		RUN_ESMF_RegridWeightGenUTest \
		RUN_ESMF_FileRegridUTest

TESTS_RUN_UNI = \
		RUN_ESMF_RegridWeightGenUTestUNI \
		RUN_ESMF_FileRegridUTestUNI

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

RUN_ESMF_FileRegridUTest:
	mkdir -p $(ESMF_TESTDIR)/data
	cp -f data/*.nc $(ESMF_TESTDIR)/data
	chmod u+rw $(ESMF_TESTDIR)/data/*
	$(MAKE) TNAME=FileRegrid NP=4 ftest

RUN_ESMF_FileRegridUTestUNI:
	mkdir -p $(ESMF_TESTDIR)/data
	cp -f data/*.nc $(ESMF_TESTDIR)/data
	chmod u+rw $(ESMF_TESTDIR)/data/*
	$(MAKE) TNAME=FileRegrid NP=1 ftest



