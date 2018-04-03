# $Id$

ALL:     build_unit_tests
run:     run_unit_tests
run_uni: run_unit_tests_uni

LOCDIR	  = src/Superstructure/IOAPI/tests

.NOTPARALLEL:
TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_IOCompUTest

TESTS_RUN     = RUN_ESMF_IOCompUTest

TESTS_RUN_UNI = RUN_ESMF_IOCompUTestUNI


include ${ESMF_DIR}/makefile

DIRS	  =

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD) 
CLOBBERDIRS =

# --- 

RUN_ESMF_IOCompUTest:
	$(MAKE) TNAME=IOComp NP=4 ftest

RUN_ESMF_IOCompUTestUNI:
	$(MAKE) TNAME=IOComp NP=1 ftest

