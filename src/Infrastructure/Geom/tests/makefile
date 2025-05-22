# $Id$ 

ALL:      build_unit_tests
run:      run_unit_tests
run_uni:  run_unit_tests_uni

LOCDIR	  = src/Infrastructure/Grid/tests

.NOTPARALLEL:
TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_GeomUTest

TESTS_RUN     = RUN_ESMF_GeomUTest 

TESTS_RUN_UNI = RUN_ESMF_GeomUTestUNI

include ${ESMF_DIR}/makefile

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =

DIRS        = 

#
# unit test targets
#

# --- Grid Creation

RUN_ESMF_GeomUTest:
	$(MAKE) TNAME=Geom NP=4 ftest

RUN_ESMF_GeomUTestUNI:
	$(MAKE) TNAME=Geom NP=1 ftest

