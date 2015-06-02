# $Id$ 

ALL:      build_unit_tests
run:      run_unit_tests
run_uni:  run_unit_tests_uni

LOCDIR    = src/Infrastructure/PointList/tests

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_PointListUTest 

TESTS_OBJ = 

TESTS_RUN     = RUN_ESMF_PointListUTest 

TESTS_RUN_UNI = RUN_ESMF_PointListUTestUNI



include ${ESMF_DIR}/makefile

CLEANDIRS   =
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =

ifeq ($(ESMF_MOAB),internal)
ESMF_CXXCOMPILECPPFLAGS += -I../src/Moab
endif


#
# unit test targets
#

# --- Mesh tests

RUN_ESMF_PointListUTest:
	$(MAKE) TNAME=PointList NP=4 ftest

RUN_ESMF_PointListUTestUNI:
	$(MAKE) TNAME=PointList NP=1 ftest
