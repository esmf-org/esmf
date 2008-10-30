# $Id: makefile,v 1.26 2008/10/30 20:06:14 oehmke Exp $ 

ALL:      build_unit_tests
run:      run_unit_tests
run_uni:  run_unit_tests_uni

LOCDIR	  = src/Infrastructure/Array/tests

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_GridCreateUTest \
	        $(ESMF_TESTDIR)/ESMF_GridCoordUTest \
	        $(ESMF_TESTDIR)/ESMF_GridItemUTest \


TESTS_RUN     = RUN_ESMF_GridCreateUTest \
	        RUN_ESMF_GridCoordUTest \
	        RUN_ESMF_GridItemUTest \

TESTS_RUN_UNI = RUN_ESMF_GridCreateUTestUNI \
	        RUN_ESMF_GridCoordUTestUNI \
	        RUN_ESMF_GridItemUTestUNI \

include ${ESMF_DIR}/makefile

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =

DIRS        = 

#
# unit test targets
#

# --- Grid Creation

RUN_ESMF_GridCreateUTest:
	$(MAKE) TNAME=GridCreate NP=4 ftest

RUN_ESMF_GridCreateUTestUNI:
	$(MAKE) TNAME=GridCreate NP=1 ftest


# --- Grid Coord Tests

RUN_ESMF_GridCoordUTest:
	$(MAKE) TNAME=GridCoord NP=4 ftest

RUN_ESMF_GridCoordUTestUNI:
	$(MAKE) TNAME=GridCoord NP=1 ftest 


# --- Grid Item Tests

RUN_ESMF_GridItemUTest:
	$(MAKE) TNAME=GridItem NP=4 ftest

RUN_ESMF_GridItemUTestUNI:
	$(MAKE) TNAME=GridItem NP=1 ftest 
