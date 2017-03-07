# $Id$ 

ALL:      build_unit_tests
run:      run_unit_tests
run_uni:  run_unit_tests_uni

LOCDIR	  = src/Infrastructure/Array/tests

.NOTPARALLEL:
TESTS_BUILD   = $(ESMF_TESTDIR)/ESMC_GridUTest \
                $(ESMF_TESTDIR)/ESMF_GridCreateUTest \
		$(ESMF_TESTDIR)/ESMF_GridCoordUTest \
		$(ESMF_TESTDIR)/ESMF_GridItemUTest \
		$(ESMF_TESTDIR)/ESMF_GridArbitraryUTest

TESTS_RUN     = RUN_ESMC_GridUTest \
                RUN_ESMF_GridCreateUTest \
	        RUN_ESMF_GridCoordUTest \
	        RUN_ESMF_GridItemUTest \
		RUN_ESMF_GridArbitraryUTest

TESTS_RUN_UNI = RUN_ESMC_GridUTestUNI \
                RUN_ESMF_GridCreateUTestUNI \
	        RUN_ESMF_GridCoordUTestUNI \
	        RUN_ESMF_GridItemUTestUNI \
		RUN_ESMF_GridArbitraryUTestUNI

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


# --- Arb Grid Tests

RUN_ESMF_GridArbitraryUTest:
	$(MAKE) TNAME=GridArbitrary NP=4 ftest

RUN_ESMF_GridArbitraryUTestUNI:
	$(MAKE) TNAME=GridArbitrary NP=1 ftest 

# --- ESMC_Grid Tests

RUN_ESMC_GridUTest:
	cp -r data $(ESMF_TESTDIR)
	$(MAKE) TNAME=Grid NP=4 ctest

RUN_ESMC_GridUTestUNI:
	cp -r data $(ESMF_TESTDIR)
	$(MAKE) TNAME=Grid NP=1 ctest

