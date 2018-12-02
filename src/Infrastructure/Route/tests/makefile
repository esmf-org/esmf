# $Id$ 

ALL:      build_unit_tests
run:      run_unit_tests
run_uni:  run_unit_tests_uni

LOCDIR	  = src/Infrastructure/Route/tests

.NOTPARALLEL:
TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_RouteHandleUTest \
                $(ESMF_TESTDIR)/ESMF_RouteHandleAdvancedUTest

TESTS_RUN     = RUN_ESMF_RouteHandleUTest \
                RUN_ESMF_RouteHandleAdvancedUTest

TESTS_RUN_UNI = RUN_ESMF_RouteHandleUTestUNI


include ${ESMF_DIR}/makefile

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =

DIRS        = 

#
# RouteHandle unit test
#
RUN_ESMF_RouteHandleUTest:
	$(MAKE) TNAME=RouteHandle NP=4 ftest

RUN_ESMF_RouteHandleUTestUNI:
	$(MAKE) TNAME=RouteHandle NP=1 ftest

#
# RouteHandleAdvanced unit test
#
RUN_ESMF_RouteHandleAdvancedUTest:
	$(MAKE) TNAME=RouteHandleAdvanced NP=4 ftest

