# $Id$ 

ALL:  build_unit_tests
run:  run_unit_tests
run_uni:  run_unit_tests_uni

LOCDIR	  = src/Infrastructure/Attribute/tests

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMC_AttributeJSONUTest

TESTS_RUN     = RUN_ESMC_AttributeJSONUTest

TESTS_RUN_UNI = RUN_ESMC_AttributeJSONUTestUNI


include ${ESMF_DIR}/makefile

DIRS        =

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =


RUN_ESMC_AttributeJSONUTest:
	$(MAKE) TNAME=AttributeJSON NP=4 ctest

RUN_ESMC_AttributeJSONUTestUNI:
	$(MAKE) TNAME=AttributeJSON NP=1 ctest


