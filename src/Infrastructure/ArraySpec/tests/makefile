# $Id$ 

ALL:      build_unit_tests
run:      run_unit_tests
run_uni:  run_unit_tests_uni

LOCDIR	  = src/Infrastructure/ArraySpec/tests

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_ArraySpecUTest \
		$(ESMF_TESTDIR)/ESMC_ArraySpecUTest



TESTS_RUN     = RUN_ESMF_ArraySpecUTest \
		RUN_ESMC_ArraySpecUTest

TESTS_RUN_UNI = RUN_ESMC_ArraySpecUTestUNI \
		RUN_ESMF_ArraySpecUTestUNI

include ${ESMF_DIR}/makefile

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =

DIRS        = 

#
# unit test targets
#

# --- ArraySpec Test

RUN_ESMF_ArraySpecUTest:
	$(MAKE) TNAME=ArraySpec NP=4 ftest

RUN_ESMF_ArraySpecUTestUNI:
	$(MAKE) TNAME=ArraySpec NP=1 ftest

RUN_ESMC_ArraySpecUTest:
	$(MAKE) TNAME=ArraySpec NP=4 ctest

RUN_ESMC_ArraySpecUTestUNI:
	$(MAKE) TNAME=ArraySpec NP=1 ctest
