# $Id$ 

ALL:  build_unit_tests
run:  run_unit_tests

LOCDIR	  = src/Infrastructure/Base/tests

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_BaseUTest \
                $(ESMF_TESTDIR)/ESMC_BaseUTest

TESTS_RUN     = RUN_ESMF_BaseUTest \
                RUN_ESMC_BaseUTest

TESTS_RUN_UNI = RUN_ESMF_BaseUTestUNI \
                RUN_ESMC_BaseUTestUNI


include ${ESMF_DIR}/makefile

DIRS        =

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =


#
#  Base class
#

RUN_ESMF_BaseUTest:
	$(MAKE) TNAME=Base NP=4 ftest

RUN_ESMF_BaseUTestUNI:
	$(MAKE) TNAME=Base NP=1 ftest

RUN_ESMC_BaseUTest:
	$(MAKE) TNAME=Base NP=4 ctest

RUN_ESMC_BaseUTestUNI:
	$(MAKE) TNAME=Base NP=1 ctest