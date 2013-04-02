# $Id$ 

ALL:      build_unit_tests
run:      run_unit_tests
run_uni:  run_unit_tests_uni

LOCDIR	  = src/Infrastructure/DistGrid/tests

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_DistGridCreateGetUTest \
                $(ESMF_TESTDIR)/ESMC_DistGridUTest

TESTS_RUN     = RUN_ESMF_DistGridCreateGetUTest \
                RUN_ESMC_DistGridUTest

TESTS_RUN_UNI = RUN_ESMF_DistGridCreateGetUTestUNI \
                RUN_ESMC_DistGridUTestUNI


include ${ESMF_DIR}/makefile

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =

DIRS        = 

#
# unit test targets
#

RUN_ESMF_DistGridCreateGetUTest:
	$(MAKE) TNAME=DistGridCreateGet NP=4 ftest

RUN_ESMF_DistGridCreateGetUTestUNI:
	$(MAKE) TNAME=DistGridCreateGet NP=1 ftest

# ---

RUN_ESMC_DistGridUTest:
	$(MAKE) TNAME=DistGrid NP=4 ctest

RUN_ESMC_DistGridUTestUNI:
	$(MAKE) TNAME=DistGrid NP=1 ctest
