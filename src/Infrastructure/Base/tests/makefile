# $Id$ 

ALL:      build_unit_tests
run:      run_unit_tests
run_uni:  run_unit_tests_uni

LOCDIR	  = src/Infrastructure/Base/tests

.NOTPARALLEL:
#TESTS_BUILD = $(ESMF_TESTDIR)/ESMF_InfoProfileUTest
TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_BaseUTest \
                $(ESMF_TESTDIR)/ESMC_BaseUTest \
                $(ESMF_TESTDIR)/ESMC_NlohmannJSONUTest \
                $(ESMF_TESTDIR)/ESMC_InfoUTest \
                $(ESMF_TESTDIR)/ESMF_InfoUTest \
                $(ESMF_TESTDIR)/ESMF_InfoArrayUTest

#TESTS_RUN     = RUN_ESMF_InfoProfileUTest
TESTS_RUN     = RUN_ESMF_BaseUTest \
                RUN_ESMC_BaseUTest \
                RUN_ESMC_NlohmannJSONUTest \
                RUN_ESMC_InfoUTest \
                RUN_ESMF_InfoUTest \
                RUN_ESMF_InfoArrayUTest

TESTS_RUN_UNI = RUN_ESMF_BaseUTestUNI \
                RUN_ESMC_BaseUTestUNI \
                RUN_ESMC_NlohmannJSONUTestUNI \
                RUN_ESMC_InfoUTestUNI \
                RUN_ESMF_InfoUTestUNI \
                RUN_ESMF_InfoArrayUTestUNI

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

# Info ========================================================================

RUN_ESMC_NlohmannJSONUTest:
	$(MAKE) TNAME=NlohmannJSON NP=4 ctest

RUN_ESMC_NlohmannJSONUTestUNI:
	$(MAKE) TNAME=NlohmannJSON NP=1 ctest

RUN_ESMC_InfoUTest:
	$(MAKE) TNAME=Info NP=4 ctest

RUN_ESMC_InfoUTestUNI:
	rm -f test-esmf-info-write.json
	$(MAKE) TNAME=Info NP=1 ctest

RUN_ESMF_InfoUTest:
	rm -f test-esmf-info-write.json
	$(MAKE) TNAME=Info NP=4 ftest

RUN_ESMF_InfoUTestUNI:
	$(MAKE) TNAME=Info NP=1 ftest

RUN_ESMF_InfoProfileUTest:
	env ESMF_RUNTIME_PROFILE=ON $(MAKE) TNAME=InfoProfile NP=4 ftest

RUN_ESMF_InfoProfileUTestUNI:
	env ESMF_RUNTIME_PROFILE=ON $(MAKE) TNAME=InfoProfile NP=1 ftest

RUN_ESMF_InfoArrayUTest:
	$(MAKE) TNAME=InfoArray NP=4 ftest

RUN_ESMF_InfoArrayUTestUNI:
	$(MAKE) TNAME=InfoArray NP=1 ftest

