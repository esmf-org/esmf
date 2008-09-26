# $Id: makefile,v 1.2 2008/09/26 21:11:40 cdeluca Exp $ 

ALL:      build_unit_tests
run:      run_unit_tests
run_uni:  run_unit_tests_uni

LOCDIR	  = src/Infrastructure/Array/tests

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_TestUTest \
                $(ESMF_TESTDIR)/ESMCI_TestUTest \
                $(ESMF_TESTDIR)/ESMC_TestUTest

TESTS_RUN     = RUN_ESMF_TestUTest \
                RUN_ESMCI_TestUTest \
                RUN_ESMC_TestUTest

TESTS_RUN_UNI = RUN_ESMF_TestUTestUNI \
                RUN_ESMCI_TestUTestUNI \
                RUN_ESMC_TestUTestUNI


include ${ESMF_DIR}/makefile

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =

DIRS        = 

#
# unit test targets
#

RUN_ESMF_TestUTest:
	$(MAKE) TNAME=Test NP=4 ftest

RUN_ESMF_TestUTestUNI:
	$(MAKE) TNAME=Test NP=1 ftest

# ---

RUN_ESMCI_TestUTest:
	$(MAKE) TNAME=Test NP=4 citest

RUN_ESMCI_TestUTestUNI:
	$(MAKE) TNAME=Test NP=1 citest

# ---

RUN_ESMC_TestUTest:
	$(MAKE) TNAME=Test NP=4 ctest

RUN_ESMC_TestUTestUNI:
	$(MAKE) TNAME=Test NP=1 ctest
