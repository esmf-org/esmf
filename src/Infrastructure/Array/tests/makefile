# $Id: makefile,v 1.24 2008/08/13 15:52:47 donstark Exp $ 

ALL:      build_unit_tests
run:      run_unit_tests
run_uni:  run_unit_tests_uni

LOCDIR	  = src/Infrastructure/Array/tests

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_ArrayCreateGetUTest \
                $(ESMF_TESTDIR)/ESMF_ArrayDataUTest \
                $(ESMF_TESTDIR)/ESMF_ArrayScatterUTest
#               $(ESMF_TESTDIR)/ESMF_ArrayArbIdxUTest


TESTS_RUN     = RUN_ESMF_ArrayCreateGetUTest \
                 RUN_ESMF_ArrayDataUTest \
                 RUN_ESMF_ArrayScatterUTest \
                 RUN_ESMF_ArrayHarness2UTest \
                 RUN_ESMF_ArrayHarness4UTest  \
                 RUN_ESMF_ArrayHarness8UTest
#                RUN_ESMF_ArrayArbIdxUTest \


TESTS_RUN_UNI = ESMF_ArrayCreateGetUTestUNI \
                RUN_ESMF_ArrayDataUTestUNI \
                RUN_ESMC_ArrayUTestUNI


include ${ESMF_DIR}/makefile

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =


#
# unit test targets
#

# ---

RUN_ESMF_ArrayCreateGetUTest:
	$(MAKE) TNAME=ArrayCreateGet NP=4 ftest

RUN_ESMF_ArrayCreateGetUTestUNI:
	$(MAKE) TNAME=ArrayCreateGet NP=1 ftest

# ---

RUN_ESMF_ArrayDataUTest:
	$(MAKE) TNAME=ArrayData NP=4 ftest

RUN_ESMF_ArrayDataUTestUNI:
	$(MAKE) TNAME=ArrayData NP=1 ftest
# ---

RUN_ESMF_ArrayScatterUTest:
	$(MAKE) TNAME=ArrayScatter NP=4 ftest

RUN_ESMF_ArrayScatterUTestUNI:
	$(MAKE) TNAME=ArrayScatter NP=1 ftest
# ---

RUN_ESMF_ArrayArbIdxUTest:
	$(MAKE) TNAME=ArrayArbIdx NP=6 ftest

# ---

RUN_ESMF_ArrayHarness1UTest:
	echo "ESMF_ArrayHarness1UTest" > $(ESMF_TESTDIR)/test_harness.list
	cp -f test_harness.rc $(ESMF_TESTDIR)
	cp -f harness_config/*.rc $(ESMF_TESTDIR)
	$(MAKE) TNAME=TestHarness HNAME=ArrayHarness1 NP=1 htest

RUN_ESMF_ArrayHarness2UTest:
	echo "ESMF_ArrayHarness2UTest" > $(ESMF_TESTDIR)/test_harness.list
	cp -f test_harness.rc $(ESMF_TESTDIR)
	cp -f harness_config/*.rc $(ESMF_TESTDIR)
	$(MAKE) TNAME=TestHarness HNAME=ArrayHarness2 NP=2 htest

RUN_ESMF_ArrayHarness4UTest:
	echo "ESMF_ArrayHarness4UTest" >> $(ESMF_TESTDIR)/test_harness.list
	cp -f test_harness.rc $(ESMF_TESTDIR)
	cp -f harness_config/*.rc $(ESMF_TESTDIR)
	$(MAKE) TNAME=TestHarness HNAME=ArrayHarness4 NP=4 htest

RUN_ESMF_ArrayHarness8UTest:
	echo "ESMF_ArrayHarness8UTest" >> $(ESMF_TESTDIR)/test_harness.list
	cp -f test_harness.rc $(ESMF_TESTDIR)
	cp -f harness_config/*.rc $(ESMF_TESTDIR)
	$(MAKE) TNAME=TestHarness HNAME=ArrayHarness8 NP=8 htest

