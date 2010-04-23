# $Id: makefile,v 1.76 2010/04/23 00:44:38 theurich Exp $ 
#

ALL:      build_unit_tests
run:      run_unit_tests
run_uni:  run_unit_tests_uni

LOCDIR	  = src/Infrastructure/Field/tests

TESTS_BUILD   = \
                $(ESMF_TESTDIR)/ESMF_FieldUTest \
		$(ESMF_TESTDIR)/ESMC_FieldUTest \
		$(ESMF_TESTDIR)/ESMF_FieldCreateGetUTest \
		$(ESMF_TESTDIR)/ESMF_FieldRegridCsrvUTest \
		$(ESMF_TESTDIR)/ESMF_FieldRegridUTest \
		$(ESMF_TESTDIR)/ESMF_FieldArbGridUTest \
		$(ESMF_TESTDIR)/ESMF_FieldStressUTest
               
TESTS_RUN     = RUN_ESMF_FieldUTest \
		RUN_ESMF_FieldCreateGetUTest \
		RUN_ESMF_FieldRegridUTest \
		RUN_ESMF_FieldRegridCsrvUTest \
		RUN_ESMF_FieldArbGridUTest \
		RUN_ESMF_FieldStressUTest \
		RUN_ESMC_FieldUTest

#
# if ESMF_TESTHARNESS_FIELD not null, append test harness suite target,
# otherwise use default target
#
ifneq ($(ESMF_TESTHARNESS_FIELD), )
        TESTS_RUN := $(TESTS_RUN) $(ESMF_TESTHARNESS_FIELD)
else
	TESTS_RUN := $(TESTS_RUN) RUN_ESMF_TestHarnessField_default
endif

TESTS_RUN_UNI = RUN_ESMF_FieldStressUTestUNI \
	RUN_ESMF_FieldRegridUTestUNI \
	RUN_ESMF_FieldRegridCsrvUTestUNI \
	RUN_ESMF_FieldArbGridUTestUNI \
	RUN_ESMC_FieldUTestUNI

include ${ESMF_DIR}/makefile

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD) $(CPFILES)
CLOBBERDIRS =

DIRS        = 

#
# Field
#

RUN_ESMF_FieldUTest:
	$(MAKE) TNAME=Field NP=4 ftest

RUN_ESMC_FieldUTest:
	cp -r data $(ESMF_TESTDIR)
	chmod u+rw $(ESMF_TESTDIR)/data/*
	$(MAKE) TNAME=Field NP=1 ctest

RUN_ESMC_FieldUTestUNI:
	cp -r data $(ESMF_TESTDIR)
	chmod u+rw $(ESMF_TESTDIR)/data/*
	$(MAKE) TNAME=Field NP=1 ctest

RUN_ESMF_FieldCreateGetUTest:
	$(MAKE) TNAME=FieldCreateGet NP=4 ftest

RUN_ESMF_FieldRegridUTest:
	$(MAKE) TNAME=FieldRegrid NP=4 ftest

RUN_ESMF_FieldRegridUTestUNI:
	$(MAKE) TNAME=FieldRegrid NP=1 ftest

RUN_ESMF_FieldRegridCsrvUTest:
	$(MAKE) TNAME=FieldRegridCsrv NP=4 ftest

RUN_ESMF_FieldRegridCsrvUTestUNI:
	$(MAKE) TNAME=FieldRegridCsrv NP=1 ftest

RUN_ESMF_FieldArbGridUTest:
	$(MAKE) TNAME=FieldArbGrid NP=4 ftest

RUN_ESMF_FieldArbGridUTestUNI:
	$(MAKE) TNAME=FieldArbGrid NP=1 ftest
#
# FieldStress
#

RUN_ESMF_FieldStressUTest:
	$(MAKE) TNAME=FieldStress NP=1 ftest

RUN_ESMF_FieldStressUTestUNI:
	$(MAKE) TNAME=FieldStress NP=1 ftest

#
# TestHarness tests
#
RUN_ESMF_TestHarnessField_default:
	$(MAKE) TESTHARNESSCASE=field_default NP=4 run_test_harness

#
RUN_ESMF_TestHarnessField_1:
	$(MAKE) TESTHARNESSCASE=field_1 NP=4 run_test_harness
