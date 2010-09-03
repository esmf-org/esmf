# $Id: makefile,v 1.81 2010/09/03 15:47:33 feiliu Exp $ 

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
		$(ESMF_TESTDIR)/ESMF_FieldIOUTest \
		$(ESMF_TESTDIR)/ESMF_FieldRegridXGUTest \
		$(ESMF_TESTDIR)/ESMF_FieldStressUTest
               
TESTS_RUN     = RUN_ESMF_FieldUTest \
		RUN_ESMF_FieldCreateGetUTest \
		RUN_ESMF_FieldRegridUTest \
		RUN_ESMF_FieldRegridCsrvUTest \
		RUN_ESMF_FieldArbGridUTest \
		RUN_ESMF_FieldIOUTest \
		RUN_ESMF_FieldStressUTest \
#		RUN_ESMF_FieldRegridXGUTest \
		RUN_ESMC_FieldUTest

TESTS_RUN_UNI = RUN_ESMF_FieldStressUTestUNI \
                RUN_ESMF_FieldRegridUTestUNI \
                RUN_ESMF_FieldRegridCsrvUTestUNI \
                RUN_ESMF_FieldArbGridUTestUNI \
#                RUN_ESMF_FieldRegridXGUTestUNI \
                RUN_ESMC_FieldUTestUNI

#
# check ESMF_TESTHARNESS_FIELD for default, 
# otherwise append test harness suite target
#
ifeq ($(ESMF_TESTHARNESS_FIELD), default)
  TESTS_RUN := $(TESTS_RUN) RUN_ESMF_TestHarnessField_default
  TESTS_RUN_UNI := $(TESTS_RUN_UNI) RUN_ESMF_TestHarnessFieldUNI_default
else
  TESTS_RUN := $(TESTS_RUN) $(ESMF_TESTHARNESS_FIELD)
  TESTS_RUN_UNI := $(TESTS_RUN_UNI) $(ESMF_TESTHARNESS_FIELD)
endif

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

RUN_ESMF_FieldRegridXGUTest:
	$(MAKE) TNAME=FieldRegridXG NP=4 ftest

RUN_ESMF_FieldRegridXGUTestUNI:
	$(MAKE) TNAME=FieldRegridXG NP=1 ftest

RUN_ESMF_FieldArbGridUTest:
	$(MAKE) TNAME=FieldArbGrid NP=4 ftest

RUN_ESMF_FieldIOUTest:
	$(MAKE) TNAME=FieldIO NP=4 ftest

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

# UNI test harness
RUN_ESMF_TestHarnessFieldUNI_default:
	$(MAKE) TESTHARNESSCASE=field_default NP=1 run_test_harness

#
RUN_ESMF_TestHarnessFieldUNI_1:
	$(MAKE) TESTHARNESSCASE=field_1 NP=1 run_test_harness

##
