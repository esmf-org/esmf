# $Id$ 

ALL:      build_unit_tests
run:      run_unit_tests
run_uni:  run_unit_tests_uni

LOCDIR	  = src/Infrastructure/Field/tests

.NOTPARALLEL:
TESTS_BUILD   = \
		$(ESMF_TESTDIR)/ESMC_FieldUTest \
		$(ESMF_TESTDIR)/ESMC_FieldRegridUTest \
		$(ESMF_TESTDIR)/ESMC_FieldRegridCsrvUTest \
		$(ESMF_TESTDIR)/ESMC_FieldGridRegridUTest \
		$(ESMF_TESTDIR)/ESMC_FieldGridRegrid2UTest \
		$(ESMF_TESTDIR)/ESMC_FieldGridRegridCsrvUTest \
		$(ESMF_TESTDIR)/ESMC_FieldGridRegridCsrv2UTest \
		$(ESMF_TESTDIR)/ESMC_FieldGridRegridParUTest \
		$(ESMF_TESTDIR)/ESMC_FieldGridGridRegridUTest \
		$(ESMF_TESTDIR)/ESMC_FieldGridGridRegridCsrvUTest \
		$(ESMF_TESTDIR)/ESMC_FieldTripoleRegridUTest \
		$(ESMF_TESTDIR)/ESMC_FieldSMMFromFileUTest \
		$(ESMF_TESTDIR)/ESMF_FieldUTest \
		$(ESMF_TESTDIR)/ESMF_FieldCreateGetUTest \
		$(ESMF_TESTDIR)/ESMF_FieldRegridCsrvUTest \
		$(ESMF_TESTDIR)/ESMF_FieldRegridCsrv2ndUTest \
		$(ESMF_TESTDIR)/ESMF_FieldRegridUTest \
		$(ESMF_TESTDIR)/ESMF_FieldRegridCSUTest \
		$(ESMF_TESTDIR)/ESMF_FieldArbGridUTest \
		$(ESMF_TESTDIR)/ESMF_FieldIOUTest \
		$(ESMF_TESTDIR)/ESMF_FieldRegridXGSMMUTest \
		$(ESMF_TESTDIR)/ESMF_FieldRegridXGUTest \
		$(ESMF_TESTDIR)/ESMF_FieldStressUTest \
		$(ESMF_TESTDIR)/ESMF_FieldGatherUTest \
		$(ESMF_TESTDIR)/ESMF_FieldRedistUTest \
		$(ESMF_TESTDIR)/ESMF_FieldLSSMMUTest \
		$(ESMF_TESTDIR)/ESMF_FieldSMMUTest   \
		$(ESMF_TESTDIR)/ESMF_FieldRedistArbUTest \
		$(ESMF_TESTDIR)/ESMF_FieldMeshSMMUTest \
		$(ESMF_TESTDIR)/ESMF_FieldHaloUTest \
		$(ESMF_TESTDIR)/ESMF_FieldSMMFromFileUTest

TESTS_RUN     = \
		RUN_ESMC_FieldUTest \
		RUN_ESMC_FieldRegridUTest \
		RUN_ESMC_FieldRegridCsrvUTest \
		RUN_ESMC_FieldGridRegridUTest \
		RUN_ESMC_FieldGridRegrid2UTest \
		RUN_ESMC_FieldGridRegridCsrvUTest \
		RUN_ESMC_FieldGridRegridCsrv2UTest \
		RUN_ESMC_FieldGridRegridParUTest \
		RUN_ESMC_FieldGridGridRegridUTest \
		RUN_ESMC_FieldGridGridRegridCsrvUTest \
		RUN_ESMC_FieldSMMFromFileUTest \
		RUN_ESMF_FieldUTest \
		RUN_ESMF_FieldCreateGetUTest \
		RUN_ESMF_FieldRegridUTest \
		RUN_ESMF_FieldRegridCSUTest \
		RUN_ESMF_FieldRegridCsrvUTest \
		RUN_ESMF_FieldRegridCsrv2ndUTest \
		RUN_ESMF_FieldArbGridUTest \
		RUN_ESMF_FieldIOUTest \
		RUN_ESMF_FieldStressUTest \
		RUN_ESMF_FieldRegridXGSMMUTest \
		RUN_ESMF_FieldRegridXGUTest \
		RUN_ESMF_FieldGatherUTest \
		RUN_ESMF_FieldRedistUTest \
		RUN_ESMF_FieldLSSMMUTest \
		RUN_ESMF_FieldSMMUTest   \
		RUN_ESMF_FieldRedistArbUTest \
		RUN_ESMF_FieldMeshSMMUTest \
		RUN_ESMF_FieldHaloUTest \
		RUN_ESMF_FieldSMMFromFileUTest

#		RUN_ESMC_FieldGridRegridParUTest \

TESTS_RUN_UNI = \
		RUN_ESMC_FieldUTestUNI \
		RUN_ESMC_FieldRegridUTestUNI \
		RUN_ESMC_FieldRegridCsrvUTestUNI \
		RUN_ESMC_FieldGridRegridUTestUNI \
		RUN_ESMC_FieldGridRegrid2UTestUNI \
		RUN_ESMC_FieldGridRegridCsrvUTestUNI \
		RUN_ESMC_FieldGridRegridCsrv2UTestUNI \
		RUN_ESMC_FieldGridGridRegridUTestUNI \
		RUN_ESMC_FieldSMMFromFileUTestUNI \
		RUN_ESMC_FieldGridGridRegridCsrvUTestUNI \
		RUN_ESMF_FieldStressUTestUNI \
		RUN_ESMF_FieldRegridUTestUNI \
		RUN_ESMF_FieldRegridCSUTestUNI \
		RUN_ESMF_FieldRegridXGUTestUNI \
		RUN_ESMF_FieldArbGridUTestUNI \
		RUN_ESMF_FieldRedistArbUTestUNI \
		RUN_ESMF_FieldRegridCsrvUTestUNI \
		RUN_ESMF_FieldRegridCsrv2ndUTestUNI \
		RUN_ESMF_FieldSMMFromFileUTestUNI


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
CLEANFILES  = $(TESTS_BUILD) $(CPFILES) *.nc
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

RUN_ESMC_FieldRegridUTest:
	$(MAKE) TNAME=FieldRegrid NP=1 ctest

RUN_ESMC_FieldRegridUTestUNI:
	$(MAKE) TNAME=FieldRegrid NP=1 ctest

RUN_ESMC_FieldRegridCsrvUTest:
	$(MAKE) TNAME=FieldRegridCsrv NP=1 ctest

RUN_ESMC_FieldRegridCsrvUTestUNI:
	$(MAKE) TNAME=FieldRegridCsrv NP=1 ctest

RUN_ESMC_FieldGridRegridUTest:
	$(MAKE) TNAME=FieldGridRegrid NP=1 ctest

RUN_ESMC_FieldGridRegridUTestUNI:
	$(MAKE) TNAME=FieldGridRegrid NP=1 ctest

RUN_ESMC_FieldGridRegrid2UTest:
	$(MAKE) TNAME=FieldGridRegrid2 NP=1 ctest

RUN_ESMC_FieldGridRegrid2UTestUNI:
	$(MAKE) TNAME=FieldGridRegrid2 NP=1 ctest

RUN_ESMC_FieldGridRegridCsrvUTest:
	$(MAKE) TNAME=FieldGridRegridCsrv NP=1 ctest

RUN_ESMC_FieldGridRegridCsrvUTestUNI:
	$(MAKE) TNAME=FieldGridRegridCsrv NP=1 ctest

RUN_ESMC_FieldGridRegridCsrv2UTest:
	$(MAKE) TNAME=FieldGridRegridCsrv2 NP=1 ctest

RUN_ESMC_FieldGridRegridCsrv2UTestUNI:
	$(MAKE) TNAME=FieldGridRegridCsrv2 NP=1 ctest

RUN_ESMC_FieldGridGridRegridUTest:
	$(MAKE) TNAME=FieldGridGridRegrid NP=4 ctest

RUN_ESMC_FieldGridGridRegridUTestUNI:
	$(MAKE) TNAME=FieldGridGridRegrid NP=1 ctest

RUN_ESMC_FieldGridGridRegridCsrvUTest:
	$(MAKE) TNAME=FieldGridGridRegridCsrv NP=4 ctest

RUN_ESMC_FieldGridGridRegridCsrvUTestUNI:
	$(MAKE) TNAME=FieldGridGridRegridCsrv NP=1 ctest

RUN_ESMC_FieldGridRegridParUTest:
	$(MAKE) TNAME=FieldGridRegridPar NP=4 ctest

RUN_ESMF_FieldCreateGetUTest:
	$(MAKE) TNAME=FieldCreateGet NP=4 ftest

RUN_ESMF_FieldRegridUTest:
	$(MAKE) TNAME=FieldRegrid NP=4 ftest

RUN_ESMF_FieldRegridUTestUNI:
	$(MAKE) TNAME=FieldRegrid NP=1 ftest

RUN_ESMF_FieldRegridCSUTest:
	cp -r data $(ESMF_TESTDIR)
	chmod u+rw $(ESMF_TESTDIR)/data/*
	$(MAKE) TNAME=FieldRegridCS NP=4 ftest

RUN_ESMF_FieldRegridCSUTestUNI:
	$(MAKE) TNAME=FieldRegridCS NP=1 ftest

RUN_ESMF_FieldRegridCsrvUTest:
	$(MAKE) TNAME=FieldRegridCsrv NP=4 ftest

RUN_ESMF_FieldRegridCsrvUTestUNI:
	$(MAKE) TNAME=FieldRegridCsrv NP=1 ftest

RUN_ESMF_FieldRegridCsrv2ndUTest:
	$(MAKE) TNAME=FieldRegridCsrv2nd NP=4 ftest

RUN_ESMF_FieldRegridCsrv2ndUTestUNI:
	$(MAKE) TNAME=FieldRegridCsrv2nd NP=1 ftest

RUN_ESMF_FieldRegridXGSMMUTest:
	$(MAKE) TNAME=FieldRegridXGSMM NP=2 ftest

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
# FieldComm
#

RUN_ESMF_FieldGatherUTest:
	$(MAKE) TNAME=FieldGather NP=4 ftest

RUN_ESMF_FieldRedistUTest:
	$(MAKE) TNAME=FieldRedist NP=4 ftest

RUN_ESMF_FieldRedistArbUTest:
	$(MAKE) TNAME=FieldRedistArb NP=4 ftest

RUN_ESMF_FieldRedistArbUTestUNI:
	$(MAKE) TNAME=FieldRedistArb NP=1 ftest

RUN_ESMF_FieldLSSMMUTest:
	$(MAKE) TNAME=FieldLSSMM NP=4 ftest

RUN_ESMF_FieldMeshSMMUTest:
	$(MAKE) TNAME=FieldMeshSMM NP=4 ftest

RUN_ESMF_FieldSMMUTest:
	$(MAKE) TNAME=FieldSMM NP=4 ftest

RUN_ESMF_FieldHaloUTest:
	$(MAKE) TNAME=FieldHalo NP=4 ftest

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

# ---

RUN_ESMC_FieldSMMFromFileUTest:
	mkdir -p $(ESMF_TESTDIR)/data
	$(MAKE) TNAME=FieldSMMFromFile NP=4 ctest

RUN_ESMC_FieldSMMFromFileUTestUNI:
	mkdir -p $(ESMF_TESTDIR)/data
	$(MAKE) TNAME=FieldSMMFromFile NP=1 ctest

RUN_ESMF_FieldSMMFromFileUTest:
	mkdir -p $(ESMF_TESTDIR)/data
	rm -f $(ESMF_TESTDIR)/data/test_smm_from_file_weights.nc
	cp -f ../../IO/tests/T42_grid.nc $(ESMF_TESTDIR)/data
	chmod u+rw $(ESMF_TESTDIR)/data/*
	$(MAKE) TNAME=FieldSMMFromFile NP=4 ftest

RUN_ESMF_FieldSMMFromFileUTestUNI:
	mkdir -p $(ESMF_TESTDIR)/data
	rm -f $(ESMF_TESTDIR)/data/test_smm_from_file_weights.nc
	cp -f ../../IO/tests/T42_grid.nc $(ESMF_TESTDIR)/data
	chmod u+rw $(ESMF_TESTDIR)/data/*
	$(MAKE) TNAME=FieldSMMFromFile NP=1 ftest

# ---
