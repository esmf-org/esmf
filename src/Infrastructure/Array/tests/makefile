# $Id$ 

ALL:      build_unit_tests
run:      run_unit_tests
run_uni:  run_unit_tests_uni

LOCDIR	  = src/Infrastructure/Array/tests

.NOTPARALLEL:
TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_ArrayCreateGetUTest \
                $(ESMF_TESTDIR)/ESMF_ArrayDataUTest \
                $(ESMF_TESTDIR)/ESMF_ArrayScatterUTest \
                $(ESMF_TESTDIR)/ESMF_ArrayGatherUTest \
                $(ESMF_TESTDIR)/ESMF_ArrayIOUTest \
                $(ESMF_TESTDIR)/ESMF_ArraySMMUTest \
                $(ESMF_TESTDIR)/ESMF_ArrayArbIdxSMMUTest \
                $(ESMF_TESTDIR)/ESMF_ArrayRedistUTest \
                $(ESMF_TESTDIR)/ESMF_ArrayHaloUTest \
                $(ESMF_TESTDIR)/ESMC_ArrayUTest 

TESTS_RUN     = RUN_ESMF_ArrayCreateGetUTest \
                RUN_ESMF_ArrayDataUTest  \
                RUN_ESMF_ArrayScatterUTest \
                RUN_ESMF_ArrayGatherUTest \
                RUN_ESMF_ArrayIOUTest \
                RUN_ESMF_ArraySMMUTest \
                RUN_ESMF_ArrayArbIdxSMMUTest \
                RUN_ESMF_ArrayRedistUTest \
                RUN_ESMF_ArrayHaloUTest \
                RUN_ESMC_ArrayUTest 

TESTS_RUN_UNI = RUN_ESMF_ArrayDataUTestUNI \
                RUN_ESMF_ArraySMMUTestUNI \
                RUN_ESMC_ArrayUTestUNI

#
# check ESMF_TESTHARNESS_ARRAY for default, 
# otherwise append test harness suite target
#
ifeq ($(ESMF_TESTHARNESS_ARRAY), default)
  TESTS_RUN := $(TESTS_RUN) RUN_ESMF_TestHarnessArray_default
  TESTS_RUN_UNI := $(TESTS_RUN_UNI) RUN_ESMF_TestHarnessArrayUNI_default
else
  TESTS_RUN := $(TESTS_RUN) $(ESMF_TESTHARNESS_ARRAY)
  TESTS_RUN_UNI := $(TESTS_RUN_UNI) $(ESMF_TESTHARNESS_ARRAY)
endif

include ${ESMF_DIR}/makefile

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =

DIRS        = 

#
# unit test targets
#

RUN_ESMF_ArrayCreateGetUTest:
	$(MAKE) TNAME=ArrayCreateGet NP=4 ftest

# ---

RUN_ESMF_ArrayDataUTest:
	$(MAKE) TNAME=ArrayData NP=4 ftest

RUN_ESMF_ArrayDataUTestUNI:
	$(MAKE) TNAME=ArrayData NP=1 ftest

# ---

RUN_ESMF_ArrayScatterUTest:
	$(MAKE) TNAME=ArrayScatter NP=4 ftest

# ---

RUN_ESMF_ArrayGatherUTest:
	$(MAKE) TNAME=ArrayGather NP=4 ftest

# ---

RUN_ESMF_ArrayIOUTest:
	$(MAKE) TNAME=ArrayIO NP=6 ftest

# ---

RUN_ESMF_ArraySMMUTest:
	$(MAKE) TNAME=ArraySMM NP=6 ftest

RUN_ESMF_ArraySMMUTestUNI:
	$(MAKE) TNAME=ArraySMM NP=1 ftest

# ---

RUN_ESMF_ArrayArbIdxSMMUTest:
	$(MAKE) TNAME=ArrayArbIdxSMM NP=6 ftest

# ---

RUN_ESMF_ArrayRedistUTest:
	$(MAKE) TNAME=ArrayRedist NP=6 ftest

# ---

RUN_ESMF_ArrayHaloUTest:
	$(MAKE) TNAME=ArrayHalo NP=4 ftest

# ---

RUN_ESMC_ArrayUTest:
	$(MAKE) TNAME=Array NP=4 ctest

RUN_ESMC_ArrayUTestUNI:
	$(MAKE) TNAME=Array NP=1 ctest

# ---
#
# TestHarness tests
#
RUN_ESMF_TestHarnessArray_default:
	$(MAKE) TESTHARNESSCASE=array_default NP=4 run_test_harness

#
RUN_ESMF_TestHarnessArray_1:
	$(MAKE) TESTHARNESSCASE=array_1 NP=4 run_test_harness

#
RUN_ESMF_TestHarnessArray_2:
	$(MAKE) TESTHARNESSCASE=array_2 NP=4 run_test_harness

# UNI test harness
RUN_ESMF_TestHarnessArrayUNI_default:
	$(MAKE) TESTHARNESSCASE=array_default NP=1 run_test_harness

#
RUN_ESMF_TestHarnessArrayUNI_1:
	$(MAKE) TESTHARNESSCASE=array_1 NP=1 run_test_harness

#
RUN_ESMF_TestHarnessArrayUNI_2:
	$(MAKE) TESTHARNESSCASE=array_2 NP=1 run_test_harness


##
