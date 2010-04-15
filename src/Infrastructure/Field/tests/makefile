# $Id: makefile,v 1.72 2010/04/15 15:22:21 garyblock Exp $ 
#
# 03/15/2010  gblock  removed exhaustive/non-exhaustive test harness logic
#                     implemented test harness selection using taget based on ESMF_TESTHARNESS_FIELD variable
#                     implemented simplified test harness run targets in /build/common.mk
#

ALL:      build_unit_tests
run:      run_unit_tests
run_uni:  run_unit_tests_uni

LOCDIR	  = src/Infrastructure/Field/tests

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_FieldUTest \
		$(ESMF_TESTDIR)/ESMC_FieldUTest \
		$(ESMF_TESTDIR)/ESMF_FieldCreateGetUTest \
		$(ESMF_TESTDIR)/ESMF_FieldRegridUTest \
		$(ESMF_TESTDIR)/ESMF_FieldArbGridUTest \
		$(ESMF_TESTDIR)/ESMF_FieldStressUTest
               
TESTS_RUN     = RUN_ESMF_FieldUTest \
		RUN_ESMF_FieldCreateGetUTest \
		RUN_ESMF_FieldRegridUTest \
		RUN_ESMF_FieldArbGridUTest \
		RUN_ESMF_FieldStressUTest \
		RUN_ESMC_FieldUTest

#
# if ESMF_TESTHARNESS_FIELD not null, append test harness suite target, otherwise use default target
#
ifneq ($(ESMF_TESTHARNESS_FIELD), )
        TESTS_RUN := $(TESTS_RUN) $(ESMF_TESTHARNESS_FIELD)
else
	TESTS_RUN := $(TESTS_RUN) RUN_ESMF_TestHarnessField_default
endif

TESTS_RUN_UNI = RUN_ESMF_FieldStressUTestUNI \
	RUN_ESMF_FieldRegridUTestUNI \
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
# FieldHarness
#
RUN_ESMF_TestHarnessField_default:
	$(MAKE) TESTHARNESSCASE=field_default NP=4 run_test_harness

#
RUN_ESMF_TestHarnessField_1:
	$(MAKE) TESTHARNESSCASE=field_1 NP=4 run_test_harness

# remove old test harness stuff
#RUN_ESMF_FieldHarness1UTest:        
#	@if [ -f $(ESMF_TESTDIR)/test_harness.list ] ; then \
#	  if ! grep FieldHarness1UTest $(ESMF_TESTDIR)/test_harness.list ; then \
#	    echo "ESMF_FieldHarness1UTest" >> $(ESMF_TESTDIR)/test_harness.list ; \
#	  fi ; \
#	else \
#	  echo "ESMF_FieldHarness1UTest" > $(ESMF_TESTDIR)/test_harness.list ; \
#	fi
#	cp -f test_harness.rc $(ESMF_TESTDIR)        
#	cp -f harness_config/*.rc $(ESMF_TESTDIR)        
#	rm -f $(ESMF_TESTDIR)/ESMF_TestHarnessUTest.stdout        
#	$(MAKE) TNAME=TestHarness HNAME=FieldHarness1 NP=1 htest        
#	@if [ -f $(ESMF_TESTDIR)/ESMF_TestHarnessUTest.stdout ] ; then \
#	  cp -f $(ESMF_TESTDIR)/ESMF_TestHarnessUTest.stdout $(ESMF_TESTDIR)/ESMF_FieldHarness1UTest.stdout ; \
#	fi 
#
#RUN_ESMF_FieldHarness2UTest:        
#	@if [ -f $(ESMF_TESTDIR)/test_harness.list ] ; then \
#	  if ! grep FieldHarness2UTest $(ESMF_TESTDIR)/test_harness.list ; then \
#	    echo "ESMF_FieldHarness2UTest" >> $(ESMF_TESTDIR)/test_harness.list ; \
#	  fi ; \
#	else \
#	  echo "ESMF_FieldHarness2UTest" > $(ESMF_TESTDIR)/test_harness.list ; \
#	fi
#	cp -f test_harness.rc $(ESMF_TESTDIR)        
#	cp -f harness_config/*.rc $(ESMF_TESTDIR)        
#	rm -f $(ESMF_TESTDIR)/ESMF_TestHarnessUTest.stdout        
#	$(MAKE) TNAME=TestHarness HNAME=FieldHarness2 NP=2 htest        
#	@if [ -f $(ESMF_TESTDIR)/ESMF_TestHarnessUTest.stdout ] ; then \
#	  cp -f $(ESMF_TESTDIR)/ESMF_TestHarnessUTest.stdout $(ESMF_TESTDIR)/ESMF_FieldHarness2UTest.stdout ; \
#	fi 
#
#RUN_ESMF_FieldHarness4UTest:        
#	@if [ -f $(ESMF_TESTDIR)/test_harness.list ] ; then \
#	  if ! grep FieldHarness4UTest $(ESMF_TESTDIR)/test_harness.list ; then \
#	    echo "ESMF_FieldHarness4UTest" >> $(ESMF_TESTDIR)/test_harness.list ; \
#	  fi ; \
#	else \
#	  echo "ESMF_FieldHarness4UTest" > $(ESMF_TESTDIR)/test_harness.list ; \
#	fi
#	cp -f test_harness.rc $(ESMF_TESTDIR)        
#	cp -f harness_config/*.rc $(ESMF_TESTDIR)        
#	rm -f $(ESMF_TESTDIR)/ESMF_TestHarnessUTest.stdout        
#	$(MAKE) TNAME=TestHarness HNAME=FieldHarness4 NP=4 htest        
#	@if [ -f $(ESMF_TESTDIR)/ESMF_TestHarnessUTest.stdout ] ; then \
#	  cp -f $(ESMF_TESTDIR)/ESMF_TestHarnessUTest.stdout $(ESMF_TESTDIR)/ESMF_FieldHarness4UTest.stdout ; \
#	fi 

