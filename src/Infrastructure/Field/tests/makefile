# $Id: makefile,v 1.71 2009/10/20 18:45:46 feiliu Exp $ 

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
 
ifeq ($(ESMF_TESTHARNESS), NONEXHAUSTIVE)
  TESTS_RUN  := $(TESTS_RUN) \
		RUN_ESMF_FieldHarness2UTest
else
ifeq ($(ESMF_TESTHARNESS), EXHAUSTIVE)
  TESTS_RUN  := $(TESTS_RUN) \
		RUN_ESMF_FieldHarness1UTest \
		RUN_ESMF_FieldHarness4UTest
endif
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

RUN_ESMF_FieldHarness1UTest:        
	@if [ -f $(ESMF_TESTDIR)/test_harness.list ] ; then \
	  if ! grep FieldHarness1UTest $(ESMF_TESTDIR)/test_harness.list ; then \
	    echo "ESMF_FieldHarness1UTest" >> $(ESMF_TESTDIR)/test_harness.list ; \
	  fi ; \
	else \
	  echo "ESMF_FieldHarness1UTest" > $(ESMF_TESTDIR)/test_harness.list ; \
	fi
	cp -f test_harness.rc $(ESMF_TESTDIR)        
	cp -f harness_config/*.rc $(ESMF_TESTDIR)        
	rm -f $(ESMF_TESTDIR)/ESMF_TestHarnessUTest.stdout        
	$(MAKE) TNAME=TestHarness HNAME=FieldHarness1 NP=1 htest        
	@if [ -f $(ESMF_TESTDIR)/ESMF_TestHarnessUTest.stdout ] ; then \
	  cp -f $(ESMF_TESTDIR)/ESMF_TestHarnessUTest.stdout $(ESMF_TESTDIR)/ESMF_FieldHarness1UTest.stdout ; \
	fi 

RUN_ESMF_FieldHarness2UTest:        
	@if [ -f $(ESMF_TESTDIR)/test_harness.list ] ; then \
	  if ! grep FieldHarness2UTest $(ESMF_TESTDIR)/test_harness.list ; then \
	    echo "ESMF_FieldHarness2UTest" >> $(ESMF_TESTDIR)/test_harness.list ; \
	  fi ; \
	else \
	  echo "ESMF_FieldHarness2UTest" > $(ESMF_TESTDIR)/test_harness.list ; \
	fi
	cp -f test_harness.rc $(ESMF_TESTDIR)        
	cp -f harness_config/*.rc $(ESMF_TESTDIR)        
	rm -f $(ESMF_TESTDIR)/ESMF_TestHarnessUTest.stdout        
	$(MAKE) TNAME=TestHarness HNAME=FieldHarness2 NP=2 htest        
	@if [ -f $(ESMF_TESTDIR)/ESMF_TestHarnessUTest.stdout ] ; then \
	  cp -f $(ESMF_TESTDIR)/ESMF_TestHarnessUTest.stdout $(ESMF_TESTDIR)/ESMF_FieldHarness2UTest.stdout ; \
	fi 

RUN_ESMF_FieldHarness4UTest:        
	@if [ -f $(ESMF_TESTDIR)/test_harness.list ] ; then \
	  if ! grep FieldHarness4UTest $(ESMF_TESTDIR)/test_harness.list ; then \
	    echo "ESMF_FieldHarness4UTest" >> $(ESMF_TESTDIR)/test_harness.list ; \
	  fi ; \
	else \
	  echo "ESMF_FieldHarness4UTest" > $(ESMF_TESTDIR)/test_harness.list ; \
	fi
	cp -f test_harness.rc $(ESMF_TESTDIR)        
	cp -f harness_config/*.rc $(ESMF_TESTDIR)        
	rm -f $(ESMF_TESTDIR)/ESMF_TestHarnessUTest.stdout        
	$(MAKE) TNAME=TestHarness HNAME=FieldHarness4 NP=4 htest        
	@if [ -f $(ESMF_TESTDIR)/ESMF_TestHarnessUTest.stdout ] ; then \
	  cp -f $(ESMF_TESTDIR)/ESMF_TestHarnessUTest.stdout $(ESMF_TESTDIR)/ESMF_FieldHarness4UTest.stdout ; \
	fi 



