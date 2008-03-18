# $Id: makefile,v 1.12.2.1 2008/03/18 18:39:31 feiliu Exp $ 

ALL:  build_unit_tests
run:  run_unit_tests

CFLAGS    =
FFLAGS    =
SOURCEC	  = 
SOURCEF	  =

SOURCEH	  = 
OBJSC	  =  
OBJSF	  = 
MANSEC	  = 
LIBBASE	  = 
DIRS	  = 
LOCDIR	  = src/Infrastructure/Bundle/tests

TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_BundleUTest \
	$(ESMC_TESTDIR)/ESMF_BundleCreateGetUTest

TESTS_RUN     = RUN_ESMF_BundleUTest \
	RUN_ESMF_BundleCreateGetUTest \
	RUN_ESMF_BundleHarness2UTest \
	RUN_ESMF_BundleHarness4UTest

TESTS_RUN_UNI = RUN_ESMF_BundleUTestUNI 


include ${ESMF_DIR}/makefile

DIRS      =

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =


#
#  Bundles
#
RUN_ESMF_BundleUTest:
	$(MAKE) TNAME=Bundle NP=4 ftest

RUN_ESMF_BundleUTestUNI:
	$(MAKE) TNAME=Bundle NP=1 ftest

RUN_ESMF_BundleCreateGetUTest:
	$(MAKE) TNAME=BundleCreateGet NP=4 ftest

RUN_ESMF_BundleHarness2UTest:
	cp -f test_harness.rc $(ESMF_TESTDIR)
	cp -f harness_config/*.rc $(ESMF_TESTDIR)
	$(MAKE) TNAME=TestHarness HNAME=BundleHarness2 NP=2 htest

RUN_ESMF_BundleHarness4UTest:
	cp -f test_harness.rc $(ESMF_TESTDIR)
	cp -f harness_config/*.rc $(ESMF_TESTDIR)
	$(MAKE) TNAME=TestHarness HNAME=BundleHarness4 NP=4 htest
