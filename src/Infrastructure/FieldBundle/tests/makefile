# $Id: makefile,v 1.7 2008/05/28 18:38:42 feiliu Exp $ 

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
LOCDIR	  = src/Infrastructure/FieldBundle/tests

TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_FieldBundleUTest \
                $(ESMC_TESTDIR)/ESMF_FieldBundleCrGetUTest \
                $(ESMC_TESTDIR)/ESMF_FieldBundleRedistUTest \
                $(ESMC_TESTDIR)/ESMF_FieldBundleSMMUTest

TESTS_RUN     = RUN_ESMF_FieldBundleUTest \
                RUN_ESMF_FieldBundleCrGetUTest \
                RUN_ESMF_FieldBundleRedistUTest \
                RUN_ESMF_FieldBundleSMMUTest \
                RUN_ESMF_FieldBundleHarness2UTest \
                RUN_ESMF_FieldBundleHarness4UTest

TESTS_RUN_UNI = RUN_ESMF_FieldBundleUTestUNI 


include ${ESMF_DIR}/makefile

DIRS      =

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =


#
#  FieldBundles
#
RUN_ESMF_FieldBundleUTest:
	$(MAKE) TNAME=FieldBundle NP=4 ftest

RUN_ESMF_FieldBundleUTestUNI:
	$(MAKE) TNAME=FieldBundle NP=1 ftest

RUN_ESMF_FieldBundleCrGetUTest:
	$(MAKE) TNAME=FieldBundleCrGet NP=4 ftest

RUN_ESMF_FieldBundleRedistUTest:
	$(MAKE) TNAME=FieldBundleRedist NP=4 ftest

RUN_ESMF_FieldBundleSMMUTest:
	$(MAKE) TNAME=FieldBundleSMM NP=4 ftest

RUN_ESMF_FieldBundleHarness2UTest:
	cp -f test_harness.rc $(ESMF_TESTDIR)
	cp -f harness_config/*.rc $(ESMF_TESTDIR)
	$(MAKE) TNAME=TestHarness HNAME=FieldBundleHarness2 NP=2 htest

RUN_ESMF_FieldBundleHarness4UTest:
	cp -f test_harness.rc $(ESMF_TESTDIR)
	cp -f harness_config/*.rc $(ESMF_TESTDIR)
	$(MAKE) TNAME=TestHarness HNAME=FieldBundleHarness4 NP=4 htest
