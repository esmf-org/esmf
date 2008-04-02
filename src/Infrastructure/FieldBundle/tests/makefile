# $Id: makefile,v 1.2 2008/04/02 20:42:55 cdeluca Exp $ 

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
                $(ESMC_TESTDIR)/ESMF_FieldBundleCreateGetUTest \
                $(ESMC_TESTDIR)/ESMF_FieldBundleAttrUTest

TESTS_RUN     = RUN_ESMF_FieldBundleUTest \
                RUN_ESMF_FieldBundleCreateGetUTest \
                RUN_ESMF_FieldBundleAttrUTest \
                RUN_ESMF_FieldBundleHarness2UTest \
                RUN_ESMF_FieldBundleHarness4UTest

TESTS_RUN_UNI = RUN_ESMF_FieldBundleUTestUNI \
                RUN_ESMF_FieldBundleAttrUTestUNI 


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

RUN_ESMF_FieldBundleCreateGetUTest:
	$(MAKE) TNAME=FieldBundleCreateGet NP=4 ftest

RUN_ESMF_FieldBundleAttrUTest:
	$(MAKE) TNAME=FieldBundleAttr NP=4 ftest

RUN_ESMF_FieldBundleAttrUTestUNI:
	$(MAKE) TNAME=FieldBundleAttr NP=1 ftest

RUN_ESMF_FieldBundleHarness2UTest:
	cp -f test_harness.rc $(ESMF_TESTDIR)
	cp -f harness_config/*.rc $(ESMF_TESTDIR)
	$(MAKE) TNAME=TestHarness HNAME=FieldBundleHarness2 NP=2 htest

RUN_ESMF_FieldBundleHarness4UTest:
	cp -f test_harness.rc $(ESMF_TESTDIR)
	cp -f harness_config/*.rc $(ESMF_TESTDIR)
	$(MAKE) TNAME=TestHarness HNAME=FieldBundleHarness4 NP=4 htest
