# $Id: makefile,v 1.11 2007/05/02 22:58:13 donstark Exp $ 

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

TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_BundleUTest

TESTS_RUN     = RUN_ESMF_BundleUTest \
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


RUN_ESMF_BundleHarness2UTest:
	$(MAKE) TNAME=TestHarness HNAME=BundleHarness2 NP=2 htest

RUN_ESMF_BundleHarness4UTest:
	$(MAKE) TNAME=TestHarness HNAME=BundleHarness4 NP=4 htest
