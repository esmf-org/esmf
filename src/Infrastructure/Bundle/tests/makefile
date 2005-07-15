# $Id: makefile,v 1.9 2005/07/15 03:41:46 nscollins Exp $ 

ALL:  build_unit_tests
run:  tree_run_unit_tests

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

TESTS_RUN     = RUN_ESMF_BundleUTest 

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


