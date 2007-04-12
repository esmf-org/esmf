# $Id: makefile,v 1.13 2007/04/12 20:16:52 theurich Exp $

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
LOCDIR	  = src/Infrastructure/BundleDataMap/tests

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_BundleDataMapUTest 

TESTS_RUN     = RUN_ESMF_BundleDataMapUTest \

TESTS_RUN_UNI = RUN_ESMF_BundleDataMapUTestUNI \



include ${ESMF_DIR}/makefile

DIRS	  = 

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =

#
# Fortran unit test
#
RUN_ESMF_BundleDataMapUTest:
	$(MAKE) TNAME=BundleDataMap NP=4 ftest

RUN_ESMF_BundleDataMapUTestUNI:
	$(MAKE) TNAME=BundleDataMap NP=1 ftest
