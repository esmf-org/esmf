# $Id: makefile,v 1.11 2005/07/15 03:41:46 nscollins Exp $

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
LOCDIR	  = src/Infrastructure/BundleDataMap/tests

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_BundleDataMapUTest 
#	        $(ESMF_TESTDIR)/ESMC_BundleDataMapUTest 
# Comment out temporarily

TESTS_RUN     = RUN_ESMF_BundleDataMapUTest \
#		RUN_ESMC_BundleDataMapUTest 
# Comment out temporarily

TESTS_RUN_UNI = RUN_ESMF_BundleDataMapUTestUNI \
#		RUN_ESMC_BundleDataMapUTestUNI 
# Comment out temporarily



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


#
# C++ Unit tests
#
RUN_ESMC_BundleDataMapUTest:
	$(MAKE) TNAME=BundleDataMap NP=4 ctest

RUN_ESMC_BundleDataMapUTestUNI:
	$(MAKE) TNAME=BundleDataMap NP=1 ctest



