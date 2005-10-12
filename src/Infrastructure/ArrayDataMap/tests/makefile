# $Id: makefile,v 1.13 2005/10/12 19:06:16 nscollins Exp $

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
LOCDIR	  = src/Infrastructure/ArrayDataMap/tests

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_ArrayDataMapUTest
#	        $(ESMF_TESTDIR)/ESMC_ArrayDataMapUTest 

TESTS_RUN     = RUN_ESMF_ArrayDataMapUTest
#		RUN_ESMC_ArrayDataMapUTest 

TESTS_RUN_UNI = RUN_ESMF_ArrayDataMapUTestUNI
#		RUN_ESMC_ArrayDataMapUTestUNI 



include ${ESMF_DIR}/makefile

DIRS	  = 

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =

#
# Fortran unit test
#
RUN_ESMF_ArrayDataMapUTest:
	$(MAKE) TNAME=ArrayDataMap NP=4 ftest

RUN_ESMF_ArrayDataMapUTestUNI:
	$(MAKE) TNAME=ArrayDataMap NP=1 ftest


#
# C++ Unit tests
#

RUN_ESMC_ArrayDataMapUTest:
	$(MAKE) TNAME=ArrayDataMap NP=4 ctest

RUN_ESMC_ArrayDataMapUTestUNI:
	$(MAKE) TNAME=ArrayDataMap NP=1 ctest



