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
LOCDIR	  = src/Infrastructure/FieldDataMap/tests

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_FieldDataMapUTest \
#	        $(ESMF_TESTDIR)/ESMC_FieldDataMapUTest 

TESTS_RUN     = RUN_ESMF_FieldDataMapUTest \
#		RUN_ESMC_FieldDataMapUTest 

TESTS_RUN_UNI = RUN_ESMF_FieldDataMapUTestUNI \
#		RUN_ESMC_FieldDataMapUTestUNI 


include ${ESMF_DIR}/makefile

DIRS	  = 

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =

#
# Fortran unit test
#
RUN_ESMF_FieldDataMapUTest:
	$(MAKE) TNAME=FieldDataMap NP=4 ftest

RUN_ESMF_FieldDataMapUTestUNI:
	$(MAKE) TNAME=FieldDataMap NP=1 ftest


#
# C++ Unit tests
#
RUN_ESMC_FieldDataMapUTest:
	$(MAKE) TNAME=FieldDataMap NP=4 ctest

RUN_ESMC_FieldDataMapUTestUNI:
	$(MAKE) TNAME=FieldDataMap NP=1 ctest



