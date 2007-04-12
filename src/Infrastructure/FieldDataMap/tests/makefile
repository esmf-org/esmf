# $Id: makefile,v 1.13 2007/04/12 20:37:25 theurich Exp $

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
LOCDIR	  = src/Infrastructure/FieldDataMap/tests

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_FieldDataMapUTest

TESTS_RUN     = RUN_ESMF_FieldDataMapUTest

TESTS_RUN_UNI = RUN_ESMF_FieldDataMapUTestUNI


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

