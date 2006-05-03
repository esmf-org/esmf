# $Id: makefile,v 1.12 2006/05/03 21:54:33 theurich Exp $ 

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
LOCDIR	  = src/Infrastructure/Array/tests

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_ArrayScatterUTest


TESTS_RUN     = RUN_ESMF_ArrayScatterUTest

TESTS_RUN_UNI = RUN_ESMF_ArrayScatterUTestUNI


include ${ESMF_DIR}/makefile

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =


#
# DELayout unit test
#
RUN_ESMF_ArrayScatterUTest:
	$(MAKE) TNAME=ArrayScatter NP=4 ftest

RUN_ESMF_ArrayScatterUTestUNI:
	$(MAKE) TNAME=ArrayScatter NP=1 ftest
