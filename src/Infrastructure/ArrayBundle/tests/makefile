# $Id: makefile,v 1.1.2.1 2008/04/24 00:15:56 theurich Exp $ 

ALL:      build_unit_tests
run:      run_unit_tests
run_uni:  run_unit_tests_uni

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
LOCDIR	  = src/Infrastructure/ArrayBundle/tests

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_ArrayBundleCreateUTest


TESTS_RUN     = RUN_ESMF_ArrayBundleCreateUTest

TESTS_RUN_UNI =


include ${ESMF_DIR}/makefile

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =


#
# unit test targets
#

RUN_ESMF_ArrayBundleCreateUTest:
	$(MAKE) TNAME=ArrayBundleCreate NP=4 ftest
