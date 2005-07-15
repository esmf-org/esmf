# $Id: makefile,v 1.8 2005/07/15 03:41:46 nscollins Exp $ 

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
LOCDIR	  = src/Infrastructure/Array/tests


TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_ArrayUTest

TESTS_RUN     = RUN_ESMF_ArrayUTest

TESTS_RUN_UNI = RUN_ESMF_ArrayUTestUNI



include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD) 
CLOBBERDIRS =


#
#  ESMF_ArrayUTest
#
RUN_ESMF_ArrayUTest:
	$(MAKE) TNAME=Array NP=4 ftest

RUN_ESMF_ArrayUTestUNI:
	$(MAKE) TNAME=Array NP=1 ftest



