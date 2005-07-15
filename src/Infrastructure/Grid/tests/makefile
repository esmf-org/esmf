# $Id: makefile,v 1.14 2005/07/15 03:41:46 nscollins Exp $ 

ALL:  tree_build_unit_tests
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
LOCDIR	  = src/Infrastructure/Grid/tests

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_GridUTest \
	        $(ESMF_TESTDIR)/ESMF_GridCreateUTest

TESTS_RUN     = RUN_ESMF_GridUTest \
                RUN_ESMF_GridCreateUTest

TESTS_RUN_UNI = RUN_ESMF_GridUTestUNI \
                RUN_ESMF_GridCreateUTestUNI


include $(ESMF_DIR)/makefile

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =


#
#  Grid unit tests
# 
RUN_ESMF_GridUTest:
	$(MAKE) TNAME=Grid NP=4 ftest

RUN_ESMF_GridUTestUNI:
	$(MAKE) TNAME=Grid NP=1 ftest



RUN_ESMF_GridCreateUTest:
	$(MAKE) TNAME=GridCreate NP=4 ftest

RUN_ESMF_GridCreateUTestUNI:
	$(MAKE) TNAME=GridCreate NP=1 ftest


