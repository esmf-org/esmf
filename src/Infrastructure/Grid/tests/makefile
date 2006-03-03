# $Id: makefile,v 1.16 2006/03/03 21:52:47 nscollins Exp $ 

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
LOCDIR	  = src/Infrastructure/Grid/tests

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_GridUTest \
	        $(ESMF_TESTDIR)/ESMF_GridWrapUTest \
	        $(ESMF_TESTDIR)/ESMF_GridCreateUTest

TESTS_RUN     = RUN_ESMF_GridUTest \
                RUN_ESMF_GridWrapUTest \
                RUN_ESMF_GridCreateUTest

TESTS_RUN_UNI = RUN_ESMF_GridUTestUNI \
                RUN_ESMF_GridWrapUTest \
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



RUN_ESMF_GridWrapUTest:
	$(MAKE) TNAME=GridWrap NP=4 ftest

RUN_ESMF_GridWrapUTestUNI:
	$(MAKE) TNAME=GridWrap NP=1 ftest




RUN_ESMF_GridCreateUTest:
	$(MAKE) TNAME=GridCreate NP=4 ftest

RUN_ESMF_GridCreateUTestUNI:
	$(MAKE) TNAME=GridCreate NP=1 ftest


