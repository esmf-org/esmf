# $Id: makefile,v 1.1 2007/06/22 23:21:38 cdeluca Exp $ 

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
LOCDIR	  = src/Infrastructure/InternGrid/tests

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_InternGridUTest \
	        $(ESMF_TESTDIR)/ESMF_InternGridWrapUTest \
	        $(ESMF_TESTDIR)/ESMF_InternGridCreateUTest

TESTS_RUN     = RUN_ESMF_InternGridUTest \
                RUN_ESMF_InternGridWrapUTest \
                RUN_ESMF_InternGridCreateUTest

TESTS_RUN_UNI = RUN_ESMF_InternGridUTestUNI \
                RUN_ESMF_InternGridWrapUTestUNI \
                RUN_ESMF_InternGridCreateUTestUNI


include $(ESMF_DIR)/makefile

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =


#
#  InternGrid unit tests
# 
RUN_ESMF_InternGridUTest:
	$(MAKE) TNAME=InternGrid NP=4 ftest

RUN_ESMF_InternGridUTestUNI:
	$(MAKE) TNAME=InternGrid NP=1 ftest



RUN_ESMF_InternGridWrapUTest:
	$(MAKE) TNAME=InternGridWrap NP=4 ftest

RUN_ESMF_InternGridWrapUTestUNI:
	$(MAKE) TNAME=InternGridWrap NP=1 ftest




RUN_ESMF_InternGridCreateUTest:
	$(MAKE) TNAME=InternGridCreate NP=4 ftest

RUN_ESMF_InternGridCreateUTestUNI:
	$(MAKE) TNAME=InternGridCreate NP=1 ftest


