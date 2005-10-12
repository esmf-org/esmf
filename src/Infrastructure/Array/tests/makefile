# $Id: makefile,v 1.10 2005/10/12 19:06:15 nscollins Exp $ 

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
LOCDIR	  = src/Infrastructure/Array/tests


TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_ArrayUTest \
		$(ESMF_TESTDIR)/ESMF_ArrayGatherUTest

TESTS_RUN     = RUN_ESMF_ArrayUTest \
		RUN_ESMF_ArrayGatherUTest

TESTS_RUN_UNI = RUN_ESMF_ArrayUTestUNI \
		RUN_ESMF_ArrayGatherUTestUNI



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



#
#  ESMF_ArrayGatherUTest
#
RUN_ESMF_ArrayGatherUTest:
	$(MAKE) TNAME=ArrayGather NP=4 ftest

RUN_ESMF_ArrayGatherUTestUNI:
	$(MAKE) TNAME=ArrayGather NP=1 ftest




