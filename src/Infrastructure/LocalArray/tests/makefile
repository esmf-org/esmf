# $Id: makefile,v 1.11 2005/07/15 03:41:47 nscollins Exp $ 

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
LOCDIR	  = src/Infrastructure/LocalArray/tests


TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_ArrayDataUTest \
		$(ESMF_TESTDIR)/ESMF_LocalArrayUTest 

TESTS_RUN     = RUN_ESMF_ArrayDataUTest \
		RUN_ESMF_LocalArrayUTest 

TESTS_RUN_UNI = RUN_ESMF_ArrayDataUTestUNI \
		RUN_ESMF_LocalArrayUTestUNI



include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD) TestArrayData
CLOBBERDIRS =



#
#  ESMF_ArrayDataUTest
#
RUN_ESMF_ArrayDataUTest:
	$(MAKE) TNAME=ArrayData NP=4 ftest

RUN_ESMF_ArrayDataUTestUNI:
	$(MAKE) TNAME=ArrayData NP=1 ftest


#
#  ESMF_LocalArrayUTest
#
RUN_ESMF_LocalArrayUTest:
	$(MAKE) TNAME=LocalArray NP=4 ftest

RUN_ESMF_LocalArrayUTestUNI:
	$(MAKE) TNAME=LocalArray NP=1 ftest

