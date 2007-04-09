# $Id: makefile,v 1.13 2007/04/09 18:20:26 theurich Exp $ 

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
LOCDIR	  = src/Infrastructure/LocalArray/tests


TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_LocalArrayDataUTest \
		$(ESMF_TESTDIR)/ESMF_LocalArrayUTest 

TESTS_RUN     = RUN_ESMF_LocalArrayDataUTest \
		RUN_ESMF_LocalArrayUTest 

TESTS_RUN_UNI = RUN_ESMF_LocalArrayDataUTestUNI \
		RUN_ESMF_LocalArrayUTestUNI



include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD) TestArrayData
CLOBBERDIRS =



#
#  ESMF_LocalArrayDataUTest
#
RUN_ESMF_LocalArrayDataUTest:
	$(MAKE) TNAME=LocalArrayData NP=4 ftest

RUN_ESMF_LocalArrayDataUTestUNI:
	$(MAKE) TNAME=LocalArrayData NP=1 ftest


#
#  ESMF_LocalArrayUTest
#
RUN_ESMF_LocalArrayUTest:
	$(MAKE) TNAME=LocalArray NP=4 ftest

RUN_ESMF_LocalArrayUTestUNI:
	$(MAKE) TNAME=LocalArray NP=1 ftest

