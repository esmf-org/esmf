# $Id: makefile,v 1.16.2.2 2008/06/18 19:14:54 theurich Exp $ 

ALL:  build_unit_tests
run:  run_unit_tests
run_uni: run_unit_tests_uni

CFLAGS    =
FFLAGS    =
SOURCEC	  = 
SOURCEF	  =

SOURCEH	  = 
OBJSC	  =  
OBJSF	  = 
MANSEC	  = 
LIBBASE	  = 
LOCDIR	  = src/Superstructure/State/tests

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_StateCreateUTest
# TODO:FIELDINTEGRATION Restore State reconcile and other tests.
#TESTS_BUILD   =	$(ESMF_TESTDIR)/ESMF_StateCreateUTest \
#		$(ESMF_TESTDIR)/ESMF_StateUTest
#		$(ESMF_TESTDIR)/ESMF_StateReconcileUTest \
#		$(ESMF_TESTDIR)/ESMF_StateLimitUTest \

TESTS_RUN     = RUN_ESMF_StateCreateUTest
#TESTS_RUN     =	RUN_ESMF_StateCreateUTest \
#		RUN_ESMF_StateUTest
#		RUN_ESMF_StateReconcileUTest \
#		RUN_ESMF_StateLimitUTest \

TESTS_RUN_UNI = RUN_ESMF_StateCreateUTestUNI
#TESTS_RUN_UNI =	RUN_ESMF_StateCreateUTestUNI \
#		RUN_ESMF_StateUTestUNI
#		RUN_ESMF_StateReconcileUTestUNI \
#		RUN_ESMF_StateLimitUTestUNI \


include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =

#       
# State Create
#

RUN_ESMF_StateCreateUTest:
	$(MAKE) TNAME=StateCreate NP=4 ftest


RUN_ESMF_StateCreateUTestUNI:
	$(MAKE) TNAME=StateCreate NP=1 ftest


#       
# State Unit Test
#

RUN_ESMF_StateUTest:
	$(MAKE) TNAME=State NP=4 ftest

RUN_ESMF_StateUTestUNI:
	$(MAKE) TNAME=State NP=1 ftest



#       
# StateReconcile Unit Test
#

RUN_ESMF_StateReconcileUTest:
	$(MAKE) TNAME=StateReconcile NP=4 ftest

RUN_ESMF_StateReconcileUTestUNI:
	$(MAKE) TNAME=StateReconcile NP=1 ftest



#       
# StateLimit Unit Test
#

UTEST_StateLimit_OBJS = ESMF_StateHelpers.o

ESMF_StateLimitUTest.o: ESMF_StateHelpers.o

RUN_ESMF_StateLimitUTest:
	$(MAKE) TNAME=StateLimit NP=4 ftest

RUN_ESMF_StateLimitUTestUNI:
	$(MAKE) TNAME=StateLimit NP=1 ftest


