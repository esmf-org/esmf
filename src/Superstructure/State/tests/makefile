# $Id: makefile,v 1.12 2005/10/12 19:06:22 nscollins Exp $ 

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
LOCDIR	  = src/Superstructure/State/tests

TESTS_BUILD   =	$(ESMF_TESTDIR)/ESMF_StateCreateUTest \
		$(ESMF_TESTDIR)/ESMF_StateReconcileUTest \
		$(ESMF_TESTDIR)/ESMF_StateUTest

TESTS_RUN     =	RUN_ESMF_StateCreateUTest \
		RUN_ESMF_StateReconcileUTest \
		RUN_ESMF_StateUTest

TESTS_RUN_UNI =	RUN_ESMF_StateCreateUTestUNI \
		RUN_ESMF_StateReconcileUTestUNI \
		RUN_ESMF_StateUTestUNI


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


