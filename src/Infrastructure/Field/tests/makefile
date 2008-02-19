# $Id: makefile,v 1.46 2008/02/19 18:33:49 theurich Exp $ 

ALL:  build_unit_tests
run:  run_unit_tests
run_uni:  run_unit_tests_uni

CFLAGS    =
FFLAGS    =
SOURCEC	  = 
SOURCEF	  =
CPFILES   =

SOURCEH	  = 
OBJSC	  =  
OBJSF	  = 
MANSEC	  = 
LIBBASE	  = 
DIRS	  = 
LOCDIR	  = src/Infrastructure/Field/tests

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_FieldUTest 
#		$(ESMF_TESTDIR)/ESMF_FRoute4UTest \
#		$(ESMF_TESTDIR)/ESMF_FRoute8UTest 

TESTS_RUN     = RUN_ESMF_FieldUTest 
#	 	RUN_ESMF_FRouteUTest \
#               RUN_ESMF_FieldHarness2UTest \
#		RUN_ESMF_FieldHarness4UTest

#TESTS_RUN_UNI = RUN_ESMF_FieldUTestUNI 
#		RUN_ESMF_FRoute4UTestUNI \
#		RUN_ESMF_FRoute8UTestUNI

include ${ESMF_DIR}/makefile

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD) $(CPFILES)
CLOBBERDIRS =

#
# Field
#
RUN_ESMF_FieldUTest:
	$(MAKE) TNAME=Field NP=4 ftest

RUN_ESMF_FieldCreateGetUTest:
	$(MAKE) TNAME=FieldCreateGet NP=4 ftest
