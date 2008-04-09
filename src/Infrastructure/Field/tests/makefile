# $Id: makefile,v 1.42.2.5 2008/04/09 01:19:38 theurich Exp $ 

ALL:      build_unit_tests
run:      run_unit_tests
run_uni:  run_unit_tests_uni

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
LOCDIR	  = src/Infrastructure/Field/tests

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_FieldUTest \
                $(ESMF_TESTDIR)/ESMF_FieldCreateGetUTest \
                $(ESMF_TESTDIR)/ESMF_FieldStressUTest

TESTS_RUN     = RUN_ESMF_FieldUTest \
                RUN_ESMF_FieldCreateGetUTest \
                RUN_ESMF_FieldStressUTest

TESTS_RUN_UNI = RUN_ESMF_FieldStressUTestUNI

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

RUN_ESMF_FieldStressUTest:
	$(MAKE) TNAME=FieldStress NP=1 ftest

RUN_ESMF_FieldStressUTestUNI:
	$(MAKE) TNAME=FieldStress NP=1 ftest
