# $Id: makefile,v 1.42.2.2 2008/03/07 01:10:26 feiliu Exp $ 

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

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_FieldUTest  \
		$(ESMF_TESTDIR)/ESMF_FieldCreateGetUTest

TESTS_RUN     = RUN_ESMF_FieldUTest \
		RUN_ESMF_FieldCreateGetUTest

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
