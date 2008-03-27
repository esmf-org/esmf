# $Id: makefile,v 1.50 2008/03/27 01:21:25 theurich Exp $ 

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

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_FieldUTest \
                $(ESMF_TESTDIR)/ESMF_FieldCreateGetUTest \
                $(ESMF_TESTDIR)/ESMF_FieldAttrUTest

TESTS_RUN     = RUN_ESMF_FieldUTest \
		RUN_ESMF_FieldCreateGetUTest \
                RUN_ESMF_FieldAttrUTest

TESTS_RUN_UNI = RUN_ESMF_FieldAttrUTestUNI

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

#
# Field Attributes
#

RUN_ESMF_FieldAttrUTest:
	$(MAKE) TNAME=FieldAttr NP=4 ftest

RUN_ESMF_FieldAttrUTestUNI:
	$(MAKE) TNAME=FieldAttr NP=1 ftest
