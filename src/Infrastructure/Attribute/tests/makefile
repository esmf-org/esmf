# $Id: makefile,v 1.3 2008/03/26 17:34:58 rokuingh Exp $ 

ALL:  build_unit_tests
run:  run_unit_tests
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
LOCDIR	  = src/Infrastructure/Attribute/tests

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_AttributeUTest

TESTS_RUN     = RUN_ESMF_AttributeUTest

TESTS_RUN_UNI = RUN_ESMF_AttributeUTestUNI


include ${ESMF_DIR}/makefile

DIRS        =

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =


#
#  Attribute class
#

RUN_ESMF_AttributeUTest:
	$(MAKE) TNAME=Attribute NP=4 ftest

RUN_ESMF_AttributeUTestUNI:
	$(MAKE) TNAME=Attribute NP=1 ftest


