# $Id: makefile,v 1.20 2005/10/12 19:06:16 nscollins Exp $ 

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
LOCDIR	  = src/Infrastructure/Base/tests

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_BaseUTest

TESTS_RUN     = RUN_ESMF_BaseUTest

TESTS_RUN_UNI = RUN_ESMF_BaseUTestUNI


include ${ESMF_DIR}/makefile

DIRS        =

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =


#
#  Base class
#

RUN_ESMF_BaseUTest:
	$(MAKE) TNAME=Base NP=4 ftest

RUN_ESMF_BaseUTestUNI:
	$(MAKE) TNAME=Base NP=1 ftest


