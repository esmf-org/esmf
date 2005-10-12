# $Id: makefile,v 1.10 2005/10/12 19:06:16 nscollins Exp $ 

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
DIRS	  = 
LOCDIR	  = src/Infrastructure/DELayout/tests

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_DELayoutUTest

TESTS_RUN     = RUN_ESMF_DELayoutUTest

TESTS_RUN_UNI = RUN_ESMF_DELayoutUTestUNI


include ${ESMF_DIR}/makefile

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =


#
# DELayout tests
#
RUN_ESMF_DELayoutUTest:
	$(MAKE) TNAME=DELayout NP=4 ftest

RUN_ESMF_DELayoutUTestUNI:
	$(MAKE) TNAME=DELayout NP=1 ftest


