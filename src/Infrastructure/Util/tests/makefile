# $Id: makefile,v 1.3 2005/07/15 03:41:47 nscollins Exp $ 

ALL:  build_unit_tests
run:  tree_run_unit_tests

CFLAGS    =
FFLAGS    =
SOURCEC	  = 
SOURCEF	  =

SOURCEH	  = 
OBJSC	  =  
OBJSF	  = 
MANSEC	  = 
LIBBASE	  = 
LOCDIR	  = src/Infrastructure/Util/tests

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_UtilUTest

TESTS_RUN     = RUN_ESMF_UtilUTest

TESTS_RUN_UNI = RUN_ESMF_UtilUTestUNI


include ${ESMF_DIR}/makefile

DIRS        =

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =


#
#  Util class
#

RUN_ESMF_UtilUTest:
	$(MAKE) TNAME=Util NP=4 ftest

RUN_ESMF_UtilUTestUNI:
	$(MAKE) TNAME=Util NP=1 ftest


