# $Id: makefile,v 1.9 2005/07/15 03:41:46 nscollins Exp $

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
LOCDIR	  =  src/Infrastructure/Config/tests

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_ConfigUTest
#	        $(ESMF_TESTDIR)/ESMC_ConfigUTest 

TESTS_RUN     = RUN_ESMF_ConfigUTest
#		RUN_ESMC_ConfigUTest 

TESTS_RUN_UNI = RUN_ESMF_ConfigUTestUNI
#		RUN_ESMC_ConfigUTestUNI 




include ${ESMF_DIR}/makefile

DIRS	  = 

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =

#
# ESMF_ConfigUTest
#
RUN_ESMF_ConfigUTest:
	$(MAKE) TNAME=Config NP=4 ftest

RUN_ESMF_ConfigUTestUNI:
	$(MAKE) TNAME=Config NP=1 ftest


#
# ESMC_ConfigUTest
#
RUN_ESMC_ConfigUTest:
	$(MAKE) TNAME=Config NP=4 ctest

RUN_ESMC_ConfigUTestUNI:
	$(MAKE) TNAME=Config NP=1 ctest



