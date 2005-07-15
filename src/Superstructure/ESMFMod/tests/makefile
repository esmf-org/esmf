# $Id: makefile,v 1.10 2005/07/15 03:41:48 nscollins Exp $

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
LOCDIR	  = src/Superstructure/ESMFMod/tests

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_ESMFModUTest \
	        $(ESMF_TESTDIR)/ESMC_ESMFModUTest 

TESTS_RUN     = RUN_ESMF_ESMFModUTest \
		RUN_ESMC_ESMFModUTest 

TESTS_RUN_UNI = RUN_ESMF_ESMFModUTestUNI \
		RUN_ESMC_ESMFModUTestUNI 


include ${ESMF_DIR}/makefile

DIRS	  = 

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =

#
# Fortran unit test
#
RUN_ESMF_ESMFModUTest:
	$(MAKE) TNAME=ESMFMod NP=4 ftest

RUN_ESMF_ESMFModUTestUNI:
	$(MAKE) TNAME=ESMFMod NP=1 ftest


#
# C++ Unit tests
#

RUN_ESMC_ESMFModUTest:
	$(MAKE) TNAME=ESMFMod NP=4 ctest

RUN_ESMC_ESMFModUTestUNI:
	$(MAKE) TNAME=ESMFMod NP=1 ctest




