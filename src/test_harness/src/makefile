# $Id: makefile,v 1.1 2007/09/28 17:22:00 donstark Exp $ 

ALL:      build_unit_tests

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
LOCDIR	  = src/test_harness

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_TestHarnessUTest


include ${ESMF_DIR}/makefile

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =

UTEST_TestHarness_OBJS = ESMF_TestHarnessMod.o

ESMF_TestHarnessUTest.o: ESMF_TestHarnessMod.o
