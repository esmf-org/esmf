# $Id: makefile,v 1.4 2008/07/23 23:42:15 donstark Exp $ 

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

UTEST_TestHarness_OBJS = ESMF_TestHarnessTypesMod.o ESMF_TestHarnessMod.o ESMF_TestHarnessUtilMod.o ESMF_TestHarnessGridMod.o  ESMF_TestHarnessDistMod.o 

ESMF_TestHarnessMod.o: ESMF_TestHarnessTypesMod.o  ESMF_TestHarnessUtilMod.o

ESMF_TestHarnessUTest.o: ESMF_TestHarnessTypesMod.o ESMF_TestHarnessUtilMod.o ESMF_TestHarnessMod.o ESMF_TestHarnessGridMod.o  ESMF_TestHarnessDistMod.o

