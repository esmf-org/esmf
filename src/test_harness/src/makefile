# $Id: makefile,v 1.6 2008/08/13 02:00:39 donstark Exp $ 

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

UTEST_TestHarness_OBJS = ESMF_TestHarnessTypesMod.o ESMF_TestHarnessMod.o ESMF_TestHarnessUtilMod.o ESMF_TestHarnessGridMod.o  ESMF_TestHarnessDistMod.o ESMF_TestHarnessReportMod.o

ESMF_TestHarnessMod.o: ESMF_TestHarnessTypesMod.o  ESMF_TestHarnessUtilMod.o ESMF_TestHarnessReportMod.o

ESMF_TestHarnessUTest.o: ESMF_TestHarnessTypesMod.o ESMF_TestHarnessMod.o ESMF_TestHarnessGridMod.o  ESMF_TestHarnessDistMod.o ESMF_TestHarnessReportMod.o 
