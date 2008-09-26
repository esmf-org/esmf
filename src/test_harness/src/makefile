# $Id: makefile,v 1.8 2008/09/26 04:17:37 cdeluca Exp $ 

ALL:      build_unit_tests

LOCDIR	  = src/test_harness

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_TestHarnessUTest


include ${ESMF_DIR}/makefile

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =

ESMF_UTEST_TestHarness_OBJS = ESMF_TestHarnessTypesMod.o ESMF_TestHarnessMod.o ESMF_TestHarnessUtilMod.o ESMF_TestHarnessGridMod.o  ESMF_TestHarnessDistMod.o ESMF_TestHarnessReportMod.o

ESMF_TestHarnessMod.o: ESMF_TestHarnessTypesMod.o  ESMF_TestHarnessUtilMod.o ESMF_TestHarnessReportMod.o

ESMF_TestHarnessUTest.o: ESMF_TestHarnessTypesMod.o ESMF_TestHarnessMod.o ESMF_TestHarnessGridMod.o  ESMF_TestHarnessDistMod.o ESMF_TestHarnessReportMod.o 
