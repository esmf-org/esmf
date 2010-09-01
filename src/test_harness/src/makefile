# $Id: makefile,v 1.11 2010/09/01 20:24:59 garyblock Exp $ 

ALL:      build_unit_tests

LOCDIR	  = src/test_harness

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_TestHarnessUTest


include ${ESMF_DIR}/makefile

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =

ESMF_UTEST_TestHarness_OBJS = ESMF_TestHarnessTypesMod.o ESMF_TestHarnessMod.o ESMF_TestHarnessParser.o ESMF_TestHarnessUtilMod.o ESMF_TestHarnessGridMod.o  ESMF_TestHarnessDistMod.o ESMF_TestHarnessReportMod.o

ESMF_TestHarnessMod.o: ESMF_TestHarnessTypesMod.o  ESMF_TestHarnessUtilMod.o ESMF_TestHarnessReportMod.o

ESMF_TestHarnessReport.o: ESMF_TestHarnessTypesMod.o  ESMF_TestHarnessUtilMod.o

ESMF_TestHarnessParser.o: ESMF_TestHarnessTypesMod.o  ESMF_TestHarnessGridMod.o  ESMF_TestHarnessDistMod.o ESMF_TestHarnessUtilMod.o

ESMF_TestHarnessUTest.o: ESMF_TestHarnessTypesMod.o ESMF_TestHarnessMod.o ESMF_TestHarnessParser.o ESMF_TestHarnessGridMod.o  ESMF_TestHarnessDistMod.o ESMF_TestHarnessReportMod.o 
