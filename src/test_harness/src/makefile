# $Id$ 

ALL:      build_unit_tests

LOCDIR	  = src/test_harness

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_TestHarnessUTest \
                $(ESMF_TESTDIR)/ESMC_TestHarnessLatexPPUTest


include ${ESMF_DIR}/makefile

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =

ESMF_UTEST_TestHarness_OBJS = ESMF_TestHarnessTypesMod.o ESMF_TestHarnessMod.o ESMF_TestHarnessParser.o ESMF_TestHarnessUtilMod.o ESMF_TestHarnessGridMod.o  ESMF_TestHarnessDistMod.o ESMF_TestHarnessReportMod.o

ESMF_TestHarnessMod.o: ESMF_TestHarnessTypesMod.o  ESMF_TestHarnessUtilMod.o ESMF_TestHarnessReportMod.o

ESMF_TestHarnessReport.o: ESMF_TestHarnessTypesMod.o  ESMF_TestHarnessUtilMod.o

ESMF_TestHarnessParser.o: ESMF_TestHarnessTypesMod.o  ESMF_TestHarnessGridMod.o  ESMF_TestHarnessDistMod.o ESMF_TestHarnessUtilMod.o

ESMF_TestHarnessUTest.o: ESMF_TestHarnessTypesMod.o ESMF_TestHarnessMod.o ESMF_TestHarnessParser.o ESMF_TestHarnessGridMod.o  ESMF_TestHarnessDistMod.o ESMF_TestHarnessReportMod.o 

#ESMC_TestHarnessLatexPPUTest.o : ESMF_TestHarnessLatexPPUTest.o
