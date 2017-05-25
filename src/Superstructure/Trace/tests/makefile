# $Id$

ALL:  build_unit_tests
run:  run_unit_tests
run_uni:  run_unit_tests_uni

LOCDIR	  = src/Superstructure/Trace/tests

.NOTPARALLEL:

TESTS_BUILD = \
		$(ESMF_TESTDIR)/ESMF_TraceUTest \

TESTS_RUN = \
		RUN_ESMF_TraceUTest \

TESTS_RUN_UNI = \
		RUN_ESMF_TraceUTestUNI \

include ${ESMF_DIR}/makefile

DIRS        =

CLEANDIRS   =
CLEANFILES  = $(TESTS_BUILD) $(ESMF_TESTDIR)/traceout
CLOBBERDIRS =


# --- TraceUTest

RUN_ESMF_TraceUTest:
#	cp -f $(ESMF_DIR)/src/Superstructure/AttributeAPI/tests/data/baseline_Array002.xml $(ESMF_TESTDIR)
#	cp -f $(ESMF_DIR)/src/Superstructure/AttributeAPI/tests/data/baseline_Array002.stdout $(ESMF_TESTDIR)
	rm -rf $(ESMF_TESTDIR)/traceout
	env ESMF_RUNTIME_TRACE=ON $(MAKE) TNAME=Trace NP=4 ftest

RUN_ESMF_TraceUTestUNI:
#	cp -f $(ESMF_DIR)/src/Superstructure/AttributeAPI/tests/data/baseline_Array002.xml $(ESMF_TESTDIR)
#	cp -f $(ESMF_DIR)/src/Superstructure/AttributeAPI/tests/data/baseline_Array002.stdout $(ESMF_TESTDIR)
	rm -rf $(ESMF_TESTDIR)/traceout
	env ESMF_RUNTIME_TRACE=ON $(MAKE) TNAME=Trace NP=1 ftest

ESMF_TraceUTest.o: ESMF_SimpleComp.o	
ESMF_UTEST_Trace_OBJS = ESMF_SimpleComp.o
