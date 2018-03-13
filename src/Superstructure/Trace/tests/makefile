# $Id$

ALL:  build_unit_tests
run:  run_unit_tests
run_uni:  run_unit_tests_uni

LOCDIR	  = src/Superstructure/Trace/tests

.NOTPARALLEL:

TESTS_BUILD = \
		$(ESMF_TESTDIR)/ESMF_TraceClkMonoUTest \
		$(ESMF_TESTDIR)/ESMF_TraceClkMonoSyncUTest \
		$(ESMF_TESTDIR)/ESMF_TraceUTest \
		$(ESMF_TESTDIR)/ESMF_TraceIOUTest \

TESTS_RUN = \
		RUN_ESMF_TraceUTest \
		RUN_ESMF_TraceClkMonoUTest \
		RUN_ESMF_TraceClkMonoSyncUTest \
		RUN_ESMF_TraceIOUTest \

TESTS_RUN_UNI = \
		RUN_ESMF_TraceUTestUNI \
		RUN_ESMF_TraceClkMonoUTestUNI \
		RUN_ESMF_TraceClkMonoSyncUTestUNI \
		RUN_ESMF_TraceIOUTest \

include ${ESMF_DIR}/makefile

DIRS        =

CLEANDIRS   =
CLEANFILES  = $(TESTS_BUILD) $(ESMF_TESTDIR)/traceout
CLOBBERDIRS =


# --- TraceUTest

ESMF_TraceUTest.o: ESMF_SimpleComp.o
ESMF_UTEST_Trace_OBJS = ESMF_SimpleComp.o

RUN_ESMF_TraceUTest:
	rm -rf $(ESMF_TESTDIR)/traceout
	env ESMF_RUNTIME_TRACE=ON $(MAKE) TNAME=Trace NP=4 ftest
	ls -la $(TESTS_BUILD) $(ESMF_TESTDIR)/traceout/*

RUN_ESMF_TraceUTestUNI:
	rm -rf $(ESMF_TESTDIR)/traceout
	env ESMF_RUNTIME_TRACE=ON $(MAKE) TNAME=Trace NP=1 ftest
	ls -la $(TESTS_BUILD) $(ESMF_TESTDIR)/traceout/*

# --- TraceClkMonoUTest

RUN_ESMF_TraceClkMonoUTest:
	rm -rf $(ESMF_TESTDIR)/traceout
	env ESMF_RUNTIME_TRACE=ON ESMF_RUNTIME_TRACE_CLOCK=MONOTONIC $(MAKE) TNAME=TraceClkMono NP=4 ftest
	ls -la $(TESTS_BUILD) $(ESMF_TESTDIR)/traceout/*

RUN_ESMF_TraceClkMonoUTestUNI:
	rm -rf $(ESMF_TESTDIR)/traceout
	env ESMF_RUNTIME_TRACE=ON ESMF_RUNTIME_TRACE_CLOCK=MONOTONIC $(MAKE) TNAME=TraceClkMono NP=1 ftest
	ls -la $(TESTS_BUILD) $(ESMF_TESTDIR)/traceout/*

# --- TraceClkMonoSyncUTest

RUN_ESMF_TraceClkMonoSyncUTest:
	rm -rf $(ESMF_TESTDIR)/traceout
	env ESMF_RUNTIME_TRACE=ON ESMF_RUNTIME_TRACE_CLOCK=MONOTONIC_SYNC $(MAKE) TNAME=TraceClkMonoSync NP=4 ftest
	ls -la $(TESTS_BUILD) $(ESMF_TESTDIR)/traceout/*

RUN_ESMF_TraceClkMonoSyncUTestUNI:
	rm -rf $(ESMF_TESTDIR)/traceout
	env ESMF_RUNTIME_TRACE=ON ESMF_RUNTIME_TRACE_CLOCK=MONOTONIC_SYNC $(MAKE) TNAME=TraceClkMonoSync NP=1 ftest
	ls -la $(TESTS_BUILD) $(ESMF_TESTDIR)/traceout/*

# --- TraceIOUTest

RUN_ESMF_TraceIOUTest:
	rm -rf $(ESMF_TESTDIR)/traceout
	env ESMF_RUNTIME_TRACE=ON $(MAKE) TNAME=TraceIO NP=4 ftest_preload
	ls -la $(TESTS_BUILD) $(ESMF_TESTDIR)/traceout/*

RUN_ESMF_TraceIOUTestUNI:
	rm -rf $(ESMF_TESTDIR)/traceout
	env ESMF_RUNTIME_TRACE=ON $(MAKE) TNAME=TraceIO NP=1 ftest_preload
	ls -la $(TESTS_BUILD) $(ESMF_TESTDIR)/traceout/*

# --- customized target for testing with LD_PRELOAD --- #
ESMF_LDPRELOAD=$(ESMF_LDIR)/libesmftrace_preload.$(ESMF_SL_SUFFIX)

ftest_preload:
	-@cd $(ESMF_TESTDIR) ; \
	$(ESMF_RM) ./PET*$(TNAME)UTest.Log ; \
	if [ $(ESMF_BATCHDEPRECATED) = "true" ] ; then \
	  echo  $(ESMF_MPIRUN) -np $(NP) $(ESMF_TOOLRUN) ./ESMF_$(TNAME)UTest ; \
	  $(ESMF_MPIRUN) -np $(NP) $(ESMF_TOOLRUN) ./ESMF_$(TNAME)UTest ; \
	else \
	  echo env LD_PRELOAD=$(ESMF_LDPRELOAD) $(ESMF_MPIRUN) -np $(NP) $(ESMF_TOOLRUN) ./ESMF_$(TNAME)UTest 1\> ./ESMF_$(TNAME)UTest.stdout 2\>\&1 ; \
	  env LD_PRELOAD=$(ESMF_LDPRELOAD) $(ESMF_MPIRUN) -np $(NP) $(ESMF_TOOLRUN) ./ESMF_$(TNAME)UTest 1> ./ESMF_$(TNAME)UTest.stdout 2>&1 ; \
	fi ; \
	cat ./PET*$(TNAME)UTest.Log > ./ESMF_$(TNAME)UTest.Log ; \
	$(ESMF_RM) ./PET*$(TNAME)UTest.Log
