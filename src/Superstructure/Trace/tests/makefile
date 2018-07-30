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
		$(ESMF_TESTDIR)/ESMF_TraceMPIUTest 

TESTS_RUN = \
		RUN_ESMF_TraceUTest \
		RUN_ESMF_TraceClkMonoUTest \
		RUN_ESMF_TraceClkMonoSyncUTest \
		RUN_ESMF_TraceIOUTest \
		RUN_ESMF_TraceMPIUTest 

TESTS_RUN_UNI = \
		RUN_ESMF_TraceUTestUNI \
		RUN_ESMF_TraceClkMonoUTestUNI \
		RUN_ESMF_TraceClkMonoSyncUTestUNI \
		RUN_ESMF_TraceIOUTestUNI \
		RUN_ESMF_TraceMPIUTestUNI 

include ${ESMF_DIR}/makefile

PRELOADSCRIPT = $(ESMF_TESTDIR)/preload.sh

DIRS        =

CLEANDIRS   =
CLEANFILES  = $(TESTS_BUILD) $(ESMF_TESTDIR)/traceout $(PRELOADSCRIPT)
CLOBBERDIRS =

ESMF_TESTTRACE_TARGET = ftest

ifeq ($(ESMF_TESTTRACE),ON)
ESMF_F90COMPILEOPTS += -DESMF_TESTTRACE
# add link options for static builds, if required
ifneq ($(ESMF_TRACE_BUILD_SHARED),ON)
ESMF_F90LINKOPTS += $(ESMF_TRACE_STATICLINKOPTS)
ESMF_F90ESMFLINKLIBS += $(ESMF_TRACE_STATICLINKLIBS)
else
ESMF_TESTTRACE_TARGET = ftest_preload
endif
endif

# --- TraceUTest

ESMF_TraceUTest.o: ESMF_SimpleComp.o
ESMF_UTEST_Trace_OBJS = ESMF_SimpleComp.o

RUN_ESMF_TraceUTest:
	rm -rf $(ESMF_TESTDIR)/traceout
	env ESMF_RUNTIME_TRACE=ON $(MAKE) TNAME=Trace NP=4 ftest

RUN_ESMF_TraceUTestUNI:
	rm -rf $(ESMF_TESTDIR)/traceout
	env ESMF_RUNTIME_TRACE=ON $(MAKE) TNAME=Trace NP=1 ftest

# --- TraceClkMonoUTest

RUN_ESMF_TraceClkMonoUTest:
	rm -rf $(ESMF_TESTDIR)/traceout
	env ESMF_RUNTIME_TRACE=ON ESMF_RUNTIME_TRACE_CLOCK=MONOTONIC $(MAKE) TNAME=TraceClkMono NP=4 ftest

RUN_ESMF_TraceClkMonoUTestUNI:
	rm -rf $(ESMF_TESTDIR)/traceout
	env ESMF_RUNTIME_TRACE=ON ESMF_RUNTIME_TRACE_CLOCK=MONOTONIC $(MAKE) TNAME=TraceClkMono NP=1 ftest

# --- TraceClkMonoSyncUTest

RUN_ESMF_TraceClkMonoSyncUTest:
	rm -rf $(ESMF_TESTDIR)/traceout
	env ESMF_RUNTIME_TRACE=ON ESMF_RUNTIME_TRACE_CLOCK=MONOTONIC_SYNC $(MAKE) TNAME=TraceClkMonoSync NP=4 ftest

RUN_ESMF_TraceClkMonoSyncUTestUNI:
	rm -rf $(ESMF_TESTDIR)/traceout
	env ESMF_RUNTIME_TRACE=ON ESMF_RUNTIME_TRACE_CLOCK=MONOTONIC_SYNC $(MAKE) TNAME=TraceClkMonoSync NP=1 ftest

# --- TraceIOUTest

RUN_ESMF_TraceIOUTest:
	rm -rf $(ESMF_TESTDIR)/traceout
	env ESMF_RUNTIME_TRACE=ON $(MAKE) TNAME=TraceIO NP=4 $(ESMF_TESTTRACE_TARGET) 

RUN_ESMF_TraceIOUTestUNI:
	rm -rf $(ESMF_TESTDIR)/traceout
	env ESMF_RUNTIME_TRACE=ON $(MAKE) TNAME=TraceIO NP=1 $(ESMF_TESTTRACE_TARGET) 

# --- TraceMPIUTest

RUN_ESMF_TraceMPIUTest:
	rm -rf $(ESMF_TESTDIR)/traceout
	env ESMF_RUNTIME_TRACE=ON $(MAKE) TNAME=TraceMPI NP=4 $(ESMF_TESTTRACE_TARGET) 

RUN_ESMF_TraceMPIUTestUNI:
	rm -rf $(ESMF_TESTDIR)/traceout
	env ESMF_RUNTIME_TRACE=ON $(MAKE) TNAME=TraceMPI NP=1 $(ESMF_TESTTRACE_TARGET)


ifneq ($(ESMF_OS),Darwin)
ENV_PRELOAD = LD_PRELOAD
else
ENV_PRELOAD = DYLD_INSERT_LIBRARIES
endif

# MPT implementations do not pick up LD_PRELOAD
# so we pass a small script to each MPI task
ifneq (,$(findstring mpich,$(ESMF_COMM)))
ESMF_PRELOAD_SH = ./preload.sh
endif
ifeq ($(ESMF_COMM),mpi)
ESMF_PRELOAD_SH = ./preload.sh
endif

# --- customized target for testing with LD_PRELOAD --- #
ftest_preload: $(PRELOADSCRIPT)
	-@cd $(ESMF_TESTDIR) ; \
	$(ESMF_RM) ./PET*$(TNAME)UTest.Log ; \
	if [ -z $(ESMF_PRELOAD_SH) ] ; then \
	  echo env $(ENV_PRELOAD)=$(ESMF_TRACE_LDPRELOAD) $(ESMF_MPIRUN) -np $(NP) ./ESMF_$(TNAME)UTest 1\> ./ESMF_$(TNAME)UTest.stdout 2\>\&1 ; \
	  env $(ENV_PRELOAD)=$(ESMF_TRACE_LDPRELOAD) $(ESMF_MPIRUN) -np $(NP) ./ESMF_$(TNAME)UTest 1> ./ESMF_$(TNAME)UTest.stdout 2>&1 ; \
	else \
	  echo $(ESMF_MPIRUN) -np $(NP) $(ESMF_PRELOAD_SH) ./ESMF_$(TNAME)UTest 1\> ./ESMF_$(TNAME)UTest.stdout 2\>\&1 ; \
	  $(ESMF_MPIRUN) -np $(NP) $(ESMF_PRELOAD_SH) ./ESMF_$(TNAME)UTest 1> ./ESMF_$(TNAME)UTest.stdout 2>&1 ; \
	fi ; \
	cat ./PET*$(TNAME)UTest.Log > ./ESMF_$(TNAME)UTest.Log ; \
	$(ESMF_RM) ./PET*$(TNAME)UTest.Log

$(PRELOADSCRIPT):
	-@cd $(ESMF_TESTDIR)
	-@echo "#!/bin/sh" > $(PRELOADSCRIPT)
	-@echo "# Script to preload ESMF dynamic trace library" >> $(PRELOADSCRIPT)
	-@echo 'env LD_PRELOAD="$$LD_PRELOAD $(ESMF_TRACE_LDPRELOAD)" $$*' >> $(PRELOADSCRIPT)
	chmod 755 $@
