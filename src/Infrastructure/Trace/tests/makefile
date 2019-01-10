# $Id$

ALL:  build_unit_tests
run:  run_unit_tests
run_uni:  run_unit_tests_uni

LOCDIR	  = src/Infrastructure/Trace/tests

.NOTPARALLEL:

TESTS_BUILD = \
		$(ESMF_TESTDIR)/ESMF_TraceClkMonoUTest \
		$(ESMF_TESTDIR)/ESMF_TraceClkMonoSyncUTest \
		$(ESMF_TESTDIR)/ESMF_TraceUTest \
		$(ESMF_TESTDIR)/ESMF_TraceIOUTest \
		$(ESMF_TESTDIR)/ESMF_TraceMPIUTest \
                $(ESMF_TESTDIR)/ESMC_TraceRegionUTest \
		$(ESMF_TESTDIR)/ESMF_ProfileUTest 


TESTS_RUN = \
		RUN_ESMF_TraceUTest \
		RUN_ESMF_TraceClkMonoUTest \
		RUN_ESMF_TraceClkMonoSyncUTest \
		RUN_ESMF_TraceIOUTest \
		RUN_ESMF_TraceMPIUTest \
                RUN_ESMF_TraceRegionUTest \
		RUN_ESMF_ProfileUTest	

TESTS_RUN_UNI = \
		RUN_ESMF_TraceUTestUNI \
		RUN_ESMF_TraceClkMonoUTestUNI \
		RUN_ESMF_TraceClkMonoSyncUTestUNI \
		RUN_ESMF_TraceIOUTestUNI \
		RUN_ESMF_TraceMPIUTestUNI \
		RUN_ESMF_TraceRegionUTestUNI \
		RUN_ESMF_ProfileUTestUNI

include ${ESMF_DIR}/makefile

DIRS        =

CLEANDIRS   =
CLEANFILES  = $(TESTS_BUILD) $(ESMF_TESTDIR)/traceout $(ESMF_TESTDIR)/ESMF_Profile.*
CLOBBERDIRS =

ESMF_TESTTRACE_TARGET = ftest_profile

ifeq ($(ESMF_TESTTRACE),ON)
ESMF_F90COMPILEOPTS += -DESMF_TESTTRACE
# add link options for static builds, if required
ifneq ($(ESMF_TRACE_BUILD_SHARED),ON)
ESMF_F90LINKOPTS += $(ESMF_TRACE_STATICLINKOPTS)
ESMF_F90ESMFLINKLIBS += $(ESMF_TRACE_STATICLINKLIBS)
else
ESMF_TESTTRACE_TARGET = ftest_profile_preload
endif
endif

# --- TraceUTest

ESMF_TraceUTest.o: ESMF_SimpleComp.o
ESMF_UTEST_Trace_OBJS = ESMF_SimpleComp.o

RUN_ESMF_TraceUTest:
	$(MAKE) TNAME=Trace NP=4 ftest_profile

RUN_ESMF_TraceUTestUNI:
	$(MAKE) TNAME=Trace NP=1 ftest_profile

# --- TraceClkMonoUTest

RUN_ESMF_TraceClkMonoUTest:
	env ESMF_RUNTIME_TRACE_CLOCK=MONOTONIC $(MAKE) TNAME=TraceClkMono NP=4 ftest_profile

RUN_ESMF_TraceClkMonoUTestUNI:
	env ESMF_RUNTIME_TRACE_CLOCK=MONOTONIC $(MAKE) TNAME=TraceClkMono NP=1 ftest_profile

# --- TraceClkMonoSyncUTest

RUN_ESMF_TraceClkMonoSyncUTest:
	env ESMF_RUNTIME_TRACE_CLOCK=MONOTONIC_SYNC $(MAKE) TNAME=TraceClkMonoSync NP=4 ftest_profile

RUN_ESMF_TraceClkMonoSyncUTestUNI:
	env ESMF_RUNTIME_TRACE_CLOCK=MONOTONIC_SYNC $(MAKE) TNAME=TraceClkMonoSync NP=1 ftest_profile

# --- TraceIOUTest

RUN_ESMF_TraceIOUTest:
	$(MAKE) TNAME=TraceIO NP=4 $(ESMF_TESTTRACE_TARGET) 

RUN_ESMF_TraceIOUTestUNI:
	$(MAKE) TNAME=TraceIO NP=1 $(ESMF_TESTTRACE_TARGET) 

# --- TraceMPIUTest

RUN_ESMF_TraceMPIUTest:
	env ESMF_RUNTIME_PROFILE=ON $(MAKE) TNAME=TraceMPI NP=4 $(ESMF_TESTTRACE_TARGET) 

RUN_ESMF_TraceMPIUTestUNI:
	$(MAKE) TNAME=TraceMPI NP=1 $(ESMF_TESTTRACE_TARGET)

# --- TraceRegionUTest

RUN_ESMF_TraceRegionUTest:
	$(MAKE) TNAME=TraceRegion NP=4 ctest

RUN_ESMF_TraceRegionUTestUNI:
	$(MAKE) TNAME=TraceRegion NP=1 ctest


# --- ProfileUTest
ESMF_ProfileUTest.o: ESMF_SimpleCompB.o
ESMF_UTEST_Profile_OBJS = ESMF_SimpleCompB.o

RUN_ESMF_ProfileUTest:
	env ESMF_RUNTIME_PROFILE=ON ESMF_RUNTIME_PROFILE_OUTPUT="TEXT BINARY SUMMARY" $(MAKE) TNAME=Profile NP=8 ftest

RUN_ESMF_ProfileUTestUNI:
	$(MAKE) TNAME=Profile NP=1 ftest_profile
