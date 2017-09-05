# $Id$ 

ALL: tree_build_system_tests

run: tree_run_system_tests

LOCDIR	  = src/system_tests/ESMF_Trace


SYSTEM_TESTS_BUILD    = $(ESMC_TESTDIR)/ESMF_TraceSTest

# Object files other than SysTest%.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ      = user_model1.o user_model2.o user_coupler.o

SYSTEM_TESTS_RUN      = RUN_Trace

SYSTEM_TESTS_RUN_UNI  =

ESMF_TraceSTest.o : $(SYSTEM_TESTS_OBJ)

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  = $(SYSTEM_TESTS_BUILD) $(ESMC_TESTDIR)/traceout
CLOBBERDIRS =


RUN_Trace:
	rm -rf $(ESMC_TESTDIR)/traceout
	env ESMF_RUNTIME_TRACE=ON $(MAKE) TNAME=Trace NP=8 stest

