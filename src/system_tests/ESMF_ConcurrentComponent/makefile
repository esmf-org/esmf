# $Id$ 

ALL: tree_build_system_tests

run: tree_run_system_tests

LOCDIR	  = src/system_tests/ESMF_ConcurrentComponent


SYSTEM_TESTS_BUILD    = $(ESMF_TESTDIR)/ESMF_ConcurrentCompSTest

# Object files other than *STest.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ      = user_model1.o user_model2.o user_coupler.o

SYSTEM_TESTS_RUN      = RUN_ConcurrentComp

SYSTEM_TESTS_RUN_UNI  =

ESMF_ConcurrentCompSTest.o : $(SYSTEM_TESTS_OBJ)

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  = $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =


RUN_ConcurrentComp:
	$(MAKE) TNAME=ConcurrentComp NP=8 stest

