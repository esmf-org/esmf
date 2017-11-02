# $Id$ 

ALL: tree_build_system_tests

run: tree_run_system_tests

LOCDIR	  = src/system_tests/ESMF_MapperTwoConcurrentComps


SYSTEM_TESTS_BUILD    = $(ESMC_TESTDIR)/ESMF_MapperTwoConcurrentCompsSTest

# Object files other than *STest.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ      = clock_utils.o comp_utils.o user_model1.o user_model2.o user_coupler.o

SYSTEM_TESTS_RUN      = RUN_MapperTwoConcurrentComps

SYSTEM_TESTS_RUN_UNI  =

ESMF_MapperTwoConcurrentCompsSTest.o : $(SYSTEM_TESTS_OBJ)

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  = $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =


RUN_MapperTwoConcurrentComps:
	$(MAKE) TNAME=MapperTwoConcurrentComps NP=8 stest

