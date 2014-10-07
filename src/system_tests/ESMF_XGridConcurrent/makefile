# $Id$ 

ALL: tree_build_system_tests

run: tree_run_system_tests

LOCDIR	  = src/system_tests/ESMF_XGridConcurrent


SYSTEM_TESTS_BUILD    = $(ESMC_TESTDIR)/ESMF_XGridConcurrentSTest

# Object files other than SysTest%.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
.NOTPARALLEL:
SYSTEM_TESTS_OBJ      = util_mod.o atmos_comp.o land_comp.o coupler_comp.o

SYSTEM_TESTS_RUN      = RUN_XGridConcurrent

SYSTEM_TESTS_RUN_UNI  =

ESMF_XGridConcurrentSTest.o : $(SYSTEM_TESTS_OBJ)

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  = $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =


RUN_XGridConcurrent:
	env MPI_REQUEST_MAX=8192 MPI_MSG_RETRIES=8192 MPI_MSGS_PER_PROC=8192 $(MAKE) TNAME=XGridConcurrent NP=8 stest

