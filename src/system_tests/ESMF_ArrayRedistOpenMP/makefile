# $Id$ 

ALL: tree_build_system_tests

run: tree_run_system_tests

LOCDIR	  = src/system_tests/ESMF_ArrayRedistOpenMP

SYSTEM_TESTS_BUILD    = $(ESMC_TESTDIR)/ESMF_ArrayRedistOpenMPSTest

# Object files other than SysTest%.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ      = user_model1.o user_model2.o user_coupler.o

SYSTEM_TESTS_RUN      = RUN_ArrayRedistOpenMP

ESMF_ArrayRedistOpenMPSTest.o : $(SYSTEM_TESTS_OBJ)

include $(ESMF_DIR)/makefile

ifeq ($(ESMF_TESTFORCEOPENMP),ON)
ifeq ($(ESMF_OPENMP),OFF)
ESMF_F90COMPILEOPTS += $(ESMF_OPENMP_F90COMPILEOPTS)
ESMF_F90LINKOPTS    += $(ESMF_OPENMP_F90LINKOPTS)
endif
endif

DIRS = 

CLEANDIRS   =
CLEANFILES  = $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =


RUN_ArrayRedistOpenMP:
	env MPI_REQUEST_MAX=8192 MPI_MSG_RETRIES=8192 MPI_MSGS_PER_PROC=8192 $(MAKE) TNAME=ArrayRedistOpenMP NP=8 stest

