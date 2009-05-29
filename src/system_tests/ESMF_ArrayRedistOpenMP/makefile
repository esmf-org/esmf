# $Id: makefile,v 1.3 2009/05/29 19:52:05 theurich Exp $ 

ifeq ($(ESMF_TESTOPENMP),ON)
ifeq ($(ESMF_OPENMP),OFF)
ESMF_F90COMPILEOPTS = $(ESMF_OPENMP_F90COMPILEOPTS)
ESMF_F90LINKOPTS = $(ESMF_OPENMP_F90LINKOPTS)
endif
endif


ALL: tree_build_system_tests

run: tree_run_system_tests

LOCDIR	  = src/system_tests/ESMF_ArrayRedistOpenMP

ifeq ($(ESMF_TESTOPENMP),ON)
SYSTEM_TESTS_BUILD    = $(ESMC_TESTDIR)/ESMF_ArrayRedistOpenMPSTest
else
SYSTEM_TESTS_BUILD    = 
endif

# Object files other than SysTest%.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ      = user_model1.o user_model2.o user_coupler.o

ifeq ($(ESMF_TESTOPENMP),ON)
SYSTEM_TESTS_RUN      = RUN_ArrayRedistOpenMP
else
SYSTEM_TESTS_RUN      = 
endif
SYSTEM_TESTS_RUN_UNI  =

ESMF_ArrayRedistOpenMPSTest.o : $(SYSTEM_TESTS_OBJ)

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  = $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =


RUN_ArrayRedistOpenMP:
	env MPI_REQUEST_MAX=8192 MPI_MSG_RETRIES=8192 MPI_MSGS_PER_PROC=8192 $(MAKE) TNAME=ArrayRedistOpenMP NP=8 stest

