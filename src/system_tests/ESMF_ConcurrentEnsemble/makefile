# $Id: makefile,v 1.4 2009/10/22 03:25:27 svasquez Exp $ 

ALL: tree_build_system_tests

run: tree_run_system_tests

LOCDIR	  = src/system_tests/ESMF_ConcurrentEnsemble


SYSTEM_TESTS_BUILD    = $(ESMC_TESTDIR)/ESMF_ConcurrentEnsembleSTest

# Object files other than SysTest%.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ      = user_modelA.o user_modelB.o user_modelC.o user_coupler.o

SYSTEM_TESTS_RUN      = RUN_ConcurrentEnsemble

ESMF_ConcurrentEnsembleSTest.o : $(SYSTEM_TESTS_OBJ)

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  = $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =


RUN_ConcurrentEnsemble:
	env MPI_REQUEST_MAX=8192 MPI_MSG_RETRIES=8192 MPI_MSGS_PER_PROC=8192 $(MAKE) TNAME=ConcurrentEnsemble NP=8 stest


