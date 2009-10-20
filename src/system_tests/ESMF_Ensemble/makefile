# $Id: makefile,v 1.1 2009/10/20 17:41:38 peggyli Exp $ 

ALL: tree_build_system_tests

run: tree_run_system_tests

LOCDIR	  = src/system_tests/ESMF_Ensemble


SYSTEM_TESTS_BUILD    = $(ESMC_TESTDIR)/ESMF_EnsembleSTest

# Object files other than SysTest%.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ      = user_modelA.o user_modelB.o user_modelC.o user_coupler.o

SYSTEM_TESTS_RUN      = RUN_Ensemble

ESMF_EnsembleSTest.o : $(SYSTEM_TESTS_OBJ)

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  = $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =


RUN_Ensemble:
	env MPI_REQUEST_MAX=8192 MPI_MSG_RETRIES=8192 MPI_MSGS_PER_PROC=8192 $(MAKE) TNAME=Ensemble NP=8 stest
	env MPI_REQUEST_MAX=8192 MPI_MSG_RETRIES=8192 MPI_MSGS_PER_PROC=8192 $(MAKE) TNAME=Ensemble NP=4 stest

