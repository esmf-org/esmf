# $Id: makefile,v 1.2 2009/10/20 23:10:18 svasquez Exp $ 

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
	-@cd $(ESMF_TESTDIR) ; \
	env MPI_REQUEST_MAX=8192 MPI_MSG_RETRIES=8192 MPI_MSGS_PER_PROC=8192 ${ESMF_MPIRUN} -n 8 ./ESMF_EnsembleSTest > ./ESMF_EnsembleConcurrentSTest.stdout ; \
	cat ./PET*EnsembleSTest.Log > ./ESMF_EnsembleSTest.tmpLog ; \
	$(ESMF_RM) ./PET*EnsembleSTest.Log ; \
	env MPI_REQUEST_MAX=8192 MPI_MSG_RETRIES=8192 MPI_MSGS_PER_PROC=8192 ${ESMF_MPIRUN} -n 4 ./ESMF_EnsembleSTest > ./ESMF_EnsembleSequentialSTest.stdout ; \
	cat ./PET*EnsembleSTest.Log > ./ESMF_EnsembleSTest.Log ; \
	cat ./ESMF_EnsembleSTest.tmpLog >> ./ESMF_EnsembleSTest.Log ; \
	$(ESMF_RM) ./PET*EnsembleSTest.Log ; \
	$(ESMF_RM) ./ESMF_EnsembleSTest.tmpLog

