# $Id: makefile,v 1.3 2009/03/25 00:09:57 theurich Exp $ 

# share the symbol table of the main executable to allow dynamic
# linking against its symbols
ESMF_F90LINKOPTS = $(ESMF_SO_F90LINKOPTSEXE)

ALL: tree_build_system_tests

run: tree_run_system_tests

LOCDIR	  = src/system_tests/ESMF_ArrayRedistSharedObj


SYSTEM_TESTS_BUILD    = $(ESMC_TESTDIR)/ESMF_ArrayRedistSharedObjSTest

# Object files other than STest%.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ      = user_model2.o
SYSTEM_TESTS_SHOBJ    = user_model1 user_coupler

SYSTEM_TESTS_RUN      = RUN_ArrayRedistSharedObj

SYSTEM_TESTS_RUN_UNI  =

ESMF_ArrayRedistSharedObjSTest.o : $(SYSTEM_TESTS_OBJ)

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  = $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =


RUN_ArrayRedistSharedObj:
	env MPI_REQUEST_MAX=8192 MPI_MSG_RETRIES=8192 MPI_MSGS_PER_PROC=8192 $(MAKE) TNAME=ArrayRedistSharedObj NP=6 stest
