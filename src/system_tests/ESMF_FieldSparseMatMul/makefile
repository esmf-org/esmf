# $Id: makefile,v 1.1.2.1 2008/04/18 23:11:07 svasquez Exp $ 

ALL: tree_build_system_tests

run: tree_run_system_tests

CFLAGS    = 
FFLAGS    =
SOURCEC	  = 
SOURCEF	  =

SOURCEH	  = 
OBJSC	  =  
OBJSF	  = 
MANSEC	  = 
LIBBASE	  = 
LOCDIR	  = src/system_tests/ESMF_FieldSparseMatMul


SYSTEM_TESTS_BUILD    = $(ESMC_TESTDIR)/ESMF_FieldSparseMatMulSTest

# Object files other than SysTest%.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ      = user_model1.o user_model2.o user_coupler.o

SYSTEM_TESTS_RUN      = RUN_FieldSparseMatMul

SYSTEM_TESTS_RUN_UNI  =


ESMF_FieldSparseMatMulSTest.o : $(SYSTEM_TESTS_OBJ)

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  = $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =


RUN_FieldSparseMatMul:
	env MPI_REQUEST_MAX=8192 MPI_MSG_RETRIES=8192 MPI_MSGS_PER_PROC=8192 $(MAKE) TNAME=FieldSparseMatMul NP=6 stest

