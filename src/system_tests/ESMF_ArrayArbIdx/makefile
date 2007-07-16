# $Id: makefile,v 1.3 2007/07/16 19:36:27 theurich Exp $ 

ALL: tree_build_system_tests

run: tree_run_system_tests
run_uni: tree_run_system_tests_uni

CFLAGS    = 
FFLAGS    =
SOURCEC	  = 
SOURCEF	  =

SOURCEH	  = 
OBJSC	  =  
OBJSF	  = 
MANSEC	  = 
LIBBASE	  = 
LOCDIR	  = src/system_tests/ESMF_ArrayArbIdx


SYSTEM_TESTS_BUILD    = $(ESMC_TESTDIR)/ESMF_ArrayArbIdxSTest

# Object files other than SysTest%.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ      = 

SYSTEM_TESTS_RUN      = RUN_ArrayArbIdx

SYSTEM_TESTS_RUN_UNI  = RUN_ArrayArbIdxUNI


ESMF_ArrayArbIdxSTest.o : $(SYSTEM_TESTS_OBJ)

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  = $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =


RUN_ArrayArbIdx:
	env MPI_REQUEST_MAX=8192 MPI_MSG_RETRIES=8192 MPI_MSGS_PER_PROC=8192 $(MAKE) TNAME=ArrayArbIdx NP=6 stest

RUN_ArrayArbIdxUNI:
	$(MAKE) TNAME=ArrayArbIdx NP=1 stest
