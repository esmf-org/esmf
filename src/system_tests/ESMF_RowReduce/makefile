# $Id: makefile,v 1.5 2005/03/01 21:09:02 nscollins Exp $ 

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
LOCDIR	  = src/system_tests/ESMF_RowReduce

SYSTEM_TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_RowReduceSTest

# Object files other than SysTest%.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ     =

SYSTEM_TESTS_RUN     = RUN_RowReduce  

SYSTEM_TESTS_RUN_UNI = RUN_RowReduceUNI


include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = 
CLEANFILES  = $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =

RUN_RowReduce:
	-${MPIRUN} -np 2 $(ESMC_TESTDIR)/ESMF_RowReduceSTest > $(ESMC_TESTDIR)/ESMF_RowReduceSTest.stdout

RUN_RowReduceUNI:
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_RowReduceSTest > $(ESMC_TESTDIR)/ESMF_RowReduceSTest.stdout




