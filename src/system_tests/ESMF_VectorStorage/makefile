# $Id: makefile,v 1.2 2005/03/01 21:09:10 nscollins Exp $ 

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
LOCDIR	  = src/system_tests/ESMF_VectorStorage

SYSTEM_TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_VectorStorageSTest

# Object files other than %STest.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ     =

SYSTEM_TESTS_RUN     = RUN_VectorStorage

SYSTEM_TESTS_RUN_UNI = RUN_VectorStorageUNI


include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = 
CLEANFILES  = $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =

RUN_VectorStorage:
	-${MPIRUN} -np 6 $(ESMC_TESTDIR)/ESMF_VectorStorageSTest > $(ESMC_TESTDIR)/ESMF_VectorStorageSTest.stdout

RUN_VectorStorageUNI:
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_VectorStorageSTest > $(ESMC_TESTDIR)/ESMF_VectorStorageSTest.stdout




