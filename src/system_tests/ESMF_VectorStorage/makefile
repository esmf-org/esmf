# $Id: makefile,v 1.1 2004/12/17 19:43:36 jwolfe Exp $ 

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

SYSTEM_TESTS_RM      = VectorStorage.rm

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = coredir.*
CLEANFILES  = *.o *.mod *.txt core ESMF_LogFile $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =

RUN_VectorStorage:
	-@rm -f testlog stdlog.* logfile.* VectorStorage.tmps
	-${MPIRUN} -np 6 $(ESMC_TESTDIR)/ESMF_VectorStorageSTest > $(ESMC_TESTDIR)/ESMF_VectorStorageSTest.stdout

RUN_VectorStorageUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_VectorStorageSTest.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_VectorStorageSTest > $(ESMC_TESTDIR)/ESMF_VectorStorageSTest.stdout

VectorStorage.rm:
	echo "VectorStorage.rm target called here."



