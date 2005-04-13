# $Id: makefile,v 1.3 2005/04/13 23:03:23 nscollins Exp $ 

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

# The Darwin (Mac OS X), xlf 8.1 compiler cannot compile user_model1.F90
# with the -g flag (produces an internal compiler error).  removing the -g
# allows the file to build.
ifeq ($(ESMF_ARCH),Darwin)
ifeq ($(ESMF_COMPILER),xlf)
ifeq ($(ESMF_BOPT),g)
G_FFLAGS =
endif
endif
endif

DIRS = 

CLEANDIRS   = 
CLEANFILES  = $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =

RUN_VectorStorage:
	-${MPIRUN} -np 6 $(ESMC_TESTDIR)/ESMF_VectorStorageSTest > $(ESMC_TESTDIR)/ESMF_VectorStorageSTest.stdout

RUN_VectorStorageUNI:
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_VectorStorageSTest > $(ESMC_TESTDIR)/ESMF_VectorStorageSTest.stdout




