# $Id: makefile,v 1.1 2005/07/01 17:26:59 jwolfe Exp $ 

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
LOCDIR	  = src/system_tests/ESMF_ArbitraryDistribution

SYSTEM_TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_ArbitraryDistSTest

# Object files other than %STest.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ     =

SYSTEM_TESTS_RUN     = RUN_ArbitraryDistribution

SYSTEM_TESTS_RUN_UNI = RUN_ArbitraryDistributionUNI


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

RUN_ArbitraryDistribution:
	-${MPIRUN} -np 6 $(ESMC_TESTDIR)/ESMF_ArbitraryDistSTest > $(ESMC_TESTDIR)/ESMF_ArbitraryDistSTest.stdout

RUN_ArbitraryDistributionUNI:
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_ArbitraryDistSTest > $(ESMC_TESTDIR)/ESMF_ArbitraryDistSTest.stdout




