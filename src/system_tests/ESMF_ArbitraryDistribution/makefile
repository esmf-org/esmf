# $Id: makefile,v 1.2.2.1 2006/08/18 22:33:07 theurich Exp $ 

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

DIRS = 

CLEANDIRS   = 
CLEANFILES  = $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =

RUN_ArbitraryDistribution:
	$(MAKE) TNAME=ArbitraryDist NP=6 stest

RUN_ArbitraryDistributionUNI:
	$(MAKE) TNAME=ArbitraryDist NP=1 stest



