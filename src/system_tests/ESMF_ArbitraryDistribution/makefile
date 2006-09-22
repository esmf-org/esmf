# $Id: makefile,v 1.3 2006/09/22 23:55:45 theurich Exp $ 

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



