# $Id: makefile,v 1.2 2008/09/26 04:05:32 cdeluca Exp $ 


ALL: tree_build_system_tests

run: tree_run_system_tests

LOCDIR	  = src/system_tests/ESMF_CompInFortranAndC


SYSTEM_TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_CompFortranAndCSTest

# Object files other than SysTest%.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ     = user_FortranComponent.o user_CComponent.o

SYSTEM_TESTS_RUN     = RUN_CompFortranAndC

SYSTEM_TESTS_RUN_UNI = RUN_CompFortranAndCUNI


ESMF_CompFortranAndCSTest.o : user_FortranComponent.o user_CComponent.o


include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = 
CLEANFILES  = $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =

RUN_CompFortranAndC:
	$(MAKE) TNAME=CompFortranAndC NP=4 stest

RUN_CompFortranAndCUNI:
	$(MAKE) TNAME=CompFortranAndC NP=1 stest




