# $Id: makefile,v 1.8 2008/09/26 21:11:40 cdeluca Exp $ 

ALL: tree_build_system_tests

run: tree_run_system_tests

LOCDIR	  = src/system_tests/ESMF_CplOnExclDEs


SYSTEM_TESTS_BUILD    = $(ESMC_TESTDIR)/ESMF_CplOnExclDEsSTest

# Object files other than SysTest%.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ      = user_model1.o user_model2.o user_coupler.o

SYSTEM_TESTS_RUN      = RUN_CouplingOnExclDEs  

SYSTEM_TESTS_RUN_UNI  = RUN_CouplingOnExclDEsUNI


include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  = $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =

ESMF_CplOnExclDEsSTest.o:  user_model1.o user_model2.o user_coupler.o

RUN_CouplingOnExclDEs:
	$(MAKE) TNAME=CplOnExclDEs NP=8 stest

RUN_CouplingOnExclDEsUNI:
	$(MAKE) TNAME=CplOnExclDEs NP=1 stest




