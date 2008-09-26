# $Id: makefile,v 1.10 2008/09/26 04:05:31 cdeluca Exp $ 


ALL: tree_build_system_tests

run: tree_run_system_tests

LOCDIR	  = src/system_tests/ESMF_CompCreate


SYSTEM_TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_CompCreateSTest

# Object files other than SysTest%.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ     = user_model.o

SYSTEM_TESTS_RUN     = RUN_CompCreate

SYSTEM_TESTS_RUN_UNI = RUN_CompCreateUNI


ESMF_CompCreateSTest.o : user_model.o


include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = 
CLEANFILES  = $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =

RUN_CompCreate:
	$(MAKE) TNAME=CompCreate NP=4 stest

RUN_CompCreateUNI:
	$(MAKE) TNAME=CompCreate NP=1 stest




