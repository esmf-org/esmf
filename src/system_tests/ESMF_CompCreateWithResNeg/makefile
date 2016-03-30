# $Id$ 


ALL: tree_build_system_tests

run: tree_run_system_tests

LOCDIR	  = src/system_tests/ESMF_CompCreateWithResNeg


SYSTEM_TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_CompCreateWithResNegSTest

# Object files other than SysTest%.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ     = user_model.o

SYSTEM_TESTS_RUN     = RUN_CompCreateWithResNeg

SYSTEM_TESTS_RUN_UNI = RUN_CompCreateWithResNegUNI


ESMF_CompCreateWithResNegSTest.o : user_model.o


include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = 
CLEANFILES  = $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =

RUN_CompCreateWithResNeg:
	$(MAKE) TNAME=CompCreateWithResNeg NP=4 stest

RUN_CompCreateWithResNegUNI:
	$(MAKE) TNAME=CompCreateWithResNeg NP=1 stest




