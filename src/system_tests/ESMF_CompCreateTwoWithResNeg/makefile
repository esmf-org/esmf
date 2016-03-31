# $Id$ 


ALL: tree_build_system_tests

run: tree_run_system_tests

LOCDIR	  = src/system_tests/ESMF_CompCreateTwoWithResNeg


SYSTEM_TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_CompCreateTwoWithResNegSTest

# Object files other than SysTest%.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ     = user_model1.o user_model2.o

SYSTEM_TESTS_RUN     = RUN_CompCreateTwoWithResNeg

SYSTEM_TESTS_RUN_UNI = RUN_CompCreateTwoWithResNegUNI


ESMF_CompCreateTwoWithResNegSTest.o : user_model1.o user_model2.o


include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = 
CLEANFILES  = $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =

RUN_CompCreateTwoWithResNeg:
	$(MAKE) TNAME=CompCreateTwoWithResNeg NP=4 stest

RUN_CompCreateTwoWithResNegUNI:
	$(MAKE) TNAME=CompCreateTwoWithResNeg NP=1 stest




