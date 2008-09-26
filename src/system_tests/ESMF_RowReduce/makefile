# $Id: makefile,v 1.7 2008/09/26 04:05:35 cdeluca Exp $ 

ALL: tree_build_system_tests

run: tree_run_system_tests

LOCDIR	  = src/system_tests/ESMF_RowReduce

SYSTEM_TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_RowReduceSTest

# Object files other than SysTest%.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ     =

SYSTEM_TESTS_RUN     = RUN_RowReduce  

SYSTEM_TESTS_RUN_UNI = RUN_RowReduceUNI


include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = 
CLEANFILES  = $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =

RUN_RowReduce:
	$(MAKE) TNAME=RowReduce NP=2 stest

RUN_RowReduceUNI:
	$(MAKE) TNAME=RowReduce NP=1 stest




