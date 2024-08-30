# $Id$ 


ALL: tree_build_system_tests

run: tree_run_system_tests
run_uni: tree_run_system_tests_uni

LOCDIR	  = src/system_tests/ESMF_CFieldRegrid


SYSTEM_TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_CFieldRegridSTest

# Object files other than SysTest%.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ     = 

SYSTEM_TESTS_RUN     = RUN_CFieldRegrid

SYSTEM_TESTS_RUN_UNI = RUN_CFieldRegridUNI

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = 
CLEANFILES  = $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =

RUN_CFieldRegrid:
	$(MAKE) TNAME=CFieldRegrid NP=4 stest

RUN_CFieldRegridUNI:
	$(MAKE) TNAME=CFieldRegrid NP=1 stest

