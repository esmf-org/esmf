# $Id$ 


ALL: tree_build_system_tests

run: tree_run_system_tests
run_uni: tree_run_system_tests_uni

LOCDIR	  = src/system_tests/ESMC_FieldRegrid


SYSTEM_TESTS_BUILD   = $(ESMC_TESTDIR)/ESMC_FieldRegridSTest

# Object files other than SysTest%.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ     = 

SYSTEM_TESTS_RUN     = RUN_ESMC_FieldRegrid

SYSTEM_TESTS_RUN_UNI = RUN_ESMC_FieldRegridUNI

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = 
CLEANFILES  = $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =

RUN_ESMC_FieldRegrid:
	$(MAKE) TNAME=FieldRegrid NP=4 sctest

RUN_ESMC_FieldRegridUNI:
	$(MAKE) TNAME=FieldRegrid NP=1 sctest

