ALL: tree_build_system_tests

run: tree_run_system_tests

LOCDIR	  = src/system_tests/ESMF_FieldBundleRedistBlk2Arb

SYSTEM_TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_FieldBundleRedistBlk2ArbSTest

# Object files other than %STest.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ     =

SYSTEM_TESTS_RUN     = RUN_FieldBundleRedistBlk2Arb

SYSTEM_TESTS_RUN_UNI = RUN_FieldBundleRedistBlk2ArbUNI


include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = 
CLEANFILES  = $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =

RUN_FieldBundleRedistBlk2Arb:
	$(MAKE) TNAME=FieldBundleRedistBlk2Arb NP=6 stest

RUN_FieldBundleRedistBlk2ArbUNI:
	$(MAKE) TNAME=FieldBundleRedistBlk2Arb NP=1 stest



