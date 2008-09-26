ALL: tree_build_system_tests

run: tree_run_system_tests

LOCDIR	  = src/system_tests/ESMF_FieldRedistBlk2Blk

SYSTEM_TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_FieldRedistBlk2BlkSTest

# Object files other than %STest.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ     =

SYSTEM_TESTS_RUN     = RUN_FieldRedistBlk2Blk

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = 
CLEANFILES  = $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =

RUN_FieldRedistBlk2Blk:
	$(MAKE) TNAME=FieldRedistBlk2Blk NP=6 stest
