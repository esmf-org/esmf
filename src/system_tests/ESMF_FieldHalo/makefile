# $Id: makefile,v 1.7 2008/09/26 04:05:33 cdeluca Exp $ 

ALL: tree_build_system_tests

run: tree_run_system_tests

LOCDIR	  = src/system_tests/ESMF_FieldHalo


SYSTEM_TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_FieldHaloSTest

# Object files other than SysTest%.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ     =

SYSTEM_TESTS_RUN     = RUN_FieldHalo  

SYSTEM_TESTS_RUN_UNI = RUN_FieldHaloUNI


include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = 
CLEANFILES  = $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =


RUN_FieldHalo:
	$(MAKE) TNAME=FieldHalo NP=4 stest

RUN_FieldHaloUNI:
	$(MAKE) TNAME=FieldHalo NP=1 stest



