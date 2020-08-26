# $Id$ 

ALL:      tree_build_system_tests
run:      tree_run_system_tests
run_uni:  tree_run_system_tests_uni

LOCDIR	  = src/system_tests/ESMF_RecursiveComponent


SYSTEM_TESTS_BUILD    = $(ESMC_TESTDIR)/ESMF_RecursiveComponentSTest

# Object files other than STest%.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ      = component.o

SYSTEM_TESTS_RUN      = RUN_RecursiveComponent

SYSTEM_TESTS_RUN_UNI  =


ESMF_RecursiveComponentSTest.o : $(SYSTEM_TESTS_OBJ)

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  = $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =


RUN_RecursiveComponent:
	$(MAKE) TNAME=RecursiveComponent NP=6 stest

