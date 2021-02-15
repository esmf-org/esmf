# $Id$ 


ALL: tree_build_system_tests

run: tree_run_system_tests
run_uni: tree_run_system_tests_uni

LOCDIR	  = src/system_tests/ESMF_CompFortranAndC


SYSTEM_TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_CompFortranAndCSTest

# Object files other than SysTest%.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ     = user_FortranComponent.o user_CComponent.o

SYSTEM_TESTS_RUN     = RUN_CompFortranAndC

SYSTEM_TESTS_RUN_UNI = RUN_CompFortranAndCUNI


ESMF_CompFortranAndCSTest.o : user_FortranComponent.o user_CComponent.o


include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = 
CLEANFILES  = $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =

RUN_CompFortranAndC:
	cp -r data $(ESMF_TESTDIR)
	chmod u+rw $(ESMF_TESTDIR)/data/*
	$(MAKE) TNAME=CompFortranAndC NP=6 stest

RUN_CompFortranAndCUNI:
	cp -r data $(ESMF_TESTDIR)
	chmod u+rw $(ESMF_TESTDIR)/data/*
	$(MAKE) TNAME=CompFortranAndC NP=1 stest

