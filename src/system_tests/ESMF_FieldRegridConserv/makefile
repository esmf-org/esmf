# $Id$ 

ALL: tree_build_system_tests

run: tree_run_system_tests

LOCDIR	  = src/system_tests/ESMF_FieldRegridConserv


SYSTEM_TESTS_BUILD    = $(ESMC_TESTDIR)/ESMF_FieldRegridConsrvSTest

# Object files other than SysTest%.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ      = user_model1.o user_model2.o user_coupler.o

SYSTEM_TESTS_RUN      = RUN_FieldRegridConserv

SYSTEM_TESTS_RUN_UNI  = RUN_FieldRegridConservUNI


ESMF_FieldRegridConsrvSTest.o : $(SYSTEM_TESTS_OBJ)

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  = $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =


RUN_FieldRegridConserv:
	$(MAKE) TNAME=FieldRegridConsrv NP=6 stest

RUN_FieldRegridConservUNI:
	$(MAKE) TNAME=FieldRegridConsrv NP=1 stest



