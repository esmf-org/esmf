# $Id: makefile,v 1.4 2008/09/26 04:05:33 cdeluca Exp $ 

ALL: tree_build_system_tests

run: tree_run_system_tests

LOCDIR	  = src/system_tests/ESMF_FieldConcurrentComp


SYSTEM_TESTS_BUILD    = $(ESMC_TESTDIR)/ESMF_FieldConCompSTest

# Object files other than *STest.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ      = user_model1.o user_model2.o user_coupler.o

SYSTEM_TESTS_RUN      = RUN_FieldConComp

SYSTEM_TESTS_RUN_UNI  = 

ESMF_FieldConCompSTest.o : $(SYSTEM_TESTS_OBJ)

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  = $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =


RUN_FieldConComp:
	$(MAKE) TNAME=FieldConComp NP=8 stest
