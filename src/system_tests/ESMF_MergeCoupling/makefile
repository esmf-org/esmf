# $Id: makefile,v 1.6 2008/09/26 04:05:35 cdeluca Exp $ 

ALL: tree_build_system_tests

run: tree_run_system_tests

LOCDIR	  = src/system_tests/ESMF_MergeCoupling


SYSTEM_TESTS_BUILD    = $(ESMC_TESTDIR)/ESMF_MergeCouplingSTest

# Object files other than SysTest%.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ      = user_model1.o user_model2.o user_model3.o user_coupler.o

SYSTEM_TESTS_RUN      = RUN_MergeCoupling  

SYSTEM_TESTS_RUN_UNI  = RUN_MergeCouplingUNI


ESMF_MergeCouplingSTest.o: $(SYSTEM_TESTS_OBJ)

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = 
CLEANFILES  = $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =


RUN_MergeCoupling:
	$(MAKE) TNAME=MergeCoupling NP=4 stest

RUN_MergeCouplingUNI:
	$(MAKE) TNAME=MergeCoupling NP=1 stest




