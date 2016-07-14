# $Id$ 


ALL: tree_build_system_tests

run: tree_run_system_tests

LOCDIR	  = src/system_tests/ESMF_CompCreateTwoConcurrentAccWithResNeg


SYSTEM_TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_CompCreateTwoConcurrentAccWithResNegSTest

# Object files other than SysTest%.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ     = user_neg_info.o pet_partitioner.o user_model1.o user_model2.o user_neg.o

SYSTEM_TESTS_RUN     = RUN_CompCreateTwoConcurrentAccWithResNeg

SYSTEM_TESTS_RUN_UNI =


ESMF_CompCreateTwoConcurrentAccWithResNegSTest.o : user_neg_info.o pet_partitioner.o user_model1.o user_model2.o user_neg.o


include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = 
CLEANFILES  = $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =

RUN_CompCreateTwoConcurrentWithResNeg:
	$(MAKE) TNAME=CompCreateTwoConcurrentAccWithResNeg NP=8 stest
