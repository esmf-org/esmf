# $Id$ 


ALL: tree_build_system_tests

run: tree_run_system_tests

LOCDIR	  = src/system_tests/ESMF_CompCreateThreeConcurrentAccWithResNegOpenCL


SYSTEM_TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_CompCreateThreeConcurrentAccWithResNegOpenCLSTest

# Object files other than SysTest%.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ     = user_neg_info.o pet_partitioner.o user_model1.o user_model2.o user_model3.o user_neg.o opencl_mmul2d.o ser_mmul2d.o

SYSTEM_TESTS_RUN     = RUN_CompCreateThreeConcurrentAccWithResNegOpenCL

SYSTEM_TESTS_RUN_UNI =


ESMF_CompCreateThreeConcurrentAccWithResNegOpenCLSTest.o : user_neg_info.o pet_partitioner.o user_model1.o user_model2.o user_model3.o user_neg.o opencl_mmul2d.o ser_mmul2d.o


include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = 
CLEANFILES  = $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =

RUN_CompCreateThreeConcurrentWithResNegOpenCL:
	$(MAKE) TNAME=CompCreateThreeConcurrentAccWithResNegOpenCL NP=3 stest
