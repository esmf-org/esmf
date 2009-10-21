# $Id: makefile,v 1.3 2009/10/21 22:30:00 feiliu Exp $ 

ALL: tree_build_system_tests

run: tree_run_system_tests

LOCDIR	  = src/system_tests/ESMF_FieldMeshSMM


SYSTEM_TESTS_BUILD    = $(ESMC_TESTDIR)/ESMF_FieldMeshSMMSTest

# Object files other than SysTest%.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ      = user_model1.o user_model2.o user_coupler.o

SYSTEM_TESTS_RUN      = RUN_FieldMeshSMM  

SYSTEM_TESTS_RUN_UNI  = 


ESMF_FieldMeshSMMSTest.o : $(SYSTEM_TESTS_OBJ)

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  = $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =


RUN_FieldMeshSMM:
	$(MAKE) TNAME=FieldMeshSMM NP=4 stest
