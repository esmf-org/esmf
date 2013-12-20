# $Id$ 

ALL: tree_build_system_tests

run: tree_run_system_tests

LOCDIR	  = src/system_tests/ESMF_ShareMesh


SYSTEM_TESTS_BUILD    = $(ESMC_TESTDIR)/ESMF_ShareMeshSTest

# Object files other than SysTest%.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ      = user_model1.o user_model2.o user_coupler.o

SYSTEM_TESTS_RUN      = RUN_ShareMesh

SYSTEM_TESTS_RUN_UNI  =

ESMF_ShareMeshSTest.o : $(SYSTEM_TESTS_OBJ)

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  = $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =


RUN_ShareMeshAsItShouldBe:
	$(MAKE) TNAME=ShareMesh NP=6 stest

RUN_ShareMesh:
	$(MAKE) TNAME=ShareMesh NP=4 stest
