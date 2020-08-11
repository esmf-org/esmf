# $Id$ 

# share the symbol table of the main executable to allow dynamic
# linking against its symbols
ESMF_F90LINKOPTS = $(ESMF_SO_F90LINKOPTSEXE)

ALL: tree_build_system_tests

run: tree_run_system_tests

LOCDIR	  = src/system_tests/ESMF_ArrayRedistSharedObj


ifeq ($(ESMF_TESTSHAREDOBJ),ON)
SYSTEM_TESTS_BUILD    = $(ESMF_TESTDIR)/ESMF_ArrayRedistSharedObjSTest
SYSTEM_TESTS_RUN      = RUN_ArrayRedistSharedObj
else
SYSTEM_TESTS_BUILD    =
SYSTEM_TESTS_RUN      =
endif

SYSTEM_TESTS_RUN_UNI  =

# Object files other than STest%.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ      = user_model2.o
SYSTEM_TESTS_SHOBJ    = user_model1 user_coupler


ESMF_ArrayRedistSharedObjSTest.o : $(SYSTEM_TESTS_OBJ)

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  = $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =


RUN_ArrayRedistSharedObj:
	cp -f *.$(ESMF_SL_SUFFIX) $(ESMF_TESTDIR)
	$(MAKE) TNAME=ArrayRedistSharedObj NP=6 stest
