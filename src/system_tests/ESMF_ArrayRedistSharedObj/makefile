# $Id: makefile,v 1.1 2009/01/12 21:37:47 theurich Exp $ 

LOCAL_SHARED_OBJECTS  = user_model1.so user_coupler.so

ALL: tree_build_system_tests $(LOCAL_SHARED_OBJECTS)

run: tree_run_system_tests

LOCDIR	  = src/system_tests/ESMF_ArrayRedistSharedObj


SYSTEM_TESTS_BUILD    = $(ESMC_TESTDIR)/ESMF_ArrayRedistSharedObjSTest

# Object files other than SysTest%.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ      = user_model2.o

SYSTEM_TESTS_RUN      = RUN_ArrayRedistSharedObj

SYSTEM_TESTS_RUN_UNI  =

ESMF_ArrayRedistSharedObjSTest.o : $(SYSTEM_TESTS_OBJ)

.F90.so:
	$(ESMF_F90COMPILER) -fPIC -c $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILEFREECPP) $(ESMF_F90COMPILECPPFLAGS) $<
	$(ESMF_F90LINKER) -shared $(ESMF_F90LINKOPTS) $(ESMF_F90LINKPATHS) $(ESMF_F90LINKRPATHS) -o $@ $*.o $(ESMF_F90ESMFLINKLIBS)


include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  = $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =


RUN_ArrayRedistSharedObj:
	env MPI_REQUEST_MAX=8192 MPI_MSG_RETRIES=8192 MPI_MSGS_PER_PROC=8192 $(MAKE) TNAME=ArrayRedistSharedObj NP=6 stest

