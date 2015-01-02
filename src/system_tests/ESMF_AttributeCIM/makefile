# $Id$

ALL: tree_build_system_tests

run: tree_run_system_tests

LOCDIR	  = src/system_tests/ESMF_AttributeCIM


SYSTEM_TESTS_BUILD    = $(ESMC_TESTDIR)/ESMF_AttributeCIMSTest

# Object files other than SysTest%.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ      = user_model1.o user_model2.o \
                        user_model3.o user_model4.o user_coupler.o

SYSTEM_TESTS_RUN      = RUN_AttributeCIM

SYSTEM_TESTS_RUN_UNI  = RUN_AttributeCIMUNI


ESMF_AttributeCIMSTest.o : $(SYSTEM_TESTS_OBJ)

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  = $(SYSTEM_TESTS_BUILD) *.xml *.stdout
CLOBBERDIRS =


RUN_AttributeCIM:
	env MPI_REQUEST_MAX=8192 MPI_MSG_RETRIES=8192 MPI_MSGS_PER_PROC=8192 $(MAKE) TNAME=AttributeCIM NP=8 stest

RUN_AttributeCIMUNI:
	$(MAKE) TNAME=AttributeCIM NP=1 stest
