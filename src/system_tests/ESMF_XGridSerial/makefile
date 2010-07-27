# $Id: makefile,v 1.1 2010/07/27 15:11:33 feiliu Exp $ 

ALL: tree_build_system_tests

run: tree_run_system_tests

LOCDIR	  = src/system_tests/ESMF_XGridSerial


SYSTEM_TESTS_BUILD    = $(ESMC_TESTDIR)/ESMF_XGridSerialSTest

# Object files other than SysTest%.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ      = atmos_comp.o land_comp.o ocean_comp.o coupler_comp.o

SYSTEM_TESTS_RUN      = RUN_XGridSerial

SYSTEM_TESTS_RUN_UNI  =

ESMF_XGridSerialSTest.o : $(SYSTEM_TESTS_OBJ)

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  = $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =


RUN_XGridSerial:
	env MPI_REQUEST_MAX=8192 MPI_MSG_RETRIES=8192 MPI_MSGS_PER_PROC=8192 $(MAKE) TNAME=XGridSerial NP=6 stest

