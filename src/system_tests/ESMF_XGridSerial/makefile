# $Id$ 

ALL: tree_build_system_tests

run: tree_run_system_tests
run_uni: tree_run_system_tests_uni

LOCDIR	  = src/system_tests/ESMF_XGridSerial


SYSTEM_TESTS_BUILD    = $(ESMC_TESTDIR)/ESMF_XGridSerialSTest

# Object files other than SysTest%.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
.NOTPARALLEL:
SYSTEM_TESTS_OBJ      = util_mod.o atmos_comp.o land_comp.o ocean_comp.o coupler_comp.o

SYSTEM_TESTS_RUN      = RUN_XGridSerial

SYSTEM_TESTS_RUN_UNI  = RUN_XGridSerialUNI

ESMF_XGridSerialSTest.o : $(SYSTEM_TESTS_OBJ)

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  = $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =


RUN_XGridSerial:
	$(MAKE) TNAME=XGridSerial NP=4 stest

RUN_XGridSerialUNI:
	$(MAKE) TNAME=XGridSerial NP=1 stest

