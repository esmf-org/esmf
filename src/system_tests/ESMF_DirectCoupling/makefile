# $Id$ 

ALL:      tree_build_system_tests
run:      tree_run_system_tests
run_uni:  tree_run_system_tests_uni

LOCDIR	  = src/system_tests/ESMF_DirectCoupling


SYSTEM_TESTS_BUILD    = $(ESMF_TESTDIR)/ESMF_DirectCouplingSTest

# Object files other than STest%.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
.NOTPARALLEL:
SYSTEM_TESTS_OBJ      = cplComp.o ioComp.o modelAComp.o modelBComp.o modelComp.o

SYSTEM_TESTS_RUN      = RUN_DirectCoupling

SYSTEM_TESTS_RUN_UNI  =

ESMF_DirectCouplingSTest.o : $(SYSTEM_TESTS_OBJ)

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  = $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =


RUN_DirectCoupling:
	$(MAKE) TNAME=DirectCoupling NP=6 stest

