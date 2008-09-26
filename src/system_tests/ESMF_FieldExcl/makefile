# $Id: makefile,v 1.9 2008/09/26 04:05:33 cdeluca Exp $ 

ALL: tree_build_system_tests

run: tree_run_system_tests

LOCDIR	  = src/system_tests/ESMF_FieldExcl


SYSTEM_TESTS_BUILD    = $(ESMC_TESTDIR)/ESMF_FieldExclSTest

# Object files other than *STest.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ      = user_model1.o user_model2.o user_coupler.o

SYSTEM_TESTS_RUN      = RUN_FieldExcl  

SYSTEM_TESTS_RUN_UNI  = RUN_FieldExclUNI

ESMF_FieldExclSTest.o : $(SYSTEM_TESTS_OBJ)

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  = $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =


RUN_FieldExcl:
	$(MAKE) TNAME=FieldExcl NP=8 stest

RUN_FieldExclUNI:
	$(MAKE) TNAME=FieldExcl NP=1 stest



