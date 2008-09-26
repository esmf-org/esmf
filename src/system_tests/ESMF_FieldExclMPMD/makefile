# $Id: makefile,v 1.4 2008/09/26 04:05:33 cdeluca Exp $ 

ALL: tree_build_system_tests

run: tree_run_system_tests

LOCDIR	  = src/system_tests/ESMF_FieldExclMPMD


SYSTEM_TESTS_BUILD    = $(ESMC_TESTDIR)/ESMF_FieldExclMPMDSTestA $(ESMC_TESTDIR)/ESMF_FieldExclMPMDSTestB MPMDCLEANUP

# Object files other than *STest.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ_A     = user_model1.o user_coupler.o
SYSTEM_TESTS_OBJ_B     = user_model2.o user_coupler.o

SYSTEM_TESTS_RUN      = RUN_FieldExclMPMD  

SYSTEM_TESTS_RUN_UNI  = RUN_FieldExclUNI

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  = $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =


RUN_FieldExclMPMD:
	-${MPIMPMDRUN} -np 1 $(ESMC_TESTDIR)/ESMF_FieldExclMPMDSTestA : -np 1 $(ESMC_TESTDIR)/ESMF_FieldExclMPMDSTestB : -np 1 $(ESMC_TESTDIR)/ESMF_FieldExclMPMDSTestA : -np 1 $(ESMC_TESTDIR)/ESMF_FieldExclMPMDSTestB : -np 1 $(ESMC_TESTDIR)/ESMF_FieldExclMPMDSTestA : -np 1 $(ESMC_TESTDIR)/ESMF_FieldExclMPMDSTestB : -np 2 $(ESMC_TESTDIR)/ESMF_FieldExclMPMDSTestA >$(ESMC_TESTDIR)/ESMF_FieldExclMPMDSTest.stdout

RUN_FieldExclUNI:
	$(MAKE) TNAME=FieldExcl NP=1 stest



