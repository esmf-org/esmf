# $Id: makefile,v 1.5 2008/09/26 04:05:33 cdeluca Exp $ 

ALL:      tree_build_system_tests
run:      tree_run_system_tests
run_uni:  tree_run_system_tests_uni

LOCDIR	  = src/system_tests/ESMF_FieldRedistArb2Arb/

SYSTEM_TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_FieldRedistArb2ArbSTest

# Object files other than %STest.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ     =

SYSTEM_TESTS_RUN     = RUN_FieldRedistArb2Arb

SYSTEM_TESTS_RUN_UNI = RUN_FieldRedistArb2ArbUNI


include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = 
CLEANFILES  = $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =

RUN_FieldRedistArb2Arb:
	$(MAKE) TNAME=FieldRedistArb2Arb NP=6 stest

RUN_FieldRedistArb2ArbUNI:
	$(MAKE) TNAME=FieldRedistArb2Arb NP=1 stest



