# $Id$

ALL:  build_unit_tests
run:  run_unit_tests
run_uni: run_unit_tests_uni

LOCDIR	  = src/Superstructure/ESMFMod/tests

TESTS_BUILD   =	$(ESMF_TESTDIR)/ESMF_FrameworkUTest

TESTS_RUN     =	RUN_ESMF_FramworkUTest

TESTS_RUN_UNI =	RUN_ESMF_FramworkUTestUNI

include $(ESMF_DIR)/makefile

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =

DIRS        = 

#
# unit test targets
#

RUN_ESMF_FramworkUTest:
	$(MAKE) TNAME=Framework NP=4 ftest

RUN_ESMF_FramworkUTestUNI:
	$(MAKE) TNAME=Framework NP=1 ftest
