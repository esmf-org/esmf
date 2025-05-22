# $Id$ 

ALL:      build_unit_tests
run:      run_unit_tests
run_uni:  run_unit_tests_uni

LOCDIR	  = src/Superstructure/Component/tests

.NOTPARALLEL:
TESTS_BUILD   =	$(ESMF_TESTDIR)/ESMF_InternalStateUTest

TESTS_RUN     =	RUN_ESMF_InternalStateUTest

TESTS_RUN_UNI =	RUN_ESMF_InternalStateUTestUNI


include $(ESMF_DIR)/makefile

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =

DIRS        = 


#
# InternalState
#
RUN_ESMF_InternalStateUTest:
	$(MAKE) TNAME=InternalState NP=4 ftest

RUN_ESMF_InternalStateUTestUNI:
	$(MAKE) TNAME=InternalState NP=1 ftest
