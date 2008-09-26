# $Id: makefile,v 1.21 2008/09/26 21:11:37 cdeluca Exp $ 

ALL:      build_unit_tests
run:      run_unit_tests
run_uni:  run_unit_tests_uni

LOCDIR	  = src/Infrastructure/FieldComm/tests

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_FieldGatherUTest \
	$(ESMF_TESTDIR)/ESMF_FieldRedistUTest \
	$(ESMF_TESTDIR)/ESMF_FieldSMMUTest
               
TESTS_RUN     = RUN_ESMF_FieldGatherUTest \
	RUN_ESMF_FieldRedistUTest \
	RUN_ESMF_FieldSMMUTest
                
TESTS_RUN_UNI = 

include ${ESMF_DIR}/makefile

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD) $(CPFILES)
CLOBBERDIRS =

DIRS        = 

#
# Field
#

RUN_ESMF_FieldGatherUTest:
	$(MAKE) TNAME=FieldGather NP=4 ftest

RUN_ESMF_FieldRedistUTest:
	$(MAKE) TNAME=FieldRedist NP=4 ftest

RUN_ESMF_FieldSMMUTest:
	$(MAKE) TNAME=FieldSMM NP=4 ftest
